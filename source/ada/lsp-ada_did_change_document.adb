------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2026, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Containers;

with GNATCOLL.Traces; use GNATCOLL.Traces;
with GNATCOLL.VFS;

with Libadalang;
with Libadalang.Analysis;
with LSP.Ada_Documents;
with LSP.Ada_Documents.Semantic_Diagnostics;
with LSP.Client_Message_Receivers;
with LSP.Structures;
with LSP.Server_Notifications.DidChange;

package body LSP.Ada_Did_Change_Document is

   Did_Change_Indexing : constant Trace_Handle :=
     Create ("ALS.DID_CHANGE.INDEXING", On);
   --  Should be disabled when running the testsuite

   type Did_Change_Job
     (Parent : not null access constant Ada_Did_Change_Handler)
   is limited new LSP.Server_Jobs.Server_Job with record
      Message : LSP.Server_Messages.Server_Message_Access;
   end record;

   type Did_Change_Job_Access is access all Did_Change_Job;

   overriding
   function Priority
     (Self : Did_Change_Job) return LSP.Server_Jobs.Job_Priority
   is (LSP.Server_Jobs.Fence);

   overriding
   procedure Execute
     (Self   : in out Did_Change_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   overriding
   procedure Complete
     (Self : in out Did_Change_Job;
      Next : LSP.Server_Messages.Server_Message_Access);

   overriding
   function Message
     (Self : Did_Change_Job) return LSP.Server_Messages.Server_Message_Access
   is (Self.Message);

   function Is_Incremental
     (Changes : LSP.Structures.TextDocumentContentChangeEvent_Vector)
      return Boolean
   is (not Changes.Is_Empty and then Changes.First_Element.a_range.Is_Set);
   --  Return True when the first change edits a range of the document.

   procedure Apply_Changes_And_Rebase_Diagnostics
     (Document : not null LSP.Ada_Documents.Document_Access;
      Params   : LSP.Structures.DidChangeTextDocumentParams);
   --  Apply the edits and move saved diagnostics to their new positions.

   ------------------------------------------
   -- Apply_Changes_And_Rebase_Diagnostics --
   ------------------------------------------

   procedure Apply_Changes_And_Rebase_Diagnostics
     (Document : not null LSP.Ada_Documents.Document_Access;
      Params   : LSP.Structures.DidChangeTextDocumentParams)
   is
      package Diagnostics renames LSP.Ada_Documents.Semantic_Diagnostics;

      Source : constant Diagnostics.Semantic_Diagnostic_Source_Access :=
        Diagnostics.Semantic_Diagnostic_Source_Access
          (Document.Semantic_Diagnostic_Source);
   begin
      Document.Apply_Changes
        (Params.textDocument.version, Params.contentChanges);
      Source.Rebase_Diagnostics (Params.contentChanges);
   end Apply_Changes_And_Rebase_Diagnostics;

   --------------
   -- Complete --
   --------------

   overriding
   procedure Complete
     (Self : in out Did_Change_Job;
      Next : LSP.Server_Messages.Server_Message_Access)
   is
      use type Ada.Containers.Count_Type;
      use type LSP.Server_Messages.Server_Message_Access;
      use type GNATCOLL.VFS.Virtual_File;
      use type LSP.Ada_Documents.Document_Access;

      Message : LSP.Server_Notifications.DidChange.Notification renames
        LSP.Server_Notifications.DidChange.Notification (Self.Message.all);

      Changes : LSP.Structures.TextDocumentContentChangeEvent_Vector renames
        Message.Params.contentChanges;

      File     : constant GNATCOLL.VFS.Virtual_File :=
        Self.Parent.Context.To_File (Message.Params.textDocument.uri);
      Document : constant LSP.Ada_Documents.Document_Access :=
        Self.Parent.Context.Get_Open_Document
          (Message.Params.textDocument.uri);

   begin
      if Next /= null
        and then
          Next.all in LSP.Server_Notifications.DidChange.Notification'Class
      then
         declare
            Change : LSP.Server_Notifications.DidChange.Notification renames
              LSP.Server_Notifications.DidChange.Notification (Next.all);

            Next_File : constant GNATCOLL.VFS.Virtual_File :=
              Self.Parent.Context.To_File (Change.Params.textDocument.uri);
         begin
            if File = Next_File then
               --  Another change to this file is already waiting. Apply it
               --  before reparsing, indexing or checking for errors. For a
               --  full text replacement, only the latest text is needed.
               return;
            end if;
         end;
      end if;

      if Document = null then
         return;
      end if;

      if not Is_Incremental (Changes) then
         Apply_Changes_And_Rebase_Diagnostics (Document, Message.Params);
      end if;

      --  The edits may have changed the symbols in this document.
      LSP.Ada_Documents.Reset_Symbol_Cache (Document.all);
      --  Reparse the file in every context that uses it, so later requests
      --  see the updated text.
      for Context of Self.Parent.Context.Contexts_For_File (File) loop
         declare
            Dummy : Libadalang.Analysis.Analysis_Unit;
         begin
            Dummy := Context.Get_AU (File, Reparse => True);
         end;
      end loop;
      --  Update the index in a separate job at the lowest priority.
      if Did_Change_Indexing.Is_Active then
         Self.Parent.Context.Enqueue_Indexing (File);
      end if;

      --  Publish any diagnostics that are ready now.
      Self.Parent.Context.Publish_Diagnostics (Document);

      --  Schedule semantic checks for the updated text.
      declare
         Ranges : LSP.Structures.Range_Vector;
      begin
         if Changes.Length > 1 then
            --  Later edits can move text changed by earlier edits. For
            --  example, adding an error on line 8 then inserting six lines
            --  above it puts the error on line 14. The first range still
            --  points to line 8, so it could make us check the wrong code.
            --  Check all open files: changed declarations may also cause
            --  errors in other files that use them.
            Self.Parent.Context.Enqueue_Semantic_Diagnostics;
         else
            --  A single ranged edit gets a quick local check first.
            if Is_Incremental (Changes) then
               Ranges.Append (Changes.First_Element.a_range.Value);
            end if;

            Self.Parent.Context.Enqueue_Semantic_Diagnostics
              (Document => Document, Ranges => Ranges);
         end if;
      end;
   end Complete;

   ----------------
   -- Create_Job --
   ----------------

   overriding
   function Create_Job
     (Self    : Ada_Did_Change_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
      return LSP.Server_Jobs.Server_Job_Access
   is
      Result : constant Did_Change_Job_Access :=
        new Did_Change_Job'
          (Parent => Self'Unchecked_Access, Message => Message);
   begin
      return LSP.Server_Jobs.Server_Job_Access (Result);
   end Create_Job;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute
     (Self   : in out Did_Change_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is
      use type LSP.Ada_Documents.Document_Access;

      Message : LSP.Server_Notifications.DidChange.Notification renames
        LSP.Server_Notifications.DidChange.Notification (Self.Message.all);

      Changes : LSP.Structures.TextDocumentContentChangeEvent_Vector renames
        Message.Params.contentChanges;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Self.Parent.Context.Get_Open_Document
          (Message.Params.textDocument.uri);
   begin
      Status := LSP.Server_Jobs.Done;

      if Document = null then
         return;
      end if;

      if Is_Incremental (Changes) then
         --  Apply these edits now: the next change may use their result.
         --  Complete will reparse, index and check the updated document.
         Apply_Changes_And_Rebase_Diagnostics (Document, Message.Params);
      end if;
   exception
      when E : others =>
         Self.Parent.Context.Trace_Exception (E);
   end Execute;

end LSP.Ada_Did_Change_Document;

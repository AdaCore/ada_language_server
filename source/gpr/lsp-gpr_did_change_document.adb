------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2024, AdaCore                     --
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

with GNATCOLL.VFS;

with LSP.GPR_Files;
with LSP.GPR_Documents;
with LSP.Client_Message_Receivers;
with LSP.Structures;
with LSP.Server_Notifications.DidChange;

package body LSP.GPR_Did_Change_Document is

   type Did_Change_Job
     (Parent : not null access constant GPR_Did_Change_Handler)
   is limited new LSP.Server_Jobs.Server_Job with record
      Document : LSP.GPR_Documents.Document_Access;
      Message  : LSP.Server_Messages.Server_Message_Access;
   end record;

   type Did_Change_Job_Access is access all Did_Change_Job;

   overriding function Priority
     (Self : Did_Change_Job) return LSP.Server_Jobs.Job_Priority is
       (LSP.Server_Jobs.Fence);

   overriding procedure Execute
     (Self   : in out Did_Change_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   overriding procedure Complete
     (Self : in out Did_Change_Job;
      Next : LSP.Server_Messages.Server_Message_Access);

   overriding function Message (Self : Did_Change_Job)
     return LSP.Server_Messages.Server_Message_Access is (Self.Message);

   function Is_Incremental
     (Changes : LSP.Structures.TextDocumentContentChangeEvent_Vector)
       return Boolean is
         (not Changes.Is_Empty and then Changes.First_Element.a_range.Is_Set);
   --  Changes has incrimental form (not just full text update)

   --------------
   -- Complete --
   --------------

   overriding procedure Complete
     (Self : in out Did_Change_Job;
      Next : LSP.Server_Messages.Server_Message_Access)
   is
      use type LSP.Server_Messages.Server_Message_Access;
      use type GNATCOLL.VFS.Virtual_File;

      Message : LSP.Server_Notifications.DidChange.Notification renames
        LSP.Server_Notifications.DidChange.Notification (Self.Message.all);

      Changes : LSP.Structures.TextDocumentContentChangeEvent_Vector renames
        Message.Params.contentChanges;

      File : constant GNATCOLL.VFS.Virtual_File :=
        Self.Parent.Context.To_File (Message.Params.textDocument.uri);

   begin
      if Next /= null and then
        Next.all in LSP.Server_Notifications.DidChange.Notification'Class
      then
         declare
            Change : LSP.Server_Notifications.DidChange.Notification renames
              LSP.Server_Notifications.DidChange.Notification (Next.all);

            Next_File  : constant GNATCOLL.VFS.Virtual_File :=
              Self.Parent.Context.To_File (Change.Params.textDocument.uri);
         begin
            if File = Next_File then
               --  However, we should skip the Indexing part (and
               --  non-incremental changes) if the next didChange message
               --  will re-change the text document.
               return;
            end if;
         end;
      end if;

      if not Is_Incremental (Changes) then
         Self.Document.Apply_Changes
           (Message.Params.textDocument.version, Changes);
      end if;

      --  Load gpr tree & prepare diagnostics

      Self.Document.Load;

      --  Build GPR file for LSP needs.

      LSP.GPR_Files.Parse_Modified_Document
        (File_Provider => Self.Parent.Context,
         Path          => Self.Parent.Context.To_File
           (Message.Params.textDocument.uri));

      --  Emit diagnostics
      Self.Parent.Context.Publish_Diagnostics (Self.Document);
   end Complete;

   ----------------
   -- Create_Job --
   ----------------

   overriding function Create_Job
     (Self    : GPR_Did_Change_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
      return LSP.Server_Jobs.Server_Job_Access
   is
      Value : LSP.Server_Notifications.DidChange.Notification renames
        LSP.Server_Notifications.DidChange.Notification (Message.all);

      URI : LSP.Structures.DocumentUri renames
        Value.Params.textDocument.uri;

      Document : constant LSP.GPR_Documents.Document_Access :=
        Self.Context.Get_Open_Document (URI);

      Result : constant Did_Change_Job_Access :=
        new Did_Change_Job'
          (Parent   => Self'Unchecked_Access,
           Document => Document,
           Message  => Message);
   begin
      return LSP.Server_Jobs.Server_Job_Access (Result);
   end Create_Job;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : in out Did_Change_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is
      use type LSP.GPR_Documents.Document_Access;

      Message : LSP.Server_Notifications.DidChange.Notification renames
        LSP.Server_Notifications.DidChange.Notification (Self.Message.all);

      Changes : LSP.Structures.TextDocumentContentChangeEvent_Vector renames
        Message.Params.contentChanges;
   begin
      Status := LSP.Server_Jobs.Done;

      if Self.Document = null then
         return;
      end if;

      if Is_Incremental (Changes) then
         --  If we are applying incremental changes, we can't skip the
         --  call to Apply_Changes, since this would break synchronization.
         Self.Document.Apply_Changes
           (Message.Params.textDocument.version, Changes);

         --  However, we should skip the Indexing part if the next didChange
         --  message will re-change the text document.
      end if;

      --  Rest of the work in Complete routine
   exception
      when E : others =>
         Self.Parent.Context.Trace_Exception (E);
   end Execute;

end LSP.GPR_Did_Change_Document;

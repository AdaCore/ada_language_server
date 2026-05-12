------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2024, AdaCore                       --
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

with Ada.Unchecked_Deallocation;

with GNATCOLL.VFS;

with Libadalang.Analysis;
with Libadalang.Iterators;

with LSP.Ada_Context_Sets;
with LSP.Ada_Documents;
with LSP.Ada_Highlighters;
with LSP.Ada_Request_Jobs;
with LSP.Client_Message_Receivers;
with LSP.Server_Requests.Tokens_Full;
with LSP.Structures;

package body LSP.Ada_Tokens_Full is

   type Traverse_Iterator_Access is access
     Libadalang.Iterators.Traverse_Iterator'Class;

   procedure Free is new Ada.Unchecked_Deallocation
     (Libadalang.Iterators.Traverse_Iterator'Class, Traverse_Iterator_Access);

   type Tokens_Full_Job
     (Parent : not null access constant Ada_Tokens_Full_Handler) is limited
   new LSP.Ada_Request_Jobs.Ada_Request_Job
     (Priority => LSP.Server_Jobs.Lowest)
   with record
      Unit             : Libadalang.Analysis.Analysis_Unit;
      Cursor           : Traverse_Iterator_Access;
      Holder           : LSP.Ada_Highlighters.Highlights_Holder;
      Document_Version : LSP.Structures.Integer_Or_Null;
      --  LSP version of the document when this job was created.  If the
      --  document is edited between two Execute calls the version will
      --  differ and the job is cancelled to avoid working on stale data.
   end record;

   overriding procedure Execute_Ada_Request
     (Self   : in out Tokens_Full_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   type Tokens_Full_Job_Access is access all Tokens_Full_Job;

   ----------------
   -- Create_Job --
   ----------------

   overriding function Create_Job
     (Self    : Ada_Tokens_Full_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
        return LSP.Server_Jobs.Server_Job_Access
   is
      use LSP.Ada_Documents;

      Value : LSP.Structures.SemanticTokensParams
        renames LSP.Server_Requests.Tokens_Full.Request
          (Message.all).Params;

      File : constant GNATCOLL.VFS.Virtual_File :=
        Self.Context.To_File (Value.textDocument.uri);

      Context : constant LSP.Ada_Context_Sets.Context_Access :=
        Self.Context.Get_Best_Context (Value.textDocument.uri);

      Unit : constant Libadalang.Analysis.Analysis_Unit :=
        Context.Get_AU (File);

      Document : constant LSP.Ada_Documents.Document_Access :=
        Self.Context.Get_Open_Document (Value.textDocument.uri);

      Job : constant Tokens_Full_Job_Access :=
        (new Tokens_Full_Job'
           (Parent           => Self'Unchecked_Access,
            Request          => LSP.Ada_Request_Jobs.Request_Access (Message),
            Unit             => Unit,
            Cursor           => new Libadalang.Iterators.Traverse_Iterator'Class'
              (Libadalang.Iterators.Find
                 (Unit.Root, LSP.Ada_Highlighters.Need_Highlighting)),
            Holder           => <>,
            Document_Version =>
              (if Document /= null
               then Document.Identifier.version
               else (Is_Null => True))));

   begin
      LSP.Ada_Highlighters.Initialize (Job.Holder, Unit);
      return LSP.Server_Jobs.Server_Job_Access (Job);
   end Create_Job;

   -------------------------
   -- Execute_Ada_Request --
   -------------------------

   overriding procedure Execute_Ada_Request
     (Self   : in out Tokens_Full_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is
      use LSP.Ada_Documents;
      use LSP.Structures;

      Message : LSP.Server_Requests.Tokens_Full.Request
        renames LSP.Server_Requests.Tokens_Full.Request (Self.Message.all);

      Element : Libadalang.Analysis.Ada_Node;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Self.Parent.Context.Get_Open_Document
          (Message.Params.textDocument.uri);
   begin
      --  If the document was edited since this job was created, a newer
      --  tokens-full job will be enqueued.  Discard this one to avoid
      --  highlighting a stale unit and sending an outdated response.
      if Document = null
        or else Document.Identifier.version /= Self.Document_Version
      then
         Free (Self.Cursor);
         Status := LSP.Server_Jobs.Done;
         return;
      end if;

      Status := LSP.Server_Jobs.Continue;

      for J in 1 .. 300 loop
         if Self.Cursor.Next (Element) then
            Self.Parent.Context.Get_Highlighter.Highlight_Node
              (Self.Holder, Element);

         else
            declare
               Response : LSP.Structures.SemanticTokens_Or_Null
                 (Is_Null => False);
            begin

               Self.Parent.Context.Get_Highlighter.Get_Result
                 (Self.Holder, Self.Unit, Response.Value.data);

               Client.On_Tokens_Full_Response (Message.Id, Response);

               Free (Self.Cursor);

               Status := LSP.Server_Jobs.Done;

               exit;
            end;
         end if;
      end loop;
   end Execute_Ada_Request;

end LSP.Ada_Tokens_Full;

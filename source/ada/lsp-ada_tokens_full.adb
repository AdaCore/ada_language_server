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
with LSP.Ada_Highlighters;
with LSP.Client_Message_Receivers;
with LSP.Server_Request_Jobs;
with LSP.Server_Requests.Tokens_Full;
with LSP.Structures;

package body LSP.Ada_Tokens_Full is

   type Traverse_Iterator_Access is access
     Libadalang.Iterators.Traverse_Iterator'Class;

   procedure Free is new Ada.Unchecked_Deallocation
     (Libadalang.Iterators.Traverse_Iterator'Class, Traverse_Iterator_Access);

   type Tokens_Full_Job
     (Parent : not null access constant Ada_Tokens_Full_Handler) is limited
   new LSP.Server_Request_Jobs.Server_Request_Job
     (Priority => LSP.Server_Jobs.Low)
   with record
      Unit   : Libadalang.Analysis.Analysis_Unit;
      Cursor : Traverse_Iterator_Access;
      Holder : LSP.Ada_Highlighters.Highlights_Holder;
   end record;

   overriding procedure Execute_Request
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
      Value : LSP.Structures.SemanticTokensParams
        renames LSP.Server_Requests.Tokens_Full.Request
          (Message.all).Params;

      File : constant GNATCOLL.VFS.Virtual_File :=
        Self.Context.To_File (Value.textDocument.uri);

      Context : constant LSP.Ada_Context_Sets.Context_Access :=
        Self.Context.Get_Best_Context (Value.textDocument.uri);

      Unit : constant Libadalang.Analysis.Analysis_Unit :=
        Context.Get_AU (File);

      Job : constant Tokens_Full_Job_Access :=
        (new Tokens_Full_Job'
           (Parent  => Self'Unchecked_Access,
            Request => LSP.Server_Request_Jobs.Request_Access (Message),
            Unit    => Unit,
            Cursor  => new Libadalang.Iterators.Traverse_Iterator'Class'
              (Libadalang.Iterators.Find
                 (Unit.Root, LSP.Ada_Highlighters.Need_Highlighting)),
            Holder   => <>));

   begin
      LSP.Ada_Highlighters.Initialize (Job.Holder, Unit);
      return LSP.Server_Jobs.Server_Job_Access (Job);
   end Create_Job;

   ---------------------
   -- Execute_Request --
   ---------------------

   overriding procedure Execute_Request
     (Self   : in out Tokens_Full_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is
      Message : LSP.Server_Requests.Tokens_Full.Request
        renames LSP.Server_Requests.Tokens_Full.Request (Self.Message.all);

      Element : Libadalang.Analysis.Ada_Node;
   begin
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
   end Execute_Request;

end LSP.Ada_Tokens_Full;

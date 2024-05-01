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

with GNATCOLL.VFS;

with Libadalang.Analysis;

with LSP.Ada_Context_Sets;
with LSP.Ada_Request_Jobs;
with LSP.Client_Message_Receivers;
with LSP.Server_Requests.Tokens_Range;
with LSP.Structures;

package body LSP.Ada_Tokens_Range is

   type Ada_Tokens_Range_Job
     (Parent : not null access constant Ada_Tokens_Range_Handler) is limited
   new LSP.Ada_Request_Jobs.Ada_Request_Job
     (Priority => LSP.Server_Jobs.High)
        with null record;

   type Ada_Tokens_Range_Job_Access is access all Ada_Tokens_Range_Job;

   overriding procedure Execute_Ada_Request
     (Self   : in out Ada_Tokens_Range_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   ----------------
   -- Create_Job --
   ----------------

   overriding function Create_Job
     (Self    : Ada_Tokens_Range_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
      return LSP.Server_Jobs.Server_Job_Access
   is
      Result : constant Ada_Tokens_Range_Job_Access :=
        new Ada_Tokens_Range_Job'
          (Parent  => Self'Unchecked_Access,
           Request => LSP.Ada_Request_Jobs.Request_Access (Message));
   begin
      return LSP.Server_Jobs.Server_Job_Access (Result);
   end Create_Job;

   -------------------------
   -- Execute_Ada_Request --
   -------------------------

   overriding procedure Execute_Ada_Request
     (Self   : in out Ada_Tokens_Range_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is
      Message : LSP.Server_Requests.Tokens_Range.Request
        renames LSP.Server_Requests.Tokens_Range.Request (Self.Message.all);

      Response : LSP.Structures.SemanticTokens_Or_Null (Is_Null => False);

      URI : LSP.Structures.DocumentUri renames
        Message.Params.textDocument.uri;

      Context : constant LSP.Ada_Context_Sets.Context_Access :=
        Self.Parent.Context.Get_Best_Context (URI);

      File : constant GNATCOLL.VFS.Virtual_File :=
        Self.Parent.Context.To_File (URI);

      Unit : constant Libadalang.Analysis.Analysis_Unit :=
        Context.Get_AU (File);

      Result : LSP.Structures.Natural_Vector renames
        Response.Value.data;
   begin
      Status := LSP.Server_Jobs.Done;

      if Unit.Root.Is_Null then
         Client.On_Tokens_Range_Response (Message.Id, (Is_Null => True));

         return;
      end if;

      Result :=
        Self.Parent.Context.Get_Highlighter.Get_Tokens
          (Unit, Context.Tracer.all, Message.Params.a_range);

      Client.On_Tokens_Range_Response (Message.Id, Response);
   end Execute_Ada_Request;

end LSP.Ada_Tokens_Range;

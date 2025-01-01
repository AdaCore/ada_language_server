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

with Libadalang;
with Libadalang.Analysis;
with LSP.Ada_Context_Sets;
with LSP.Ada_Request_Jobs;
with LSP.Client_Message_Receivers;
with LSP.Server_Requests.SelectionRange;
with LSP.Structures;

package body LSP.Ada_Selection_Range is

   type Ada_Selection_Range_Job
     (Parent : not null access constant Ada_Selection_Range_Handler) is limited
   new LSP.Ada_Request_Jobs.Ada_Request_Job
     (Priority => LSP.Server_Jobs.High)
        with null record;

   overriding procedure Execute_Ada_Request
     (Self   : in out Ada_Selection_Range_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   type Ada_Selection_Range_Job_Access is access all Ada_Selection_Range_Job;

   function Process
     (Self : Ada_Selection_Range_Handler'Class;
      Node : Libadalang.Analysis.Ada_Node)
        return LSP.Structures.SelectionRange;

   ----------------
   -- Create_Job --
   ----------------

   overriding function Create_Job
     (Self    : Ada_Selection_Range_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
        return LSP.Server_Jobs.Server_Job_Access
   is
      Result : constant Ada_Selection_Range_Job_Access :=
        new Ada_Selection_Range_Job'
          (Parent  => Self'Unchecked_Access,
           Request => LSP.Ada_Request_Jobs.Request_Access (Message));
   begin
      return LSP.Server_Jobs.Server_Job_Access (Result);
   end Create_Job;

   -------------------------
   -- Execute_Ada_Request --
   -------------------------

   overriding procedure Execute_Ada_Request
     (Self   : in out Ada_Selection_Range_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is
      Message : LSP.Server_Requests.SelectionRange.Request
        renames LSP.Server_Requests.SelectionRange.Request (Self.Message.all);

      Value : LSP.Structures.SelectionRangeParams renames Message.Params;

      Response : LSP.Structures.SelectionRange_Vector_Or_Null;

      Context : constant LSP.Ada_Context_Sets.Context_Access :=
        Self.Parent.Context.Get_Best_Context
          (Value.textDocument.uri);

   begin
      Status := LSP.Server_Jobs.Done;

      for Item of Value.positions loop
         declare
            Pos : constant LSP.Structures.TextDocumentPositionParams :=
              (Value.textDocument, Item);
            Node : constant Libadalang.Analysis.Ada_Node :=
              Self.Parent.Context.Get_Node_At (Context.all, Pos);
         begin
            Response.Append (Self.Parent.Process (Node));
         end;
      end loop;

      Client.On_SelectionRange_Response (Message.Id, Response);
   end Execute_Ada_Request;

   -------------
   -- Process --
   -------------

   function Process
     (Self : Ada_Selection_Range_Handler'Class;
      Node : Libadalang.Analysis.Ada_Node)
        return LSP.Structures.SelectionRange
   is
      Result : LSP.Structures.SelectionRange;
      First  : Boolean := True;
      List   : constant Libadalang.Analysis.Ada_Node_Array :=
        (if Node.Is_Null then [] else Node.Parents);
   begin
      for Item of reverse List loop
         declare
            Next : LSP.Structures.SelectionRange;
         begin
            Next.a_range := Self.Context.To_LSP_Range (Item);

            if First then
               First := False;
            else
               Next.parent.Set (Result);
            end if;

            Result := Next;
         end;
      end loop;

      if First then
         Result.a_range := ((0, 0), (0, 0));
      end if;

      return Result;
   end Process;

end LSP.Ada_Selection_Range;

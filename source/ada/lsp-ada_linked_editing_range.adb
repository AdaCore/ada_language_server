------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2026, AdaCore                       --
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
with Libadalang.Common;
with LSP.Ada_Context_Sets;
with LSP.Ada_Request_Jobs;
with LSP.Client_Message_Receivers;
with LSP.Server_Requests.LinkedEditingRange;
with LSP.Structures;

package body LSP.Ada_Linked_Editing_Range is

   type Ada_Linked_Editing_Range_Job
     (Parent : not null access constant Ada_Linked_Editing_Range_Handler) is
     limited new LSP.Ada_Request_Jobs.Ada_Request_Job
       (Priority => LSP.Server_Jobs.High)
          with null record;

   overriding procedure Execute_Ada_Request
     (Self   : in out Ada_Linked_Editing_Range_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   type Ada_Linked_Editing_Range_Job_Access is
     access all Ada_Linked_Editing_Range_Job;

   ----------------
   -- Create_Job --
   ----------------

   overriding function Create_Job
     (Self    : Ada_Linked_Editing_Range_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
        return LSP.Server_Jobs.Server_Job_Access
   is
      Result : constant Ada_Linked_Editing_Range_Job_Access :=
        new Ada_Linked_Editing_Range_Job'
          (Parent  => Self'Unchecked_Access,
           Request => LSP.Ada_Request_Jobs.Request_Access (Message));
   begin
      return LSP.Server_Jobs.Server_Job_Access (Result);
   end Create_Job;

   -------------------------
   -- Execute_Ada_Request --
   -------------------------

   overriding procedure Execute_Ada_Request
     (Self   : in out Ada_Linked_Editing_Range_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is
      use all type Libadalang.Common.Ref_Result_Kind;

      Message : LSP.Server_Requests.LinkedEditingRange.Request
        renames LSP.Server_Requests.LinkedEditingRange.Request
          (Self.Message.all);

      Value : LSP.Structures.LinkedEditingRangeParams renames Message.Params;

      Response : LSP.Structures.LinkedEditingRanges_Or_Null (Is_Null => False);

      Context : constant LSP.Ada_Context_Sets.Context_Access :=
        Self.Parent.Context.Get_Best_Context
          (Value.textDocument.uri);

      Pos  : constant LSP.Structures.TextDocumentPositionParams :=
        (Value.textDocument, Value.position);

      Node : constant Libadalang.Analysis.Ada_Node :=
        Self.Parent.Context.Get_Node_At (Context.all, Pos);

      Name : constant Libadalang.Analysis.Defining_Name :=
        (if Node.Is_Null or else
         Node.Kind not in Libadalang.Common.Ada_Name
         then Libadalang.Analysis.No_Defining_Name
         else Self.Parent.Context.Imprecise_Resolve_Name (Node.As_Name));
   begin
      Status := LSP.Server_Jobs.Done;

      if not Name.Is_Null and then Name.P_Is_Defining then
         for Item of Name.P_Find_All_References ([Name.Unit])
           when Libadalang.Analysis.Kind (Item) = Precise
         loop
            declare
               Ref : constant Libadalang.Analysis.Ada_Node :=
                 Libadalang.Analysis.Ref (Item).As_Ada_Node;
            begin
               Response.Value.ranges.Append
                 (Self.Parent.Context.To_LSP_Range (Ref));
            end;
         end loop;

         if not Response.Value.ranges.Is_Empty then
            Response.Value.ranges.Append
              (Self.Parent.Context.To_LSP_Range (Node));
         end if;
      end if;

      Client.On_LinkedEditingRange_Response (Message.Id, Response);
   end Execute_Ada_Request;

end LSP.Ada_Linked_Editing_Range;

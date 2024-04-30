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
with Libadalang.Common;
with Libadalang.Iterators;

with VSS.Strings;

with LSP.Ada_Context_Sets;
with LSP.Ada_Request_Jobs;
with LSP.Client_Message_Receivers;
with LSP.Constants;
with LSP.Server_Requests.FoldingRange;
with LSP.Structures;

package body LSP.Ada_Folding_Range is

   type Traverse_Iterator_Access is access
     Libadalang.Iterators.Traverse_Iterator'Class;

   procedure Free is new Ada.Unchecked_Deallocation
     (Libadalang.Iterators.Traverse_Iterator'Class, Traverse_Iterator_Access);

   type Folding_Range_Job
     (Parent : not null access constant Ada_Folding_Range_Handler) is limited
   new LSP.Ada_Request_Jobs.Ada_Request_Job
     (Priority => LSP.Server_Jobs.Low)
   with record
      Lines_Only : Boolean;
      Unit       : Libadalang.Analysis.Analysis_Unit;
      Cursor     : Traverse_Iterator_Access;
      Response   : LSP.Structures.FoldingRange_Vector;
   end record;

   overriding procedure Execute_Ada_Request
     (Self   : in out Folding_Range_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   procedure Append_Comments (Self : in out Folding_Range_Job'Class);

   procedure Append_Node
     (Self : in out Folding_Range_Job'Class;
      Node : Libadalang.Analysis.Ada_Node);

   procedure Append_Span
     (Self : in out Folding_Range_Job'Class;
      Span : LSP.Structures.A_Range;
      Kind : LSP.Structures.FoldingRangeKind_Optional);

   ---------------------
   -- Append_Comments --
   ---------------------

   procedure Append_Comments (Self : in out Folding_Range_Job'Class) is
      use Libadalang.Common;

      Token : Token_Reference := Libadalang.Analysis.First_Token (Self.Unit);
      Item  : LSP.Structures.A_Range_Optional;
      --  Range of consecutive comments
   begin
      while Token /= No_Token loop
         case Kind (Data (Token)) is
            when Ada_Comment =>
               declare
                  Span : constant LSP.Structures.A_Range :=
                    Self.Parent.Context.To_LSP_Range (Self.Unit, Token);
               begin
                  if Item.Is_Set then
                     Item.Value.an_end := Span.an_end;
                  else
                     Item := (Is_Set => True, Value => Span);
                  end if;
               end;

            when Ada_Whitespace =>
               null;

            when others =>
               if Item.Is_Set then
                  Self.Append_Span (Item.Value, LSP.Constants.Comment);
                  Item := (Is_Set => False);
               end if;
         end case;

         Token := Next (Token);
      end loop;
   end Append_Comments;

   -----------------
   -- Append_Node --
   -----------------

   procedure Append_Node
     (Self : in out Folding_Range_Job'Class;
      Node : Libadalang.Analysis.Ada_Node)
   is
      use Libadalang.Common;
   begin
      --  Skip Ada_Node_List without with clauses
      case Node.Kind is
         when Ada_Ada_Node_List =>
            if (for all Item of Node.As_Ada_Node_List =>
                  Item.Kind /= Ada_With_Clause)
            then
               return;
            end if;
         when others =>
            null;
      end case;

      case Node.Kind is
         when Ada_Ada_Node_List |
              Ada_Package_Decl |
              Ada_Generic_Formal_Package |
              Ada_Package_Body |
              Ada_Type_Decl |
              Ada_Classwide_Type_Decl |
              Ada_Protected_Type_Decl |
              Ada_Task_Type_Decl |
              Ada_Single_Task_Type_Decl |
              Ada_Subp_Decl |
              Ada_Subp_Body |
              Ada_Generic_Formal_Subp_Decl |
              Ada_Abstract_Subp_Decl |
              Ada_Abstract_Formal_Subp_Decl |
              Ada_Concrete_Formal_Subp_Decl |
              Ada_Generic_Subp_Internal |
              Ada_Null_Subp_Decl |
              Ada_Subp_Renaming_Decl |
              Ada_Subp_Body_Stub |
              Ada_Generic_Subp_Decl |
              Ada_Generic_Subp_Instantiation |
              Ada_Generic_Subp_Renaming_Decl |
              Ada_Subp_Kind_Function |
              Ada_Subp_Kind_Procedure |
              Ada_Access_To_Subp_Def |
              Ada_Case_Stmt |
              Ada_If_Stmt |
              Ada_For_Loop_Stmt |
              Ada_While_Loop_Stmt |
              Ada_Begin_Block |
              Ada_Decl_Block |
              Ada_Extended_Return_Stmt_Object_Decl |
              Ada_Select_Stmt |
              Ada_Entry_Body |
              Ada_Exception_Handler |
              Ada_Pragma_Node_List |
              Ada_Pragma_Argument_Assoc |
              Ada_Pragma_Node |
              Ada_Aspect_Spec =>

            declare
               Location : constant LSP.Structures.Location :=
                 Self.Parent.Context.To_LSP_Location (Node);

               Span : LSP.Structures.A_Range renames Location.a_range;

               Kind : constant LSP.Structures.FoldingRangeKind_Optional :=
                 (if Node.Kind = Ada_Ada_Node_List then LSP.Constants.Imports
                  else LSP.Constants.Region);

            begin
               Self.Append_Span (Span, Kind);
            end;
         when others =>
            null;
      end case;
   end Append_Node;

   -----------------
   -- Append_Span --
   -----------------

   procedure Append_Span
     (Self : in out Folding_Range_Job'Class;
      Span : LSP.Structures.A_Range;
      Kind : LSP.Structures.FoldingRangeKind_Optional)
   is
      function Get_Column (Column : Natural)
        return LSP.Structures.Natural_Optional is
          (if Self.Lines_Only then (Is_Set => False)
           else (Is_Set => True, Value => Column));

   begin
      if not Self.Lines_Only
        or else Span.start.line /= Span.an_end.line
      then
         declare
            Item : constant LSP.Structures.FoldingRange :=
              (startLine      => Span.start.line,
               startCharacter => Get_Column (Span.start.character),
               endLine        => Span.an_end.line,
               endCharacter   => Get_Column (Span.an_end.character),
               kind           => Kind,
               collapsedText  => VSS.Strings.Empty_Virtual_String);
         begin
            Self.Response.Append (Item);
         end;
      end if;
   end Append_Span;

   ----------------
   -- Create_Job --
   ----------------

   overriding function Create_Job
     (Self    : Ada_Folding_Range_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
        return LSP.Server_Jobs.Server_Job_Access
   is
      Value : LSP.Structures.FoldingRangeParams
        renames LSP.Server_Requests.FoldingRange.Request
          (Message.all).Params;

      File : constant GNATCOLL.VFS.Virtual_File :=
        Self.Context.To_File (Value.textDocument.uri);

      Context : constant LSP.Ada_Context_Sets.Context_Access :=
        Self.Context.Get_Best_Context (Value.textDocument.uri);

      Unit : constant Libadalang.Analysis.Analysis_Unit :=
        Context.Get_AU (File);

      Job  : constant LSP.Server_Jobs.Server_Job_Access :=
        new Folding_Range_Job'
          (Parent     => Self'Unchecked_Access,
           Request    => LSP.Ada_Request_Jobs.Request_Access (Message),
           Unit       => Unit,
           Cursor     => new Libadalang.Iterators.Traverse_Iterator'Class'
              (Libadalang.Iterators.Traverse (Unit.Root)),
           Lines_Only => Self.Context.Client.Line_Folding_Only,
           Response   => <>);
   begin
      return Job;
   end Create_Job;

   -------------------------
   -- Execute_Ada_Request --
   -------------------------

   overriding procedure Execute_Ada_Request
     (Self   : in out Folding_Range_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is
      Node : Libadalang.Analysis.Ada_Node;
   begin
      Status := LSP.Server_Jobs.Continue;

      for J in 1 .. 300 loop
         if Self.Cursor.Next (Node) then
            Self.Append_Node (Node);

         else
            declare
               Message : LSP.Server_Requests.FoldingRange.Request
                 renames LSP.Server_Requests.FoldingRange.Request
                   (Self.Message.all);
            begin
               if Self.Parent.Context.Get_Configuration.Folding_Comments then
                  Self.Append_Comments;
               end if;

               Client.On_FoldingRange_Response (Message.Id, Self.Response);

               Free (Self.Cursor);
               Status := LSP.Server_Jobs.Done;

               return;
            end;
         end if;
      end loop;
   end Execute_Ada_Request;
end LSP.Ada_Folding_Range;

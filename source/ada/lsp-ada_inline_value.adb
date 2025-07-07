------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2025, AdaCore                       --
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

with Langkit_Support.Slocs;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Expr_Eval;
with Libadalang.Iterators;

with VSS.Strings;

with LSP.Ada_Context_Sets;
with LSP.Ada_Request_Jobs;
with LSP.Client_Message_Receivers;
with LSP.Server_Requests.InlineValue;
with LSP.Structures;

package body LSP.Ada_Inline_Value is

   type Ada_Inline_Value_Job
     (Parent : not null access constant Ada_Inline_Value_Handler) is limited
   new LSP.Ada_Request_Jobs.Ada_Request_Job
     (Priority => LSP.Server_Jobs.Low)
        with null record;

   overriding procedure Execute_Ada_Request
     (Self   : in out Ada_Inline_Value_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   type Ada_Inline_Value_Job_Access is access all Ada_Inline_Value_Job;

   procedure Process
     (Self   : Ada_Inline_Value_Job'Class;
      Id     : Libadalang.Analysis.Identifier;
      Result : in out LSP.Structures.InlineValue_Vector);

   procedure Process_Static
     (Self   : Ada_Inline_Value_Job'Class;
      Id     : Libadalang.Analysis.Identifier;
      Result : in out LSP.Structures.InlineValue_Vector);

   procedure Append_Variable
     (Self   : Ada_Inline_Value_Job'Class;
      Id     : Libadalang.Analysis.Identifier;
      Result : in out LSP.Structures.InlineValue_Vector);

   procedure Append_Expression
     (Self   : Ada_Inline_Value_Job'Class;
      Node   : Libadalang.Analysis.Expr;
      Result : in out LSP.Structures.InlineValue_Vector);

   function Is_Simple_Variable
     (Id : Libadalang.Analysis.Identifier) return Boolean;
   --  Check if Id denotes an object of a simple type

   function Is_Safe_To_Evaluate
     (Node : Libadalang.Analysis.Expr) return Boolean;

   procedure Append
     (Self   : Ada_Inline_Value_Job'Class;
      Id     : Libadalang.Analysis.Identifier;
      Text   : Libadalang.Text.Text_Type;
      Result : in out LSP.Structures.InlineValue_Vector);

   function Ends_With (T, Suffix : Libadalang.Text.Text_Type) return Boolean is
     (T'Length >= Suffix'Length
        and then T (T'Last - Suffix'Length + 1 .. T'Last) = Suffix);
   --  Check if T has given Suffix

   function Is_String
     (Tipe : Libadalang.Analysis.Base_Type_Decl) return Boolean is
       (Ends_With (Libadalang.Text.To_Lower (Tipe.F_Name.Text), "string"));
   --  Check if Tipe has "String" suffix

   function Find_Scalar_Expr_After_Stop
     (Root : Libadalang.Analysis.Ada_Node;
      Stop : Langkit_Support.Slocs.Source_Location)
        return Libadalang.Analysis.Ada_Node;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self   : Ada_Inline_Value_Job'Class;
      Id     : Libadalang.Analysis.Identifier;
      Text   : Libadalang.Text.Text_Type;
      Result : in out LSP.Structures.InlineValue_Vector)
   is
      Item : constant LSP.Structures.InlineValue :=
        (LSP.Structures.Variant_1,
         Variant_1 =>
           (a_range => Self.Parent.Context.To_LSP_Range (Id),
            text    => VSS.Strings.To_Virtual_String
              (Id.Text & " = " & Text)));
   begin
      Result.Append (Item);
   end Append;

   -----------------------
   -- Append_Expression --
   -----------------------

   procedure Append_Expression
     (Self   : Ada_Inline_Value_Job'Class;
      Node   : Libadalang.Analysis.Expr;
      Result : in out LSP.Structures.InlineValue_Vector)
   is
      Item : constant LSP.Structures.InlineValue :=
        (LSP.Structures.Variant_3,
         Variant_3 =>
           (a_range => Self.Parent.Context.To_LSP_Range (Node),
            expression => <>));
   begin
      Result.Append (Item);
   end Append_Expression;

   ----------------------
   -- Append_Variable --
   ----------------------

   procedure Append_Variable
     (Self   : Ada_Inline_Value_Job'Class;
      Id     : Libadalang.Analysis.Identifier;
      Result : in out LSP.Structures.InlineValue_Vector)
   is
      Item : constant LSP.Structures.InlineValue :=
        (LSP.Structures.Variant_2,
         Variant_2 =>
           (a_range => Self.Parent.Context.To_LSP_Range (Id),
            variableName => <>,
            caseSensitiveLookup => False));
   begin
      Result.Append (Item);
   end Append_Variable;

   ----------------
   -- Create_Job --
   ----------------

   overriding function Create_Job
     (Self    : Ada_Inline_Value_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
        return LSP.Server_Jobs.Server_Job_Access
   is
      Result : constant Ada_Inline_Value_Job_Access :=
        new Ada_Inline_Value_Job'
          (Parent  => Self'Unchecked_Access,
           Request => LSP.Ada_Request_Jobs.Request_Access (Message));
   begin
      return LSP.Server_Jobs.Server_Job_Access (Result);
   end Create_Job;

   -------------------------
   -- Execute_Ada_Request --
   -------------------------

   overriding procedure Execute_Ada_Request
     (Self   : in out Ada_Inline_Value_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is
      use all type Langkit_Support.Slocs.Relative_Position;

      Message : LSP.Server_Requests.InlineValue.Request
        renames LSP.Server_Requests.InlineValue.Request (Self.Message.all);

      Value : LSP.Structures.InlineValueParams renames Message.Params;

      Response : LSP.Structures.InlineValue_Vector_Or_Null;

      Context : constant LSP.Ada_Context_Sets.Context_Access :=
        Self.Parent.Context.Get_Best_Context
          (Value.textDocument.uri);

      File : constant GNATCOLL.VFS.Virtual_File :=
        Self.Parent.Context.To_File (Value.textDocument.uri);

      Unit : constant Libadalang.Analysis.Analysis_Unit :=
        Context.Get_AU (File);

      Span : constant Langkit_Support.Slocs.Source_Location_Range :=
        Self.Parent.Context.From_LSP_Range (Unit, Value.a_range);

      Stop : constant Langkit_Support.Slocs.Source_Location_Range :=
        Self.Parent.Context.From_LSP_Range
          (Unit, Value.context.stoppedLocation);

      Iter : Libadalang.Iterators.Traverse_Iterator'Class :=
        Libadalang.Iterators.Find
          (Unit.Root,
           Libadalang.Iterators.Kind_Is (Libadalang.Common.Ada_Identifier));

      Node : Libadalang.Analysis.Ada_Node;
   begin
      Status := LSP.Server_Jobs.Done;

      --  Find identifiers that represent static values ​​or variables.
      while Iter.Next (Node) loop
         if Langkit_Support.Slocs.Compare
           (Span, Langkit_Support.Slocs.Start_Sloc (Node.Sloc_Range)) = Inside
         then
            Self.Process (Node.As_Identifier, Response);
         end if;
      end loop;

      Node := Find_Scalar_Expr_After_Stop
        (Unit.Root, Langkit_Support.Slocs.Start_Sloc (Stop));

      if not Node.Is_Null then
         --  If we have a scalar expression at stop point and it doesn't
         --  have function calls then add the first such expression.
         --  In particular, this can be useful if we have stopped at a control
         --  operator that has a boolean condition.

         Self.Append_Expression (Node.As_Expr, Response);
      end if;

      Client.On_InlineValue_Response (Message.Id, Response);
   end Execute_Ada_Request;

   ---------------------------------
   -- Find_Scalar_Expr_After_Stop --
   ---------------------------------

   function Find_Scalar_Expr_After_Stop
     (Root : Libadalang.Analysis.Ada_Node;
      Stop : Langkit_Support.Slocs.Source_Location)
        return Libadalang.Analysis.Ada_Node
   is
      use all type Langkit_Support.Slocs.Relative_Position;

      function Expr_After_Stop
        (Node : Libadalang.Analysis.Ada_Node) return Boolean is
          (Node.Kind in Libadalang.Common.Ada_Expr
           and then Node.Kind not in Libadalang.Common.Ada_Name
           and then Langkit_Support.Slocs.Compare
             (Node.Sloc_Range, Stop) = Before);
      --  Node is Expr, not just a name and Stop before Expr

      Node : Libadalang.Analysis.Ada_Node;
   begin
      if not Root.Is_Null then
         Node := Root.Lookup (Stop);

         if not Node.Is_Null then
            Node := Libadalang.Iterators.Find_First
              (Node, Expr_After_Stop'Access);

            if not Node.Is_Null
              and then Node.As_Expr.P_Expression_Type.P_Is_Scalar_Type (Node)
              and then Is_Safe_To_Evaluate (Node.As_Expr)
            then
               return Node;
            end if;
         end if;
      end if;

      return Libadalang.Analysis.No_Ada_Node;
   end Find_Scalar_Expr_After_Stop;

   -------------------------
   -- Is_Safe_To_Evaluate --
   -------------------------

   function Is_Safe_To_Evaluate
     (Node : Libadalang.Analysis.Expr) return Boolean
   is
      function Is_Unsafe (X : Libadalang.Analysis.Ada_Node) return Boolean;

      ---------------
      -- Is_Unsafe --
      ---------------

      function Is_Unsafe (X : Libadalang.Analysis.Ada_Node) return Boolean is
         Prefix : Libadalang.Analysis.Name;
         Decl   : Libadalang.Analysis.Basic_Decl;
      begin
         if X.Kind in Libadalang.Common.Ada_Call_Expr
           and then X.As_Name.P_Is_Call
         then
            Prefix := X.As_Call_Expr.F_Name;
            Decl := Prefix.P_Referenced_Decl (False);

            if Decl.Is_Null
              or else not Decl.P_Is_Predefined_Operator
            then
               --  Let's consider call (except to a predefined operator) as
               --  unsafe expresssion
               return True;
            end if;
         end if;

         return False;
      exception
         when Libadalang.Common.Property_Error =>
            return True;
      end Is_Unsafe;

      Bad : constant Libadalang.Analysis.Ada_Node :=
        Libadalang.Iterators.Find_First (Node, Is_Unsafe'Access);
   begin
      return Bad.Is_Null;
   end Is_Safe_To_Evaluate;

   ------------------------
   -- Is_Simple_Variable --
   ------------------------

   function Is_Simple_Variable
     (Id : Libadalang.Analysis.Identifier) return Boolean
   is
      Decl : Libadalang.Analysis.Basic_Decl;
      Decl_Type : Libadalang.Analysis.Base_Type_Decl;
   begin
      Decl := Id.P_Referenced_Decl (Imprecise_Fallback => False);
      Decl_Type := Id.P_Expression_Type;

      if Id.P_Is_Defining or else Decl.Is_Null or else Decl_Type.Is_Null then
         return False;
      elsif not Decl_Type.P_Full_View.Is_Null then
         Decl_Type := Decl_Type.P_Full_View;
      end if;

      if not Decl_Type.P_Is_Scalar_Type (Decl_Type)
        and then not Is_String (Decl_Type)
      then
         --  Let's display only variables of elementary and string types for
         --  now.
         return False;
      end if;

      if Decl.Kind not in
        Libadalang.Common.Ada_Object_Decl |
        Libadalang.Common.Ada_Param_Spec |
        Libadalang.Common.Ada_Base_Formal_Param_Decl |
        Libadalang.Common.Ada_For_Loop_Var_Decl |
        Libadalang.Common.Ada_Entry_Index_Spec
      then
         return False;
      end if;

      return True;
   exception
      when Libadalang.Common.Property_Error =>
         return False;
   end Is_Simple_Variable;

   -------------
   -- Process --
   -------------

   procedure Process
     (Self   : Ada_Inline_Value_Job'Class;
      Id     : Libadalang.Analysis.Identifier;
      Result : in out LSP.Structures.InlineValue_Vector) is
   begin
      if Id.P_Is_Static_Expr then
         Self.Process_Static (Id, Result);
      elsif Is_Simple_Variable (Id) then
         Self.Append_Variable (Id, Result);
      end if;
   end Process;

   --------------------
   -- Process_Static --
   --------------------

   procedure Process_Static
     (Self   : Ada_Inline_Value_Job'Class;
      Id     : Libadalang.Analysis.Identifier;
      Result : in out LSP.Structures.InlineValue_Vector) is
   begin
      declare
         Value : constant Libadalang.Expr_Eval.Eval_Result :=
           Libadalang.Expr_Eval.Expr_Eval (Id.As_Expr);
      begin
         case Value.Kind is
            when Libadalang.Expr_Eval.Enum_Lit =>
               declare
                  Text : constant Libadalang.Text.Text_Type :=
                    Value.Enum_Result.Text;
               begin
                  if Text /= Id.Text then
                     Self.Append (Id, Text, Result);
                  end if;
               end;
            when Libadalang.Expr_Eval.Int =>
               Self.Append
                 (Id,
                  Libadalang.Text.To_Text (Value.Int_Result.Image),
                  Result);
            when Libadalang.Expr_Eval.Real =>
               Self.Append
                 (Id,
                  Libadalang.Text.To_Text (Value.Real_Result.Image),
                  Result);
            when Libadalang.Expr_Eval.String_Lit =>
               Self.Append
                 (Id,
                  Libadalang.Text.To_Text (Value.String_Result),
                  Result);
         end case;
      end;
   exception
      when Libadalang.Common.Property_Error =>
         null;
   end Process_Static;

end LSP.Ada_Inline_Value;

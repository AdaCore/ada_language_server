------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2025, AdaCore                          --
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

with Ada.Containers.Hashed_Sets;

with GNATCOLL.GMP.Integers;

with Libadalang.Common;
with Libadalang.Expr_Eval;
with Langkit_Support.Text;

with LSP.Enumerations;
with LSP.Ada_Completions.Filters;
with LSP.Utils;

with VSS.Strings.Formatters.Integers;
with VSS.Strings.Formatters.Strings;
with VSS.Strings.Templates;
with VSS.Strings.Hash;

package body LSP.Ada_Completions.Record_Representation is

   package String_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => VSS.Strings.Virtual_String,
      Hash                => VSS.Strings.Hash,
      Equivalent_Elements => VSS.Strings."=",
      "="                 => VSS.Strings."=");

   package Name_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Libadalang.Analysis.Defining_Name,
        Element_Type    => VSS.Strings.Virtual_String,
        Hash            => Hash,
        Equivalent_Keys => Is_Full_Sloc_Equal,
        "="             => VSS.Strings."=");

   function "-" (Left, Right : Libadalang.Analysis.Expr) return Integer;
   --  Suppose Left >= Right. Try to evaluate Left and Right, return
   --  (Left - Right) if success. Return -1 otherwise.

   function Find_Clause
     (Node : Libadalang.Analysis.Ada_Node) return Libadalang.Analysis.Ada_Node;

   function Predict_Word_Size
     (Clause : Libadalang.Analysis.Record_Rep_Clause) return Positive;
   --  Look through component clauses and find minimal step between two
   --  positions /= 0.

   function Estimate_Component_Size
     (Component : Libadalang.Analysis.Defining_Name) return Natural;
   --  Try to find component size in bits

   function Find_Last_Used_Bit
     (Clause : Libadalang.Analysis.Record_Rep_Clause) return Integer;

   procedure Find_Components
     (Type_Decl : Libadalang.Analysis.Basic_Decl;
      Type_Name : Libadalang.Analysis.Name;
      Prefix    : VSS.Strings.Virtual_String;
      Result    : out Name_Maps.Map);

   procedure Find_Already_Defined
     (Clause : Libadalang.Analysis.Record_Rep_Clause;
      Result : out String_Sets.Set);

   function Diff_Size (Left, Right : Libadalang.Analysis.Expr) return Natural;

   function Log (Left : Libadalang.Analysis.Expr) return Natural;
   function To_Natural (Left : Libadalang.Analysis.Expr) return Natural;

   function Enum_Size
     (List : Libadalang.Analysis.Enum_Literal_Decl_List) return Natural;

   Size_Text : constant Langkit_Support.Text.Unbounded_Text_Type :=
     Langkit_Support.Text.To_Unbounded_Text ("Size");

   ---------
   -- "-" --
   ---------

   function "-" (Left, Right : Libadalang.Analysis.Expr) return Integer is
      use Libadalang.Expr_Eval;
      use type Libadalang.Expr_Eval.Big_Integer;
      use all type GNATCOLL.GMP.Long;

      Left_Eval : constant Eval_Result := Expr_Eval (Left);
      Right_Eval : constant Eval_Result := Expr_Eval (Right);
   begin
      if Left_Eval.Kind = Int and Right_Eval.Kind = Int then
         declare
            Diff : constant Big_Integer :=
              Left_Eval.Int_Result - Right_Eval.Int_Result;

            Zero : constant GNATCOLL.GMP.Long := 0;

            Last : constant GNATCOLL.GMP.Long :=
              GNATCOLL.GMP.Long (Positive'Last);

            Step : Natural;
         begin
            if Diff >= Zero and Diff <= Last then
               Step :=
                 Natural'Value (GNATCOLL.GMP.Integers.Image (Diff));

               return Step;
            end if;
         end;
      end if;

      return -1;
   end "-";

   ---------------
   -- Diff_Size --
   ---------------

   function Diff_Size
     (Left, Right : Libadalang.Analysis.Expr) return Natural
   is
      Diff : constant Integer := Right - Left;
   begin
      if Diff = -1 then
         return 0;
      else
         for J in 0 .. 64 loop
            if Diff < 2 ** J then
               return J;
            end if;
         end loop;

         return 0;
      end if;
   end Diff_Size;

   ---------------
   -- Enum_Size --
   ---------------

   function Enum_Size
     (List : Libadalang.Analysis.Enum_Literal_Decl_List) return Natural
   is
      First : Libadalang.Analysis.Enum_Literal_Decl;
      Last  : Libadalang.Analysis.Enum_Literal_Decl;
      Diff  : Natural := 0;
   begin
      for Item of List loop
         if First.Is_Null then
            First := Item.As_Enum_Literal_Decl;
         end if;

         Last := Item.As_Enum_Literal_Decl;
      end loop;

      declare
         use Libadalang.Expr_Eval;
         use type Libadalang.Expr_Eval.Big_Integer;
         use all type GNATCOLL.GMP.Long;

         Result : constant Libadalang.Expr_Eval.Big_Integer :=
           Last.P_Enum_Rep - First.P_Enum_Rep;

         Limit : constant GNATCOLL.GMP.Long :=
           GNATCOLL.GMP.Long (Positive'Last);

      begin
         if Result <= Limit then
            Diff := Natural'Value (GNATCOLL.GMP.Integers.Image (Result));
         end if;
      end;

      for J in 0 .. 64 loop
         if Diff < 2 ** J then
            return J;
         end if;
      end loop;

      return 0;
   end Enum_Size;

   -----------------------------
   -- Estimate_Component_Size --
   -----------------------------

   function Estimate_Component_Size
     (Component : Libadalang.Analysis.Defining_Name) return Natural
   is
      use all type Libadalang.Common.Ada_Node_Kind_Type;
      Node : Libadalang.Analysis.Ada_Node := Component.As_Ada_Node;
   begin
      while not Node.Is_Null loop
         case Node.Kind is

         when Ada_Defining_Name =>
            Node := Node.As_Defining_Name.P_Basic_Decl.As_Ada_Node;

         when Ada_Discriminant_Spec =>
            Node := Node.As_Discriminant_Spec.F_Type_Expr.As_Ada_Node;

         when Ada_Component_Decl =>
            Node := Node.As_Component_Decl.F_Component_Def.As_Ada_Node;

         when Ada_Component_Def =>
            Node := Node.As_Component_Def.F_Type_Expr.As_Ada_Node;

         when Libadalang.Common.Ada_Subtype_Indication_Range =>
            exit when not Node.As_Subtype_Indication.P_Is_Static_Subtype;

            if Node.As_Subtype_Indication.F_Constraint.Is_Null
              or else not Node.As_Subtype_Indication.P_Designated_Type_Decl.
                P_Is_Discrete_Type
            then
               Node := Node.As_Subtype_Indication.P_Designated_Type_Decl.
                 As_Ada_Node;
            else
               Node := Node.As_Subtype_Indication.F_Constraint.As_Ada_Node;
            end if;

         when Ada_Range_Constraint =>
            Node := Node.As_Range_Constraint.F_Range.As_Ada_Node;

         when Ada_Range_Spec =>
            Node := Node.As_Range_Spec.F_Range.As_Ada_Node;

         when Ada_Bin_Op =>
            exit when Node.As_Bin_Op.F_Op /= Ada_Op_Double_Dot;

            return
              Diff_Size (Node.As_Bin_Op.F_Left, Node.As_Bin_Op.F_Right);

         when Libadalang.Common.Ada_Type_Decl =>
            declare
               Size : constant Libadalang.Analysis.Attribute_Def_Clause :=
                 Node.As_Type_Decl.P_Get_Representation_Clause (Size_Text);
            begin
               if Size.Is_Null then
                  Node := Node.As_Type_Decl.F_Type_Def.As_Ada_Node;
               else
                  return To_Natural (Size.F_Expr);
               end if;
            end;

         when Ada_Signed_Int_Type_Def =>
            Node := Node.As_Signed_Int_Type_Def.F_Range.As_Ada_Node;

         when Ada_Mod_Int_Type_Def =>
            return Log (Node.As_Mod_Int_Type_Def.F_Expr);

         when Ada_Enum_Type_Def =>
            Node := Node.As_Enum_Type_Def.F_Enum_Literals.As_Ada_Node;

         when Ada_Enum_Literal_Decl_List =>
            return Enum_Size (Node.As_Enum_Literal_Decl_List);

         when others =>
            exit;
         end case;
      end loop;

      return 0;

   exception
      when Libadalang.Common.Property_Error =>
         return 0;
   end Estimate_Component_Size;

   --------------------------
   -- Find_Already_Defined --
   --------------------------

   procedure Find_Already_Defined
     (Clause : Libadalang.Analysis.Record_Rep_Clause;
      Result : out String_Sets.Set)
   is
      use all type Libadalang.Common.Ada_Node_Kind_Type;
   begin
      for Item of Clause.F_Components loop
         if Item.Kind = Ada_Component_Clause then
            declare
               Name  : constant Libadalang.Analysis.Identifier :=
                 Item.As_Component_Clause.F_Id;
               Image : constant VSS.Strings.Virtual_String :=
                 VSS.Strings.To_Virtual_String (Name.Text);
            begin
               Result.Include (Image);
            end;
         end if;
      end loop;
   end Find_Already_Defined;

   -----------------
   -- Find_Clause --
   -----------------

   function Find_Clause
     (Node : Libadalang.Analysis.Ada_Node) return Libadalang.Analysis.Ada_Node
   is
      use all type Libadalang.Common.Ada_Node_Kind_Type;

      Next : Libadalang.Analysis.Ada_Node := Node;
   begin
      while not Next.Is_Null and then Next.Kind /= Ada_Record_Rep_Clause loop
         Next := Next.Parent;
      end loop;

      return Next;
   end Find_Clause;

   ---------------------
   -- Find_Components --
   ---------------------

   procedure Find_Components
     (Type_Decl : Libadalang.Analysis.Basic_Decl;
      Type_Name : Libadalang.Analysis.Name;
      Prefix    : VSS.Strings.Virtual_String;
      Result    : out Name_Maps.Map) is
   begin
      for Part of
        Type_Decl.P_All_Parts
        when Part.Kind in Libadalang.Common.Ada_Base_Type_Decl
        and then Part.As_Base_Type_Decl.P_Is_Record_Type
      loop
         for Shape of
           Part.As_Base_Type_Decl.P_Shapes
             (Include_Discriminants => True, Origin => Type_Name)
         loop
            for Component of Shape.Components loop
               for Def_Name of Component.P_Defining_Names loop
                  declare
                     Name : constant Libadalang.Analysis.Defining_Name :=
                       Def_Name.P_Canonical_Part;
                     Image : constant VSS.Strings.Virtual_String :=
                       VSS.Strings.To_Virtual_String (Name.Text);
                     Canonical_Image : constant VSS.Strings.Virtual_String :=
                       LSP.Utils.Canonicalize (Image);
                  begin
                     if Canonical_Image.Starts_With (Prefix) then
                        Result.Include (Name, Image);
                     end if;
                  end;
               end loop;
            end loop;
         end loop;
      end loop;
   end Find_Components;

   ------------------------
   -- Find_Last_Used_Bit --
   ------------------------

   function Find_Last_Used_Bit
     (Clause : Libadalang.Analysis.Record_Rep_Clause) return Integer
   is
      use Libadalang.Expr_Eval;
      use type Libadalang.Expr_Eval.Big_Integer;
      use all type Libadalang.Common.Ada_Node_Kind_Type;
      use all type GNATCOLL.GMP.Long;

      Result : Big_Integer := GNATCOLL.GMP.Integers.Make ("-1");
   begin
      for Item of Clause.F_Components when Item.Kind = Ada_Component_Clause
      loop
         declare
            Pos_Eval : constant Eval_Result :=
              Expr_Eval (Item.As_Component_Clause.F_Position);

            Bit_Range : constant Libadalang.Analysis.Expr :=
              Item.As_Component_Clause.F_Range.F_Range;

            Is_Range : constant Boolean :=
              Bit_Range.Kind = Ada_Bin_Op
              and then Bit_Range.As_Bin_Op.F_Op = Ada_Op_Double_Dot
              and then not Bit_Range.As_Bin_Op.F_Right.Is_Null;
         begin
            if Is_Range and then Pos_Eval.Kind = Int then
               declare
                  Last_Eval : constant Eval_Result :=
                    Expr_Eval (Bit_Range.As_Bin_Op.F_Right);
               begin
                  if Last_Eval.Kind = Int then
                     declare
                        Bit : constant Big_Integer :=
                          Pos_Eval.Int_Result * 8 + Last_Eval.Int_Result;
                     begin
                        if Bit > Result then
                           GNATCOLL.GMP.Integers.Set (Result, Bit);
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;
      end loop;

      return Integer'Value (GNATCOLL.GMP.Integers.Image (Result));
   end Find_Last_Used_Bit;

   ---------
   -- Log --
   ---------

   function Log
     (Left : Libadalang.Analysis.Expr) return Natural
   is
      use Libadalang.Expr_Eval;
      use type Libadalang.Expr_Eval.Big_Integer;

      Left_Eval : constant Eval_Result := Expr_Eval (Left);
      Two : constant Big_Integer := GNATCOLL.GMP.Integers.Make ("2");
   begin
      if Left_Eval.Kind = Int then
         for J in GNATCOLL.GMP.Unsigned_Long'(0) .. 64 loop
            if Left_Eval.Int_Result <= Two ** J then
               return Natural (J);
            end if;
         end loop;
      end if;

      return 0;
   end Log;

   -----------------------
   -- Predict_Word_Size --
   -----------------------

   function Predict_Word_Size
     (Clause : Libadalang.Analysis.Record_Rep_Clause) return Positive
   is
      use all type Libadalang.Common.Ada_Node_Kind_Type;

      Result : Positive := Positive'Last;
   begin
      for Left of Clause.F_Components when Left.Kind = Ada_Component_Clause
      loop
         for Right of Clause.F_Components when Left.Kind = Ada_Component_Clause
         loop
            declare
               Left_Expr : constant Libadalang.Analysis.Expr :=
                 Left.As_Component_Clause.F_Position;

               Right_Expr : constant Libadalang.Analysis.Expr :=
                 Right.As_Component_Clause.F_Position;

               Diff       : constant Integer := Left_Expr - Right_Expr;
            begin
               if Diff > 0 and Diff < Result then
                  Result := Diff;
               end if;
            end;
         end loop;
      end loop;

      --  If nothing found let's use 4 bytes as default
      Result := (if Result = Positive'Last then 4 else Result);

      return Result;
   end Predict_Word_Size;

   ------------------------
   -- Propose_Completion --
   ------------------------

   overriding
   procedure Propose_Completion
     (Self   : Record_Repr_Completion_Provider;
      Sloc   : Langkit_Support.Slocs.Source_Location;
      Token  : Libadalang.Common.Token_Reference;
      Node   : Libadalang.Analysis.Ada_Node;
      Filter : in out LSP.Ada_Completions.Filters.Filter;
      Names  : in out Ada_Completions.Completion_Maps.Map;
      Result : in out LSP.Structures.CompletionList)
   is
      pragma Unreferenced (Filter);
      use all type Libadalang.Common.Token_Kind;

      Prefix : constant VSS.Strings.Virtual_String :=
        LSP.Utils.Canonicalize
          (VSS.Strings.To_Virtual_String
             (if Token.Data.Kind = Ada_Identifier
              then Libadalang.Common.Text (Token)
              else ""));

      Clause : constant Libadalang.Analysis.Ada_Node := Find_Clause (Node);

      Type_Name : constant Libadalang.Analysis.Name :=
        (if Clause.Is_Null
         then Libadalang.Analysis.No_Name
         else Clause.As_Record_Rep_Clause.F_Name);

      Type_Decl : constant Libadalang.Analysis.Basic_Decl :=
        (if Type_Name.Is_Null
         then Libadalang.Analysis.No_Basic_Decl
         else Type_Name.P_Referenced_Decl);

      Word_Size     : Positive;

      Last_Used_Bit : Integer;

      Label : constant VSS.Strings.Templates.Virtual_String_Template :=
        "{1} at {2} range {3} .. {4};";

      Text : constant VSS.Strings.Templates.Virtual_String_Template :=
        "{1} at ${{1:{2}} range ${{2:{3}} .. ${{3:{4}};";

      Fields  : Name_Maps.Map;
      Exclude : String_Sets.Set;

      procedure Each_Field
        (Name : VSS.Strings.Virtual_String;
         Size : Natural);

      procedure Each_Field
        (Name : VSS.Strings.Virtual_String;
         Size : Natural)
      is
         Item : LSP.Structures.CompletionItem;

         Position : constant Natural :=
           ((Last_Used_Bit + 1) / 8 / Word_Size) * Word_Size;
         First_Bit : Natural;
         Last_Bit  : Integer;
      begin
         First_Bit := Last_Used_Bit - Position * 8 + 1;
         Last_Bit := First_Bit + Size - 1;

         Item :=
           (label               =>
              Label.Format
                (VSS.Strings.Formatters.Strings.Image (Name),
                 VSS.Strings.Formatters.Integers.Image (Position),
                 VSS.Strings.Formatters.Integers.Image (First_Bit),
                 VSS.Strings.Formatters.Integers.Image (Last_Bit)),
            labelDetails        => <>,
            kind                => (True, LSP.Enumerations.Snippet),
            tags                => <>,
            detail              => <>,
            documentation       => <>,
            deprecated          => <>,
            preselect           => <>,
            sortText            => <>,
            filterText          => <>,
            insertText          =>  --  " at $1 range ${2:7} .. ${3:10};",
              Text.Format
                (VSS.Strings.Formatters.Strings.Image (Name),
                 VSS.Strings.Formatters.Integers.Image (Position),
                 VSS.Strings.Formatters.Integers.Image (First_Bit),
                 VSS.Strings.Formatters.Integers.Image (Last_Bit),
                 VSS.Strings.Formatters.Strings.Image ("{"),
                 VSS.Strings.Formatters.Strings.Image ("}")),
            insertTextFormat    => (True, LSP.Enumerations.Snippet),
            insertTextMode      => <>,
            textEdit            => <>,
            textEditText        => <>,
            additionalTextEdits => <>,
            commitCharacters    => <>,
            command             => <>,
            data                => <>);
         Result.items.Append (Item);
      end Each_Field;

   begin
      if Clause.Is_Null or else Type_Decl.Is_Null then
         return;
      end if;

      Find_Already_Defined (Clause.As_Record_Rep_Clause, Exclude);

      --  Find all components with name that starts with given prefix
      Find_Components (Type_Decl, Type_Name, Prefix, Fields);

      Word_Size := Predict_Word_Size (Clause.As_Record_Rep_Clause);
      Last_Used_Bit := Find_Last_Used_Bit (Clause.As_Record_Rep_Clause);

      for Cursor in Fields.Iterate
        when not Exclude.Contains (Name_Maps.Element (Cursor))
      loop
         Each_Field
           (Name_Maps.Element (Cursor),
            Estimate_Component_Size (Name_Maps.Key (Cursor)));
      end loop;
   end Propose_Completion;

   ----------------
   -- To_Natural --
   ----------------

   function To_Natural (Left : Libadalang.Analysis.Expr) return Natural
   is
      use Libadalang.Expr_Eval;
      use type Libadalang.Expr_Eval.Big_Integer;

      Left_Eval : constant Eval_Result := Expr_Eval (Left);

      Last      : constant GNATCOLL.GMP.Long :=
        GNATCOLL.GMP.Long (Positive'Last);
   begin
      if Left_Eval.Kind = Int and then Left_Eval.Int_Result <= Last then
         return
           Natural'Value (GNATCOLL.GMP.Integers.Image (Left_Eval.Int_Result));
      end if;

      return 0;
   end To_Natural;

end LSP.Ada_Completions.Record_Representation;

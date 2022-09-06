------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;
with System;

with GNATCOLL.Utils;

with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;
with VSS.String_Vectors;
with VSS.Unicode;

with Langkit_Support;
with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Libadalang.Common;       use Libadalang.Common;
with Libadalang.Doc_Utils;
with Libadalang.Sources;

with Laltools.Call_Hierarchy;

with Libadalang.Lexer;
with Langkit_Support.Diagnostics;
pragma Warnings (Off, "redundant with clause");
with Langkit_Support.Symbols;  --  Fails with gnat ce 2020
pragma Warnings (On, "redundant with clause");
with Langkit_Support.Token_Data_Handlers;

with Pp.Actions;

with LSP.Common;
with LSP.Types;             use LSP.Types;

package body LSP.Lal_Utils is

   function To_Unbounded_String
     (Input : Utils.Char_Vectors.Char_Vector)
       return Ada.Strings.Unbounded.Unbounded_String;
   --  Convert Input to unbounded string.

   type Restricted_Kind_Predicate is
     new Libadalang.Iterators.Ada_Node_Predicate_Interface with null record;
   --  A custom node predicate to filter some declaration kinds.
   --  See Is_Restricted_Kind for details.

   overriding function Evaluate
     (Ignore : in out Restricted_Kind_Predicate;
      Node   : Libadalang.Analysis.Ada_Node) return Boolean;
   --  Evaluate the Restricted_Kind_Predicate filter
   --  See Is_Restricted_Kind for details.

   type Is_Global_Visible_Predicate is
     new Libadalang.Iterators.Ada_Node_Predicate_Interface with null record;
   --  A custom node predicate to filter some declaration kinds.
   --  See Is_Restricted_Kind for details.

   overriding function Evaluate
     (Ignore : in out Is_Global_Visible_Predicate;
      Node   : Libadalang.Analysis.Ada_Node) return Boolean;
   --  Evaluate the Restricted_Kind_Predicate filter
   --  See Is_Restricted_Kind for details.

   ---------------------
   -- Append_Location --
   ---------------------

   procedure Append_Location
     (Result : in out LSP.Messages.Location_Or_Link_Vector;
      Node   : Libadalang.Analysis.Ada_Node'Class;
      Kind   : LSP.Messages.AlsReferenceKind_Set := LSP.Messages.Empty_Set)
   is
   begin
      Append_Location (Result.Locations, Node, Kind);
   end Append_Location;

   ---------------------
   -- Append_Location --
   ---------------------

   procedure Append_Location
     (Result : in out LSP.Messages.Location_Vector;
      Node   : Libadalang.Analysis.Ada_Node'Class;
      Kind   : LSP.Messages.AlsReferenceKind_Set := LSP.Messages.Empty_Set)
   is
      Location : constant LSP.Messages.Location :=
        LSP.Lal_Utils.Get_Node_Location
          (Libadalang.Analysis.As_Ada_Node (Node), Kind);
   begin
      if not Is_Synthetic (Node) then
         Result.Append (Location);
      end if;
   end Append_Location;

   ---------------------
   -- Append_Location --
   ---------------------

   procedure Append_Location
     (Result   : in out LSP.Messages.DocumentHighlight_Vector;
      Document : not null access LSP.Ada_Documents.Document'Class;
      File     : GNATCOLL.VFS.Virtual_File;
      Node     : Libadalang.Analysis.Ada_Node'Class;
      Kind     : LSP.Messages.Optional_DocumentHighlightKind)
   is
      use type GNATCOLL.VFS.Virtual_File;

      Node_File : constant GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.Create_From_UTF8 (Node.Unit.Get_Filename);

   begin
      if File = Node_File then
         Result.Append
           (LSP.Messages.DocumentHighlight'
              (span => Document.To_LSP_Range (Node.Sloc_Range),
               kind => Kind));
      end if;
   end Append_Location;

   --------------------------------
   -- Sort_And_Remove_Duplicates --
   --------------------------------

   procedure Sort_And_Remove_Duplicates
     (Result : in out LSP.Messages.Location_Vector)
   is
      use type VSS.Unicode.UTF16_Code_Unit_Count;

      function URI_Inf (Left, Right : LSP.Types.LSP_URI) return Boolean;
      --  Comparison function for URIs, return True if Left < Right

      function "<" (Left, Right : LSP.Messages.Location) return Boolean is
        (URI_Inf (Left.uri, Right.uri) or else
           (LSP.Types.Equal (Left.uri, Right.uri)
            and then (Left.span.first.line < Right.span.first.line
                      or else (Left.span.first.line = Right.span.first.line
                               and then Left.span.first.character <
                                 Right.span.first.character))));

      -------------
      -- URI_Inf --
      -------------

      function URI_Inf (Left, Right : LSP.Types.LSP_URI) return Boolean is

         function URI_Dir (X : String) return String;
         --  Return the dir in X

         -------------
         -- URI_Dir --
         -------------

         function URI_Dir (X : String) return String is
         begin
            for J in reverse X'First .. X'Last loop
               --  In an URI the directory separator is '/'
               if X (J) = '/' then
                  return X (X'First .. J - 1);
               end if;
            end loop;
            return X;
         end URI_Dir;

         L : constant String := To_UTF_8_String (Left);
         R : constant String := To_UTF_8_String (Right);

         L_Dir : constant String := URI_Dir (L);
         R_Dir : constant String := URI_Dir (R);
         Ind   : Natural;
      begin
         --  We're being a bit clever when comparing two URIs:
         --    * for a same file, return ".ads" before ".adb"
         --    * return "pack.adb" before "pack-child.adb"

         if L_Dir'Length /= R_Dir'Length then
            --  The directories are different: compare them
            return L_Dir < R_Dir;
         else
            --  The directories have the same length: are they the same?
            for Ind in 1 .. L_Dir'Length - 1 loop
               if L_Dir (L_Dir'First + Ind) /= R_Dir (R_Dir'First + Ind) then
                  return L_Dir (L_Dir'First + Ind) < R_Dir (R_Dir'First + Ind);
               end if;
            end loop;

            --  The directories are the same: compare the filenames
            Ind := L_Dir'Length + 1;
            loop
               if L'First + Ind > L'Last then
                  if R'First + Ind > R'Last then
                     return False;
                  end if;
                  return True;
               end if;

               if R'First + Ind > R'Last then
                  return False;
               end if;

               if L (L'First + Ind) /= R (R'First + Ind) then
                  --  Return "pack.adb" before "pack-child.adb"
                  if L (L'First + Ind) = '-'
                    and then R (R'First + Ind) = '.'
                  then
                     return False;
                  elsif L (L'First + Ind) = '.'
                    and then R (R'First + Ind) = '-'
                  then
                     return True;

                     --   Return ".ads" before ".adb"
                  elsif L'First + Ind = L'Last
                    and then R'First + Ind = R'Last
                  then
                     if L (L'First + Ind) = 's'
                       and then R (R'First + Ind) = 'b'
                     then
                        return True;
                     elsif L (L'First + Ind) = 'b'
                       and then R (R'First + Ind) = 's'
                     then
                        return False;
                     else
                        return L (L'First + Ind) < R (R'First + Ind);
                     end if;

                     --  Normal comparison
                  else
                     return L (L'First + Ind) < R (R'First + Ind);
                  end if;
               end if;

               Ind := Ind + 1;
            end loop;
         end if;
      end URI_Inf;

      package Sort is new
        LSP.Messages.Location_Vectors.Element_Vectors.Generic_Sorting;

      R : LSP.Messages.Location_Vectors.Element_Vectors.Vector :=
        LSP.Messages.Location_Vectors.Element_Vectors.Vector (Result);
      New_Result : LSP.Messages.Location_Vector;
      use type LSP.Messages.Location;
   begin
      Sort.Sort (R);
      for Loc of R loop
         if New_Result.Is_Empty
           or else Loc /= New_Result.Last_Element
         then
            New_Result.Append (Loc);
         end if;
      end loop;
      Result := New_Result;
   end Sort_And_Remove_Duplicates;

   -------------------
   -- Get_Decl_Kind --
   -------------------

   function Get_Decl_Kind
     (Node         : Libadalang.Analysis.Basic_Decl;
      Ignore_Local : Boolean := False)
        return LSP.Messages.SymbolKind is
   begin
      case Node.Kind is
         when Ada_Classic_Subp_Decl |
              Ada_Base_Subp_Body |
              Ada_Entry_Body_Range |
              Ada_Entry_Decl_Range |
              Ada_Generic_Subp_Decl_Range |
              Ada_Generic_Subp_Instantiation_Range |
              Ada_Generic_Subp_Renaming_Decl_Range |
              Ada_Subp_Body_Stub_Range  =>
            return LSP.Messages.A_Function;

         when Ada_Component_Decl |
              Ada_Discriminant_Spec =>
            return LSP.Messages.Field;

         when Ada_Generic_Formal_Obj_Decl |
              Ada_Param_Spec |
              Ada_Exception_Handler |
              Ada_Object_Decl |
              Ada_Extended_Return_Stmt_Object_Decl |
              Ada_Single_Protected_Decl |
              Ada_Single_Task_Decl =>
            return (if Ignore_Local
                    then LSP.Messages.A_Null
                    else
                      (if Laltools.Common.Is_Constant (Node)
                       then LSP.Messages.A_Constant
                       else LSP.Messages.Variable));

         when Ada_Base_Package_Decl |
              Ada_Generic_Formal_Package |
              --  Ignore: Ada_Generic_Package_Decl kind, this node always have
              --  an Ada_Generic_Package_Internal as a child and we will use it
              --  to create the CompletionItem/DocumentSymbol
              Ada_Generic_Package_Instantiation |
              Ada_Generic_Package_Renaming_Decl |
              Ada_Package_Renaming_Decl =>
            return LSP.Messages.A_Package;

         when Ada_Package_Body_Stub |
              Ada_Protected_Body_Stub |
              Ada_Task_Body_Stub |
              Ada_Package_Body |
              Ada_Protected_Body |
              Ada_Task_Body =>
            return LSP.Messages.Module;

         when Ada_Concrete_Type_Decl |
              Ada_Formal_Type_Decl =>
            return (if Laltools.Common.Is_Structure (Node)
                    then LSP.Messages.Struct
                    else LSP.Messages.Class);

         when Ada_Generic_Formal_Type_Decl |
              Ada_Classwide_Type_Decl |
              Ada_Incomplete_Type_Decl |
              Ada_Incomplete_Tagged_Type_Decl |
              Ada_Protected_Type_Decl |
              Ada_Task_Type_Decl |
              Ada_Subtype_Decl |
              Ada_Anonymous_Type_Decl |
              Ada_Synth_Anonymous_Type_Decl =>
            return LSP.Messages.Class;

         when Ada_Entry_Index_Spec |
              Ada_Number_Decl =>
            return LSP.Messages.Number;

         when Ada_Enum_Literal_Decl =>
            return (if Ignore_Local
                    then LSP.Messages.A_Null
                    else LSP.Messages.Enum);

         when Ada_Exception_Decl =>
            return LSP.Messages.String;

         when Ada_For_Loop_Var_Decl |
              Ada_Label_Decl |
              Ada_Named_Stmt_Decl =>
            return (if Ignore_Local
                    then LSP.Messages.A_Null
                    else LSP.Messages.A_Constant);

         when others =>
            null;
      end case;

      return LSP.Messages.A_Null;
   end Get_Decl_Kind;

   -------------------
   -- Get_Call_Expr --
   -------------------

   function Get_Call_Expr
     (Node : Libadalang.Analysis.Ada_Node'Class)
      return Libadalang.Analysis.Call_Expr
   is
      Cur_Node : Ada_Node := Node.As_Ada_Node;
   begin
      if not Cur_Node.Is_Null
        and then Cur_Node.Kind in Ada_Error_Stmt_Range
      then
         --  In case of Error_Stmt, find the nearest previous sibling
         --  which is not also an Error_Stmt
         while not Cur_Node.Is_Null
           and then Cur_Node.Kind in Ada_Error_Stmt_Range
         loop
            Cur_Node := Cur_Node.Previous_Sibling;
         end loop;

         --  Find the nearest Call_Expr node in the children
         if not Cur_Node.Is_Null then
            for Child_Node of Cur_Node.Children loop
               if Child_Node.Kind in Ada_Call_Expr_Range then
                  Cur_Node := Child_Node;
                  exit;
               end if;
            end loop;
         end if;
      end if;

      --  Find the nearest Call_Expr node in the parents or itself
      while not Cur_Node.Is_Null loop
         exit when Cur_Node.Kind in Ada_Call_Expr_Range;

         Cur_Node := Cur_Node.Parent;
      end loop;

      --  At this point we have null or a Call_Expr
      if Cur_Node.Is_Null then
         return No_Call_Expr;
      else
         return Cur_Node.As_Call_Expr;
      end if;
   end Get_Call_Expr;

   --------------------------
   -- Get_Call_Designators --
   --------------------------

   function Get_Call_Designators
     (Node : Libadalang.Analysis.Call_Expr)
      return Laltools.Common.Node_Vectors.Vector
   is
      Designator : Libadalang.Analysis.Ada_Node;
      Res        : Laltools.Common.Node_Vectors.Vector :=
        Laltools.Common.Node_Vectors.Empty_Vector;
   begin
      if Node = No_Call_Expr then
         return Res;
      end if;

      declare
         Suffix_Node : constant Libadalang.Analysis.Ada_Node'Class :=
           Node.F_Suffix;
      begin
         if Suffix_Node /= Libadalang.Analysis.No_Ada_Node
           and then Suffix_Node.Kind in Ada_Assoc_List_Range
         then
            for Assoc of Suffix_Node.As_Assoc_List loop
               Designator := Assoc.As_Param_Assoc.F_Designator;
               if Designator /= No_Ada_Node then
                  Res.Append (Designator);
               end if;
            end loop;
         end if;
      end;
      return Res;
   end Get_Call_Designators;

   ------------------------
   -- Get_Call_Expr_Name --
   ------------------------

   procedure Get_Call_Expr_Name
     (Node             : Libadalang.Analysis.Ada_Node'Class;
      Cursor           : Langkit_Support.Slocs.Source_Location;
      Active_Position  : out LSP.Types.LSP_Number;
      Designator       : out Libadalang.Analysis.Ada_Node;
      Prev_Designators : out Laltools.Common.Node_Vectors.Vector;
      Name_Node        : out Libadalang.Analysis.Name)
   is
      use Langkit_Support.Slocs;
      In_Assoc_List  : Boolean  := False;
      --  True if the cursor is a node of the Assoc_List
      Is_New_Param  : Boolean  := False;

      Call_Expr_Node : constant Libadalang.Analysis.Call_Expr :=
        Get_Call_Expr (Node);

      function Cursor_In_Node (N : Ada_Node) return Boolean;
      --  Check if N contains the cursor

      function Cursor_On_Last_Par return Boolean;
      --  Return True when the cursor is located on the closing parenthesis
      --  of Call_Expr_Node

      --------------------
      -- Cursor_In_Node --
      --------------------

      function Cursor_In_Node (N : Ada_Node) return Boolean is
      begin
         case Libadalang.Analysis.Compare (N, Cursor) is
            when Inside =>
               In_Assoc_List := True;
               return True;
            when Before =>
               --  Case to handle:
               --      Foo (1, |
               --      Bar (1, 2);
               --  LAL error recovery will assume that Bar (1, 2); is the
               --  second param of Foo. At this point we are in the Assoc_List
               --  and we need to stop going through the list of parameters.
               In_Assoc_List := True;
               return True;
            when After =>
               In_Assoc_List := False;
               return False;
         end case;
      end Cursor_In_Node;

      ------------------------
      -- Cursor_On_Last_Par --
      ------------------------

      function Cursor_On_Last_Par return Boolean is
         Call_Text : constant Langkit_Support.Text.Text_Type :=
           Call_Expr_Node.Text;
         Open_Cpt  : Natural := 0;
         Close_Cpt : Natural := 0;
      begin
         if Call_Text (Call_Text'Last) /= ')' then
            --  Not on a closing parenthesis
            return False;
         end if;

         --  Count the open/closing parentheses
         for C of Call_Text loop
            if C = '(' then
               Open_Cpt := Open_Cpt + 1;
            elsif C = ')' then
               Close_Cpt := Close_Cpt + 1;
            end if;
         end loop;

         return Open_Cpt = Close_Cpt;
      end Cursor_On_Last_Par;

   begin
      Active_Position := 0;
      Designator := Libadalang.Analysis.No_Ada_Node;
      Prev_Designators := Laltools.Common.Node_Vectors.Empty_Vector;
      Name_Node := Libadalang.Analysis.No_Name;

      if Call_Expr_Node = No_Call_Expr
        or else (End_Sloc (Call_Expr_Node.Sloc_Range) = Cursor
                 and then Cursor_On_Last_Par)
      then
         return;
      end if;

      declare
         Suffix_Node : constant Libadalang.Analysis.Ada_Node'Class :=
           Call_Expr_Node.F_Suffix;
      begin
         Name_Node := Call_Expr_Node.F_Name;

         if Name_Node.Kind in Ada_Dotted_Name_Range then
            declare
               Dot_Name : constant Dotted_Name := Name_Node.As_Dotted_Name;
            begin
               --  If the prefix is a parameter then increase the
               --  Active_Position by 1
               if Dot_Name.P_Is_Dot_Call (Imprecise_Fallback => True) then
                  Active_Position := Active_Position + 1;
               end if;
            end;
         end if;

         if Suffix_Node = Libadalang.Analysis.No_Ada_Node then
            return;
         end if;

         --  Find the position in the Assoc_List
         if Suffix_Node.Kind in Ada_Assoc_List_Range then
            declare
               Assoc_Text : constant Langkit_Support.Text.Text_Type :=
                 Suffix_Node.Text;
            begin
               --  In case of "Foo (1," the assoc_list text will be "1,"
               Is_New_Param := Assoc_Text (Assoc_Text'Last) = ',';
            end;

            for Assoc of Suffix_Node.As_Assoc_List loop
               Designator := Assoc.As_Param_Assoc.F_Designator;
               Active_Position := Active_Position + 1;
               exit when Cursor_In_Node (Assoc.As_Ada_Node);
               if Designator /= No_Ada_Node then
                  Prev_Designators.Append (Designator);
               end if;
            end loop;
         end if;

         if not In_Assoc_List and then Is_New_Param then
            --  The user has written "Foo (1,|" or "Foo (1, |" at this point
            --  LAL only has one node in the Assoc_List however we are at the
            --  second active position.
            Designator := No_Ada_Node;
         else
            Active_Position := Active_Position - 1;
         end if;
      end;
   end Get_Call_Expr_Name;

   --------------------
   -- Get_Parameters --
   --------------------

   procedure Get_Parameters
     (Node : Libadalang.Analysis.Basic_Decl;
      Parameters : in out LSP.Messages.ParameterInformation_Vector)
   is
      Spec : constant Libadalang.Analysis.Base_Subp_Spec :=
        Node.P_Subp_Spec_Or_Null;
   begin
      if Spec = Libadalang.Analysis.No_Base_Subp_Spec then
         return;
      end if;

      for Param of Spec.P_Params loop
         for Id of Param.F_Ids loop
            declare
               P : constant LSP.Messages.ParameterInformation :=
                 (label         =>
                    (Is_String => True,
                     String    => LSP.Lal_Utils.To_Virtual_String (Id.Text)),
                  documentation =>
                    (Is_Set => False)
                 );
            begin
               Parameters.Append (P);
            end;
         end loop;
      end loop;
   end Get_Parameters;

   --------------------------
   -- Get_Active_Parameter --
   --------------------------

   function Get_Active_Parameter
     (Node             : Libadalang.Analysis.Basic_Decl;
      Designator       : Libadalang.Analysis.Ada_Node;
      Prev_Designators : Laltools.Common.Node_Vectors.Vector;
      Position         : LSP.Types.LSP_Number)
      return LSP.Types.LSP_Number
   is
      Spec : constant Libadalang.Analysis.Base_Subp_Spec :=
        Node.P_Subp_Spec_Or_Null;
      Res  : LSP.Types.LSP_Number := -1;

      function Find_Designator
        (D      : Libadalang.Analysis.Ada_Node;
         Params : Libadalang.Analysis.Param_Spec_Array)
         return LSP.Types.LSP_Number;

      function Count_Parameters
        (Params : Libadalang.Analysis.Param_Spec_Array)
         return LSP.Types.LSP_Number;

      ---------------------
      -- Find_Designator --
      ---------------------

      function Find_Designator
        (D      : Libadalang.Analysis.Ada_Node;
         Params : Libadalang.Analysis.Param_Spec_Array)
         return LSP.Types.LSP_Number
      is
         Index : LSP.Types.LSP_Number := 0;
      begin
         for Param of Params loop
            for Id of Param.F_Ids loop
               if Id.Text = D.Text then
                  return Index;
               end if;
               Index := Index + 1;
            end loop;
         end loop;

         return -1;
      end Find_Designator;

      ----------------------
      -- Count_Parameters --
      ----------------------

      function Count_Parameters
        (Params : Libadalang.Analysis.Param_Spec_Array)
         return LSP.Types.LSP_Number
      is
         Index : LSP.Types.LSP_Number := 0;
      begin
         for Param of Params loop
            for Id of Param.F_Ids loop
               Index := Index + 1;
            end loop;
         end loop;

         return Index;
      end Count_Parameters;

   begin
      if Spec = Libadalang.Analysis.No_Base_Subp_Spec then
         return -1;
      end if;

      declare
         Params : constant Libadalang.Analysis.Param_Spec_Array :=
           Spec.P_Params;
      begin
         if Designator = Libadalang.Analysis.No_Ada_Node then
            --  Check if the given position is a valid index for Node

            if Position >= Count_Parameters (Params) then
               return -1;
            else
               Res := Position;
            end if;

         else
            --  If we have a designator then try to find the position of a
            --  parameter with the same name
            Res := Find_Designator (Designator, Params);

         end if;

         --  Invalidate the result if it doesn't match the previous designators
         if not Match_Designators (Params, Prev_Designators) then
            return -1;
         end if;
      end;

      return Res;
   end Get_Active_Parameter;

   -----------------------
   -- Match_Designators --
   -----------------------

   function Match_Designators
     (Params      : Libadalang.Analysis.Param_Spec_Array;
      Designators : Laltools.Common.Node_Vectors.Vector)
      return Boolean
   is
      function Find_Designator
        (D : Libadalang.Analysis.Ada_Node)
         return Boolean;

      ---------------------
      -- Find_Designator --
      ---------------------

      function Find_Designator
        (D : Libadalang.Analysis.Ada_Node)
         return Boolean is
      begin
         for Param of Params loop
            for Id of Param.F_Ids loop
               if Id.Text = D.Text then
                  return True;
               end if;
            end loop;
         end loop;

         return False;
      end Find_Designator;
   begin
      for D of Designators loop
         if not Find_Designator (D) then
            return False;
         end if;
      end loop;

      return True;
   end Match_Designators;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location
     (Unit : Libadalang.Analysis.Analysis_Unit;
      Span : Langkit_Support.Slocs.Source_Location_Range;
      Kind : LSP.Messages.AlsReferenceKind_Set := LSP.Messages.Empty_Set)
      return LSP.Messages.Location is
   begin
      return
        (uri     => LSP.Types.File_To_URI (Unit.Get_Filename),
         span    => To_Span (Span),
         alsKind => Kind);
   end Get_Location;

   -----------------------
   -- Get_Node_Location --
   -----------------------

   function Get_Node_Location
     (Node : Libadalang.Analysis.Ada_Node'Class;
      Kind : LSP.Messages.AlsReferenceKind_Set := LSP.Messages.Empty_Set)
      return LSP.Messages.Location is
   begin
      return Get_Location (Node.Unit, Node.Sloc_Range, Kind);
   end Get_Node_Location;

   ------------------------
   -- Get_Token_Location --
   ------------------------

   function Get_Token_Span
     (Token : Libadalang.Common.Token_Reference)
      return LSP.Messages.Span is
   begin
      return To_Span (Sloc_Range (Data (Token)));
   end Get_Token_Span;

   -------------
   -- To_Span --
   -------------

   function To_Span
     (Value : Langkit_Support.Slocs.Source_Location_Range)
      return LSP.Messages.Span
   is
      use type VSS.Unicode.UTF16_Code_Unit_Count;
      use type Langkit_Support.Slocs.Source_Location_Range;

      Result : constant LSP.Messages.Span :=
        (if Value = Langkit_Support.Slocs.No_Source_Location_Range then
           LSP.Messages.Empty_Span
         else
           (first =>
                (line      => LSP.Types.Line_Number (Value.Start_Line) - 1,
                 character => LSP.Types.UTF_16_Index   --  FIXME (UTF16 index)!
                   (Value.Start_Column) - 1),
            last =>
              (line => LSP.Types.Line_Number (Value.End_Line) - 1,
               character => LSP.Types.UTF_16_Index  --  FIXME (UTF16 index)!
                 (Value.End_Column) - 1)));
         --  XXX Code unit offset computation is incorrect here

   begin
      return Result;
   end To_Span;

   -------------
   -- To_Span --
   -------------

   function To_Span
     (Value : Langkit_Support.Slocs.Source_Location)
      return LSP.Messages.Span
   is
      use type VSS.Unicode.UTF16_Code_Unit_Count;

      Result : constant LSP.Messages.Span :=
        (first =>
           (line      => LSP.Types.Line_Number (Value.Line) - 1,
            character => LSP.Types.UTF_16_Index
              (Value.Column) - 1),
         last =>
           (line => LSP.Types.Line_Number (Value.Line) - 1,
            character => LSP.Types.UTF_16_Index
              (Value.Column) - 1));
   begin
      return Result;
   end To_Span;

   -----------------
   -- To_TextEdit --
   -----------------

   function To_TextEdit
     (E : Laltools.Refactor.Text_Edit)
      return LSP.Messages.TextEdit
   is (LSP.Messages.TextEdit'
         (To_Span (E.Location),
          VSS.Strings.Conversions.To_Virtual_String (E.Text)));

   -----------------------
   -- To_Workspace_Edit --
   -----------------------

   function To_Workspace_Edit
     (EM                  : Laltools.Refactor.Text_Edit_Map;
      Versioned_Documents : Boolean := False;
      Document_Provider   : access LSP.Ada_Documents.Document_Provider'Class
      := null)
      return LSP.Messages.WorkspaceEdit
   is
      File_URI : LSP.Messages.DocumentUri;

      Text_Edits : LSP.Messages.TextEdit_Vector;

      use Laltools.Refactor.Text_Edit_Ordered_Maps;

      Edits_Cursor : Cursor := EM.First;

   begin
      return WE : LSP.Messages.WorkspaceEdit do
         while Has_Element (Edits_Cursor) loop
            Text_Edits.Clear;

            for Edit of Element (Edits_Cursor) loop
               Text_Edits.Append (To_TextEdit (Edit));
            end loop;

            File_URI := LSP.Types.File_To_URI (Key (Edits_Cursor));

            if Versioned_Documents then
               declare
                  Annotaded_Edits : LSP.Messages.AnnotatedTextEdit_Vector;
               begin
                  Annotaded_Edits.Reserve_Capacity (Text_Edits.Capacity);
                  for X of Text_Edits loop
                     Annotaded_Edits.Append
                       (LSP.Messages.AnnotatedTextEdit'
                          (X with annotationId => <>));
                  end loop;

                  WE.documentChanges.Append
                    (LSP.Messages.Document_Change'(
                     (Kind               => LSP.Messages.Text_Document_Edit,
                      Text_Document_Edit => LSP.Messages.TextDocumentEdit'
                        (textDocument => Document_Provider.
                                          Get_Open_Document_Version (File_URI),
                         edits        => Annotaded_Edits))));
               end;
            else
               WE.changes.Insert (File_URI, Text_Edits);
            end if;

            Next (Edits_Cursor);
         end loop;
      end return;
   end To_Workspace_Edit;

   -----------------------
   -- To_Workspace_Edit --
   -----------------------

   function To_Workspace_Edit
     (Edits               : Laltools.Refactor.Refactoring_Edits;
      Resource_Operations : LSP.Messages.Optional_ResourceOperationKindSet :=
        LSP.Messages.Optional_ResourceOperationKindSet'(Is_Set => False);
      Versioned_Documents : Boolean := False;
      Document_Provider   : access LSP.Ada_Documents.Document_Provider'Class :=
        null;
      Rename              : Boolean := False)
      return LSP.Messages.WorkspaceEdit
   is
      use type VSS.Strings.Virtual_String;

      File_URI : LSP.Types.LSP_URI;

      Text_Edits : LSP.Messages.TextEdit_Vector;

      use Laltools.Refactor;
      use LSP.Messages;

      Text_Edits_Cursor     : Text_Edit_Ordered_Maps.Cursor
        := Edits.Text_Edits.First;
      File_Deletions_Cursor : Unbounded_String_Ordered_Sets.Cursor
        := Edits.File_Deletions.First;

      Is_Create_Supported : constant Boolean :=
        Resource_Operations.Is_Set
        and then ResourceOperationKindSets.Contains
          (ResourceOperationKindSets.Set (Resource_Operations.Value), create);
      Is_Rename_Supported : constant Boolean :=
        Resource_Operations.Is_Set
        and then ResourceOperationKindSets.Contains
          (ResourceOperationKindSets.Set (Resource_Operations.Value),
           LSP.Messages.rename);
      Is_Delete_Supported : constant Boolean :=
        Resource_Operations.Is_Set
        and then ResourceOperationKindSets.Contains
          (ResourceOperationKindSets.Set (Resource_Operations.Value), delete);

   begin
      return WE : LSP.Messages.WorkspaceEdit do
         --  Text edits

         while Text_Edit_Ordered_Maps.Has_Element (Text_Edits_Cursor) loop
            Text_Edits.Clear;

            for Edit of Text_Edit_Ordered_Maps.Element (Text_Edits_Cursor) loop
               Text_Edits.Append (To_TextEdit (Edit));
            end loop;

            File_URI := LSP.Types.File_To_URI
              (Text_Edit_Ordered_Maps.Key (Text_Edits_Cursor));

            --  If `workspace.workspaceEdit.documentChanges` client capability
            --  was true, then use `TextDocumentEdit[]` instead of
            --  `TextEdit[]`.

            if Versioned_Documents then
               declare
                  Annotaded_Edits : LSP.Messages.AnnotatedTextEdit_Vector;

               begin
                  Annotaded_Edits.Reserve_Capacity (Text_Edits.Capacity);
                  for X of Text_Edits loop
                     Annotaded_Edits.Append
                       (LSP.Messages.AnnotatedTextEdit'
                          (X with annotationId => <>));
                  end loop;

                  WE.documentChanges.Append
                    (LSP.Messages.Document_Change'(
                     (Kind               => LSP.Messages.Text_Document_Edit,
                      Text_Document_Edit => LSP.Messages.TextDocumentEdit'
                        (textDocument => Document_Provider.
                           Get_Open_Document_Version (File_URI),
                         edits        => Annotaded_Edits))));
               end;
            else
               WE.changes.Insert (File_URI, Text_Edits);
            end if;

            Text_Edit_Ordered_Maps.Next (Text_Edits_Cursor);
         end loop;

         --  Resource operations are only supported if
         --  `workspace.workspaceEdit.documentChanges` is True since they
         --  must be sent in the `documentChanges` field.
         --  `workspace.workspaceEdit.resourceOperations` client capability
         --  must be checked in order to know which kind of operations are
         --  supported.

         --  File creations

         if Versioned_Documents and then Is_Create_Supported then
            for File_Creation of Edits.File_Creations loop
               WE.documentChanges.Append
                 (LSP.Messages.Document_Change'(
                  (Kind        => LSP.Messages.Create_File,
                   Create_File => LSP.Messages.CreateFile'
                     (uri    => LSP.Types.File_To_URI (File_Creation.Filepath),
                      others => <>))));

               declare
                  Annotaded_Edits : LSP.Messages.AnnotatedTextEdit_Vector;
                  Content : constant LSP.Messages.AnnotatedTextEdit :=
                    LSP.Messages.AnnotatedTextEdit'
                      (span    => ((0, 0), (0, 0)),
                       newText =>
                         VSS.Strings.Conversions.To_Virtual_String
                           (File_Creation.Content),
                       others  => <>);

               begin
                  Annotaded_Edits.Append (Content);

                  WE.documentChanges.Append
                    (LSP.Messages.Document_Change'(
                     (Kind               => LSP.Messages.Text_Document_Edit,
                      Text_Document_Edit => LSP.Messages.TextDocumentEdit'
                        (edits => Annotaded_Edits,
                         others => <>))));
               end;
            end loop;
         end if;

         --  File deletions

         if Versioned_Documents and then Is_Delete_Supported then
            while Unbounded_String_Ordered_Sets.Has_Element
              (File_Deletions_Cursor)
            loop

               File_URI := LSP.Types.File_To_URI
                 (Unbounded_String_Ordered_Sets.Element
                    (File_Deletions_Cursor));

               WE.documentChanges.Append
                 (LSP.Messages.Document_Change'(
                  (Kind        => LSP.Messages.Rename_File,
                   Rename_File => LSP.Messages.RenameFile'
                     (oldUri       => File_URI,
                      newUri       =>
                        (if Rename
                         then LSP.Types.To_LSP_URI
                           (LSP.Types.To_Virtual_String (File_URI) & ".bak")
                         else File_URI),
                      others => <>))));

               Unbounded_String_Ordered_Sets.Next (File_Deletions_Cursor);
            end loop;
         end if;

         --  File renames

         if Versioned_Documents and then Is_Rename_Supported then
            for File_Rename of Edits.File_Renames loop
               WE.documentChanges.Append
                 (LSP.Messages.Document_Change'(
                  (Kind        => LSP.Messages.Rename_File,
                   Rename_File => LSP.Messages.RenameFile'
                     (oldUri => LSP.Types.File_To_URI (File_Rename.Filepath),
                      newUri => LSP.Types.File_To_URI (File_Rename.New_Name),
                      others => <>))));
            end loop;
         end if;
      end return;
   end To_Workspace_Edit;

   ------------------
   -- Canonicalize --
   ------------------

   function Canonicalize
     (Text : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String
   is
      UTF_32 : constant Wide_Wide_String :=
        VSS.Strings.Conversions.To_Wide_Wide_String (Text);
      Result : constant Symbolization_Result :=
        Libadalang.Sources.Canonicalize (UTF_32);

   begin
      if Result.Success then
         return LSP.Lal_Utils.To_Virtual_String (Result.Symbol);
      else
         return VSS.Strings.Empty_Virtual_String;
      end if;
   end Canonicalize;

   --------------------
   -- Compute_Detail --
   --------------------

   function Compute_Completion_Detail
     (BD : Libadalang.Analysis.Basic_Decl) return VSS.Strings.Virtual_String
   is
      Result : VSS.Strings.Virtual_String;

   begin

      --  If the basic declaration is an enum literal, display the whole
      --  enumeration type declaration instead.
      if BD.Kind in Ada_Enum_Literal_Decl then
         Result := LSP.Common.Get_Hover_Text
           (As_Enum_Literal_Decl (BD).P_Enum_Type.As_Basic_Decl);
      else
         Result := LSP.Common.Get_Hover_Text (BD);
      end if;

      return Result;
   end Compute_Completion_Detail;

   ----------------------------
   -- Compute_Completion_Doc --
   ----------------------------

   function Compute_Completion_Doc
     (BD : Libadalang.Analysis.Basic_Decl) return VSS.Strings.Virtual_String
   is
      Doc_Text : VSS.Strings.Virtual_String;
      Loc_Text : VSS.Strings.Virtual_String;
   begin
      Doc_Text :=
        VSS.Strings.To_Virtual_String
          (Libadalang.Doc_Utils.Get_Documentation
             (BD).Doc.To_String);

      --  Append the declaration's location.
      --  In addition, append the project's name if we are dealing with
      --  an aggregate project.

      Loc_Text.Append (LSP.Lal_Utils.Node_Location_Image (BD));

      if not Doc_Text.Is_Empty then
         Loc_Text.Append
           (VSS.Strings.To_Virtual_String
              ((1 .. 2 => Ada.Characters.Wide_Wide_Latin_1.LF)));

         Loc_Text.Append (Doc_Text);
      end if;

      return Loc_Text;
   end Compute_Completion_Doc;

   -----------------------
   -- Containing_Entity --
   -----------------------

   function Containing_Entity
     (Ref       : Ada_Node;
      Canonical : Boolean := True) return Defining_Name
   is
      Parents : constant Ada_Node_Array := Ref.Parents;
   begin
      for Parent of Parents loop
         if Parent.Kind in Ada_Subp_Decl
                         | Ada_Subp_Body
                         | Ada_Task_Def
                         | Ada_Task_Body
                         | Ada_Package_Body
                         | Ada_Package_Decl
         then
            if Canonical then
               return Parent.As_Basic_Decl.P_Canonical_Part.P_Defining_Name;
            else
               return Parent.As_Basic_Decl.P_Defining_Name;
            end if;
         end if;
      end loop;

      return No_Defining_Name;
   end Containing_Entity;

   -------------------------
   -- Find_Incoming_Calls --
   -------------------------

   function Find_Incoming_Calls
     (Context           : LSP.Ada_Contexts.Context;
      Definition        : Defining_Name;
      Imprecise_Results : out Boolean)
      return Laltools.Common.References_By_Subprogram.Map
   is
      use Laltools.Common.References_By_Subprogram;
      use Laltools.Common.References_Sets;

      procedure Callback
        (Ref    : Libadalang.Analysis.Base_Id;
         Kind   : Libadalang.Common.Ref_Result_Kind;
         Cancel : in out Boolean);

      Result     : Map;

      --------------
      -- Callback --
      --------------

      procedure Callback
        (Ref     : Libadalang.Analysis.Base_Id;
         Kind    : Libadalang.Common.Ref_Result_Kind;
         Cancel  : in out Boolean)
      is
         pragma Unreferenced (Cancel);
         Containing : Defining_Name;
      begin
         if Kind = Libadalang.Common.Imprecise then
            Imprecise_Results := True;
         end if;

         --  We have a reference, and this a call: find the containing
         --  subprogram or task
         Containing := Containing_Entity
           (Ref.As_Ada_Node, Canonical => False);

         if Containing /= No_Defining_Name then
            if Result.Contains (Containing) then
               Result (Containing).Include (Ref);
            else
               declare
                  L : Set;
               begin
                  L.Include (Ref);
                  Result.Insert (Containing, L);
               end;
            end if;
         end if;
      end Callback;

   begin
      Imprecise_Results := False;

      --  Go through all references to Definition, organising them by
      --  containing subprogram.

      --  Obtain all the references
      Context.Find_All_Calls (Definition, Callback'Access);

      return Result;
   end Find_Incoming_Calls;

   -------------------------
   -- Find_Outgoing_Calls --
   -------------------------

   function Find_Outgoing_Calls
     (Context           : LSP.Ada_Contexts.Context;
      Definition        : Defining_Name;
      Imprecise_Results : out Boolean)
      return Laltools.Common.References_By_Subprogram.Map
   is
      use Laltools.Common.References_By_Subprogram;
      use Laltools.Common.References_Sets;

      Result : Laltools.Common.References_By_Subprogram.Map;

      procedure Callback (Subp_Call : Ada_Node'Class);

      --------------
      -- Callback --
      --------------

      procedure Callback (Subp_Call : Ada_Node'Class)
      is
         Call_Definition : Defining_Name;
         Subp_Call_Name  : constant Name :=
           Laltools.Common.Get_Node_As_Name (Subp_Call.As_Ada_Node);
      begin

         --  First try to resolve the called function

         Call_Definition := Laltools.Common.Resolve_Name
           (Subp_Call_Name, Context.Trace, Imprecise_Results);

         if Call_Definition /= No_Defining_Name then
            if Result.Contains (Call_Definition) then
               declare
                  R              : constant
                    Laltools.Common.References_By_Subprogram.
                      Reference_Type :=
                        Result.Reference (Call_Definition);
               begin
                  R.Include (Subp_Call.As_Base_Id);
               end;
            else
               declare
                  L : Laltools.Common.References_Sets.Set;
               begin
                  L.Include (Subp_Call.As_Base_Id);
                  Result.Insert (Call_Definition, L);
               end;
            end if;
         end if;

      end Callback;
   begin
      Imprecise_Results := False;

      Laltools.Call_Hierarchy.Find_Outgoing_Calls
           (Definition => Definition,
            Callback   => Callback'Access,
            Trace      => Context.Trace,
            Imprecise  => Imprecise_Results);

      return Result;
   end Find_Outgoing_Calls;

   ----------------------------
   -- To_Call_Hierarchy_Item --
   ----------------------------

   function To_Call_Hierarchy_Item
     (Name : Libadalang.Analysis.Defining_Name)
      return LSP.Messages.CallHierarchyItem
   is
      Main_Item : constant Libadalang.Analysis.Basic_Decl :=
        Name.P_Basic_Decl;

      Where     : constant LSP.Messages.Location :=
        LSP.Lal_Utils.Get_Node_Location (Main_Item);
   begin
      return LSP.Messages.CallHierarchyItem'
        (name           => VSS.Strings.To_Virtual_String (Name.Text),
         kind           => LSP.Lal_Utils.Get_Decl_Kind (Main_Item),
         tags           => (Is_Set => False),
         detail         => (True, LSP.Lal_Utils.Node_Location_Image (Name)),
         uri            => Where.uri,
         span           => Where.span,
         selectionRange => LSP.Lal_Utils.To_Span (Name.Sloc_Range));
   end To_Call_Hierarchy_Item;

   ------------------------------
   -- To_Call_Hierarchy_Result --
   ------------------------------

   procedure To_Call_Hierarchy_Result
     (Node  : Libadalang.Analysis.Defining_Name;
      Refs  : Laltools.Common.References_Sets.Set;
      Item  : out LSP.Messages.CallHierarchyItem;
      Spans : out LSP.Messages.Span_Vector;
      Kinds : out LSP.Messages.AlsReferenceKind_Vector)
   is
      Decl     : constant Libadalang.Analysis.Basic_Decl :=
        Node.P_Basic_Decl;
      Location : constant LSP.Messages.Location :=
        Get_Node_Location (Ada_Node (Node));
   begin
      Item := LSP.Messages.CallHierarchyItem'
        (name           => VSS.Strings.To_Virtual_String (Node.Text),
         kind           => Get_Decl_Kind (Decl),
         tags           => <>,
         detail         => <>,
         uri            => Location.uri,
         span           => Location.span,
         selectionRange => LSP.Lal_Utils.To_Span (Node.Sloc_Range));

      Spans.Clear;
      Kinds.Clear;

      for Ref of Refs loop
         declare
            Ref_Location : constant LSP.Messages.Location :=
              Get_Node_Location (Ada_Node (Ref));
         begin
            Spans.Append (Ref_Location.span);
            if Ref.P_Is_Dispatching_Call then
               Kinds.Append (LSP.Messages.Dispatching_Call);
            else
               Kinds.Append (LSP.Messages.Simple);
            end if;
         end;
      end loop;
   end To_Call_Hierarchy_Result;

   -----------------------
   -- To_Virtual_String --
   -----------------------

   function To_Virtual_String
     (Item : Langkit_Support.Text.Text_Type)
      return VSS.Strings.Virtual_String is
   begin
      return VSS.Strings.To_Virtual_String (Item);
   end To_Virtual_String;

   -----------------------
   -- To_Virtual_String --
   -----------------------

   function To_Virtual_String
     (Item : Langkit_Support.Text.Unbounded_Text_Type)
      return VSS.Strings.Virtual_String is
   begin
      return
        VSS.Strings.To_Virtual_String
          (Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String (Item));
   end To_Virtual_String;

   -------------------------
   -- Node_Location_Image --
   -------------------------

   function Node_Location_Image
     (Node : Libadalang.Analysis.Ada_Node'Class)
      return VSS.Strings.Virtual_String
   is
      Decl_Unit_File : constant GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.Create_From_UTF8 (Node.Unit.Get_Filename);

   begin
      return Result : VSS.Strings.Virtual_String do
         Result.Append ("at ");
         Result.Append
           (VSS.Strings.Conversions.To_Virtual_String
              (Decl_Unit_File.Display_Base_Name));
         Result.Append (" (");
         Result.Append
           (VSS.Strings.Conversions.To_Virtual_String
              (GNATCOLL.Utils.Image
                   (Integer (Node.Sloc_Range.Start_Line), Min_Width => 1)));
         Result.Append (':');
         Result.Append
           (VSS.Strings.Conversions.To_Virtual_String
              (GNATCOLL.Utils.Image
                   (Integer (Node.Sloc_Range.Start_Column), Min_Width => 1)));
         Result.Append (')');
      end return;
   end Node_Location_Image;

   -------------
   -- Is_Task --
   -------------

   --  TODO: Reactivate these lines when libadalang supports
   --  P_Next_Part for tasks: T716-049

   --  function Is_Task
   --    (Node      : Ada_Node'Class;
   --     Trace     : GNATCOLL.Traces.Trace_Handle;
   --     Imprecise : out Boolean) return Boolean is
   --  begin
   --     return Ada_Node (N) /= No_Ada_Node
   --       and then N.Kind in Ada_Name
   --       and then (N.P_Basic_Decl.Kind = Ada_Task_Body or else
   --                 N.P_Basic_Decl.Kind = Ada_Single_Task_Type_Decl or else
   --                 N.P_Basic_Decl.Kind = Ada_Task_Type_Decl);
   --  end Is_Task;

   -------------------------
   -- To_Unbounded_String --
   -------------------------

   function To_Unbounded_String
     (Input : Utils.Char_Vectors.Char_Vector)
      return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Result : Ada.Strings.Unbounded.Unbounded_String do
         for Char of Input loop
            Ada.Strings.Unbounded.Append (Result, Char);
         end loop;
      end return;
   end To_Unbounded_String;

   -------------------
   -- Format_Vector --
   -------------------

   procedure Format_Vector
     (Cmd       : Utils.Command_Lines.Command_Line;
      Input     : Utils.Char_Vectors.Char_Vector;
      Node      : Ada_Node;
      In_Sloc   : Langkit_Support.Slocs.Source_Location_Range;
      Output    : out Utils.Char_Vectors.Char_Vector;
      Out_Sloc  : out Langkit_Support.Slocs.Source_Location_Range;
      Messages  : out Pp.Scanner.Source_Message_Vector)
   is
      use type Langkit_Support.Slocs.Source_Location_Range;

      procedure Tokenize_Output;
      --  Split Output document into tokens and store them into TDH

      procedure Synchronize_Tokens
        (In_Stop   : Token_Reference;
         Out_Stop  : out Langkit_Support.Token_Data_Handlers.Token_Index;
         In_Start  : Token_Reference;
         Out_Start : Langkit_Support.Token_Data_Handlers.Token_Index;
         Ok        : out Boolean);
      --  Find a token in Output document that corresponds to Is_Stop token in
      --  the Input document. Store token index into Out_Stop. To do this
      --  start scanning both token chains starting from In_Start (for Input)
      --  and Out_Start (for Output document). If no corresponding token found
      --  return Ok = False.

      function Lookup_Token
        (Sloc : Langkit_Support.Slocs.Source_Location) return Token_Reference;
      --  Like Node.Unit.Lookup_Token, but skip Trivia

      TDH     : Langkit_Support.Token_Data_Handlers.Token_Data_Handler;
      Diags   : Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector;
      Symbols : Langkit_Support.Symbols.Symbol_Table :=
        Langkit_Support.Symbols.Create_Symbol_Table;

      ------------------
      -- Lookup_Token --
      ------------------

      function Lookup_Token
        (Sloc : Langkit_Support.Slocs.Source_Location) return Token_Reference
      is
         Result : Token_Reference := Node.Unit.Lookup_Token (Sloc);
      begin
         if Is_Trivia (Result) then
            Result := Previous (Result, Exclude_Trivia => True);
         end if;

         return Result;
      end Lookup_Token;

      ------------------------
      -- Synchronize_Tokens --
      ------------------------

      procedure Synchronize_Tokens
        (In_Stop   : Token_Reference;
         Out_Stop  : out Langkit_Support.Token_Data_Handlers.Token_Index;
         In_Start  : Token_Reference;
         Out_Start : Langkit_Support.Token_Data_Handlers.Token_Index;
         Ok        : out Boolean)
      is
         procedure Find_Next_Token
           (Kind  : Token_Kind;
            Index : in out Langkit_Support.Token_Data_Handlers.Token_Index;
            Ok    : out Boolean);
         --  Find nearest token of a given Kind in the Output document starting
         --  from Index. Set Ok to False in no such token found and don't
         --  update Index in this case.

         ---------------------
         -- Find_Next_Token --
         ---------------------

         procedure Find_Next_Token
           (Kind  : Token_Kind;
            Index : in out Langkit_Support.Token_Data_Handlers.Token_Index;
            Ok    : out Boolean)
         is
            use type Langkit_Support.Token_Data_Handlers.Token_Index;
            Max_Look_Ahead : constant := 4;  --  How far search for the token

            Next_Kind : Token_Kind;
         begin
            Ok := False;

            for J in Index + 1 .. Index + Max_Look_Ahead loop
               Next_Kind := Libadalang.Common.To_Token_Kind
                 (Langkit_Support.Token_Data_Handlers.Get_Token
                    (TDH, J).Kind);

               if Next_Kind = Kind then
                  Ok := True;
                  Index := J;
                  exit;
               end if;
            end loop;
         end Find_Next_Token;

         Input : Token_Reference;
      begin
         Input := In_Start;
         Out_Stop := Out_Start;
         Ok := True;  --  Now Out_Stop is synchronized with Input

         while Input /= In_Stop loop
            Input := Next (Input, Exclude_Trivia => True);
            Find_Next_Token (Kind (Data (Input)), Out_Stop, Ok);
         end loop;
      end Synchronize_Tokens;

      ---------------------
      -- Tokenize_Output --
      ---------------------

      procedure Tokenize_Output is
         Input : constant Libadalang.Lexer.Lexer_Input :=
           (Kind     => Libadalang.Common.Bytes_Buffer,
            Charset  => Ada.Strings.Unbounded.To_Unbounded_String ("utf-8"),
            Read_BOM => False,
            Bytes    => To_Unbounded_String (Output));

      begin
         Langkit_Support.Token_Data_Handlers.Initialize
           (TDH, Symbols, System.Null_Address);

         Libadalang.Lexer.Extract_Tokens
           (Input,
            TDH         => TDH,
            Diagnostics => Diags,
            With_Trivia => True);
      end Tokenize_Output;

      use type Langkit_Support.Slocs.Line_Number;

      From : Token_Reference;
      --  Nearest to range start token (in Input document)
      To   : Token_Reference;
      --  Nearest to range end token (in Input document)
      From_Index : Langkit_Support.Token_Data_Handlers.Token_Index;
      --  Corresponding From-token in Output document
      To_Index : Langkit_Support.Token_Data_Handlers.Token_Index;
      --  Corresponding To-token in Output document
      Ignore : Utils.Char_Vectors.Char_Subrange;
      Ok : Boolean;

   begin
      Pp.Actions.Format_Vector
        (Cmd, Input, Node, Output, Messages);

      if In_Sloc = Langkit_Support.Slocs.No_Source_Location_Range then
         --  Return full range of Output
         Out_Sloc := In_Sloc;
         return;
      elsif Node.Unit.Token_Count = 0 then  --  Ignore a cornercase for now
         Out_Sloc := Langkit_Support.Slocs.No_Source_Location_Range;
         return;
      end if;

      Tokenize_Output;  --  Fill TDH
      From := Lookup_Token (Langkit_Support.Slocs.Start_Sloc (In_Sloc));
      To := Lookup_Token (Langkit_Support.Slocs.End_Sloc (In_Sloc));

      Synchronize_Tokens
        (In_Stop   => From,
         Out_Stop  => From_Index,
         In_Start  => Node.Unit.First_Token,
         Out_Start => Langkit_Support.Token_Data_Handlers.First_Token_Index,
         Ok        => Ok);

      if Ok then
         Synchronize_Tokens
           (In_Stop   => To,
            Out_Stop  => To_Index,
            In_Start  => From,
            Out_Start => From_Index,
            Ok        => Ok);
      end if;

      if Ok then
         Out_Sloc.Start_Line :=
           Langkit_Support.Token_Data_Handlers.Sloc_Start
             (TDH, Langkit_Support.Token_Data_Handlers.Get_Token
                (TDH, From_Index)).Line
           + In_Sloc.Start_Line
           - Sloc_Range (Data (From)).Start_Line;

         Out_Sloc.End_Line :=
           Langkit_Support.Token_Data_Handlers.Sloc_End
             (TDH, Langkit_Support.Token_Data_Handlers.Get_Token
                (TDH, To_Index)).Line
           + In_Sloc.End_Line
           - Sloc_Range (Data (To)).End_Line;

         Out_Sloc.Start_Column := 1;
         Out_Sloc.End_Column := 1;
      end if;

      Langkit_Support.Token_Data_Handlers.Free (TDH);
      Langkit_Support.Symbols.Destroy (Symbols);
   end Format_Vector;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Ignore : in out Restricted_Kind_Predicate;
      Node   : Libadalang.Analysis.Ada_Node) return Boolean
   is
      Decl : constant Libadalang.Analysis.Basic_Decl :=
        Node.As_Defining_Name.P_Basic_Decl;
      Next : Libadalang.Analysis.Ada_Node := Decl.Parent;

   begin
      if not Decl.Is_Null and then
        Decl.Kind in Libadalang.Common.Ada_For_Loop_Var_Decl
          | Libadalang.Common.Ada_Base_Formal_Param_Decl
          | Libadalang.Common.Ada_Extended_Return_Stmt_Object_Decl
          | Libadalang.Common.Ada_Anonymous_Expr_Decl
          | Libadalang.Common.Ada_Exception_Handler
          | Libadalang.Common.Ada_Label_Decl
          | Libadalang.Common.Ada_Named_Stmt_Decl
          | Libadalang.Common.Ada_Entry_Index_Spec
          | Libadalang.Common.Ada_Entry_Decl
      then
         return True;

      elsif not Decl.Is_Null and then
        Decl.Kind in Libadalang.Common.Ada_Object_Decl and then
        Decl.Parent.Kind = Libadalang.Common.Ada_Generic_Formal_Obj_Decl
      then
         --  This is a special case for the formal_object_declaration
         return True;
      end if;

      while not Next.Is_Null loop
         --  Any program unit body excluding library level package bodies
         if Next.Kind in Libadalang.Common.Ada_Body_Node and then
           (Next.Kind not in Libadalang.Common.Ada_Package_Body or else
              Next.Parent.Kind not in Libadalang.Common.Ada_Library_Item)
         then
            return True;
         end if;

         Next := Next.Parent;
      end loop;

      return False;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Ignore : in out Is_Global_Visible_Predicate;
      Node   : Libadalang.Analysis.Ada_Node) return Boolean
   is
      Decl : constant Libadalang.Analysis.Basic_Decl :=
        Node.As_Defining_Name.P_Basic_Decl;
      Next : Libadalang.Analysis.Ada_Node := Decl.Parent;
   begin
      while not Next.Is_Null loop
         if Next.Kind in
           Libadalang.Common.Ada_Body_Node
           | Libadalang.Common.Ada_Private_Part
           | Libadalang.Common.Ada_Protected_Type_Decl
           | Libadalang.Common.Ada_Single_Protected_Decl
         then
            return False;
         end if;

         Next := Next.Parent;
      end loop;

      return True;
   end Evaluate;

   ------------------------
   -- Is_Restricted_Kind --
   ------------------------

   function Is_Restricted_Kind
     return Libadalang.Iterators.Ada_Node_Predicate is
   begin
      return Result : Libadalang.Iterators.Ada_Node_Predicate do
         Result.Set (Restricted_Kind_Predicate'(null record));
      end return;
   end Is_Restricted_Kind;

   -----------------------
   -- Is_Global_Visible --
   -----------------------

   function Is_Global_Visible return Libadalang.Iterators.Ada_Node_Predicate is
   begin
      return Result : Libadalang.Iterators.Ada_Node_Predicate do
         Result.Set (Is_Global_Visible_Predicate'(null record));
      end return;
   end Is_Global_Visible;

   ------------------
   -- Is_End_Token --
   ------------------

   function Is_End_Token (Token : Libadalang.Common.Token_Reference)
                             return Boolean
   is
      End_Token : constant Libadalang.Common.Token_Data_Type :=
        Libadalang.Common.Data (Token);

      Token_Kind : constant Libadalang.Common.Token_Kind :=
        Libadalang.Common.Kind (End_Token);
   begin
      return Token_Kind = Libadalang.Common.Ada_End;
   end Is_End_Token;

   ------------------
   -- Is_Synthetic --
   ------------------

   function Is_Synthetic
     (Node : Libadalang.Analysis.Ada_Node'Class) return Boolean
   is
      Std  : constant String := "__standard";
      File : constant String := Node.Unit.Get_Filename;
   begin
      return File'Length >= Std'Length
        and then File (File'Last - Std'Length + 1 .. File'Last) = Std;
   end Is_Synthetic;

   -----------------------
   -- Skip_Dotted_Names --
   -----------------------

   function Skip_Dotted_Names (Node : Libadalang.Analysis.Ada_Node)
                                  return Libadalang.Analysis.Ada_Node
   is
      Parent : Libadalang.Analysis.Ada_Node := Node;
   begin
      while not Parent.Is_Null
        and then Parent.Kind = Libadalang.Common.Ada_Dotted_Name
      loop
         Parent := Parent.Parent;
      end loop;

      return Parent;
   end Skip_Dotted_Names;

   -------------------
   -- Span_To_Slice --
   -------------------

   procedure Span_To_Slice
     (Text  : VSS.Strings.Virtual_String;
      Span  : LSP.Messages.Span;
      Slice : out VSS.Strings.Virtual_String)
   is
      use type VSS.Unicode.UTF16_Code_Unit_Offset;
      Dummy : Boolean;
      Lines : VSS.String_Vectors.Virtual_String_Vector;
      Line  : VSS.Strings.Virtual_String;
      Num   : Natural := Natural (Span.first.line) + 1;
   begin
      Lines :=
        Text.Split_Lines
          (Terminators     => LSP.Common.LSP_New_Line_Function_Set,
           Keep_Terminator => True);
      Line := Lines.Element (Num);

      declare
         J1 : VSS.Strings.Character_Iterators.Character_Iterator :=
           Line.At_First_Character;
         U1 : constant VSS.Unicode.UTF16_Code_Unit_Offset :=
           J1.First_UTF16_Offset;
      begin
         while Span.first.character /= J1.First_UTF16_Offset - U1
           and then J1.Forward
         loop
            null;
         end loop;

         if Span.first.line /= Span.last.line then
            Slice.Append
              (Line.Slice (J1.Marker, Line.At_Last_Character.Marker));
         end if;

         loop
            Num := Num + 1;
            exit when Num > Natural (Span.last.line);
            Slice.Append (Lines.Element (Num));
         end loop;

         Line := Lines.Element (Natural (Span.last.line) + 1);
         declare
            J2 : VSS.Strings.Character_Iterators.Character_Iterator :=
              Line.At_First_Character;
            U2 : constant VSS.Unicode.UTF16_Code_Unit_Offset :=
              J2.First_UTF16_Offset;
         begin
            while Span.last.character /= J2.First_UTF16_Offset - U2
              and then J2.Forward
            loop
               null;
            end loop;
            Dummy := J2.Backward;

            if Span.first.line /= Span.last.line then
               Slice.Append
                 (Line.Slice (Line.At_First_Character.Marker, J2.Marker));
            else
               Slice.Append (Line.Slice (J1.Marker, J2.Marker));
            end if;
         end;
      end;
   end Span_To_Slice;

end LSP.Lal_Utils;

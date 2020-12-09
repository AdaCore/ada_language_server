------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with GNATCOLL.VFS;
with GNATCOLL.Utils;

with Langkit_Support;
with Libadalang.Common; use Libadalang.Common;
with Libadalang.Sources;

with VSS.Unicode;

with LSP.Types;         use LSP.Types;

package body LSP.Lal_Utils is

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
      function Is_Synthetic return Boolean;
      --  Check if Node is in a synthetic file (like "__standard").
      --  TODO: Replace this with LAL property as it will be available.

      ------------------
      -- Is_Synthetic --
      ------------------

      function Is_Synthetic return Boolean is
         Std  : constant String := "__standard";
         File : constant String := Node.Unit.Get_Filename;
      begin
         return File'Length >= Std'Length
           and then File (File'Last - Std'Length + 1 .. File'Last) = Std;
      end Is_Synthetic;

      Location : constant LSP.Messages.Location :=
        LSP.Lal_Utils.Get_Node_Location
          (Libadalang.Analysis.As_Ada_Node (Node), Kind);
   begin
      if not Is_Synthetic then
         Result.Append (Location);
      end if;
   end Append_Location;

   ---------------------
   -- Append_Location --
   ---------------------

   procedure Append_Location
     (Result : in out LSP.Messages.DocumentHighlight_Vector;
      Node   : Libadalang.Analysis.Ada_Node'Class;
      Kind   : LSP.Messages.Optional_DocumentHighlightKind)
   is
      use LSP.Messages;

      Location : constant LSP.Messages.Location :=
        LSP.Lal_Utils.Get_Node_Location
          (Libadalang.Analysis.As_Ada_Node (Node));
   begin
      Result.Append
        (DocumentHighlight'
           (span => Location.span,
            kind => Kind));
   end Append_Location;

   --------------------------------
   -- Sort_And_Remove_Duplicates --
   --------------------------------

   procedure Sort_And_Remove_Duplicates
     (Result : in out LSP.Messages.Location_Vector)
   is
      use type VSS.Unicode.UTF16_Code_Unit_Count;

      function URI_Inf (Left, Right : LSP.Types.LSP_String) return Boolean;
      --  Comparison function for URIs, return True if Left < Right

      function "<" (Left, Right : LSP.Messages.Location) return Boolean is
        (URI_Inf (Left.uri, Right.uri) or else
           (Left.uri = Right.uri
            and then (Left.span.first.line < Right.span.first.line
                      or else (Left.span.first.line = Right.span.first.line
                               and then Left.span.first.character <
                                 Right.span.first.character))));

      -------------
      -- URI_Inf --
      -------------

      function URI_Inf (Left, Right : LSP.Types.LSP_String) return Boolean is

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
         when Ada_Generic_Formal_Subp_Decl |
              Ada_Abstract_Subp_Decl |
              Ada_Abstract_Formal_Subp_Decl |
              Ada_Concrete_Formal_Subp_Decl |
              Ada_Null_Subp_Decl |
              Ada_Subp_Decl |
              Ada_Subp_Renaming_Decl |
              Ada_Expr_Function |
              Ada_Subp_Body |
              Ada_Subp_Body_Stub |
              Ada_Entry_Body |
              Ada_Entry_Decl |
              Ada_Generic_Subp_Decl |
              Ada_Generic_Subp_Instantiation |
              Ada_Generic_Subp_Renaming_Decl =>
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

         when Ada_Generic_Formal_Package |
              Ada_Package_Decl |
              Ada_Generic_Package_Decl |
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

         when Ada_Type_Decl =>
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

   -----------------------
   -- Get_Node_Location --
   -----------------------

   function Get_Node_Location
     (Node : Libadalang.Analysis.Ada_Node'Class;
      Kind : LSP.Messages.AlsReferenceKind_Set := LSP.Messages.Empty_Set)
      return LSP.Messages.Location
   is
      use type VSS.Unicode.UTF16_Code_Unit_Count;

      Start_Sloc_Range                                     :
      constant Langkit_Support.Slocs.Source_Location_Range :=
         Sloc_Range (Data (Node.Token_Start));
      End_Sloc_Range                                       :
      constant Langkit_Support.Slocs.Source_Location_Range :=
         Sloc_Range (Data (Node.Token_End));

      First_Position : constant LSP.Messages.Position :=
                         (Line_Number (Start_Sloc_Range.Start_Line) - 1,
                          UTF_16_Index (Start_Sloc_Range.Start_Column) - 1);
      Last_Position  : constant LSP.Messages.Position :=
                         (Line_Number (End_Sloc_Range.End_Line) - 1,
                          UTF_16_Index (End_Sloc_Range.End_Column) - 1);
      --  XXX Code unit offset computation is incorrect here

      File : constant LSP.Types.LSP_String :=
        LSP.Types.To_LSP_String (Node.Unit.Get_Filename);

      Location : constant LSP.Messages.Location :=
        (uri     => LSP.Ada_Contexts.File_To_URI (File),
         span    => LSP.Messages.Span'(First_Position, Last_Position),
         alsKind => Kind);

   begin
      return Location;
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

      Result : constant LSP.Messages.Span :=
        (first =>
           (line      => LSP.Types.Line_Number (Value.Start_Line) - 1,
            character => LSP.Types.UTF_16_Index   --  FIXME (UTF16 index)!
              (Value.Start_Column) - 1),
         last =>
           (line => LSP.Types.Line_Number (Value.End_Line) - 1,
            character => LSP.Types.UTF_16_Index  --  FIXME (UTF16 index)!
              (Value.End_Column) - 1));
      --  XXX Code unit offset computation is incorrect here

   begin
      return Result;
   end To_Span;

   ------------------
   -- Canonicalize --
   ------------------

   function Canonicalize
     (Text : LSP.Types.LSP_String) return VSS.Strings.Virtual_String
   is
      UTF_32 : constant Wide_Wide_String :=
        Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode
          (LSP.Types.To_UTF_8_String (Text));
      Result : constant Symbolization_Result :=
        Libadalang.Sources.Canonicalize (UTF_32);
   begin
      if Result.Success then
         return VSS.Strings.To_Virtual_String (Result.Symbol);
      else
         return VSS.Strings.Empty_Virtual_String;
      end if;
   end Canonicalize;

   -----------------------
   -- Containing_Entity --
   -----------------------

   function Containing_Entity (Ref : Ada_Node) return Defining_Name is
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
            return Parent.As_Basic_Decl.P_Canonical_Part.P_Defining_Name;
         end if;
      end loop;

      return No_Defining_Name;
   end Containing_Entity;

   --------------------
   -- Find_All_Calls --
   --------------------

   function Find_All_Calls
     (Context           : LSP.Ada_Contexts.Context;
      Definition        : Defining_Name;
      Imprecise_Results : out Boolean)
      return Laltools.Common.References_By_Subprogram.Map
   is
      use Laltools.Common.References_By_Subprogram;
      use Laltools.Common.References_List;

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
         Containing := Containing_Entity (Ref.As_Ada_Node);

         if Containing /= No_Defining_Name then
            if Result.Contains (Containing) then
               Result (Containing).Append (Ref);
            else
               declare
                  L : List;
               begin
                  L.Append (Ref);
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
   end Find_All_Calls;

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
      return Result : LSP.Messages.CallHierarchyItem do
         Result.name := To_LSP_String (Name.Text);
         Result.kind := LSP.Lal_Utils.Get_Decl_Kind (Main_Item);
         Result.detail := (True, LSP.Lal_Utils.Node_Location_Image (Name));
         Result.uri := Where.uri;
         Result.span := Where.span;
         Result.selectionRange :=
           LSP.Lal_Utils.To_Span (Name.Sloc_Range);
      end return;
   end To_Call_Hierarchy_Item;

   ----------------------------
   -- To_Unbounded_Text_Type --
   ----------------------------

   function To_Unbounded_Text_Type
     (Item : LSP_String)
      return Langkit_Support.Text.Unbounded_Text_Type
   is
      use Langkit_Support.Text;
   begin
      return To_Unbounded_Text (From_UTF8 (To_UTF_8_String (Item)));
   end To_Unbounded_Text_Type;

   -------------------------
   -- Node_Location_Image --
   -------------------------

   function Node_Location_Image
     (Node : Libadalang.Analysis.Ada_Node'Class) return LSP.Types.LSP_String
   is
      use type GNATCOLL.VFS.Filesystem_String;
      Decl_Unit_File : constant GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.Create (+Node.Unit.Get_Filename);

      Location_Text : constant LSP.Types.LSP_String  := To_LSP_String
        ("at " & Decl_Unit_File.Display_Base_Name & " ("
         & GNATCOLL.Utils.Image
           (Integer (Node.Sloc_Range.Start_Line), Min_Width => 1)
         & ":"
         & GNATCOLL.Utils.Image
           (Integer (Node.Sloc_Range.Start_Column), Min_Width => 1)
         & ")");
   begin
      return Location_Text;
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

end LSP.Lal_Utils;

------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
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

with Langkit_Support.Text;

with Libadalang.Common;
with Libadalang.Iterators;

with VSS.Characters;

with LSP.Ada_Handlers.Locations;
with LSP.Constants;
with LSP.Enumerations;
with LSP.Utils;

package body LSP.Ada_Handlers.Symbols is

   function Get_Profile
     (Node        : Libadalang.Analysis.Basic_Decl;
      Is_Function : out Boolean)
      return VSS.Strings.Virtual_String;
   --  Return the profile of Node.

   ---------------------------
   -- Flat_Document_Symbols --
   ---------------------------

   procedure Flat_Document_Symbols
     (Self    : in out Message_Handler'Class;
      Unit    : Libadalang.Analysis.Analysis_Unit;
      Pattern : LSP.Search.Search_Pattern'Class;
      Result  : in out LSP.Structures.DocumentSymbol_Result)
   is
      Element : Libadalang.Analysis.Ada_Node;

      Is_Defining_Name : constant Libadalang.Iterators.Ada_Node_Predicate :=
        Libadalang.Iterators.Kind_Is (Libadalang.Common.Ada_Defining_Name);
      --  This object will be deallocated by Cursor's finalization

      Cursor : Libadalang.Iterators.Traverse_Iterator'Class :=
        Libadalang.Iterators.Find (Unit.Root, Is_Defining_Name);

   begin
      while not Self.Is_Canceled.all
        and then Cursor.Next (Element)
      loop
         declare
            use type LSP.Enumerations.SymbolKind;

            Item : LSP.Structures.SymbolInformation;
            Kind : constant LSP.Enumerations.SymbolKind :=
              LSP.Utils.Get_Decl_Kind
                (Element.As_Defining_Name.P_Basic_Decl, Ignore_Local => True);
         begin
            if Kind /= LSP.Enumerations.A_Null
              and then Pattern.Match
                (VSS.Strings.To_Virtual_String (Element.Text))
            then
               Item :=
                 (name              =>
                    VSS.Strings.To_Virtual_String (Element.Text),
                  kind              => Kind,
                  tags              => LSP.Constants.Empty,
                  deprecated        => <>,
                  location          =>
                    LSP.Ada_Handlers.Locations.To_LSP_Location (Self, Element),
                  containerName     => <>);

               Result.Variant_1.Append (Item);
            end if;
         end;
      end loop;
   end Flat_Document_Symbols;

   -----------------
   -- Get_Profile --
   -----------------

   function Get_Profile
     (Node        : Libadalang.Analysis.Basic_Decl;
      Is_Function : out Boolean) return VSS.Strings.Virtual_String
   is
      use Libadalang.Analysis;
      use Libadalang.Common;

      function To_Text
        (Node : Ada_Node'Class) return VSS.Strings.Virtual_String;
      --  Retrieve the node text and format it

      function To_Profile
        (Node : Libadalang.Analysis.Subp_Spec'Class)
         return VSS.Strings.Virtual_String;

      -------------
      -- To_Text --
      -------------

      function To_Text
        (Node : Ada_Node'Class) return VSS.Strings.Virtual_String
      is
         Node_Text : constant Langkit_Support.Text.Text_Type := Node.Text;
         Was_Space : Boolean := False;
         Result    : VSS.Strings.Virtual_String;
      begin
         for I in Node_Text'Range loop
            if Node_Text (I) = ' ' then
               --  Trim multiple whitespace to only keep one

               if not Was_Space then
                  Result.Append
                    (VSS.Characters.Virtual_Character (Node_Text (I)));
               end if;

               Was_Space := True;

               --  Remove the new line character

            elsif Node_Text (I) /= Ada.Characters.Wide_Wide_Latin_1.LF then
               Was_Space := False;
                  Result.Append
                    (VSS.Characters.Virtual_Character (Node_Text (I)));
            end if;
         end loop;

         return Result;
      end To_Text;

      ----------------
      -- To_Profile --
      ----------------

      function To_Profile
        (Node : Libadalang.Analysis.Subp_Spec'Class)
         return VSS.Strings.Virtual_String
      is
         Result  : VSS.Strings.Virtual_String;
         Params  : constant Param_Spec_Array := Node.P_Params;
         Returns : constant Type_Expr := Node.F_Subp_Returns;

      begin
         if Params'Length > 0 then
            Result.Append ('(');
         end if;

         for Param of Params loop
            declare
               use type VSS.Strings.Character_Count;

               Names : constant Defining_Name_List := Param.F_Ids;
               Init  : constant Expr := Param.F_Default_Expr;
               Item  : VSS.Strings.Virtual_String;
               Mode  : constant Ada_Mode := Param.F_Mode;

            begin
               Item.Append (" :");

               case Mode is
                  when Ada_Mode_Default | Ada_Mode_In =>
                     Item.Append (" in ");
                  when Ada_Mode_In_Out =>
                     Item.Append (" in out ");
                  when Ada_Mode_Out =>
                     Item.Append (" out ");
               end case;

               Item.Append (To_Text (Param.F_Type_Expr));

               if not Init.Is_Null then
                  Item.Append (" := ");
                  Item.Append (To_Text (Init));
               end if;

               for J in Names.First_Child_Index .. Names.Last_Child_Index loop
                  if Result.Character_Length /= 1 then
                     Result.Append ("; ");
                  end if;

                  Result.Append (To_Text (Names.Child (J)));
                  Result.Append (Item);
               end loop;
            end;
         end loop;

         if Params'Length > 0 then
            Result.Append (')');
         end if;

         if not Returns.Is_Null then
            Is_Function := True;
            Result.Append (" return ");
            Result.Append (To_Text (Returns));
         end if;

         return Result;
      end To_Profile;

   begin
      Is_Function := False;

      case Node.Kind is
         when Ada_Classic_Subp_Decl =>
            return To_Profile (Node.As_Classic_Subp_Decl.F_Subp_Spec);
         when Ada_Base_Subp_Body    =>
            return To_Profile (Node.As_Base_Subp_Body.F_Subp_Spec);
         when Ada_Generic_Subp_Decl =>
            return To_Profile
              (Node.As_Generic_Subp_Decl.F_Subp_Decl.F_Subp_Spec);
         when others =>
            return VSS.Strings.Empty_Virtual_String;
      end case;
   end Get_Profile;

   -----------------------------------
   -- Hierarchical_Document_Symbols --
   -----------------------------------

   procedure Hierarchical_Document_Symbols
     (Self    : in out Message_Handler'Class;
      Unit    : Libadalang.Analysis.Analysis_Unit;
      Pattern : LSP.Search.Search_Pattern'Class;
      Result  : in out LSP.Structures.DocumentSymbol_Vector)
   is
      use all type LSP.Enumerations.SymbolKind;

      Empty : LSP.Structures.DocumentSymbol_Vector;

      procedure Walk
        (Node         : Libadalang.Analysis.Ada_Node;
         Nested_Level : Integer;
         Vector       : in out LSP.Structures.DocumentSymbol_Vector);
      --  Traverse Node and all its children recursively. Find any defining
      --  name and construct corresponding symbol node, then append it to
      --  the Tree under a position pointed by the Cursor.

      ----------
      -- Walk --
      ----------

      procedure Walk
        (Node         : Libadalang.Analysis.Ada_Node;
         Nested_Level : Integer;
         Vector       : in out LSP.Structures.DocumentSymbol_Vector)
      is
         use Libadalang.Analysis;

         Children : LSP.Structures.DocumentSymbol_Vector;
         Next_Level : Integer := Nested_Level;
      begin
         if Node.Is_Null
           or else Node.Kind in Libadalang.Common.Ada_Expr
           or else Self.Is_Canceled.all
         then
            return;

         end if;

         Next_Level := Next_Level +
           (if Node.Kind in Libadalang.Common.Ada_Basic_Decl then 1 else 0);

         for Child of Node.Children loop
            if not Child.Is_Null then
               Walk (Child, Next_Level, Children);
               exit when Self.Is_Canceled.all;
            end if;
         end loop;

         case Node.Kind is
         when Libadalang.Common.Ada_Basic_Decl =>
            declare
               Decl : constant Libadalang.Analysis.Basic_Decl :=
                 Node.As_Basic_Decl;

               Kind : constant LSP.Enumerations.SymbolKind :=
                 LSP.Utils.Get_Decl_Kind
                   (Decl, Ignore_Local => Nested_Level > 1);

            begin
               if Kind /= LSP.Enumerations.A_Null then
                  declare
                     Names : constant Libadalang.Analysis.Defining_Name_Array
                       := Decl.P_Defining_Names;
                  begin

                     for Name of Names loop
                        exit when Name = Libadalang.Analysis.No_Defining_Name;

                        if Pattern.Match
                          (VSS.Strings.To_Virtual_String (Name.Text))
                        then
                           declare
                              Is_Function : Boolean;
                              Profile : constant VSS.Strings.Virtual_String :=
                                Get_Profile (Decl, Is_Function);
                              Item : constant LSP.Structures.DocumentSymbol :=
                                (name           =>
                                   VSS.Strings.To_Virtual_String (Name.Text),
                                 detail         => Profile,
                                 kind           => Kind,
                                 deprecated     => (Is_Set => False),
                                 tags           => LSP.Constants.Empty,
                                 a_range        => Locations.To_LSP_Location
                                   (Self, Node).a_range,
                                 selectionRange => Locations.To_LSP_Location
                                   (Self, Name).a_range,
                                 children       => Children);
                           begin
                              Vector.Append (Item);
                           end;
                        end if;
                     end loop;
                  end;
               end if;
            end;

         when Libadalang.Common.Ada_With_Clause_Range =>
            declare
               With_Node : constant Libadalang.Analysis.With_Clause :=
                 Node.As_With_Clause;
            begin
               for Name of With_Node.F_Packages loop
                  declare
                     Item : constant LSP.Structures.DocumentSymbol :=
                       (name           =>
                          VSS.Strings.To_Virtual_String (Name.Text),
                        detail         => VSS.Strings.Empty_Virtual_String,
                        kind           => Namespace,
                        deprecated     => (Is_Set => False),
                        tags           => LSP.Constants.Empty,
                        a_range        => Locations.To_LSP_Location
                          (Self, Node).a_range,
                        selectionRange => Locations.To_LSP_Location
                          (Self, Name).a_range,
                        children       => Empty);
                  begin
                     Vector.Append (Item);
                  end;
               end loop;
            end;

         when Libadalang.Common.Ada_Pragma_Node =>
            declare
               Pragma_Node : constant Libadalang.Analysis.Pragma_Node :=
                 Node.As_Pragma_Node;
               Name        : constant Libadalang.Analysis.Identifier  :=
                 Pragma_Node.F_Id;
               Item        : constant LSP.Structures.DocumentSymbol :=
                 (name           =>
                    VSS.Strings.To_Virtual_String (Name.Text),
                  detail         =>
                     VSS.Strings.To_Virtual_String
                      ("(" & (Pragma_Node.F_Args.Text & ")")),
                  kind           => Property,
                  deprecated     => (Is_Set => False),
                  tags           => LSP.Constants.Empty,
                  a_range        => Locations.To_LSP_Location
                    (Self, Node).a_range,
                  selectionRange => Locations.To_LSP_Location
                    (Self, Name).a_range,
                  children       => Empty);
            begin
               if Nested_Level <= 1 then
                  Vector.Append (Item);
               end if;
            end;

         when others =>
            for J in 1 .. Children.Length loop
               Vector.Append (Children (J));
            end loop;
         end case;
      end Walk;

      Root : constant Libadalang.Analysis.Ada_Node := Unit.Root;
   begin
      Walk (Root, 0, Result);
   end Hierarchical_Document_Symbols;

   -------------------
   -- Write_Symbols --
   -------------------

   procedure Write_Symbols
     (Self   : in out Message_Handler'Class;
      Names  : LSP.Ada_Completions.Completion_Maps.Map;
      Result : in out LSP.Structures.SymbolInformation_Vector) is
   begin
      for Cursor in Names.Iterate loop
         declare
            Name : constant Libadalang.Analysis.Defining_Name :=
              LSP.Ada_Completions.Completion_Maps.Key (Cursor);
            Node : Libadalang.Analysis.Ada_Node := Name.As_Ada_Node;
         begin
            while not Node.Is_Null and then
              Node.Kind not in Libadalang.Common.Ada_Basic_Decl
            loop
               Node := Node.Parent;
            end loop;

            if not Node.Is_Null then
               Result.Append
                 (LSP.Structures.SymbolInformation'
                    (name     => VSS.Strings.To_Virtual_String (Name.Text),
                     kind     => LSP.Utils.Get_Decl_Kind
                                  (Node.As_Basic_Decl),
                     location => Locations.To_LSP_Location
                                  (Self, Name),
                     tags          => LSP.Constants.Empty,
                     deprecated    => (Is_Set => False),
                     containerName => <>));
            end if;
         end;
      end loop;
   end Write_Symbols;

end LSP.Ada_Handlers.Symbols;

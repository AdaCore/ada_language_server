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

with VSS.String_Vectors;
with VSS.Strings;

with LSP_Gen.Entities;
with LSP_Gen.Mappings;
with LSP_Gen.Puts; use LSP_Gen.Puts;

package body LSP_Gen.Outputs is

   use type VSS.Strings.Virtual_String;
   use all type LSP_Gen.Entities.Enum.AType_Variant;
   use all type LSP_Gen.Entities.Enum.BaseTypes;

   function "+" (Text : Wide_Wide_String) return VSS.Strings.Virtual_String
     renames VSS.Strings.To_Virtual_String;

   procedure Write_Subprogram_Definition
     (Name   : VSS.Strings.Virtual_String;
      Prefix : VSS.Strings.Virtual_String);

   procedure Write_Enum
     (Info : LSP_Gen.Entities.Enumeration;
      Spec : Boolean);

   procedure Write_Nested_Types
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String);

   procedure Write_Type
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String;
      Tipe  : LSP_Gen.Entities.AType;
      Spec  : Boolean);

   procedure Write_Structure
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String);

   procedure Write_Call
     (Done   : LSP_Gen.Dependencies.Dependency_Map;
      Tipe   : LSP_Gen.Entities.AType;
      Suffix : VSS.Strings.Virtual_String);

   procedure Write_Subprogram_Name
     (Done : LSP_Gen.Dependencies.Dependency_Map;
      Tipe : LSP_Gen.Entities.AType);

   Empty_Vector : LSP_Gen.Entities.Property_Vector;

   procedure Write_Properties
     (Model       : LSP_Gen.Meta_Models.Meta_Model;
      Done        : LSP_Gen.Dependencies.Dependency_Map;
      List        : LSP_Gen.Entities.Property_Vector;
      Is_Optional : Boolean := False;
      Suffix      : VSS.Strings.Virtual_String := "";
      Exclude     : LSP_Gen.Entities.Property_Vector := Empty_Vector);

   procedure Write_Property
     (Model       : LSP_Gen.Meta_Models.Meta_Model;
      Done        : LSP_Gen.Dependencies.Dependency_Map;
      Item        : LSP_Gen.Entities.Property;
      Is_Optional : Boolean := False;
      Suffix      : VSS.Strings.Virtual_String := "");

   procedure Write_Union
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      List  : LSP_Gen.Entities.AType_Vector);

   function Is_String (Tipe : LSP_Gen.Entities.AType) return Boolean is
     (Tipe.Union.Kind = base
        and then Tipe.Union.base.name = LSP_Gen.Entities.Enum.string);

   function Is_Vector (Tipe : LSP_Gen.Entities.AType) return Boolean is
     (Tipe.Union.Kind = an_array);

   function Is_LSP_Any (Tipe : LSP_Gen.Entities.AType) return Boolean is
     (Tipe.Union.Kind = reference and then
        Tipe.Union.reference.name = "LSPAny");

   function Is_Custom_Vector (Tipe : LSP_Gen.Entities.AType) return Boolean is
     (Tipe.Union.Kind = an_array and then
      Tipe.Union.an_array.element.Value.Union.Kind = reference and then
      Tipe.Union.an_array.element.Value.Union.reference.name in
        +"DocumentSymbol" | +"SelectionRange");

   function Is_Set
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Tipe  : LSP_Gen.Entities.AType) return Boolean is
     (Tipe.Union.Kind = an_array
      and then Tipe.Union.an_array.element.Value.Union.Kind = reference
      and then Model.Is_Enumeration
        (Tipe.Union.an_array.element.Value.Union.reference.name)
      and then not Model.Is_Custom_Enumeration
        (Tipe.Union.an_array.element.Value.Union.reference.name)
      and then not Tipe.Union.an_array.element.Value.Union.reference.name
       .Starts_With ("MarkupKind"));

   function Contains
     (List : LSP_Gen.Entities.Property_Vector;
      Name : VSS.Strings.Virtual_String) return Boolean is
        (for some J in 1 .. List.Length => List (J).name = Name);

   -----------
   -- Write --
   -----------

   procedure Write
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map) is
   begin
      Put_Lines (Model.License_Header, "--  ");
      New_Line;
      Put_Line ("with VSS.JSON.Content_Handlers;");
      New_Line;
      Put_Line ("with LSP.Enumerations;");
      Put_Line ("with LSP.Structures;");
      New_Line;
      Put_Line ("package LSP.Outputs is");
      Put_Line ("   pragma Preelaborate;"); New_Line;

      for Item of Done loop
         if Item.Is_Message
           or else Item.Short_Name = "ConfigurationParams"
           or else Item.Short_Name = "Integer_Or_Virtual_String"
         then
            Write_Subprogram_Definition
              (Item.Short_Name,
               (if Model.Is_Enumeration (Item.Short_Name)
                then VSS.Strings.To_Virtual_String ("LSP.Enumerations.")
                else VSS.Strings.To_Virtual_String ("LSP.Structures.")));

            Put_Line (";"); New_Line;
         end if;
      end loop;

      Put_Line ("end LSP.Outputs;");

      Put_Lines (Model.License_Header, "--  ");
      New_Line;
      Put_Line ("with Ada.Containers;");
      Put_Line ("with Interfaces;");
      Put_Line ("with VSS.Strings;");
      Put_Line ("with LSP.Output_Tools;");
      New_Line;

      Put_Line ("package body LSP.Outputs is"); New_Line;
      Put_Line ("pragma Warnings (Off, ""is not referenced"");");
      Put_Line ("use type Ada.Containers.Count_Type;"); New_Line;

      for Cursor in Done.Iterate loop
         declare
            Tipe : constant LSP_Gen.Entities.AType :=
               LSP_Gen.Dependencies.Dependency_Maps.Key (Cursor);
            Item : constant LSP_Gen.Dependencies.Dependency_Info :=
              Done (Cursor);
         begin
            if not Item.Is_Message
              and then Item.Short_Name /= "ConfigurationParams"
              and then Item.Short_Name /= "Integer_Or_Virtual_String"
              and then Item.Owner.Is_Empty
              and then (Tipe.Union.Kind /= reference or else
                         not Model.Is_Mixin (Tipe.Union.reference.name))
            then
               Write_Type (Model, Done, Item.Short_Name, Tipe, Spec => True);
            end if;
         end;
      end loop;

      for Cursor in Done.Iterate loop
         declare
            Tipe : constant LSP_Gen.Entities.AType :=
               LSP_Gen.Dependencies.Dependency_Maps.Key (Cursor);
            Item : constant LSP_Gen.Dependencies.Dependency_Info :=
              Done (Cursor);
         begin
            if Tipe.Union.Kind = reference then
               declare
                  Value : constant LSP_Gen.Meta_Models.Top_Type :=
                    Model.Get (Tipe.Union.reference.name);
               begin
                  case Value.Kind is
                     when LSP_Gen.Meta_Models.Structure =>
                        if not Model.Is_Mixin (Tipe.Union.reference.name) then
                           Write_Structure
                             (Model, Done, Tipe.Union.reference.name);
                        end if;
                     when LSP_Gen.Meta_Models.Type_Alias =>
                        Write_Type
                          (Model, Done, Tipe.Union.reference.name,
                           Value.Type_Alias.a_type, Spec => False);
                     when LSP_Gen.Meta_Models.Enumeration =>
                        Write_Enum (Value.Enumeration, Spec => False);
                  end case;
               end;
            elsif Item.Owner.Is_Empty then
               Write_Type (Model, Done, Item.Short_Name, Tipe, Spec => False);
            end if;
         end;
      end loop;

      Put_Line ("end LSP.Outputs;");
   end Write;

   ----------------
   -- Write_Call --
   ----------------

   procedure Write_Call
     (Done   : LSP_Gen.Dependencies.Dependency_Map;
      Tipe   : LSP_Gen.Entities.AType;
      Suffix : VSS.Strings.Virtual_String)
   is
      Value : constant VSS.Strings.Virtual_String := "Value" & Suffix;
   begin
      case Tipe.Union.Kind is
         when base =>
            case Tipe.Union.base.name is
               when LSP_Gen.Entities.Enum.integer | uinteger =>
                  Put ("Handler.Integer_Value (Integer'Pos (");
                  Put (Value);
                  Put_Line ("));");
               when Uri | DocumentUri | RegExp
                  | LSP_Gen.Entities.Enum.string
                  =>
                  Put ("Handler.String_Value (");
                  Put (Value);
                  Put_Line (");");
               when a_boolean =>
                  Put ("Handler.Boolean_Value (");
                  Put (Value);
                  Put_Line (");");
               when decimal =>
                  Put ("Handler.Float_Value (Interfaces.IEEE_Float_64 (");
                  Put (Value);
                  Put_Line ("));");
               when a_null =>
                  Put_Line ("Handler.Null_Value;");
            end case;
         when reference | literal | a_or | an_array | tuple =>
            Write_Subprogram_Name (Done, Tipe);
            Put (" (Handler, ");
            Put (Value);
            Put_Line (");");
         when stringLiteral =>
            Put ("Handler.String_Value (""");
            Put (Tipe.Union.stringLiteral.value);
            Put_Line (""");");
         when map =>
            Put_Line ("declare");
            Put ("use LSP.Structures.");
            Put (Done (Tipe.Union.map.value.Value).Short_Name);
            Put_Line ("_Maps;");
            Put_Line ("begin");
            Put_Line ("Handler.Start_Object;");
            Put ("for Cursor in ");
            Put (Value);
            Put_Line (".Iterate loop");
            Put_Line ("Handler.Key_Name (Key (Cursor));");
            Write_Call
              (Done, Tipe.Union.map.value.Value, Suffix & " (Cursor)");
            Put_Line ("end loop;");
            Put_Line ("Handler.End_Object;");
            Put_Line ("end;");
         when others =>
            raise Program_Error;
      end case;
   end Write_Call;

   ----------------
   -- Write_Enum --
   ----------------

   procedure Write_Enum
     (Info : LSP_Gen.Entities.Enumeration;
      Spec : Boolean)
   is
      Name : constant VSS.Strings.Virtual_String := Info.name;
   begin
      Write_Subprogram_Definition (Name, "LSP.Enumerations.");

      if Spec then
         Put_Line (";");

      elsif Info.supportsCustomValues then
         Put_Line (" is");
         Put_Line ("begin");

         case Info.a_type.name is
            when LSP_Gen.Entities.Enum.string =>
               Put ("Handler.String_Value");
               Put_Line (" (VSS.Strings.Virtual_String (Value));");

            when others =>
               Put ("Handler.Integer_Value (LSP.Enumerations.");
               Put_Id (Name);
               Put_Line ("'Pos (Value));");
         end case;

         Put_Line ("end;");
      else
         Put_Line (" is");
         Put_Line ("begin");
         Put_Line ("case Value is");

         for J in 1 .. Info.values.Length loop
            Put ("when LSP.Enumerations.");
            Put_Id (Info.values (J).name);
            Put_Line (" =>");

            case Info.a_type.name is
               when LSP_Gen.Entities.Enum.string =>
                  Put ("Handler.String_Value (""");
                  Put (Info.values (J).value.String);
                  Put_Line (""");");

               when others =>
                  Put ("Handler.Integer_Value (");
                  Put (Info.values (J).value.Integer);
                  Put_Line (");");

            end case;
         end loop;

         Put_Line ("end case;");
         Put ("end Write_");
         Put_Id (Name);
         Put_Line (";");
      end if;

      New_Line;
   end Write_Enum;

   ------------------------
   -- Write_Nested_Types --
   ------------------------

   procedure Write_Nested_Types
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String) is
   begin
      for Cursor in Done.Iterate loop
         if Done (Cursor).Owner = Name then
            Write_Type
              (Model, Done, Done (Cursor).Short_Name,
               LSP_Gen.Dependencies.Dependency_Maps.Key (Cursor),
               Spec => True);
         end if;
      end loop;

      for Cursor in Done.Iterate loop
         if Done (Cursor).Owner = Name then
            Write_Type
              (Model, Done, Done (Cursor).Short_Name,
               LSP_Gen.Dependencies.Dependency_Maps.Key (Cursor),
               Spec => False);
         end if;
      end loop;
   end Write_Nested_Types;

   ----------------------
   -- Write_Properties --
   ----------------------

   procedure Write_Properties
     (Model       : LSP_Gen.Meta_Models.Meta_Model;
      Done        : LSP_Gen.Dependencies.Dependency_Map;
      List        : LSP_Gen.Entities.Property_Vector;
      Is_Optional : Boolean := False;
      Suffix      : VSS.Strings.Virtual_String := "";
      Exclude     : LSP_Gen.Entities.Property_Vector := Empty_Vector) is
   begin
      for J in 1 .. List.Length loop
         if not Contains (Exclude, List (J).name) then
            Write_Property (Model, Done, List (J), Is_Optional, Suffix);
         end if;
      end loop;
   end Write_Properties;

   --------------------
   -- Write_Property --
   --------------------

   procedure Write_Property
     (Model       : LSP_Gen.Meta_Models.Meta_Model;
      Done        : LSP_Gen.Dependencies.Dependency_Map;
      Item        : LSP_Gen.Entities.Property;
      Is_Optional : Boolean := False;
      Suffix      : VSS.Strings.Virtual_String := "")
   is
      Value : constant VSS.Strings.Virtual_String := "Value" & Suffix;
      Id    : VSS.Strings.Virtual_String :=
        LSP_Gen.Mappings.Ada_Id (Item.name);
   begin
      if Is_Optional or Item.optional then
         Put ("if ");

         if Is_Set (Model, Item.a_type) then
            Put ("(for some Item of ");
            Put (Value);
            Put (".");
            Put (Item.name);
            Put (" => Item)");
         elsif Is_String (Item.a_type) then
            Put ("not ");
            Put (Value);
            Put (".");
            Put (Id);
            Put (".Is_Null");
         elsif Is_Custom_Vector (Item.a_type) then
            Put (Value);
            Put (".");
            Put (Id);
            Put (".Length > 0");
         elsif Is_Vector (Item.a_type)
           or else Is_LSP_Any (Item.a_type)
           or else Item.a_type.Union.Kind = map
         then
            Put ("not ");
            Put (Value);
            Put (".");
            Put (Id);
            Put (".Is_Empty");
         else
            Put (Value);
            Put (".");
            Put (Id);
            Put (".Is_Set");
            Id.Append (".Value");
         end if;

         Put_Line (" then");
         Put ("Handler.Key_Name (""");
         Put (Item.name);
         Put (""");");
         Write_Call (Done, Item.a_type, Suffix & "." & Id);
         Put_Line ("end if;");
      else
         Put ("Handler.Key_Name (""");
         Put (Item.name);
         Put (""");");

         Write_Call (Done, Item.a_type, Suffix & "." & Id);
      end if;
   end Write_Property;

   ---------------------
   -- Write_Structure --
   ---------------------

   procedure Write_Structure
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String)
   is
      procedure Write_Mixin_Propeties (Item : LSP_Gen.Entities.Structure);
      procedure Write_Extends_Propeties (Item : LSP_Gen.Entities.Structure);

      -----------------------------
      -- Write_Extends_Propeties --
      -----------------------------

      procedure Write_Extends_Propeties (Item : LSP_Gen.Entities.Structure) is
         Base : constant Positive :=
           (if Item.extends.Length <= 1 then 1
            else Mappings.Base_Index (Item.extends));
      begin
         for J in 1 .. Item.extends.Length loop
            declare
               Parent : constant LSP_Gen.Entities.Structure :=
                 Model.Structure (Item.extends (J).Union.reference.name);

            begin
               Write_Mixin_Propeties (Parent);

               --  Emit overriden properties
               for K in 1 .. Item.properties.Length loop
                  if Contains
                    (Parent.properties, Item.properties (K).name)
                  then
                     Write_Property (Model, Done, Item.properties (K));
                  end if;
               end loop;

               Write_Properties
                 (Model, Done, Parent.properties,
                  Suffix => (if J /= Base then ".Parent"
                             else VSS.Strings.Empty_Virtual_String),
                  Exclude => Item.properties);
            end;
         end loop;
      end Write_Extends_Propeties;

      ---------------------------
      -- Write_Mixin_Propeties --
      ---------------------------

      procedure Write_Mixin_Propeties (Item : LSP_Gen.Entities.Structure) is
      begin
         for J in 1 .. Item.mixins.Length loop
            declare
               Mixin  : constant LSP_Gen.Entities.AType := Item.mixins (J);
               Parent : constant LSP_Gen.Entities.Structure :=
                 Model.Structure (Mixin.Union.reference.name);
            begin
               Write_Mixin_Propeties (Parent);
               Write_Properties (Model, Done, Parent.properties, False);
            end;
         end loop;
      end Write_Mixin_Propeties;

      Item : constant LSP_Gen.Entities.Structure :=
        Model.Structure (Name);
   begin
      Write_Subprogram_Definition (Name, "LSP.Structures.");
      Put_Line ("is");

      Write_Nested_Types (Model, Done, Name);

      for J in 1 .. Item.extends.Length loop
         Write_Nested_Types
           (Model, Done, Item.extends (J).Union.reference.name);
      end loop;

      Put_Line ("begin");
      Put_Line ("Handler.Start_Object;");
      Write_Extends_Propeties (Item);
      Write_Mixin_Propeties (Item);

      if Item.extends.Length = 0 then
         Write_Properties (Model, Done, Item.properties);
      else
         --  Skip overriden properties, as they have beed written by Wr_Ext_Pr
         Write_Properties
           (Model,
            Done,
            Item.properties,
            Exclude => Model.Structure
              (Item.extends (1).Union.reference.name).properties);
      end if;

      if Name = "LSPObject" then

         Put ("LSP.Output_Tools.Write_LSPAny ");
         Put_Line ("(Handler, LSP.Structures.LSPAny (Value));");

      elsif Item.properties.Length = 0
        and Item.mixins.Length = 0
        and Item.extends.Length = 0
      then

         Put_Line ("null;");
      end if;

      Put_Line ("Handler.End_Object;");
      Put ("end Write_");
      Put_Id (Name);
      Put_Line (";");

      New_Line;
   end Write_Structure;

   ---------------------------------
   -- Write_Subprogram_Sefinition --
   ---------------------------------

   procedure Write_Subprogram_Definition
     (Name   : VSS.Strings.Virtual_String;
      Prefix : VSS.Strings.Virtual_String) is
   begin
      Put ("procedure Write_");
      Put_Id (Name);
      Put (" (Handler : in out ");
      Put_Line ("VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;");
      Put ("Value : ");
      Put (Prefix);
      Put_Id (Name);
      Put (")");
   end Write_Subprogram_Definition;

   ---------------------------
   -- Write_Subprogram_Name --
   ---------------------------

   procedure Write_Subprogram_Name
     (Done   : LSP_Gen.Dependencies.Dependency_Map;
      Tipe   : LSP_Gen.Entities.AType) is
   begin
      Put ("Write_");

      case Tipe.Union.Kind is
         when reference =>
            Put_Id (Tipe.Union.reference.name);
         when literal | a_or | an_array | tuple =>
            Put_Id (Done (Tipe).Short_Name);
         when others =>
            raise Program_Error;
      end case;
   end Write_Subprogram_Name;

   ----------------
   -- Write_Type --
   ----------------

   procedure Write_Type
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String;
      Tipe  : LSP_Gen.Entities.AType;
      Spec  : Boolean)
   is
      function Array_Element return LSP_Gen.Entities.AType is
        (Tipe.Union.an_array.element.Value);

   begin
      if Name = "LSPArray" then
         return;  --  TBD
      elsif Tipe.Union.Kind = stringLiteral then
         return;
      elsif Tipe.Union.Kind = base and then
        Tipe.Union.base.name not in LSP_Gen.Entities.Enum.string | a_null
      then
         return;
      end if;

      if Tipe.Union.Kind = reference
        and then Model.Is_Enumeration (Tipe.Union.reference.name)
      then
         Write_Subprogram_Definition (Name, "LSP.Enumerations.");
      else
         Write_Subprogram_Definition (Name, "LSP.Structures.");
      end if;

      if Spec then
         Put_Line (";");
      elsif Tipe.Union.Kind = reference then
         Put_Line (" renames");
         Write_Subprogram_Name (Done, Tipe);
         Put_Line (";");
      elsif Name = "LSPAny_Vector" or Name = "LSPAny" then
         Put_Line (" renames LSP.Output_Tools.Write_LSPAny;");
      else
         Put_Line ("is");

         Write_Nested_Types (Model, Done, Name);

         Put_Line ("begin");

         case Tipe.Union.Kind is
            when base =>
               Write_Call (Done, Tipe, "");
            when tuple =>
               Put_Line ("Handler.Start_Array;");
               Put_Line ("for J in Value'Range loop");
                  Write_Call (Done, Tipe.Union.tuple.items (1), " (J)");
               Put_Line ("end loop;");
               Put_Line ("Handler.End_Array;");
            when an_array =>
               Put_Line ("Handler.Start_Array;");

               if Array_Element.Union.Kind = reference
                 and then Model.Is_Custom_Enumeration
                   (Array_Element.Union.reference.name)
               then
                  Put_Line
                       ("for J in Value.First_Index .. Value.Last_Index loop");
                  Write_Call (Done, Array_Element, " (J)");
                  Put_Line ("end loop;");

               elsif Name.Ends_With ("_Set") then
                  Put_Line ("declare");
                  Put ("Set : LSP.Structures.");
                  Put (Name);
                  Put_Line (" renames Value;");
                  Put_Line ("begin");
                  Put_Line ("   for Value in Set'Range loop");
                  Put_Line ("      if Set (Value) then");
                  Write_Call (Done, Array_Element, "");
                  Put_Line ("      end if;");
                  Put_Line ("   end loop;");
                  Put_Line ("end;");
               else
                  if Is_Custom_Vector (Tipe) or else
                    Name = "Virtual_String_Vector"
                  then
                     Put_Line ("for J in 1 .. Value.Length loop");
                  else
                     Put_Line
                       ("for J in Value.First_Index .. Value.Last_Index loop");
                  end if;

                  Write_Call (Done, Array_Element, " (J)");
                  Put_Line ("end loop;");
               end if;
               Put_Line ("Handler.End_Array;");

            when map =>
               Write_Call (Done, Tipe, "");

            when a_or =>
               declare
                  use all type LSP_Gen.Mappings.Or_Mapping_Kind;

                  Map : constant LSP_Gen.Mappings.Or_Mapping :=
                    LSP_Gen.Mappings.Get_Or_Mapping
                      (Model, Tipe.Union.a_or.items);
               begin
                  case Map.Kind is
                     when Type_Or_Array =>
                        Put_Line ("if Value.Length = 1 then");
                        Write_Call
                          (Done,
                           Map.Array_Type.Union.an_array.element.Value,
                           " (1)");
                        New_Line;

                        Put_Line ("else");

                        Put_Line ("Handler.Start_Array;");
                        Put ("for J in 1 .. Value.");

                        if Is_String
                          (Map.Array_Type.Union.an_array.element.Value)
                        then
                           Put ("Length");
                        else
                           Put ("Last_Index");
                        end if;

                        Put_Line (" loop");
                        Write_Call
                          (Done,
                           Map.Array_Type.Union.an_array.element.Value,
                           " (J)");
                        New_Line;
                        Put_Line ("end loop;");
                        Put_Line ("Handler.End_Array;");

                        Put_Line ("end if;");

                     when Type_Or_Null =>
                        Put_Line ("if Value.Is_Null then");
                        Put_Line ("Handler.Null_Value;");
                        Put_Line ("else");
                        Write_Call (Done, Map.Tipe, ".Value");
                        Put_Line ("end if;");

                     when Array_Or_Null =>
                        if Is_Custom_Vector (Map.Array_Type) then
                           Put_Line ("if Value.Length = 0 then");
                        else
                           Put_Line ("if Value.Is_Empty then");
                        end if;
                        Put_Line ("Handler.Null_Value;");
                        Put_Line ("else");
                        Write_Call (Done, Map.Array_Type, "");
                        Put_Line ("end if;");

                     when Unknown_Mapping =>
                        Put_Line ("case Value.Kind is");

                        for J in 1 .. Tipe.Union.a_or.items.Length loop
                           Put ("   when LSP.Structures.");
                           Put_Id (Model.Get_Variant
                                   (Tipe.Union.a_or.items (J), J));
                           Put_Line (" =>");

                           Write_Call
                             (Done,
                              Tipe.Union.a_or.items (J),
                              "." & Model.Get_Variant
                                (Tipe.Union.a_or.items (J), J));
                        end loop;

                        Put_Line ("end case;");
                     when Two_Types
                      | String_Or_Tuple =>

                        declare
                           First : constant
                             LSP_Gen.Dependencies.Dependency_Info :=
                               Done (Map.First);

                           Second : constant
                             LSP_Gen.Dependencies.Dependency_Info :=
                               Done (Map.Second);
                        begin
                           Put ("case Value.Is_");
                           Put (First.Short_Name);
                           Put_Line (" is");
                           Put_Line ("   when True =>");

                           Write_Call
                             (Done,
                              Map.First,
                              "." & First.Short_Name);

                           Put_Line ("   when False =>");

                           Write_Call
                             (Done,
                              Map.Second,
                              "." & Second.Short_Name);

                           Put_Line ("end case;");
                        end;

                     when Type_Or_Something =>

                        declare
                           First : constant
                             LSP_Gen.Dependencies.Dependency_Info :=
                               Done (Map.First);
                        begin
                           Put ("case Value.Is_");
                           Put (First.Short_Name);
                           Put_Line (" is");
                           Put_Line ("   when True =>");

                           Write_Call
                             (Done,
                              Map.First,
                              "." & First.Short_Name);

                           Put_Line ("   when False =>");

                           Put_Line ("Handler.Start_Object;");

                           Write_Properties
                             (Model,
                              Done,
                              Map.Second.Union.literal.value.properties);

                           Put_Line ("Handler.End_Object;");

                           Put_Line ("end case;");
                        end;

                     when Option_Combination =>
                        Put_Line ("Handler.Start_Object;");
                        Write_Properties
                          (Model,
                           Done,
                           Map.Tipe.Union.literal.value.properties,
                           Is_Optional => True);
                        Put_Line ("Handler.End_Object;");

                     when Two_Literals =>
                        declare
                           Base : constant LSP_Gen.Entities.Property_Vector
                             := Map.First.Union.literal.value.properties;
                           Extend : constant LSP_Gen.Entities.Property_Vector
                             := Map.Second.Union.literal.value.properties;
                        begin
                           Put_Line ("Handler.Start_Object;");
                           for J in 1 .. Extend.Length loop
                              Write_Property
                                (Model,
                                 Done,
                                 Extend (J),
                                 Is_Optional => not
                                   (for some K in 1 .. Base.Length =>
                                        Base (K).name = Extend (J).name));
                           end loop;
                           Put_Line ("Handler.End_Object;");
                        end;

                     when Type_Union =>
                        Write_Union (Model, Done, Map.Items);

                     when Boolean_Or_Any =>
                        Put ("LSP.Output_Tools.Write_LSPAny ");
                        Put_Line ("(Handler, (Value with null record));");

                     when Enumeration =>
                        declare
                           List : LSP_Gen.Entities.AType_Vector renames
                             Map.Items;
                        begin
                           Put_Line ("case Value is");
                           for J in 1 .. List.Length loop
                              Put ("   when LSP.Structures.");
                              Put (List (J).Union.stringLiteral.value);
                              Put_Line (" =>");
                              Put ("Handler.String_Value (""");
                              Put (List (J).Union.stringLiteral.value);
                              Put_Line (""");");
                           end loop;
                           Put_Line ("end case;");
                        end;
                  end case;
               end;
            when literal =>
               Put_Line ("Handler.Start_Object;");

               Write_Properties
                 (Model, Done, Tipe.Union.literal.value.properties);

               Put_Line ("Handler.End_Object;");

            when others =>
               raise Program_Error;  --  Put_Line ("null;");
         end case;
         Put ("end Write_");
         Put_Id (Name);
         Put_Line (";");
      end if;

      New_Line;
   end Write_Type;

   -----------------
   -- Write_Union --
   -----------------

   procedure Write_Union
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      List  : LSP_Gen.Entities.AType_Vector)
   is
      Variants : VSS.String_Vectors.Virtual_String_Vector;
   begin
      for J in 1 .. List.Length loop
         Variants.Append (Model.Get_Variant (List (J), J));
      end loop;

      Put_Line ("case Value.Kind is");

      for J in 1 .. Variants.Length loop
         Put ("when LSP.Structures.");
         Put_Id (Variants (J));
         Put_Line (" =>");
         Write_Call (Done, List (J), "." & Variants (J));
      end loop;

      Put_Line ("end case;");
   end Write_Union;

end LSP_Gen.Outputs;

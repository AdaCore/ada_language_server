------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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

with Ada.Containers.Generic_Anonymous_Array_Sort;

with VSS.JSON.Pull_Readers.Simple;
with VSS.Text_Streams.File_Input;

with LSP_Gen.Entities.Inputs;
with LSP_Gen.Mappings;

package body LSP_Gen.Meta_Models is

   procedure Apply_Protocol_Extension (Self : in out Meta_Model'Class);

   procedure Sort (List : in out VSS.String_Vectors.Virtual_String_Vector);

   ------------
   -- Append --
   ------------

   procedure Append
     (Self  : in out Type_Dependency;
      Value : Type_Dependency) is
   begin
      Self.Required.Union (Value.Required);
      Self.Optional.Union (Value.Optional);
   end Append;

   ------------------------------
   -- Apply_Protocol_Extension --
   ------------------------------

   procedure Apply_Protocol_Extension (Self : in out Meta_Model'Class) is
      use type VSS.Strings.Virtual_String;

      Name    : constant VSS.Strings.Virtual_String :=
        Self.Config.Protocol_Extension;

      Input   : aliased VSS.Text_Streams.File_Input.File_Input_Text_Stream;
      Reader  : VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
      Patch   : LSP_Gen.Entities.MetaModel;
      Success : Boolean := True;
   begin
      Input.Open (Name, "utf-8");
      Reader.Set_Stream (Input'Unchecked_Access);
      Reader.Read_Next;
      pragma Assert (Reader.Is_Start_Document);
      Reader.Read_Next;
      LSP_Gen.Entities.Inputs.Input_MetaModel (Reader, Patch, Success);
      pragma Assert (Success);

      --  Copy enumeration types
      for J in 1 .. Patch.enumerations.Length loop
         declare
            Item : constant LSP_Gen.Entities.Enumeration :=
              Patch.enumerations (J);
         begin
            Self.Model.enumerations.Append (Item);
         end;
      end loop;

      --  Copy/patch structure types
      for J in 1 .. Patch.structures.Length loop
         declare
            Found : Boolean := False;
            Item : constant LSP_Gen.Entities.Structure :=
              Patch.structures (J);
         begin
            for K in 1 .. Self.Model.structures.Length loop
               if Self.Model.structures (K).name = Item.name then
                  for P in 1 .. Item.properties.Length loop
                     declare
                        Prop : constant LSP_Gen.Entities.Property :=
                          Item.properties (P);
                     begin
                        Self.Model.structures (K).properties.Append (Prop);
                     end;
                  end loop;

                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               Self.Model.structures.Append (Item);
            end if;
         end;
      end loop;

      --  Copy extra requests
      for J in 1 .. Patch.requests.Length loop
         declare
            Item : constant LSP_Gen.Entities.Request := Patch.requests (J);
         begin
            Self.Model.requests.Append (Item);
         end;
      end loop;
   end Apply_Protocol_Extension;

   ----------------
   -- Dependency --
   ----------------

   function Dependency
     (Self : Meta_Model'Class;
      Name : VSS.Strings.Virtual_String)
      return Type_Dependency
   is
      use type VSS.Strings.Virtual_String;

      function Dependency (Item : LSP_Gen.Entities.AType)
        return Type_Dependency;

      function Dependency (List : LSP_Gen.Entities.AType_Vector)
        return Type_Dependency;

      function Dependency (Item : LSP_Gen.Entities.Property)
        return Type_Dependency;

      function Dependency (List : LSP_Gen.Entities.Property_Vector)
        return Type_Dependency;

      ----------------
      -- Dependency --
      ----------------

      function Dependency
        (Item : LSP_Gen.Entities.AType) return Type_Dependency
      is
         use all type LSP_Gen.Entities.Enum.AType_Variant;
      begin
         return Result : Type_Dependency do
            case Item.Union.Kind is
               when reference =>
                  Result.Required.Include (Item.Union.reference.name);

               when an_array =>
                  Result.Append
                    (Dependency (Item.Union.an_array.element.Value));

               when map =>
                  Result.Append (Dependency (Item.Union.map.value.Value));

               when tuple =>
                  Result.Append (Dependency (Item.Union.tuple.items));

               when literal =>
                  Result.Append
                    (Dependency (Item.Union.literal.value.properties));

               when a_or =>
                  declare
                     use all type LSP_Gen.Mappings.Or_Mapping_Kind;

                     Map : constant LSP_Gen.Mappings.Or_Mapping :=
                       LSP_Gen.Mappings.Get_Or_Mapping
                         (Self, Item.Union.a_or.items);
                  begin
                     Result.Append (Dependency (Item.Union.a_or.items));

                     if Map.Kind = Two_Literals then
                        declare
                           List : LSP_Gen.Entities.Property_Vector renames
                             Map.Second.Union.literal.value.properties;
                           Deps : constant Type_Dependency :=
                             Dependency (List);
                        begin
                           Result.Append (Deps);
                           Result.Optional.Union (Deps.Required);
                        end;
                     end if;
                  end;

               when others =>
                  null;
            end case;
         end return;
      end Dependency;

      ----------------
      -- Dependency --
      ----------------

      function Dependency (List : LSP_Gen.Entities.AType_Vector)
        return Type_Dependency is
      begin
         return Result : Type_Dependency do
            for J in 1 .. List.Length loop
               Result.Append (Dependency (List (J)));
            end loop;
         end return;
      end Dependency;

      ----------------
      -- Dependency --
      ----------------

      function Dependency (Item : LSP_Gen.Entities.Property)
        return Type_Dependency
      is
         use all type LSP_Gen.Entities.Enum.AType_Variant;
      begin
         return Result : Type_Dependency := Dependency (Item.a_type) do
            if Item.optional and Item.a_type.Union.Kind = reference then
               Result.Optional.Union (Result.Required);
            end if;
         end return;
      end Dependency;

      ----------------
      -- Dependency --
      ----------------

      function Dependency (List : LSP_Gen.Entities.Property_Vector)
        return Type_Dependency is
      begin
         return Result : Type_Dependency do
            for J in 1 .. List.Length loop
               Result.Append (Dependency (List (J)));
            end loop;
         end return;
      end Dependency;

      Index  : constant Top_Type_Index := Self.Index (Name);
      Result : Type_Dependency;
   begin
      case Index.Kind is
         when Enumeration =>
            null;  --  No dependency for any enum

         when Type_Alias =>
            declare
               Item : constant LSP_Gen.Entities.TypeAlias :=
                 Self.Model.typeAliases (Index.Position);
            begin
               if Item.name /= "LSPAny" then
                  Result.Append (Dependency (Item.a_type));
               end if;
            end;

         when Structure =>
            declare
               Item : constant LSP_Gen.Entities.Structure :=
                 Self.Model.structures (Index.Position);
            begin
               if Name = "LSPObject" then
                  Result.Required.Insert ("LSPAny");
               else
                  Result.Append (Dependency (Item.extends));
                  Result.Append (Dependency (Item.mixins));
                  Result.Append (Dependency (Item.properties));
               end if;
            end;

      end case;

      return Result;
   end Dependency;

   -----------------
   -- Enumeration --
   -----------------

   function Enumeration
     (Self : Meta_Model'Class;
      Name : VSS.Strings.Virtual_String)
      return LSP_Gen.Entities.Enumeration
   is
      Index : constant Top_Type_Index := Self.Index (Name);
   begin
      pragma Assert (Index.Kind = Enumeration);
      return Self.Model.enumerations (Index.Position);
   end Enumeration;

   ------------------
   -- Enumerations --
   ------------------

   function Enumerations (Self : Meta_Model'Class)
     return VSS.String_Vectors.Virtual_String_Vector is
   begin
      return Result : VSS.String_Vectors.Virtual_String_Vector do
         for J in 1 .. Self.Model.enumerations.Length loop
            declare
               Name : constant VSS.Strings.Virtual_String :=
                 Self.Model.enumerations (J).name;
            begin
               Result.Append (Name);
            end;
         end loop;

         Sort (Result);
      end return;
   end Enumerations;

   ---------
   -- Get --
   ---------

   function Get
     (Self : Meta_Model'Class;
      Name : VSS.Strings.Virtual_String)
      return Top_Type
   is
      Index : constant Top_Type_Index := Self.Index (Name);
   begin
      case Index.Kind is
         when Enumeration =>
            return (Enumeration, Self.Model.enumerations (Index.Position));
         when Type_Alias =>
            return (Type_Alias, Self.Model.typeAliases (Index.Position));
         when Structure =>
            return (Structure, Self.Model.structures (Index.Position));
      end case;
   end Get;

   -------------------
   -- Get_Base_Type --
   -------------------

   function Get_Base_Type
     (Self : Meta_Model'Class;
      Tipe : LSP_Gen.Entities.AType) return LSP_Gen.Entities.Enum.BaseTypes is
   begin
      case Tipe.Union.Kind is
         when LSP_Gen.Entities.Enum.base =>
            return Tipe.Union.base.name;
         when LSP_Gen.Entities.Enum.reference =>
            declare
               Top : constant LSP_Gen.Meta_Models.Top_Type :=
                 Self.Get (Tipe.Union.reference.name);
            begin
               case Top.Kind is
                  when LSP_Gen.Meta_Models.Enumeration =>
                     if Top.Enumeration.values (1).value.Is_String then
                        return LSP_Gen.Entities.Enum.string;
                     else
                        return LSP_Gen.Entities.Enum.integer;
                     end if;
                  when LSP_Gen.Meta_Models.Type_Alias =>
                     case Top.Type_Alias.a_type.Union.Kind is
                        when LSP_Gen.Entities.Enum.base =>
                           return Top.Type_Alias.a_type.Union.base.name;
                        when others =>
                           raise Program_Error;
                     end case;
                  when others =>
                     raise Program_Error;
               end case;
            end;
         when others =>
            raise Program_Error;
      end case;
   end Get_Base_Type;

   -----------------
   -- Get_Variant --
   -----------------

   function Get_Variant
     (Self  : Meta_Model'Class;
      Item  : LSP_Gen.Entities.AType;
      Index : Positive) return VSS.Strings.Virtual_String
   is
      use type VSS.Strings.Virtual_String;
      use all type LSP_Gen.Entities.Enum.AType_Variant;
      Image : constant Wide_Wide_String := Index'Wide_Wide_Image;
   begin
      if Item.Union.Kind = reference
        and then Self.Get (Item.Union.reference.name).Kind = Structure
      then
         declare
            Tipe : constant LSP_Gen.Entities.Structure :=
              Self.Structure (Item.Union.reference.name);
            List : constant LSP_Gen.Entities.Property_Vector :=
              Tipe.properties;
         begin
            if Tipe.extends.Length > 0 and then
              LSP_Gen.Mappings.Has_Kind (Self, Tipe.extends (1))
            then
               return Self.Get_Variant (Tipe.extends (1), Index);
            end if;

            if List.Length > 0 and then List (1).name = "kind" then
               return List (1).a_type.Union.stringLiteral.value;
            end if;
         end;
      end if;

      return
        "Variant_" & VSS.Strings.To_Virtual_String (Image (2 .. Image'Last));
   end Get_Variant;

   ------------------
   -- Is_Base_Type --
   ------------------

   function Is_Base_Type
     (Self : Meta_Model'Class;
      Tipe : LSP_Gen.Entities.AType) return Boolean is
   begin
      case Tipe.Union.Kind is
         when LSP_Gen.Entities.Enum.base =>
            return True;
         when LSP_Gen.Entities.Enum.reference =>
            declare
               Top : constant LSP_Gen.Meta_Models.Top_Type :=
                 Self.Get (Tipe.Union.reference.name);
            begin
               case Top.Kind is
                  when LSP_Gen.Meta_Models.Enumeration =>
                     return True;
                  when LSP_Gen.Meta_Models.Type_Alias =>
                     case Top.Type_Alias.a_type.Union.Kind is
                        when LSP_Gen.Entities.Enum.base =>
                           return True;
                        when others =>
                           return False;
                     end case;
                  when others =>
                     return False;
               end case;
            end;
         when others =>
            return False;
      end case;
   end Is_Base_Type;

   ---------------------------
   -- Is_Custom_Enumeration --
   ---------------------------

   function Is_Custom_Enumeration
     (Self : Meta_Model'Class;
      Name : VSS.Strings.Virtual_String) return Boolean is
   begin
      return Self.Is_Enumeration (Name) and then
        Self.Model.enumerations (Self.Index (Name).Position)
          .supportsCustomValues;
   end Is_Custom_Enumeration;

   --------------------
   -- Is_Enumeration --
   --------------------

   function Is_Enumeration
     (Self : Meta_Model'Class;
      Name : VSS.Strings.Virtual_String) return Boolean is
   begin
      return Self.Index.Contains (Name)
        and then Self.Index (Name).Kind = Enumeration;
   end Is_Enumeration;

   --------------
   -- Is_Mixin --
   --------------

   function Is_Mixin
     (Self : Meta_Model'Class;
      Name : VSS.Strings.Virtual_String) return Boolean is
        (Self.Index (Name).Kind = Structure
           and then Self.Index (Name).Is_Mixin);

   ---------------
   -- Is_Tagged --
   ---------------

   function Is_Tagged
     (Self : Meta_Model'Class;
      Name : VSS.Strings.Virtual_String) return Boolean is
        (Self.Index (Name).Is_Tagged);

   ------------------
   -- Notification --
   ------------------

   function Notification
     (Self   : Meta_Model'Class;
      Method : VSS.Strings.Virtual_String) return LSP_Gen.Entities.Notification
        is (Self.Model.notifications (Self.Index (Method).Position));

   ----------------------------
   -- Notification_Direction --
   ----------------------------

   function Message_Direction
     (Self   : Meta_Model'Class;
      Method : VSS.Strings.Virtual_String)
        return LSP_Gen.Configurations.Message_Direction is
           (Self.Config.Direction (Method));

   -----------------------
   -- Notification_Name --
   -----------------------

   function Message_Name
     (Self   : Meta_Model'Class;
      Method : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String is
        (Self.Config.Name (Method));

   -------------------
   -- Notifications --
   -------------------

   function Notifications
     (Self : Meta_Model'Class) return VSS.String_Vectors.Virtual_String_Vector
   is
   begin
      return Result : VSS.String_Vectors.Virtual_String_Vector do
         for J in 1 .. Self.Model.notifications.Length loop
            declare
               Name : constant VSS.Strings.Virtual_String :=
                 Self.Model.notifications (J).method;
            begin
               Result.Append (Name);
            end;
         end loop;

         Sort (Result);
      end return;
   end Notifications;

   ----------------
   -- Read_Model --
   ----------------

   procedure Read_Model
     (Self   : out Meta_Model'Class;
      Name   : VSS.Strings.Virtual_String;
      Config : VSS.Strings.Virtual_String)
   is
      Input   : aliased VSS.Text_Streams.File_Input.File_Input_Text_Stream;
      Reader  : VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
      Success : Boolean := True;
   begin
      Self.Config.Load (Config);
      Input.Open (Name, "utf-8");
      Reader.Set_Stream (Input'Unchecked_Access);
      Reader.Read_Next;
      pragma Assert (Reader.Is_Start_Document);
      Reader.Read_Next;
      LSP_Gen.Entities.Inputs.Input_MetaModel (Reader, Self.Model, Success);
      pragma Assert (Success);

      if not Self.Config.Protocol_Extension.Is_Empty then
         Self.Apply_Protocol_Extension;
      end if;

      for J in 1 .. Self.Model.enumerations.Length loop
         declare
            Name : constant VSS.Strings.Virtual_String :=
              Self.Model.enumerations (J).name;
         begin
            Self.Index.Insert (Name, (Enumeration, J));
         end;
      end loop;

      for J in 1 .. Self.Model.structures.Length loop
         declare
            Name : constant VSS.Strings.Virtual_String :=
              Self.Model.structures (J).name;
         begin
            Self.Index.Insert (Name, (Structure, J, False, False));
         end;
      end loop;

      for J in 1 .. Self.Model.structures.Length loop
         declare
            Tipe : constant LSP_Gen.Entities.Structure :=
              Self.Model.structures (J);
         begin
            for K in 1 .. Tipe.mixins.Length loop
               declare
                  Name : constant VSS.Strings.Virtual_String :=
                    Tipe.mixins (K).Union.reference.name;
               begin
                  Self.Index (Name).Is_Mixin := True;
               end;
            end loop;

            for K in 1 .. Tipe.extends.Length loop
               declare
                  Name : constant VSS.Strings.Virtual_String :=
                    Tipe.extends (K).Union.reference.name;
               begin
                  Self.Index (Name).Is_Tagged := True;
               end;
            end loop;
         end;
      end loop;

      for J in 1 .. Self.Model.typeAliases.Length loop
         declare
            Name : constant VSS.Strings.Virtual_String :=
              Self.Model.typeAliases (J).name;
         begin
            Self.Index.Insert (Name, (Type_Alias, J));
         end;
      end loop;

      for J in 1 .. Self.Model.notifications.Length loop
         declare
            Method : constant VSS.Strings.Virtual_String :=
              Self.Model.notifications (J).method;
         begin
            Self.Index.Insert (Method, (Type_Alias, J));
         end;
      end loop;

      for J in 1 .. Self.Model.requests.Length loop
         declare
            Method : constant VSS.Strings.Virtual_String :=
              Self.Model.requests (J).method;
         begin
            Self.Index.Insert (Method, (Type_Alias, J));
         end;
      end loop;
   end Read_Model;

   -------------
   -- Request --
   -------------

   function Request
     (Self   : Meta_Model'Class;
      Method : VSS.Strings.Virtual_String) return LSP_Gen.Entities.Request
        is (Self.Model.requests (Self.Index (Method).Position));

   --------------
   -- Requests --
   --------------

   function Requests
     (Self : Meta_Model'Class) return VSS.String_Vectors.Virtual_String_Vector
   is
   begin
      return Result : VSS.String_Vectors.Virtual_String_Vector do
         for J in 1 .. Self.Model.requests.Length loop
            declare
               Name : constant VSS.Strings.Virtual_String :=
                 Self.Model.requests (J).method;
            begin
               Result.Append (Name);
            end;
         end loop;

         Sort (Result);
      end return;
   end Requests;

   ----------
   -- Sort --
   ----------

   procedure Sort (List : in out VSS.String_Vectors.Virtual_String_Vector) is
      use type VSS.Strings.Virtual_String;

      procedure Swap (Left, Right : Positive);

      function Less (Left, Right : Positive) return Boolean is
        (List (Left) < List (Right));

      procedure Swap (Left, Right : Positive) is
         Copy : constant VSS.Strings.Virtual_String := List (Left);
      begin
         List.Replace (Left, List (Right));
         List.Replace (Right, Copy);
      end Swap;

      procedure Do_Sort is new Ada.Containers.Generic_Anonymous_Array_Sort
        (Positive, Less, Swap);
   begin
      Do_Sort (1, List.Length);
   end Sort;

   ---------------
   -- Structure --
   ---------------

   function Structure
     (Self : Meta_Model'Class; Name : VSS.Strings.Virtual_String)
      return LSP_Gen.Entities.Structure
   is
      Index : constant Top_Type_Index := Self.Index (Name);
   begin
      pragma Assert (Index.Kind = Structure);
      return Self.Model.structures (Index.Position);
   end Structure;

   ----------------
   -- Structures --
   ----------------

   function Structures (Self : Meta_Model'Class)
     return VSS.String_Vectors.Virtual_String_Vector is
   begin
      return Result : VSS.String_Vectors.Virtual_String_Vector do
         for J in 1 .. Self.Model.structures.Length loop
            declare
               Name : constant VSS.Strings.Virtual_String :=
                 Self.Model.structures (J).name;
            begin
               Result.Append (Name);
            end;
         end loop;

         Sort (Result);
      end return;
   end Structures;

   ----------------
   -- Type_Alias --
   ----------------

   function Type_Alias
     (Self : Meta_Model'Class; Name : VSS.Strings.Virtual_String)
      return LSP_Gen.Entities.TypeAlias
   is
      Index : constant Top_Type_Index := Self.Index (Name);
   begin
      pragma Assert (Index.Kind = Type_Alias);
      return Self.Model.typeAliases (Index.Position);
   end Type_Alias;

   ------------------
   -- Type_Aliases --
   ------------------

   function Type_Aliases (Self : Meta_Model'Class)
     return VSS.String_Vectors.Virtual_String_Vector is
   begin
      return Result : VSS.String_Vectors.Virtual_String_Vector do
         for J in 1 .. Self.Model.typeAliases.Length loop
            declare
               Name : constant VSS.Strings.Virtual_String :=
                 Self.Model.typeAliases (J).name;
            begin
               Result.Append (Name);
            end;
         end loop;

         Sort (Result);
      end return;
   end Type_Aliases;

end LSP_Gen.Meta_Models;

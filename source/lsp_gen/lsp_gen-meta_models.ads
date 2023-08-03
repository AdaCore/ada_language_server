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

with Ada.Containers.Hashed_Maps;

with VSS.String_Vectors;
with VSS.Strings;
with VSS.Strings.Hash;

with LSP_Gen.Configurations;
with LSP_Gen.Entities;
with LSP_Gen.String_Sets;

package LSP_Gen.Meta_Models is

   type Meta_Model is tagged limited private;

   procedure Read_Model
     (Self   : out Meta_Model'Class;
      Name   : VSS.Strings.Virtual_String;
      Config : VSS.Strings.Virtual_String);
   --  Read metaModel JSON and corresponding extra configuration.
   --  Name is the file name of the meta-model JSON file.
   --  Config is the file name of the extra configuration JSON5 file.
   --  The extra configuration contains information missing in the meta-model
   --  such as message direction (server-to-client, client-to-server, both).

   type Top_Type_Kind is (Enumeration, Type_Alias, Structure);

   type Top_Type (Kind : Top_Type_Kind := Enumeration) is record
      case Kind is
         when Enumeration =>
            Enumeration : LSP_Gen.Entities.Enumeration;
         when Type_Alias =>
            Type_Alias : LSP_Gen.Entities.TypeAlias;
         when Structure =>
            Structure : LSP_Gen.Entities.Structure;
      end case;
   end record;

   function Get
     (Self : Meta_Model'Class;
      Name : VSS.Strings.Virtual_String) return Top_Type;
   --  Get a top entity by name

   function Is_Mixin
     (Self : Meta_Model'Class;
      Name : VSS.Strings.Virtual_String) return Boolean;
   --  Some structure has Name in mixins

   function Is_Tagged
     (Self : Meta_Model'Class;
      Name : VSS.Strings.Virtual_String) return Boolean;
   --  Some structure extends Name

   function Is_Enumeration
     (Self : Meta_Model'Class;
      Name : VSS.Strings.Virtual_String) return Boolean;

   function Is_Base_Type
     (Self : Meta_Model'Class;
      Tipe : LSP_Gen.Entities.AType) return Boolean;
   --  Check if Tipe is base, reference to a base or base type alias or enum

   function Get_Base_Type
     (Self : Meta_Model'Class;
      Tipe : LSP_Gen.Entities.AType) return LSP_Gen.Entities.Enum.BaseTypes
     with Pre => Is_Base_Type (Self, Tipe);

   function Enumeration
     (Self : Meta_Model'Class;
      Name : VSS.Strings.Virtual_String) return LSP_Gen.Entities.Enumeration;

   function Type_Alias
     (Self : Meta_Model'Class;
      Name : VSS.Strings.Virtual_String) return LSP_Gen.Entities.TypeAlias;

   function Structure
     (Self : Meta_Model'Class;
      Name : VSS.Strings.Virtual_String) return LSP_Gen.Entities.Structure;

   function Enumerations
     (Self : Meta_Model'Class) return VSS.String_Vectors.Virtual_String_Vector;

   function Type_Aliases
     (Self : Meta_Model'Class) return VSS.String_Vectors.Virtual_String_Vector;

   function Structures
     (Self : Meta_Model'Class) return VSS.String_Vectors.Virtual_String_Vector;

   function Notifications
     (Self : Meta_Model'Class) return VSS.String_Vectors.Virtual_String_Vector;

   function Notification
     (Self   : Meta_Model'Class;
      Method : VSS.Strings.Virtual_String)
        return LSP_Gen.Entities.Notification;

   function Message_Direction
     (Self   : Meta_Model'Class;
      Method : VSS.Strings.Virtual_String)
        return LSP_Gen.Configurations.Message_Direction;

   function Message_Name
     (Self   : Meta_Model'Class;
      Method : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String;
   --  Notification, Request, etc name as an Ada identifier

   function Requests
     (Self : Meta_Model'Class) return VSS.String_Vectors.Virtual_String_Vector;

   function Request
     (Self   : Meta_Model'Class;
      Method : VSS.Strings.Virtual_String) return LSP_Gen.Entities.Request;

   type Type_Dependency is record
      Required : LSP_Gen.String_Sets.Set;
      Optional : LSP_Gen.String_Sets.Set;
   end record;

   procedure Append
     (Self  : in out Type_Dependency;
      Value : Type_Dependency);
   --  Union value and Self

   function Dependency
     (Self : Meta_Model'Class;
      Name : VSS.Strings.Virtual_String) return Type_Dependency;
   --  Return all refenreces from given structure or type alias to any top
   --  entity type

   function Get_Variant
     (Self  : Meta_Model'Class;
      Item  : LSP_Gen.Entities.AType;
      Index : Positive) return VSS.Strings.Virtual_String;
   --  Return variant name for given `or` type item

   function License_Header
     (Self : Meta_Model'Class) return VSS.String_Vectors.Virtual_String_Vector;
   --  Lines of license header to be included in each generated file

private

   type Top_Type_Index (Kind : Top_Type_Kind := Enumeration) is record
      Position : Positive;

      case Kind is
         when Structure =>
            Is_Tagged : Boolean := False;
            Is_Mixin  : Boolean := False;
         when others =>
            null;
      end case;
   end record;

   package Top_Type_Maps is new Ada.Containers.Hashed_Maps
     (VSS.Strings.Virtual_String,
      Top_Type_Index,
      VSS.Strings.Hash,
      VSS.Strings."=");

   type Meta_Model is tagged limited record
      Config : LSP_Gen.Configurations.Configuration;
      Model  : LSP_Gen.Entities.MetaModel;
      Index  : Top_Type_Maps.Map;
   end record;

   function License_Header
     (Self : Meta_Model'Class) return VSS.String_Vectors.Virtual_String_Vector
       is (Self.Config.License_Header);

end LSP_Gen.Meta_Models;

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

with VSS.Strings;

with LSP_Gen.Entities;
with LSP_Gen.Meta_Models;

package LSP_Gen.Mappings is

   function Ada_Id (Text : VSS.Strings.Virtual_String)
     return VSS.Strings.Virtual_String;
   --  Return an Ada identifier, use some prefix if Text is an Ada reserved
   --  keyword.

   function Ada_Id (Self : LSP_Gen.Entities.EnumerationEntry)
     return VSS.Strings.Virtual_String;

   function Column_Name (Self : LSP_Gen.Entities.AType)
     return VSS.Strings.Virtual_String;
   --  If AType is part of record (like AType_Optional), return Field name

   function Base_Index (List : LSP_Gen.Entities.AType_Vector) return Positive;
   --  Return index of base type from given `extends` list.

   type Or_Mapping_Kind is
     (Type_Union,
      Type_Or_Null,
      Array_Or_Null,
      Type_Or_Array,
      Type_Or_Something,
      Two_Types,
      Two_Literals,
      String_Or_Tuple,
      Enumeration,  --  set of stringLiteral
      Option_Combination,
      Boolean_Or_Any,
      Unknown_Mapping);

   type Or_Mapping (Kind : Or_Mapping_Kind := Or_Mapping_Kind'First) is record
      case Kind is
         when Type_Or_Null
            | Option_Combination =>

            Tipe : LSP_Gen.Entities.AType;
         when Two_Literals
            | Two_Types
            | Type_Or_Something
            | String_Or_Tuple =>
            First, Second : LSP_Gen.Entities.AType;
         when Type_Or_Array
            | Array_Or_Null =>

            Array_Type    : LSP_Gen.Entities.AType;
         when Enumeration | Type_Union =>
            Items : LSP_Gen.Entities.AType_Vector;
         when Boolean_Or_Any
            | Unknown_Mapping =>

            null;
      end case;
   end record;

   function Get_Or_Mapping
     (Model : LSP_Gen.Meta_Models.Meta_Model'Class;
      Items : LSP_Gen.Entities.AType_Vector) return Or_Mapping;

   function Has_Kind
     (Model : LSP_Gen.Meta_Models.Meta_Model'Class;
      Item  : LSP_Gen.Entities.AType) return Boolean;
   --  A reference to the type which has `kind` stringLiteral property

end LSP_Gen.Mappings;

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

with VSS.Strings;
with VSS.String_Vectors;

with LSP_Gen.Entities;
with LSP_Gen.Entities.Equal;

package LSP_Gen.Dependencies is

   type Dependency_Info is record
      Short_Name : VSS.Strings.Virtual_String;
      Full_Name  : VSS.Strings.Virtual_String;
      Has_Option : Boolean := False;
      Is_Message : Boolean := False;  --  Used in request/response/notification
      Owner      : VSS.Strings.Virtual_String;  --  Enclosing "top" type
   end record;

   function Hash (Self : LSP_Gen.Entities.AType)
     return Ada.Containers.Hash_Type;

   package Dependency_Maps is new Ada.Containers.Hashed_Maps
     (LSP_Gen.Entities.AType,
      Dependency_Info,
      Hash,
      LSP_Gen.Entities.Equal);

   type Dependency_Map is new Dependency_Maps.Map with null record;
   --  A type to Dependency_Info map

   procedure Append
     (Self  : in out Dependency_Map;
      Items : Dependency_Map'Class);
   --  Join two maps and assign result to Self

   Work_Done_List : constant VSS.String_Vectors.Virtual_String_Vector :=
     [
      "ProgressBegin",
      "ProgressReport",
      "ProgressEnd"
     ];

end LSP_Gen.Dependencies;

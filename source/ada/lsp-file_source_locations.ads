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

with Ada.Containers.Hashed_Sets;

with Langkit_Support.Slocs;
with Libadalang.Analysis;
with Libadalang.Common;
with VSS.Strings.Conversions;
with VSS.Strings.Hash;

package LSP.File_Source_Locations is

   use type Ada.Containers.Hash_Type;

   type File_Source_Location is record
      File  : VSS.Strings.Virtual_String;
      Start : Langkit_Support.Slocs.Source_Location;
   end record;

   function To_File_Source_Location
     (File  : String;
      Start : Langkit_Support.Slocs.Source_Location)
      return File_Source_Location is
        (VSS.Strings.Conversions.To_Virtual_String (File), Start);

   function To_File_Source_Location
     (Unit  : Libadalang.Analysis.Analysis_Unit;
      Token : Libadalang.Common.Token_Reference) return File_Source_Location is
     (To_File_Source_Location
        (Unit.Get_Filename,
         Langkit_Support.Slocs.Start_Sloc
           (Libadalang.Common.Sloc_Range (Libadalang.Common.Data (Token)))));

   function To_File_Source_Location
     (Node : Libadalang.Analysis.Ada_Node'Class) return File_Source_Location is
     (To_File_Source_Location
        (Node.Unit.Get_Filename,
         Langkit_Support.Slocs.Start_Sloc (Node.Sloc_Range)));

   Prime : constant := 271;

   function Hash (Value : Langkit_Support.Slocs.Source_Location)
     return Ada.Containers.Hash_Type is
       (Prime * Ada.Containers.Hash_Type'Mod (Value.Line)
        + Ada.Containers.Hash_Type'Mod (Value.Column));

   function Hash
     (Value : File_Source_Location) return Ada.Containers.Hash_Type is
       (VSS.Strings.Hash (Value.File) + Hash (Value.Start));

   package File_Source_Location_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => File_Source_Location,
      Hash                => Hash,
      Equivalent_Elements => "=",
      "="                 => "=");

end LSP.File_Source_Locations;

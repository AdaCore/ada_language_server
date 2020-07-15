------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2020, AdaCore                     --
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
--
--  This package provides a set of files for Ada Language server.

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;

with GNATCOLL.VFS;

with Libadalang.Analysis;

with VSS.Strings;

with LSP.Ada_Completion_Sets;
with LSP.Types;

package LSP.Ada_File_Sets is

   package File_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type        => GNATCOLL.VFS.Virtual_File,
      "<"                 => GNATCOLL.VFS."<",
      "="                 => GNATCOLL.VFS."=");

   type Indexed_File_Set is tagged limited private;

   procedure Include
     (Self : in out Indexed_File_Set'Class;
      File : GNATCOLL.VFS.Virtual_File);

   function Length (Self : Indexed_File_Set'Class) return Natural;

   function Contains
     (Self : Indexed_File_Set'Class;
      File : GNATCOLL.VFS.Virtual_File) return Boolean;

   procedure Clear (Self : in out Indexed_File_Set'Class);

   function Iterate (Self : Indexed_File_Set'Class)
     return File_Sets.Set_Iterator_Interfaces.Reversible_Iterator'Class;

   procedure Index_File
     (Self : in out Indexed_File_Set'Class;
      File : GNATCOLL.VFS.Virtual_File;
      Unit : Libadalang.Analysis.Analysis_Unit);

   procedure Get_Any_Symbol_Completion
     (Self   : Indexed_File_Set'Class;
      Prefix : VSS.Strings.Virtual_String;
      Limit  : Ada.Containers.Count_Type;
      Result : in out LSP.Ada_Completion_Sets.Completion_Map);

private
   package File_Vectors is new Ada.Containers.Vectors
     (Positive, GNATCOLL.VFS.Virtual_File, GNATCOLL.VFS."=");

   type Symbol_Information is record
      Original : LSP.Types.LSP_String;  --  Original writting of the symbol
      Files    : File_Vectors.Vector;   --  Symbol occurrences
   end record;

   package Symbol_Maps is new Ada.Containers.Ordered_Maps
     (VSS.Strings.Virtual_String,
      Symbol_Information,
      VSS.Strings."<");

   type Indexed_File_Set is tagged limited record
      Files       : File_Sets.Set;
      All_Symbols : Symbol_Maps.Map;
      --  Index of all symbols defined in Files
   end record;

end LSP.Ada_File_Sets;

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
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;

with GNATCOLL.VFS;

with Libadalang.Analysis;
with Langkit_Support.Slocs;

with VSS.Strings;
with LSP.Search;

package LSP.Ada_File_Sets is

   package File_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type        => GNATCOLL.VFS.Virtual_File,
      "<"                 => GNATCOLL.VFS."<",
      "="                 => GNATCOLL.VFS."=");

   type Indexed_File_Set is tagged limited private;

   procedure Include
     (Self : in out Indexed_File_Set'Class;
      File : GNATCOLL.VFS.Virtual_File);

   procedure Exclude
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
   --  Append names defined in the Unit (identified by URI) to internal symbol
   --  index. After that names could be fetched using Get_Any_Symbol_Completion
   --  function.

   procedure Get_Any_Symbol
     (Self              : Indexed_File_Set'Class;
      Pattern           : LSP.Search.Search_Pattern'Class;
      Only_Public       : Boolean;
      Get_Defining_Name : not null access function
        (File : GNATCOLL.VFS.Virtual_File;
         Loc  : Langkit_Support.Slocs.Source_Location)
      return Libadalang.Analysis.Defining_Name;
      Callback          : not null access procedure
        (File          : GNATCOLL.VFS.Virtual_File;
         Defining_Name : Libadalang.Analysis.Defining_Name;
         Stop          : in out Boolean));
   --  Find symbols starting with given Prefix in all files of the set and
   --  call Callback for each. Get_Defining_Name callback is used for getting
   --  the Defining_Name at the given location Loc in a unit.
   --  Name could contain a stale reference if the File was updated since
   --  last indexing operation. If Only_Public is True it will skip any
   --  "private" symbols (like symbols in private part or body).

private
   type Name_Information is record
      File      : GNATCOLL.VFS.Virtual_File;
      Loc       : Langkit_Support.Slocs.Source_Location;
      Is_Public : Boolean;
   end record;

   package Name_Vectors is new Ada.Containers.Vectors
     (Positive, Name_Information);

   package Symbol_Maps is new Ada.Containers.Ordered_Maps
     (VSS.Strings.Virtual_String,
      Name_Vectors.Vector,
      VSS.Strings."<",
      Name_Vectors."=");
   --  A map from cannonical writting to vector of name information

   package Hashed_File_Sets is new Ada.Containers.Hashed_Sets
     (GNATCOLL.VFS.Virtual_File,
      GNATCOLL.VFS.Full_Name_Hash,
      GNATCOLL.VFS."=",
      GNATCOLL.VFS."=");

   type Indexed_File_Set is tagged limited record
      Files       : File_Sets.Set;
      All_Symbols : Symbol_Maps.Map;
      --  Index of all symbols defined in Files
      Indexed     : Hashed_File_Sets.Set;
      --  Set of document URIs presented in All_Symbols
   end record;

end LSP.Ada_File_Sets;

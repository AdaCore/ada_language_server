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
--
--  This package provides Ada unit provider for libadalang.

with GNATCOLL.Projects;

with Libadalang.Analysis;   use Libadalang.Analysis;
with Langkit_Support.Text;  use Langkit_Support.Text;
with Libadalang.Common;     use Libadalang.Common;

package LSP.Ada_Unit_Providers is

   type Unit_Provider
     (Project_Tree : not null GNATCOLL.Projects.Project_Tree_Access;
      Project      : not null GNATCOLL.Projects.Project_Type_Access) is
        new Libadalang.Analysis.Unit_Provider_Interface with private;
--  The unit provider is improved version of libadalang one.
--  It normalizes file path to avoid double directory separators.
--  This aids correct file name comparison.
--  This provider will resolve unit names in the project hierarchy denoted
--  by Project - this is a non-aggregate project roots inside Project_Tree.

private

   type Unit_Provider
     (Project_Tree : not null GNATCOLL.Projects.Project_Tree_Access;
      Project      : not null GNATCOLL.Projects.Project_Type_Access)
   is new Libadalang.Analysis.Unit_Provider_Interface with null record;

   overriding function Get_Unit_Filename
     (Self : Unit_Provider;
      Name : Text_Type;
      Kind : Analysis_Unit_Kind) return String;

   overriding function Get_Unit
     (Self    : Unit_Provider;
      Context : Analysis_Context'Class;
      Name    : Text_Type;
      Kind    : Analysis_Unit_Kind;
      Charset : String := "";
      Reparse : Boolean := False) return Analysis_Unit'Class;

   overriding procedure Release (Provider : in out Unit_Provider);

end LSP.Ada_Unit_Providers;

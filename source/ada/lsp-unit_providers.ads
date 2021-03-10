------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

--  Provides an Unit Provider which preprocesses the files as they are read
--  from disk. This is copied from Libadalang, see "NOTE" in the body.

with GNATCOLL.Projects;
with GNATCOLL.Traces; use GNATCOLL.Traces;

with Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;

package LSP.Unit_Providers is

   package LAL renames Libadalang.Analysis;
   package Prj renames GNATCOLL.Projects;

   Trace : constant GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LIBADALANG.PROJECT_PROVIDER", GNATCOLL.Traces.From_Config);

   type Provider_And_Projects is record
      Provider : LAL.Unit_Provider_Reference;
      Projects : Prj.Project_Array_Access;
   end record;
   --  Associates one project unit provider with all the projects on which it
   --  has visibility.

   function Create_Project_Unit_Provider
     (Tree             : Prj.Project_Tree_Access;
      Project          : Prj.Project_Type := Prj.No_Project;
      Env              : Prj.Project_Environment_Access;
      Is_Project_Owner : Boolean := True)
      return LAL.Unit_Provider_Reference;
   --  Likewise, but create only one unit provider.
   --
   --  If a non-null Project is given, use it to provide units. Raise an
   --  Invalid_Project exception if an aggregate projects that aggregates more
   --  than one project is in its closure.
   --
   --  If Project is not provided, run Create_Project_Unit_Providers: if it
   --  returns only one provider, return it, otherwise raise an error.
   --
   --  If ``Is_Project_Owner`` is true, the result owns ``Tree``, thus the
   --  caller must not deallocate it itself.  Otherwise, the project pointed to
   --  by Project must outlive the returned unit file provider.

   function Convert
     (Kind : Analysis_Unit_Kind) return GNATCOLL.Projects.Unit_Parts
   is
     (case Kind is
      when Unit_Specification => GNATCOLL.Projects.Unit_Spec,
      when Unit_Body          => GNATCOLL.Projects.Unit_Body);
   --  Convert our kind for analysis unit into the corresponding
   --  ``GNATCOLL.Projects`` value.

end LSP.Unit_Providers;

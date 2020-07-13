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
--  This package provides iterators over Libadalang Base_Id nodes.

with GNATCOLL.Traces;

with Libadalang.Analysis;
with Libadalang.Common;

package LSP.Ada_Id_Iterators is

   procedure Find_All_References
     (Definition : Libadalang.Analysis.Defining_Name;
      Units      : Libadalang.Analysis.Analysis_Unit_Array;
      Callback   : not null access procedure
        (Base_Id : Libadalang.Analysis.Base_Id;
         Kind    : Libadalang.Common.Ref_Result_Kind;
         Cancel  : in out Boolean));
   --  Iterate over Definition.P_Find_All_References result

   procedure Find_All_Param_References_In_Hierarchy
     (Param      : Libadalang.Analysis.Param_Spec;
      Hierarchy  : Libadalang.Analysis.Basic_Decl_Array;
      Units      : Libadalang.Analysis.Analysis_Unit_Array;
      Callback   : not null access procedure
        (Base_Id : Libadalang.Analysis.Base_Id;
         Kind    : Libadalang.Common.Ref_Result_Kind;
         Cancel  : in out Boolean));
   --  Recursive function that returns all the references of the given
   --  parameter's name in the hierarchy.

   procedure Find_All_Subp_References_In_Hierarchy
     (Hierarchy  : Libadalang.Analysis.Basic_Decl_Array;
      Trace      : GNATCOLL.Traces.Trace_Handle;
      Callback   : not null access procedure
        (Base_Id : Libadalang.Analysis.Base_Id;
         Kind    : Libadalang.Common.Ref_Result_Kind;
         Cancel  : in out Boolean));
   --  Return all the references of Decl in the given hierarchy

end LSP.Ada_Id_Iterators;

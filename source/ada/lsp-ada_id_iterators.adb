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

with Laltools.Common;

package body LSP.Ada_Id_Iterators is

   --------------------------------------------
   -- Find_All_Param_References_In_Hierarchy --
   --------------------------------------------

   procedure Find_All_Param_References_In_Hierarchy
     (Param      : Libadalang.Analysis.Param_Spec;
      Hierarchy  : Libadalang.Analysis.Basic_Decl_Array;
      Units      : Libadalang.Analysis.Analysis_Unit_Array;
      Callback   : not null access procedure
        (Base_Id : Libadalang.Analysis.Base_Id;
         Kind    : Libadalang.Common.Ref_Result_Kind;
         Cancel  : in out Boolean))
   is
      Cancel         : Boolean := False;
      Param_Name_Id : constant Libadalang.Analysis.Defining_Name :=
        Param.F_Ids.List_Child (1);
      Subp_Decl     : Libadalang.Analysis.Basic_Subp_Decl;
   begin
      Outer_Loop :
      for X of Hierarchy loop
         Subp_Decl := X.As_Basic_Subp_Decl;

         --  Iterate on all the parameters of the subprogram and find the
         --  parameter with the same name
         for Param of Subp_Decl.P_Subp_Decl_Spec.P_Params loop
            if Param_Name_Id.Text = Param.F_Ids.List_Child (1).Text then

               Callback
                 (Base_Id => Param.F_Ids.List_Child (1).F_Name.As_Base_Id,
                  Kind    => Libadalang.Common.Precise,
                  Cancel  => Cancel);

               exit Outer_Loop when Cancel;

               Find_All_References
                 (Definition => Param.F_Ids.List_Child (1),
                  Units      => Units,
                  Callback   => Callback);
            end if;
         end loop;
      end loop Outer_Loop;
   end Find_All_Param_References_In_Hierarchy;

   -------------------------------------------
   -- Find_All_Subp_References_In_Hierarchy --
   -------------------------------------------

   procedure Find_All_Subp_References_In_Hierarchy
     (Hierarchy  : Libadalang.Analysis.Basic_Decl_Array;
      Trace      : GNATCOLL.Traces.Trace_Handle;
      Callback   : not null access procedure
        (Base_Id : Libadalang.Analysis.Base_Id;
         Kind    : Libadalang.Common.Ref_Result_Kind;
         Cancel  : in out Boolean))
   is
      Cancel         : Boolean := False;
      Subp_Body_Name : Libadalang.Analysis.Defining_Name;
      Subp_Body_Node : Libadalang.Analysis.Subp_Body;
   begin
      for Subp_Decl of Hierarchy loop
         Callback
           (Base_Id => Subp_Decl.P_Defining_Name.F_Name.As_Base_Id,
            Kind    => Libadalang.Common.Precise,
            Cancel  => Cancel);

         exit when Cancel;

         --  Try to get the corresponding body
         Subp_Body_Name := Laltools.Common.Find_Next_Part
           (Subp_Decl.P_Defining_Name, Trace);

         --  If there is a body, append the body's begin and end labels
         --  to the result.
         if not Subp_Body_Name.Is_Null then
            Callback
              (Base_Id => Subp_Body_Name.F_Name.As_Base_Id,
               Kind    => Libadalang.Common.Precise,
               Cancel  => Cancel);

            exit when Cancel;

            Subp_Body_Node := Subp_Body_Name.Parent.Parent.As_Subp_Body;

            if not Subp_Body_Node.Is_Null then
               Callback
                 (Base_Id => Subp_Body_Node.F_End_Name.F_Name.As_Base_Id,
                  Kind    => Libadalang.Common.Precise,
                  Cancel  => Cancel);

               exit when Cancel;
            end if;
         end if;
      end loop;
   end Find_All_Subp_References_In_Hierarchy;

   -------------------------
   -- Find_All_References --
   -------------------------

   procedure Find_All_References
     (Definition : Libadalang.Analysis.Defining_Name;
      Units      : Libadalang.Analysis.Analysis_Unit_Array;
      Callback   : not null access procedure
        (Base_Id : Libadalang.Analysis.Base_Id;
         Kind    : Libadalang.Common.Ref_Result_Kind;
         Cancel  : in out Boolean))
   is
      Cancel : Boolean := False;
   begin
      for Item of Definition.P_Find_All_References (Units) loop
         Callback
           (Base_Id => Libadalang.Analysis.Ref (Item).As_Base_Id,
            Kind    => Libadalang.Analysis.Kind (Item),
            Cancel  => Cancel);

         exit when Cancel;
      end loop;
   end Find_All_References;

end LSP.Ada_Id_Iterators;

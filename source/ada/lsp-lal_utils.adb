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

with Libadalang.Common; use Libadalang.Common;

package body LSP.Lal_Utils is

   function Containing_Entity (Ref : Ada_Node) return Defining_Name;
   --  Return the declaration of the subprogram or task that contains Ref.
   --  Return No_Defining_Name if this fails.

   ----------------------
   -- Get_Node_As_Name --
   ----------------------

   function Get_Node_As_Name (Node : Ada_Node) return Name is
   begin

      if Node = No_Ada_Node or else Node.Kind not in Ada_Name then
         return No_Name;
      end if;

      return Node.As_Name;

   end Get_Node_As_Name;

   --------------------------
   -- Get_Name_As_Defining --
   --------------------------

   function Get_Name_As_Defining (Name_Node : Name) return Defining_Name is
   begin

      if Name_Node = No_Name or else not Name_Node.P_Is_Defining then
         return No_Defining_Name;
      end if;

      return Name_Node.P_Enclosing_Defining_Name;

   end Get_Name_As_Defining;

   ------------------
   -- Resolve_Name --
   ------------------

   function Resolve_Name
     (Name_Node : Name;
      Imprecise : out Boolean) return Defining_Name
   is
      Result : Defining_Name;
   begin
      Imprecise := False;

      --  Try to get the cross reference without enabling the imprecise
      --  fallback first.
      --  Property_Error exceptions can be raised so make sure to handle
      --  them correctly.
      begin
         Result := Name_Node.P_Xref (Imprecise_Fallback => False);
      exception
         when Property_Error =>
            Result := No_Defining_Name;
      end;

      --  Get the cross reference with the imprecise fallback if the previous
      --  query has failed.
      --  Set the Imprecise fallback to True in that case, to warn the caller
      --  that we might get an imprecise result.
      if Result = No_Defining_Name then
         Imprecise := True;
         Result := Name_Node.P_Xref (Imprecise_Fallback => True);
      end if;

      if Name_Node.P_Is_Defining and then Result = No_Defining_Name then
         --  When Name_Node is part of defining_name and it isn't a completion
         --  of another declaration, then P_Xref returns No_Defining_Name.
         --  In this case we return current defining_name.
         return Name_Node.P_Enclosing_Defining_Name;
      else
         return Result;
      end if;

   exception
      when Property_Error =>
         return No_Defining_Name;
   end Resolve_Name;

   -----------------------
   -- Containing_Entity --
   -----------------------

   function Containing_Entity (Ref : Ada_Node) return Defining_Name is
      Parents : constant Ada_Node_Array := Ref.Parents;
   begin
      for Parent of Parents loop
         if Parent.Kind in Ada_Subp_Decl
                         | Ada_Subp_Body
                         | Ada_Task_Def
                         | Ada_Task_Body
                         | Ada_Package_Body
                         | Ada_Package_Decl
         then
            return Parent.As_Basic_Decl.P_Canonical_Part.P_Defining_Name;
         end if;
      end loop;

      return No_Defining_Name;
   end Containing_Entity;

   ------------------
   -- Is_Called_By --
   ------------------

   function Is_Called_By
     (Context    : LSP.Ada_Contexts.Context;
      Definition : Defining_Name)
      return References_By_Subprogram.Map
   is
      use References_By_Subprogram;
      use References_List;
      Result     : Map;
      Containing : Defining_Name;

      --  Obtain all the references
      Refs : constant Base_Id_Array :=
        Context.Find_All_References (Definition);
   begin

      --  Go through all references to Name, organising them by containing
      --  subprogram.

      for Ref of Refs loop
         --  Only consider references that are calls.
         --  ??? To be discussed: how do we want to handle an access to
         --  a subprogram?
         if Ref.P_Is_Call then
            --  We have a reference, and this a call: find the containing
            --  subprogram or task
            Containing := Containing_Entity (Ref.As_Ada_Node);

            if Containing /= No_Defining_Name then
               if Result.Contains (Containing) then
                  declare
                     L : List := Result.Element (Containing);
                  begin
                     L.Append (Ref);
                     Result.Replace (Containing, L);
                  end;
               else
                  declare
                     L : List;
                  begin
                     L.Append (Ref);
                     Result.Insert (Containing, L);
                  end;
               end if;
            end if;
         end if;
      end loop;

      --  TODO: sort?
      return Result;
   end Is_Called_By;

end LSP.Lal_Utils;

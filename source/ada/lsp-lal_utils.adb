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

with URIs;

with Langkit_Support.Slocs;
with Libadalang.Common; use Libadalang.Common;
with LSP.Types;         use LSP.Types;

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

   function Resolve_Name (Name_Node : Name) return Defining_Name is
      --  In every case, if the unit has syntax errors, activate
      --  Imprecise_Fallback.
      --  TODO: Should we activate it in any case ?
      Result : constant Defining_Name :=
        Name_Node.P_Xref
          (Imprecise_Fallback => Name_Node.Unit.Has_Diagnostics);
   begin
      if Name_Node.P_Is_Defining and Result = No_Defining_Name then
         --  When Name_Node is part of defining_name and it isn't a completion
         --  of another declaration, then P_Xref returns No_Defining_Name.
         --  In this case we return current defining_name.
         return Name_Node.P_Enclosing_Defining_Name;
      else
         return Result;
      end if;
   end Resolve_Name;

   -------------------------
   -- Find_All_References --
   -------------------------

   function Find_All_References
     (Definition         : Defining_Name;
      Sources            : GNATCOLL.VFS.File_Array_Access;
      Charset            : String;
      Include_Definition : Boolean := False)
         return Ada_Node_Array
   is
      Context : constant Analysis_Context := Definition.Unit.Context;
      Source_Units : Analysis_Unit_Array (Sources'Range);
   begin
      for N in Sources'Range loop
         Source_Units (N) := Context.Get_From_File
           (Sources (N).Display_Full_Name,
            Charset => Charset);
      end loop;

      declare
         References : constant Ada_Node_Array :=
           Definition.P_Find_All_References (Source_Units);
      begin
         if Include_Definition then
            return References & (1 => Definition.As_Ada_Node);
         else
            return References;
         end if;
      end;
   end Find_All_References;

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
     (Name_Node : Name;
      Sources   : GNATCOLL.VFS.File_Array_Access;
      Charset   : String)
      return References_By_Subprogram.Map
   is
      use References_By_Subprogram;
      use References_List;
      Result     : Map;
      Definition : Defining_Name;
   begin
      if Name_Node = No_Name then
         return Result;
      end if;

      --  Attempt to resolve the name, return no results if we can't or if the
      --  name does not resolve to a subprogram.
      Definition := Resolve_Name (Name_Node);

      if Definition = No_Defining_Name
        or else Definition.P_Basic_Decl.Kind not in Ada_Subp_Decl
      then
         return Result;
      end if;

      --  Obtain all the references

      declare
         Refs : constant Ada_Node_Array := Find_All_References
           (Definition, Sources, Charset, Include_Definition => False);
         Containing : Defining_Name;
      begin
         --  Go through all references to Name, organising them by containing
         --  subprogram.

         for Ref of Refs loop
            --  Only consider references that are calls.
            --  ??? To be discussed: how do we want to handle an access to
            --  a subprogram?
            if Ref.As_Name.P_Is_Call then
               --  We have a reference, and this a call: find the containing
               --  subprogram or task
               Containing := Containing_Entity (Ref);

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
      end;
      --  TODO: sort?
      return Result;
   end Is_Called_By;

   -----------------
   -- Get_Node_At --
   -----------------

   function Get_Node_At
     (Context  : LSP.Ada_Contexts.Context;
      Uri      : LSP.Messages.DocumentUri;
      Position : LSP.Messages.Position) return Ada_Node
   is
      use Langkit_Support.Slocs;
      Unit : Libadalang.Analysis.Analysis_Unit;
   begin
      if Context.Has_Document (Uri) then
         return Context.Get_Document (Uri).Get_Node_At (Position);
      else
         Unit := Context.Get_LAL_Context.Get_From_File
           (URIs.Conversions.To_File (To_UTF_8_String (Uri)),
            Charset => Context.Get_Charset);

         if Unit.Root = No_Ada_Node then
            return No_Ada_Node;
         end if;

         return Unit.Root.Lookup
           ((Line   => Langkit_Support.Slocs.Line_Number (Position.line) + 1,
             Column => Column_Number (Position.character) + 1));
      end if;
   end Get_Node_At;

end LSP.Lal_Utils;

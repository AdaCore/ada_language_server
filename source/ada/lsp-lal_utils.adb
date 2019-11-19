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

with Ada.Wide_Wide_Characters.Handling;
with Ada.Strings.Wide_Wide_Fixed;

with LSP.Common;        use LSP.Common;
with LSP.Types;         use LSP.Types;

with Libadalang.Common; use Libadalang.Common;

with Langkit_Support;
with Langkit_Support.Slocs;

package body LSP.Lal_Utils is

   function Containing_Entity (Ref : Ada_Node) return Defining_Name;
   --  Return the declaration of the subprogram or task that contains Ref.
   --  Return No_Defining_Name if this fails.

   -------------
   -- Contain --
   -------------

   function Contain
     (Token   : Token_Reference;
      Pattern : Wide_Wide_String;
      Span    : out LSP.Messages.Span)
      return Boolean
   is
      use Langkit_Support.Text;
      use Langkit_Support.Slocs;

      T   : constant Text_Type := Ada.Wide_Wide_Characters.Handling.To_Lower
        (Text (Token));
      Idx : constant Integer := Ada.Strings.Wide_Wide_Fixed.Index (T, Pattern);
   begin
      if Idx < T'First then
         return False;
      end if;

      declare
         Sloc : constant Source_Location_Range := Sloc_Range (Data (Token));

         Line  : constant LSP.Types.Line_Number :=
           LSP.Types.Line_Number (Sloc.Start_Line) - 1;
         Start : constant UTF_16_Index := UTF_16_Index
           (Sloc.Start_Column + Column_Number (Idx - T'First)) - 1;
      begin
         Span := (first => (Line, Start),
                  last  => (Line, Start + T'Length - 1));
         return True;
      end;
   end Contain;

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

   -------------------
   -- Get_Last_Name --
   -------------------

   function Get_Last_Name
     (Name_Node : Name)
      return Langkit_Support.Text.Unbounded_Text_Type
   is
      Names : constant Unbounded_Text_Type_Array :=
        P_As_Symbol_Array (Name_Node);

   begin
      return Names (Names'Last);
   end Get_Last_Name;

   ------------------
   -- Resolve_Name --
   ------------------

   function Resolve_Name
     (Name_Node : Name;
      Trace     : GNATCOLL.Traces.Trace_Handle;
      Imprecise : out Boolean) return Defining_Name
   is
      Result : Defining_Name;
   begin
      Imprecise := False;

      --  First try to resolve precisely
      begin
         if Name_Node.P_Is_Defining then
            Result := Name_Node.P_Enclosing_Defining_Name.P_Canonical_Part;
         else
            Result := Name_Node.P_Referenced_Defining_Name
              (Imprecise_Fallback => False).P_Canonical_Part;
         end if;
      exception
         when E : Property_Error =>
            Log (Trace, E);
            Result := No_Defining_Name;
      end;

      --  The result was found precisely: return it
      if Result /= No_Defining_Name then
         return Result;
      end if;

      --  If we reach this, it means we've failed to get a precise result.
      --  Try again with the imprecise fallback.
      if not Name_Node.P_Is_Defining then
         Result := Name_Node.P_Referenced_Defining_Name
           (Imprecise_Fallback => True).P_Canonical_Part;

         Imprecise := Result /= No_Defining_Name;
      end if;

      return Result;

   exception
      when E : Property_Error =>
         Log (Trace, E);
         return No_Defining_Name;
   end Resolve_Name;

   ---------------------
   -- Find_First_Part --
   ---------------------

   function Find_Canonical_Part
     (Definition : Defining_Name;
      Trace      : GNATCOLL.Traces.Trace_Handle) return Defining_Name
   is
      Canonical : Defining_Name;
   begin
      Canonical := Definition.P_Canonical_Part;

      if Canonical = Definition then
         return No_Defining_Name;
      else
         return Canonical;
      end if;

   exception
      when E :  Libadalang.Common.Property_Error =>
         Log (Trace, E);
         return No_Defining_Name;
   end Find_Canonical_Part;

   --------------------
   -- Find_Next_Part --
   --------------------

   function Find_Next_Part
     (Definition : Defining_Name;
      Trace      : GNATCOLL.Traces.Trace_Handle) return Defining_Name
   is
      Next : Defining_Name;
   begin
      Next := Definition.P_Next_Part;

      if Next = Definition then
         return No_Defining_Name;
      else
         return Next;
      end if;
   exception
      when E :  Libadalang.Common.Property_Error =>
         Log (Trace, E);
         return No_Defining_Name;
   end Find_Next_Part;

   ---------------------------------
   -- Find_Defining_Name_Manually --
   ---------------------------------

   function Find_Other_Part_Fallback
     (Definition : Defining_Name;
      Trace      : GNATCOLL.Traces.Trace_Handle) return Defining_Name
   is
      Qualified_Name : constant Langkit_Support.Text.Text_Type :=
        Definition.P_Basic_Decl.P_Fully_Qualified_Name;
      --  The name that we'll try to match

      Found : Defining_Name := No_Defining_Name;
      --  The result that has been found

      function Matches
        (Node : Ada_Node'Class) return Libadalang.Common.Visit_Status;
      --  Return True if the name of Node matches Qualified_Name

      -------------
      -- Matches --
      -------------

      function Matches
        (Node : Ada_Node'Class) return Libadalang.Common.Visit_Status
      is
         use type Langkit_Support.Slocs.Line_Number;
      begin
         if Node.Kind not in Libadalang.Common.Ada_Basic_Decl then
            return Libadalang.Common.Into;
         end if;

         --  Note: in this function, we are simply looking at the first
         --  result that matches.
         --  TODO: improve this by find all entities that match, and
         --  finding the best through a distance/scoring heuristics.

         if Node.As_Basic_Decl.P_Fully_Qualified_Name = Qualified_Name
           and then Node.Sloc_Range.Start_Line
             /= Definition.Sloc_Range.Start_Line
         then
            Found := Node.As_Basic_Decl.P_Defining_Name;
            return Libadalang.Common.Stop;
         end if;

         return Libadalang.Common.Into;
      end Matches;

      Parent_Spec : Defining_Name;
      Parent_Body : Defining_Name;
   begin
      --  The heuristics implemented is the following: we're looking at the
      --  spec and body of the enclosing entity, to find an entity that
      --  could correspond to Definition.
      --
      --  For instance, if Definition points to a function Foo that is defined
      --  in a package P, we're going to look in the spec and body of P for
      --  any items named Foo, excluding Definition itself.

      --  First obtain the spec.
      --  Note: we could refine the number of calls to P_Semantic_Parent.
      --  Two calls to P_Semantic_Parents are needed in the case of a
      --  subprogram: the first jumps to the SubpDecl, the second to the
      --  PackageDecl.
      Parent_Spec := Definition.P_Semantic_Parent.
        As_Basic_Decl.P_Semantic_Parent.As_Basic_Decl.
          P_Canonical_Part.P_Defining_Name;

      --  Traverse the spec. The visiting function assigns the matching
      --  result, if any, to Found.
      Parent_Spec.Parent.Traverse (Matches'Unrestricted_Access);

      --  If we didn't find a result when traversing the spec, traverse the
      --  body of the containing entity.
      if Found = No_Defining_Name then
         Parent_Body := Find_Next_Part (Parent_Spec, Trace);
         if Parent_Body = No_Defining_Name then
            Parent_Body := Find_Next_Part (Parent_Spec, Trace);
         end if;
         if Parent_Body /= No_Defining_Name then
            Parent_Body.Parent.Traverse (Matches'Unrestricted_Access);
         end if;
      end if;

      return Found;

   exception
      when E : Property_Error =>
         Log (Trace, E);
         return No_Defining_Name;
   end Find_Other_Part_Fallback;

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

   --------------------
   -- Find_All_Calls --
   --------------------

   function Find_All_Calls
     (Context           : LSP.Ada_Contexts.Context;
      Definition        : Defining_Name;
      Imprecise_Results : out Boolean)
      return References_By_Subprogram.Map
   is
      use References_By_Subprogram;
      use References_List;
      Result     : Map;
      Containing : Defining_Name;

      --  Obtain all the references
      Refs      : constant Base_Id_Array := Context.Find_All_Calls
        (Definition, Imprecise_Results);
   begin
      --  Go through all references to Name, organising them by containing
      --  subprogram.

      for Ref of Refs loop
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
      end loop;

      --  TODO: sort?
      return Result;
   end Find_All_Calls;

end LSP.Lal_Utils;

------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

with GNATCOLL.VFS;    use GNATCOLL.VFS;

with LSP.Ada_File_Sets;

package body LSP.Ada_Context_Sets is

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (LSP.Ada_Contexts.Context, Context_Access);

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Self : in out Context_Set'Class) is
   begin
      while not Self.Contexts.Is_Empty loop
         declare
            C : Context_Access := Self.Contexts.First_Element;
         begin
            C.Free;
            Unchecked_Free (C);
         end;
         Self.Contexts.Delete_First;
      end loop;

      Self.Map.Clear;
      Self.Total := 0;
   end Cleanup;

   ------------------
   -- Each_Context --
   ------------------

   function Each_Context
     (Self      : Context_Set;
      Predicate : Context_Predicate := All_Contexts'Access)
      return Context_Lists.List
   is
      Result : Context_Lists.List;
   begin
      for C of Self.Contexts loop
         if Predicate (C.all) then
            Result.Append (C);
         end if;
      end loop;
      return Result;
   end Each_Context;

   ----------------------
   -- Get_Best_Context --
   ----------------------

   function Get_Best_Context
     (Self : Context_Set'Class;
      URI  : LSP.Messages.DocumentUri) return Context_Access is
   begin
      for Context of Self.Contexts loop
         declare
            File : constant Virtual_File :=
              Create_From_UTF8 (Context.URI_To_File (URI));
         begin
            if Context.Is_Part_Of_Project (File) then
               return Context;
            end if;
         end;
      end loop;

      return Self.Contexts.First_Element;
   end Get_Best_Context;

   ---------
   -- Get --
   ---------

   function Get
     (Self : Context_Set;
      Id   : VSS.Strings.Virtual_String) return Context_Access is
   begin
      return Self.Map (Id);
   end Get;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Context_Set'Class) return Boolean is
   begin
      return Self.Contexts.Is_Empty;
   end Is_Empty;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Self : in out Context_Set'Class;
      Item : Context_Access) is
   begin
      Self.Contexts.Prepend (Item);
      Self.Map.Insert (Item.Id, Item);
      Self.Total := Self.Total + Item.File_Count;
   end Prepend;

   -------------------------
   -- Reload_All_Contexts --
   -------------------------

   procedure Reload_All_Contexts (Self : in out Context_Set'Class) is
   begin
      Self.Total := 0;

      for C of Self.Contexts loop
         C.Reload;
         Self.Total := Self.Total + C.File_Count;
      end loop;
   end Reload_All_Contexts;

   ------------------------
   -- Total_Source_Files --
   ------------------------

   function Total_Source_Files (Self : Context_Set'Class) return Natural is
   begin
      return Self.Total;
   end Total_Source_Files;

   ------------------
   -- All_Contexts --
   ------------------

   function All_Contexts (Context : LSP.Ada_Contexts.Context) return Boolean is
      pragma Unreferenced (Context);
   begin
      return True;
   end All_Contexts;

   ----------------------------
   -- All_Source_Directories --
   ----------------------------

   function All_Source_Directories
     (Self                     : Context_Set'Class;
      Include_Externally_Built : Boolean := False)
      return GNATCOLL.VFS.File_Array
   is
      Consolidated_Set : LSP.Ada_File_Sets.File_Sets.Set;
   begin
      for C of Self.Contexts loop
         Consolidated_Set.Union
           (C.List_Source_Directories
              (Include_Externally_Built => Include_Externally_Built));
      end loop;

      declare
         Result : GNATCOLL.VFS.File_Array
           (1 .. Integer (Consolidated_Set.Length));
         J      : Natural := 1;
      begin
         for Dir of Consolidated_Set loop
            Result (J) := Dir;
            J := J + 1;
         end loop;
         return Result;
      end;
   end All_Source_Directories;

end LSP.Ada_Context_Sets;

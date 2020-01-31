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

with Ada.Unchecked_Deallocation;

with GNATCOLL.VFS;    use GNATCOLL.VFS;

with URIs;

package body LSP.Ada_Context_Sets is

   function To_File (URI : LSP.Messages.DocumentUri) return Virtual_File is
     (Create (+(URIs.Conversions.To_File (LSP.Types.To_UTF_8_String (URI)))));
   --  Utility conversion function

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

   ----------------------
   -- Contexts_For_URI --
   ----------------------

   function Contexts_For_URI
     (Self : Context_Set'Class;
      URI  : LSP.Messages.DocumentUri) return Context_Lists.List
   is
      File   : constant Virtual_File := To_File (URI);
      Result : Context_Lists.List;
   begin
      --  If the file does not exist on disk, assume this is a file
      --  being created and, as a special convenience in this case,
      --  assume it could belong to any project.
      if not File.Is_Regular_File then
         return Self.Contexts;
      end if;

      for Context of Self.Contexts loop
         if Context.Is_Part_Of_Project (File) then
            Result.Append (Context);
         end if;
      end loop;

      return Result;
   end Contexts_For_URI;

   ------------------
   -- Each_Context --
   ------------------

   function Each_Context (Self : Context_Set)
     return Context_Lists.List_Iterator_Interfaces.Forward_Iterator'Class is
   begin
      return Self.Contexts.Iterate;
   end Each_Context;

   ----------------------
   -- Get_Best_Context --
   ----------------------

   function Get_Best_Context
     (Self : Context_Set'Class;
      URI  : LSP.Messages.DocumentUri) return Context_Access
   is
      File : constant Virtual_File := To_File (URI);
   begin
      for Context of Self.Contexts loop
         if Context.Is_Part_Of_Project (File) then
            return Context;
         end if;
      end loop;

      return Self.Contexts.Last_Element;
   end Get_Best_Context;

   ---------
   -- Get --
   ---------

   function Get
     (Self : Context_Set;
      Id   : LSP.Types.LSP_String) return Context_Access is
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
      Self.Total := Self.Total + Natural (Item.List_Files.Length);
   end Prepend;

   -------------------------
   -- Reload_All_Contexts --
   -------------------------

   procedure Reload_All_Contexts (Self : in out Context_Set'Class) is
   begin
      Self.Total := 0;

      for C of Self.Contexts loop
         C.Reload;
         Self.Total := Self.Total + Natural (C.List_Files.Length);
      end loop;
   end Reload_All_Contexts;

   ------------------------
   -- Total_Source_Files --
   ------------------------

   function Total_Source_Files (Self : Context_Set'Class) return Natural is
   begin
      return Self.Total;
   end Total_Source_Files;

end LSP.Ada_Context_Sets;

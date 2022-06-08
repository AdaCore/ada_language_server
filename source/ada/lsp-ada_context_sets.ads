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
--
--  This package provides a set of contexts for Ada Language server.

with GNATCOLL.VFS;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;

with VSS.Strings;

with LSP.Ada_Contexts;
with LSP.Messages;
with LSP.Types;

package LSP.Ada_Context_Sets is

   type Context_Set is tagged limited private;

   type Context_Access is access LSP.Ada_Contexts.Context;

   package Context_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Context_Access);

   function Is_Empty (Self : Context_Set'Class) return Boolean;
   --  Check if the set has no contexts

   procedure Prepend
     (Self : in out Context_Set'Class;
      Item : Context_Access);
   --  Append an item to the set

   procedure Reload_All_Contexts (Self : in out Context_Set'Class);
   --  Reload ech context in the set

   function Get_Best_Context
     (Self : Context_Set'Class;
      URI  : LSP.Messages.DocumentUri) return Context_Access;
   --  Return the first context in Contexts which contains a project
   --  which knows about file. Return the first context if no such
   --  context was found.

   function Total_Source_Files (Self : Context_Set'Class) return Natural;
   --  Number of files in all contexts

   function All_Source_Directories
     (Self                     : Context_Set'Class;
      Include_Externally_Built : Boolean := False)
      return GNATCOLL.VFS.File_Array;
   --  Return the list of all source directories for writable projects in the
   --  context, including externally built projects' source directories when
   --  Include_Externally_Built is set to True.
   --  Each dirctory is present only once in the resulting array.

   procedure Cleanup (Self : in out Context_Set'Class);
   --  Free memory referenced by Self

   function Get
     (Self : Context_Set;
      Id   : VSS.Strings.Virtual_String) return Context_Access;
   --  Return context by its Id

   type Context_Predicate is access function
     (Context : LSP.Ada_Contexts.Context) return Boolean;

   function All_Contexts (Context : LSP.Ada_Contexts.Context) return Boolean;
   --  A Context_Predicate which matches all contexts

   function Each_Context
     (Self      : Context_Set;
      Predicate : Context_Predicate := All_Contexts'Access)
     return Context_Lists.List;
   --  Return a list of the contexts in the set that match Predicate

private

   package Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => VSS.Strings.Virtual_String,
      Element_Type    => Context_Access,
      Hash            => LSP.Types.Hash,
      Equivalent_Keys => VSS.Strings."=",
      "="             => "=");

   type Context_Set is tagged limited record
      Contexts : Context_Lists.List;
      Map      : Maps.Map;  --  A map from Context.Id to Context access
      Total    : Natural := 0;
   end record;

end LSP.Ada_Context_Sets;

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
--  This package provides a set of contexts for Ada Language server.

with Ada.Containers.Doubly_Linked_Lists;

with LSP.Messages;
with LSP.Ada_Contexts;

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
   --  which knows about file. Fallback on the "no project" context.

   function Contexts_For_URI
     (Self : Context_Set'Class;
      URI  : LSP.Messages.DocumentUri) return Context_Lists.List;
   --  Return a list of contexts that are suitable for the given URI:
   --  a list of all contexts where the file is known to be part of the
   --  project tree. If the file is not known to any project, return
   --  an empty list.
   --  The result should not be freed.

   function Total_Source_Files (Self : Context_Set'Class) return Natural;
   --  Number of files in all contexts

   procedure Cleanup (Self : in out Context_Set'Class);
   --  Free memory referenced by Self

   function Each_Context (Self : Context_Set)
     return Context_Lists.List_Iterator_Interfaces.Forward_Iterator'Class;
   --  Iterate over contexts of the set

private
   type Context_Set is tagged limited record
      Contexts : Context_Lists.List;
      Total    : Natural := 0;
   end record;

end LSP.Ada_Context_Sets;

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

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;

with LSP.Ada_Contexts;
with LSP.Messages;
with LSP.Types;

package LSP.Ada_Context_Sets is

   type Context_Set is tagged limited private;

   package Context_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (LSP.Ada_Contexts.Context,
      "=" => LSP.Ada_Contexts."=");

   function Is_Empty (Self : Context_Set'Class) return Boolean;
   --  Check if the set has no contexts

   procedure Prepend
     (Self : in out Context_Set'Class;
      Item : LSP.Ada_Contexts.Context);
   --  Append an item to the set

   procedure Reload_All_Contexts (Self : in out Context_Set'Class);
   --  Reload ech context in the set

   function Get_Best_Context
     (Self : Context_Set'Class;
      URI  : LSP.Messages.DocumentUri) return LSP.Ada_Contexts.Context;
   --  Return the first context in Contexts which contains a project
   --  which knows about file. Fallback on the "no project" context.

   function Total_Source_Files (Self : Context_Set'Class) return Natural;
   --  Number of files in all contexts

   procedure Cleanup (Self : in out Context_Set'Class);
   --  Free memory referenced by Self

   function Get
     (Self : Context_Set;
      Id   : LSP.Types.LSP_String) return LSP.Ada_Contexts.Context;
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

   package Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => LSP.Types.LSP_String,
      Element_Type    => LSP.Ada_Contexts.Context,
      Hash            => LSP.Types.Hash,
      Equivalent_Keys => LSP.Types."=",
      "="             => LSP.Ada_Contexts."=");

   type Context_Set is tagged limited record
      Contexts : Context_Lists.List;
      Map      : Maps.Map;  --  A map from Context.Id to Context access
      Total    : Natural := 0;
   end record;

end LSP.Ada_Context_Sets;

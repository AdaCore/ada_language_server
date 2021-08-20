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
--  This package provides types to support completions in Ada Language server.

with Ada.Containers.Hashed_Maps;

with Langkit_Support.Slocs;
with Libadalang.Analysis;
with Libadalang.Common;

limited with LSP.Ada_Contexts;
limited with LSP.Ada_Completions.Filters;

with LSP.Messages;

package LSP.Ada_Completions is

   function Hash (Name : Libadalang.Analysis.Defining_Name)
     return Ada.Containers.Hash_Type is
       (Name.As_Ada_Node.Hash);

   function Is_Equal (Left, Right : Libadalang.Analysis.Defining_Name)
     return Boolean;
   --  This custom Is_Equal function is here as a temporary workaround.
   --  The ticket for the corresponding compiler bug is T806-020.

   type Name_Information is record
      Is_Dot_Call  : Boolean;
      --  True if we are dealing with a dotted call.

      Is_Visible   : Boolean;
      --  True if the item is visible from the withed/used packages, False
      --  otherwise.

      Use_Snippets : Boolean;
      --  True if it's a snippet completion item.

      Pos          : Integer := -1;
      --  The position of the item in the fully computed completion list. Used
      --  for sorting properly the items on client-side.
   end record;

   package Completion_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Libadalang.Analysis.Defining_Name,
      Element_Type    => Name_Information,
      Hash            => Hash,
      Equivalent_Keys => Is_Equal,
      "="             => "=");

   type Completion_Provider is abstract tagged limited null record;

   procedure Propose_Completion
     (Self   : Completion_Provider;
      Sloc   : Langkit_Support.Slocs.Source_Location;
      Token  : Libadalang.Common.Token_Reference;
      Node   : Libadalang.Analysis.Ada_Node;
      Filter : in out LSP.Ada_Completions.Filters.Filter;
      Names  : out Ada_Completions.Completion_Maps.Map;
      Result : out LSP.Messages.CompletionList) is abstract;
   --  Populate Names and Result with completions for given Source_Location.
   --  Names works for defining name completions to create snippets and to
   --  avoid duplicates. The Token's span encloses Sloc or Sloc can be at the
   --  next character after the Token if the token not a trivia.
   --
   --  Example: abc;  or abc<space>
   --  Cursor:     ^        ^
   --  Consider `abc;` or `abc<space>` and Sloc is a character after `c`, then
   --  Token is `abc`, because a user expect it to te completed.
   --  The Node is immediate enclosing AST node for the token.
   --  The Filter could be used to quick check common completion contexts.

   procedure Write_Completions
     (Context                  : LSP.Ada_Contexts.Context;
      Names                    : Completion_Maps.Map;
      Named_Notation_Threshold : Natural;
      Result                   : in out LSP.Messages.CompletionItem_Vector);

   generic
      with function Has_Been_Canceled return Boolean;
   procedure Write_Symbols
     (Names  : Completion_Maps.Map;
      Result : in out LSP.Messages.Symbol_Vector);

   type Completion_Provider_Access is access all
     LSP.Ada_Completions.Completion_Provider'Class;

   type Completion_Provider_List is array (Positive range <>) of
     Completion_Provider_Access;

end LSP.Ada_Completions;

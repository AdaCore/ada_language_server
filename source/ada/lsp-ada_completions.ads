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
with Ada.Strings.Hash_Case_Insensitive;

with Langkit_Support.Slocs;
with Langkit_Support.Text;
with Libadalang.Analysis;
with Libadalang.Common;

limited with LSP.Ada_Contexts;
limited with LSP.Ada_Documents;
limited with LSP.Ada_Completions.Filters;

with LSP.Messages;

package LSP.Ada_Completions is

   function Hash (Name : Libadalang.Analysis.Defining_Name)
     return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash_Case_Insensitive (
        Langkit_Support.Text.To_UTF8 (Name.Full_Sloc_Image)));

   function Is_Full_Sloc_Equal
     (Left, Right : Libadalang.Analysis.Defining_Name) return Boolean;
   --  Compare the two nodes using full sloc image (filename + sloc). Needed
   --  for completion, since LAL can return several times the same declaration
   --  and specially subprograms from generic instantiations.

   subtype Completion_Item_Weight_Type is Integer range 0 .. 100;
   --  Type representing the weight returned by LAL for each completion item.
   --  Used to sort them accordingly on the client-side.

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

      Weight : Completion_Item_Weight_Type := 0;
      --  The completion item's weight. Used for sorting on the client-side.
   end record;

   package Completion_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Libadalang.Analysis.Defining_Name,
      Element_Type    => Name_Information,
      Hash            => Hash,
      Equivalent_Keys => Is_Full_Sloc_Equal,
      "="             => "=");

   type Completion_Provider is abstract tagged limited null record;

   procedure Propose_Completion
     (Self   : Completion_Provider;
      Sloc   : Langkit_Support.Slocs.Source_Location;
      Token  : Libadalang.Common.Token_Reference;
      Node   : Libadalang.Analysis.Ada_Node;
      Filter : in out LSP.Ada_Completions.Filters.Filter;
      Names  : in out Ada_Completions.Completion_Maps.Map;
      Result : in out LSP.Messages.CompletionList) is abstract;
   --  Populate Names and Result with completions for given Source_Location.
   --  Names works for defining name completions to create snippets and to
   --  avoid duplicates. The Token's span encloses Sloc-1, but not Sloc itself.
   --
   --  Example: abc|;  or abc|<space>
   --  Cursor:     ^         ^
   --  Consider `abc;` or `abc<space>` and Sloc is a character after `c`, then
   --  Token is `abc`, because a user expect it to te completed.
   --  The Node is immediate enclosing AST node for the token.
   --  The Filter could be used to quick check common completion contexts.

   procedure Write_Completions
     (Context                  : LSP.Ada_Contexts.Context;
      Document                 : LSP.Ada_Documents.Document;
      Sloc                     : Langkit_Support.Slocs.Source_Location;
      Node                     : Libadalang.Analysis.Ada_Node;
      Names                    : Completion_Maps.Map;
      Named_Notation_Threshold : Natural;
      Compute_Doc_And_Details  : Boolean;
      Result                   : in out LSP.Messages.CompletionItem_Vector);
   --  Convert all the completion Names into LSP completion items' results.
   --  Named_Notation_Threshold defines the number of parameters/components at
   --  which point named notation is used for subprogram/aggregate completion
   --  snippets.
   --  If Compute_Doc_And_Details is True, the 'detail' and 'documentation'
   --  fields for all the resulting completion items will be computed
   --  immediately, which might take time.

   procedure Pretty_Print_Snippet
     (Context : LSP.Ada_Contexts.Context;
      Prefix  : String;
      Offset  : Natural;
      Span    : LSP.Messages.Span;
      Rule    : Libadalang.Common.Grammar_Rule;
      Result  : in out LSP.Messages.CompletionItem);
   --  If Result is a snippet then generate a textEdit over span using GNATpp.
   --  Rule must match the content of "Prefix & Result.insertText.Value"

   generic
      with function Has_Been_Canceled return Boolean;
   procedure Generic_Write_Symbols
     (Names  : Completion_Maps.Map;
      Result : in out LSP.Messages.Symbol_Vector);

   type Completion_Provider_Access is access all
     LSP.Ada_Completions.Completion_Provider'Class;

   type Completion_Provider_List is array (Positive range <>) of
     Completion_Provider_Access;

end LSP.Ada_Completions;

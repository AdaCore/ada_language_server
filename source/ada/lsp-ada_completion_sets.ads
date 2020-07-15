------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2020, AdaCore                     --
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
with Ada.Containers.Hashed_Sets;

with VSS.Strings;

with LSP.Messages;
with LSP.Types;

package LSP.Ada_Completion_Sets is

   function Hash
     (Text : VSS.Strings.Virtual_String) return Ada.Containers.Hash_Type is
       (Ada.Containers.Hash_Type'Mod (Text.Hash));

   package Completion_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => VSS.Strings.Virtual_String,
      Hash                => Hash,
      Equivalent_Elements => VSS.Strings."=",
      "="                 => VSS.Strings."=");

   type Completion_Result is tagged record
      Unique_Symbols  : Completion_Sets.Set;
      --  Unique canonical symbols that included in the result
      Completion_List : LSP.Messages.CompletionItem_Vector;
      --  Corresponding ComplitionItem vector
      Is_Incomplete   : Boolean := False;
      --  Flag for an incomplete completion result.
   end record;
   --  A collection of CompletionItem. It could contain several CompletionItems
   --  for the same canonical symbol.

   procedure Append
     (Self   : in out Completion_Result'Class;
      Symbol : Wide_Wide_String;
      Item   : LSP.Messages.CompletionItem) with Inline;
   --  Append a Symbol and corresponding CompletionItem to the
   --  Completion_Result.

   package Completion_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => VSS.Strings.Virtual_String,
      Element_Type    => LSP.Messages.CompletionItem,
      Hash            => Hash,
      Equivalent_Keys => VSS.Strings."=",
      "="             => LSP.Messages."=");

   type Completion_Map is new Completion_Maps.Map with null record;
   --  A mapping from canonical symbol to a CompletionItem

   procedure Append_Invisible_Symbol
     (Self       : in out Completion_Map'Class;
      Cannonical : VSS.Strings.Virtual_String;
      Original   : LSP.Types.LSP_String) with Inline;
   --  Create CompletionItem for an invisible Original symbol. Cannonical is
   --  a canonical writting of the Original.

   procedure Write_Completions
     (Self   : in out Completion_Map'Class;
      Limit  : Ada.Containers.Count_Type;
      Result : in out Completion_Result);
   --  Append items of Self if they don't belong to Result.Unique_Symbols yet

end LSP.Ada_Completion_Sets;

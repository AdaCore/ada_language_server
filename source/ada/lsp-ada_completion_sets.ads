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

with Ada.Containers.Hashed_Sets;

with Libadalang.Analysis;

with VSS.Strings;

with LSP.Messages;

package LSP.Ada_Completion_Sets is

   function Hash
     (Text : VSS.Strings.Virtual_String) return Ada.Containers.Hash_Type is
       (Ada.Containers.Hash_Type'Mod (Text.Hash));

   package Key_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => VSS.Strings.Virtual_String,
      Hash                => Hash,
      Equivalent_Elements => VSS.Strings."=",
      "="                 => VSS.Strings."=");
   --  Sets to keep completion keys, like <file:line:column>

   type Completion_Result is tagged record
      Unique_Keys     : Key_Sets.Set;
      --  Unique keys for completions that included in the result
      Completion_List : LSP.Messages.CompletionItem_Vector;
      --  Corresponding ComplitionItem vector
      Is_Incomplete   : Boolean := False;
      --  Flag for an incomplete completion result.
   end record;
   --  A collection of CompletionItem. It could contain several CompletionItems
   --  for the same canonical symbol.

   procedure Append
     (Self : in out Completion_Result'Class;
      Name : Libadalang.Analysis.Defining_Name;
      Item : LSP.Messages.CompletionItem) with Inline;
   --  Append a Name and corresponding CompletionItem to the
   --  Completion_Result, if Self doesn't contain it yet.

   procedure Append_Invisible_Symbol
     (Self      : in out Completion_Result'Class;
      Canonical : VSS.Strings.Virtual_String;
      Name      : Libadalang.Analysis.Defining_Name) with Inline;
   --  Create CompletionItem for an invisible Name. Canonical is a canonical
   --  writting on the Name

end LSP.Ada_Completion_Sets;

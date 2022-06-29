------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Laltools.Common;
with Libadalang.Analysis; use Libadalang.Analysis;
with VSS.Strings;         use VSS.Strings;

package LSP.Ada_Completions.Generic_Assoc_Utils is

   function Hash
     (Node : Libadalang.Analysis.Ada_Node'Class)
      return Ada.Containers.Hash_Type is
     (Libadalang.Analysis.Hash (Node.As_Ada_Node));

   type Assoc_Type is record
      Node     : Libadalang.Analysis.Ada_Node;

      Is_Value : Boolean := False;
      --  True if Node.Text represents a value and not a type
   end record;

   package Param_To_Type_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Libadalang.Analysis.Ada_Node'Class,
      Element_Type    => Assoc_Type,
      Hash            => Hash,
      Equivalent_Keys => Libadalang.Analysis."=",
      "="             => "=");

   type Assoc_Data is record
      Decl         : Libadalang.Analysis.Basic_Decl :=
        Libadalang.Analysis.No_Basic_Decl;
      --  Basic_Decl to retrieve the documentation

      Title        : VSS.Strings.Virtual_String;
      --  Title of the CompletionItem

      Prefix       : VSS.Strings.Virtual_String;
      --  String prepend to the snippet

      Param_Types  : Param_To_Type_Maps.Map;
      --  Map of {Param : Type}
      --  Note: the nodes type doesn't matter only Libadalang.Analysis.Text is
      --  used on the nodes

      Param_Vector : Laltools.Common.Node_Vectors.Vector;
      --  Vector of Params, used to keep the order

   end record;

   package Assoc_Data_Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Element_Type => Assoc_Data,
        "="          => "=");
   --  List of all the possible "profile"

end LSP.Ada_Completions.Generic_Assoc_Utils;

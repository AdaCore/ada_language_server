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

with Libadalang.Analysis;

limited with LSP.Ada_Contexts;
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
      Is_Visible   : Boolean;
      Use_Snippets : Boolean;
   end record;

   package Completion_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Libadalang.Analysis.Defining_Name,
      Element_Type    => Name_Information,
      Hash            => Hash,
      Equivalent_Keys => Is_Equal,
      "="             => "=");

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

end LSP.Ada_Completions;

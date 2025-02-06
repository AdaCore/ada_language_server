------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2024-2025, AdaCore                     --
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

with LSP.Structures;
with LSP.GPR_Files;

package LSP.GPR_Highlighters is

   type GPR_Highlighter is tagged limited private;

   procedure Initialize
     (Self      : in out GPR_Highlighter'Class;
      Types     : in out LSP.Structures.Virtual_String_Vector;
      Modifiers : in out LSP.Structures.Virtual_String_Vector);

   procedure Get_Tokens
     (Self   : GPR_Highlighter'Class;
      File   : LSP.GPR_Files.File_Access;
      Result : out LSP.Structures.Natural_Vector);
   --  If Span isn't empty then return unit tokens in given Span, otherwise
   --  return all tokens in the Unit.

private

   type Token_Type is (Namespace, Enum, Variable, Property);
   type Token_Modifier is (Declaration, Modification);

   type Token_Type_Set is array (Token_Type) of Boolean;
   type Token_Modifier_Set is array (Token_Modifier) of Boolean;

   type Token_Type_Map is array (Token_Type) of Natural;
   type Token_Modifier_Map is array (Token_Modifier) of Natural;

   type GPR_Highlighter is tagged limited record
      Has_Type     : Token_Type_Set;
      Has_Modifier : Token_Modifier_Set;
      Type_Id      : Token_Type_Map;
      Modifier_Id  : Token_Modifier_Map;
   end record;

end LSP.GPR_Highlighters;

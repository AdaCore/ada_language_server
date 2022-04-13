------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

with Ada.Containers.Hashed_Maps;

with LSP.Messages;
with LSP.Types;

package LSP.Ada_Highlighters is

   type Ada_Highlighter is tagged limited private;

   procedure Initialize
     (Self : in out Ada_Highlighter'Class;
      Client : LSP.Messages.SemanticTokensClientCapabilities;
      Legend : out LSP.Messages.SemanticTokensLegend);

private

   function Hash (Value : LSP.Messages.SemanticTokenTypes)
     return Ada.Containers.Hash_Type is
       (Ada.Containers.Hash_Type
          (LSP.Messages.SemanticTokenTypes'Pos (Value)));

   package Token_Type_Maps is new Ada.Containers.Hashed_Maps
     (LSP.Messages.SemanticTokenTypes,
      LSP.Messages.uinteger,
      Hash,
      LSP.Messages."=",
      LSP.Types."=");

   function Hash (Value : LSP.Messages.SemanticTokenModifiers)
     return Ada.Containers.Hash_Type is
       (Ada.Containers.Hash_Type
          (LSP.Messages.SemanticTokenModifiers'Pos (Value)));

   package Token_Modifier_Maps is new Ada.Containers.Hashed_Maps
     (LSP.Messages.SemanticTokenModifiers,
      LSP.Messages.uinteger,
      Hash,
      LSP.Messages."=",
      LSP.Types."=");

   type Ada_Highlighter is tagged limited record
      Token_Types     : Token_Type_Maps.Map;
      Token_Modifiers : Token_Modifier_Maps.Map;
   end record;

end LSP.Ada_Highlighters;

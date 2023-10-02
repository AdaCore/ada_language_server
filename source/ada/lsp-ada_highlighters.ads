------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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
with Ada.Strings.Wide_Wide_Unbounded;

with Libadalang.Analysis;

with LSP.Ada_Client_Capabilities;
with LSP.Enumerations;
with LSP.Structures;
with LSP.Tracers;

package LSP.Ada_Highlighters is

   type Ada_Highlighter is tagged limited private;

   procedure Initialize
     (Self      : in out Ada_Highlighter'Class;
      Client    : LSP.Ada_Client_Capabilities.Client_Capability;
      Types     : out LSP.Structures.Virtual_String_Vector;
      Modifiers : out LSP.Structures.Virtual_String_Vector);

   function Get_Tokens
     (Self   : Ada_Highlighter'Class;
      Unit   : Libadalang.Analysis.Analysis_Unit;
      Tracer : in out LSP.Tracers.Tracer'Class;
      Span   : LSP.Structures.A_Range)
      return LSP.Structures.Natural_Vector;
   --  If Span isn't empty then return unit tokens in given Span, otherwise
   --  return all tokens in the Unit.

private

   function Hash (Value : LSP.Enumerations.SemanticTokenTypes)
     return Ada.Containers.Hash_Type is
       (Ada.Containers.Hash_Type
          (LSP.Enumerations.SemanticTokenTypes'Pos (Value)));

   package Token_Type_Maps is new Ada.Containers.Hashed_Maps
     (LSP.Enumerations.SemanticTokenTypes,
      Natural,
      Hash,
      LSP.Enumerations."=",
      "=");

   function Hash (Value : LSP.Enumerations.SemanticTokenModifiers)
     return Ada.Containers.Hash_Type is
       (Ada.Containers.Hash_Type
          (LSP.Enumerations.SemanticTokenModifiers'Pos (Value)));

   package Token_Modifier_Maps is new Ada.Containers.Hashed_Maps
     (LSP.Enumerations.SemanticTokenModifiers,
      Natural,
      Hash,
      LSP.Enumerations."=",
      "=");

   subtype Unbounded_Text_Type is
     Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;

   type Ada_Highlighter is tagged limited record
      Token_Types     : Token_Type_Maps.Map;
      Token_Modifiers : Token_Modifier_Maps.Map;
      Obsolescent     : Unbounded_Text_Type;
      Ada             : Unbounded_Text_Type;
      System          : Unbounded_Text_Type;
      Interfaces      : Unbounded_Text_Type;
   end record;

end LSP.Ada_Highlighters;

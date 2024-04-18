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
with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Token_Data_Handlers;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Iterators;

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

   function Need_Highlighting return Libadalang.Iterators.Ada_Node_Predicate;
   --  Predicate to filter node for highlighing

   type Highlights_Holder is limited private;
   --  A holder to keep data about highlighted tokens

   procedure Initialize
     (Holder : out Highlights_Holder;
      Unit   : Libadalang.Analysis.Analysis_Unit);
   --  Initialuze holder to keep highlighting information for every token
   --  in the Unit

   procedure Highlight_Node
     (Self   : Ada_Highlighter'Class;
      Holder : in out Highlights_Holder;
      Node   : Libadalang.Analysis.Ada_Node'Class);
   --  Highlight tokens of the Node and keep highlighting in the Holder

   procedure Get_Result
     (Self   : Ada_Highlighter'Class;
      Holder : Highlights_Holder;
      Unit   : Libadalang.Analysis.Analysis_Unit;
      Result : out LSP.Structures.Natural_Vector);
   --  Retrive highliting from Holder and encode it into Result

private

   package Highlights_Holders is
      type Highlights_Holder is tagged limited private;
      --  Highlights_Holder stores style for each token in the range given
      --  on initialization.

      procedure Initialize
        (Self  : in out Highlights_Holder'Class;
         From  : Libadalang.Common.Token_Reference;
         To    : Libadalang.Common.Token_Reference;
         Empty : out Boolean);
      --  Initialize holder by providing token range. If From or To is a trivia
      --  holder uses corresponding non-trivia token instead.

      procedure Set_Token_Kind
        (Self  : in out Highlights_Holder'Class;
         Token : Libadalang.Common.Token_Reference;
         Value : LSP.Enumerations.SemanticTokenTypes)
           with Pre => not Libadalang.Common.Is_Trivia (Token);

      procedure Set_Token_Modifier
        (Self  : in out Highlights_Holder'Class;
         Token : Libadalang.Common.Token_Reference;
         Value : LSP.Enumerations.SemanticTokenModifiers)
           with Pre => not Libadalang.Common.Is_Trivia (Token);

      procedure Set_Token_Modifier
        (Self  : in out Highlights_Holder'Class;
         From  : Libadalang.Common.Token_Reference;
         To    : Libadalang.Common.Token_Reference;
         Value : LSP.Enumerations.SemanticTokenModifiers)
           with Pre => not Libadalang.Common.Is_Trivia (From) and then
                       not Libadalang.Common.Is_Trivia (To);
      --  Set a modifier on each token in the range From .. To

      type Modifier_Set is
        array (LSP.Enumerations.SemanticTokenModifiers) of Boolean
          with Pack;

      Empty : constant Modifier_Set := (others => False);

      type Semantic_Token (Is_Set : Boolean := False) is record
         Modifiers  : Modifier_Set;

         case Is_Set is
            when True =>
               Kind : LSP.Enumerations.SemanticTokenTypes;
            when False =>
               null;
         end case;
      end record;

      function Get
        (Self  : Highlights_Holder'Class;
         Token : Libadalang.Common.Token_Reference)
           return Semantic_Token
             with Pre => not Libadalang.Common.Is_Trivia (Token);

   private

      package Semantic_Token_Vectors is new Ada.Containers.Vectors
        (Index_Type   => Langkit_Support.Token_Data_Handlers.Token_Index,
         Element_Type => Semantic_Token);

      type Highlights_Holder is tagged limited record
         First  : Langkit_Support.Token_Data_Handlers.Token_Index;
         Last   : Langkit_Support.Token_Data_Handlers.Token_Index;
         Vector : Semantic_Token_Vectors.Vector;
      end record;
   end Highlights_Holders;

   type Highlights_Holder is limited record
      Value : Highlights_Holders.Highlights_Holder;
   end record;

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
   end record;

end LSP.Ada_Highlighters;

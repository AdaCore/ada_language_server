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

with VSS.Strings;

package body LSP.Ada_Highlighters is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : in out Ada_Highlighter'Class;
      Client : LSP.Messages.SemanticTokensClientCapabilities;
      Legend : out LSP.Messages.SemanticTokensLegend)
   is
      use all type LSP.Messages.SemanticTokenTypes;
      use all type LSP.Messages.SemanticTokenModifiers;

      procedure Append_Type
        (Kind  : LSP.Messages.SemanticTokenTypes;
         Image : VSS.Strings.Virtual_String);
      --  Update Legend.tokenTypes if client understands given Kind

      procedure Append_Modifier
        (Kind  : LSP.Messages.SemanticTokenModifiers;
         Image : VSS.Strings.Virtual_String);
      --  Update Legend.tokenModifiers if client understands given Kind

      procedure Append_Modifier
        (Kind  : LSP.Messages.SemanticTokenModifiers;
         Image : VSS.Strings.Virtual_String) is
      begin
         if Client.tokenModifiers.Contains (Kind) then
            Self.Token_Modifiers.Insert
              (Kind,
               LSP.Messages.uinteger (Legend.tokenModifiers.Length));

            Legend.tokenModifiers.Append (Image);
         end if;
      end Append_Modifier;

      -----------------
      -- Append_Type --
      -----------------

      procedure Append_Type
        (Kind  : LSP.Messages.SemanticTokenTypes;
         Image : VSS.Strings.Virtual_String) is
      begin
         if Client.tokenTypes.Contains (Kind) then
            Self.Token_Types.Insert
              (Kind,
               LSP.Messages.uinteger (Legend.tokenTypes.Length));

            Legend.tokenTypes.Append (Image);
         end if;
      end Append_Type;

   begin
      Append_Type (a_type, "type");
      --  Append_Type (class, "class");
      Append_Type (enum, "enum");
      Append_Type (an_interface, "interface");
      --  Append_Type (struct, "struct");
      --  Append_Type (typeParameter, "typeParameter");
      Append_Type (parameter, "parameter");
      Append_Type (variable, "variable");
      Append_Type (property, "property");
      Append_Type (enumMember, "enumMember");
      --  Append_Type (event, "event");
      Append_Type (a_function, "function");
      Append_Type (method, "method");
      --  Append_Type (macro, "macro");
      Append_Type (keyword, "keyword");
      Append_Type (modifier, "modifier");
      Append_Type (comment, "comment");
      Append_Type (a_string, "string");
      Append_Type (number, "number");
      --  Append_Type (regexp, "regexp");
      Append_Type (operator, "operator");

      Append_Modifier (declaration, "declaration");
      Append_Modifier (definition, "definition");
      Append_Modifier (readonly, "readonly");
      Append_Modifier (static, "static");
      Append_Modifier (deprecated, "deprecated");
      Append_Modifier (an_abstract, "abstract");
      --  Append_Modifier (async, "async");
      Append_Modifier (modification, "modification");
      Append_Modifier (documentation, "documentation");
      Append_Modifier (defaultLibrary, "defaultLibrary");

   end Initialize;

end LSP.Ada_Highlighters;

------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2020-2023, AdaCore                  --
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
--  Implementation of the command to refactor positional parameters into
--  named parameters in given subprogram call.

with LSP.Ada_Commands;
with LSP.Errors;

package LSP.Ada_Handlers.Named_Parameters_Commands is

   type Command is new LSP.Ada_Commands.Command with private;

   procedure Initialize
     (Self                : in out Command'Class;
      Context             : LSP.Ada_Context_Sets.Context_Access;
      Where               : LSP.Structures.TextDocumentPositionParams;
      Versioned_Documents : Boolean);

   procedure Append_Suggestion
     (Self                : in out Command;
      Context             : LSP.Ada_Context_Sets.Context_Access;
      Commands_Vector     : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Where               : LSP.Structures.Location;
      Versioned_Documents : Boolean);
   --  Initializes Command and appends it to Commands_Vector.

private

   type Command is new LSP.Ada_Commands.Command with record
      Context             : VSS.Strings.Virtual_String;
      Where               : LSP.Structures.TextDocumentPositionParams;
      Versioned_Documents : Boolean := True;
   end record;

   overriding function Create
     (Any : not null access LSP.Structures.LSPAny_Vector)
       return Command;

   overriding procedure Execute
     (Self     : Command;
      Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Response : in out LSP.Structures.LSPAny_Or_Null;
      Error    : in out LSP.Errors.ResponseError_Optional);

   function Write_Command
     (Self : Command) return LSP.Structures.LSPAny_Vector;

   for Command'External_Tag use "als-named-parameters";

end LSP.Ada_Handlers.Named_Parameters_Commands;

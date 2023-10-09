------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

--  Implementation of the command to fetch dependencies for a given unit.

with LSP.Ada_Commands;
with LSP.Errors;

package LSP.Ada_Handlers.Show_Dependencies_Commands is

   type Command is new LSP.Ada_Commands.Command with private;

private

   type Show_Dependencies_Kind is (Show_Imported, Show_Importing);

   type Command is new LSP.Ada_Commands.Command with record
      URI           : LSP.Structures.DocumentUri;
      Kind          : Show_Dependencies_Kind := Show_Imported;
      Show_Implicit : Boolean := False;
   end record;

   overriding function Create
     (Any : not null access LSP.Structures.LSPAny_Vector)
      return Command;

   overriding procedure Execute
     (Self     : Command;
      Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Response : in out LSP.Structures.LSPAny_Or_Null;
      Error    : in out LSP.Errors.ResponseError_Optional);

   for Command'External_Tag use "als-show-dependencies";

end LSP.Ada_Handlers.Show_Dependencies_Commands;

------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2025, AdaCore                       --
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
--  Implementation of the refactoring command to extract variable

with VSS.Strings;

with LSP.Ada_Contexts;
with LSP.Server_Jobs;

package LSP.Ada_Handlers.Refactor.Extract_Variable is

   type Command is new LSP.Ada_Handlers.Refactor.Command with private;

   overriding function Name (Self : Command) return String
   is ("Extract Variable");

   procedure Append_Code_Action
     (Self            : in out Command;
      Context         : LSP.Ada_Context_Sets.Context_Access;
      Commands_Vector : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Where           : LSP.Structures.Location);
   --  Initializes Self and appends it to Commands_Vector

private

   type Command is new LSP.Ada_Handlers.Refactor.Command with record
      Context_Id              : VSS.Strings.Virtual_String;
      Section_To_Extract_SLOC : LSP.Structures.Location;
   end record;

   overriding
   function Create
     (Any : not null access LSP.Structures.LSPAny_Vector)
      return Command;
   --  Reads Any and creates a new Command

   overriding
   procedure Refactor
     (Self    : Command;
      Handler : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Edits   : out LAL_Refactor.Refactoring_Edits);
   --  Executes Self by computing the necessary refactorings

   overriding function Priority (Self : Command)
     return LSP.Server_Jobs.Job_Priority
       is (LSP.Server_Jobs.Low);

   procedure Initialize
     (Self    : in out Command'Class;
      Context : LSP.Ada_Contexts.Context;
      Where   : LSP.Structures.Location);
   --  Initializes Self

   function Write_Command
     (Self : Command) return LSP.Structures.LSPAny_Vector;

   for Command'External_Tag use "als-refactor-extract-variable";

end LSP.Ada_Handlers.Refactor.Extract_Variable;

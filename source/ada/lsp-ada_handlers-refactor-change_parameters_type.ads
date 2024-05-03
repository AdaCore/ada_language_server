------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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
--  Implementation of the refactoring command to change a subprogram
--  parameter type

with Laltools.Common;

private with VSS.String_Vectors;
private with LSP.Ada_Contexts;
private with LSP.Server_Jobs;

package LSP.Ada_Handlers.Refactor.Change_Parameters_Type is

   type Command is new LSP.Ada_Handlers.Refactor.Command with private;

   overriding function Name (Self : Command) return String
   is
      ("Change Parameters Type");

   procedure Append_Code_Action
     (Self            : in out Command;
      Context         : LSP.Ada_Context_Sets.Context_Access;
      Commands_Vector : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Where           : LSP.Structures.Location;
      Syntax_Rules    : Laltools.Common.Grammar_Rule_Vector);
   --  Initializes Self and appends it to Commands_Vector

private

   type Command is new LSP.Ada_Handlers.Refactor.Command with record
      Context             : VSS.Strings.Virtual_String;
      Where               : LSP.Structures.Location;
      Syntax_Rules        : VSS.String_Vectors.Virtual_String_Vector;
      New_Parameters_Type : VSS.Strings.Virtual_String;
   end record;

   overriding
   function Create
     (Any : not null access LSP.Structures.LSPAny_Vector)
      return Command;
   --  Reads Stream and creates a new Command

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
     (Self                : in out Command'Class;
      Context             : LSP.Ada_Contexts.Context;
      Where               : LSP.Structures.Location;
      Syntax_Rules        : Laltools.Common.Grammar_Rule_Vector;
      New_Parameters_Type : VSS.Strings.Virtual_String);
   --  Initializes Self

   function Write_Command (Self : Command) return LSP.Structures.LSPAny_Vector;
   --  Writes the command to Any_Vector

   for Command'External_Tag use "als-refactor-change_parameters_type";

end LSP.Ada_Handlers.Refactor.Change_Parameters_Type;

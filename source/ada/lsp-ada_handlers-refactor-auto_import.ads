------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2020-2023, AdaCore                     --
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
--  Implementation of the command to refactor imports.

with LAL_Refactor;
with LAL_Refactor.Auto_Import;

with LSP.Ada_Contexts;
with LSP.Server_Jobs;

package LSP.Ada_Handlers.Refactor.Auto_Import is

   type Command is new LSP.Ada_Handlers.Refactor.Command with private;

   overriding function Name (Self : Command) return String
   is
      ("Auto Import");

   procedure Initialize
     (Self         : in out Command'Class;
      Context      : LSP.Ada_Contexts.Context;
      Where        : LSP.Structures.TextDocumentPositionParams;
      With_Clause  : VSS.Strings.Virtual_String;
      Prefix       : VSS.Strings.Virtual_String);
   --  Initializes Command

   procedure Append_Suggestion
     (Self              : in out Command;
      Context           : LSP.Ada_Context_Sets.Context_Access;
      Where             : LSP.Structures.Location;
      Commands_Vector   : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Suggestion        : LAL_Refactor.Auto_Import.Import_Type);
   --  Initializes Command based on Suggestion and appends it to
   --  Commands_Vector.

   function Write_Command_Args
     (Self : Command) return LSP.Structures.LSPAny_Vector;
   --  Write the given command's arguments as LPS any vector.
   --  These arguments are then passed to the server by the LSP client when
   --  executing the command via the executeCommand LSP request.

private

   type Command is new LSP.Ada_Handlers.Refactor.Command with record
      Context    : VSS.Strings.Virtual_String;
      Where      : LSP.Structures.TextDocumentPositionParams;
      Suggestion : LAL_Refactor.Auto_Import.Import_Type;
   end record;

   overriding function Create
     (Any : not null access LSP.Structures.LSPAny_Vector)
      return Command;

   overriding procedure Refactor
     (Self    : Command;
      Handler : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Edits   : out LAL_Refactor.Refactoring_Edits);

   overriding function Priority (Self : Command)
     return LSP.Server_Jobs.Job_Priority
       is (LSP.Server_Jobs.Low);

   function Command_To_Refactoring_Edits
     (Self     : Command;
      Context  : LSP.Ada_Contexts.Context;
      Document : LSP.Ada_Documents.Document_Access)
      return LAL_Refactor.Refactoring_Edits;
   --  Converts Self into LAL_Refactor.Refactoring_Edits that can be
   --  converted in a WorkspaceEdit.

   for Command'External_Tag use "als-auto-import";

end LSP.Ada_Handlers.Refactor.Auto_Import;

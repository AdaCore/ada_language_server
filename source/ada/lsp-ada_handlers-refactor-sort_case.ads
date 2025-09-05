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
--  Implementation of the refactoring command to sort case

with VSS.Strings;

with LSP.Ada_Contexts;
with LSP.Server_Jobs;

with Libadalang.Analysis;   use Libadalang.Analysis;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

package LSP.Ada_Handlers.Refactor.Sort_Case is

   type Base_Command is abstract
     new LSP.Ada_Handlers.Refactor.Command with private;

   type Alphabetical_Command is new Base_Command with private;

   overriding function Name (Self : Alphabetical_Command) return String
   is ("Sort Cases by Alphabetical Order");

   procedure Append_Code_Action
     (Self            : in out Alphabetical_Command'Class;
      Context         : LSP.Ada_Context_Sets.Context_Access;
      Commands_Vector : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Where           : LSP.Structures.Location);

   type Declaration_Command is new Base_Command with private;

   overriding function Name (Self : Declaration_Command) return String
   is ("Sort Cases by Declaration Order");

   procedure Append_Code_Action
     (Self            : in out Declaration_Command'Class;
      Context         : LSP.Ada_Context_Sets.Context_Access;
      Commands_Vector : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Where           : LSP.Structures.Location);

private

   type Base_Command is abstract
     new LSP.Ada_Handlers.Refactor.Command with
      record
         Context_Id : VSS.Strings.Virtual_String;
         Location   : LSP.Structures.Location;
      end record;

   procedure Append_Code_Action
     (Self            : in out Base_Command'Class;
      Context         : LSP.Ada_Context_Sets.Context_Access;
      Commands_Vector : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Where           : LSP.Structures.Location;
      Tag             : String);
   --  Initializes Self and appends it to Commands_Vector

   overriding function Priority (Self : Base_Command)
     return LSP.Server_Jobs.Job_Priority
       is (LSP.Server_Jobs.Low);

   procedure Initialize
     (Self    : in out Base_Command'Class;
      Context : LSP.Ada_Contexts.Context;
      Where   : LSP.Structures.Location);
   --  Initializes Self

   function Write_Command
     (Self : Base_Command'Class) return LSP.Structures.LSPAny_Vector;

   procedure Load
     (Self : in out Base_Command'Class;
      Any  : not null access LSP.Structures.LSPAny_Vector);

   procedure Prepare_Refactor_Data
     (Self     : Base_Command'Class;
      Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Unit     : out Analysis_Unit;
      Location : out Source_Location);

   type Alphabetical_Command is new Base_Command with null record;

   overriding
   function Create
     (Any : not null access LSP.Structures.LSPAny_Vector)
      return Alphabetical_Command;
   --  Reads Any and creates a new Command

   overriding
   procedure Refactor
     (Self    : Alphabetical_Command;
      Handler : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Edits   : out LAL_Refactor.Refactoring_Edits);
   --  Executes Self by computing the necessary refactorings

   for Alphabetical_Command'External_Tag use
     "als-refactor-sort-case-alphabetical";

   type Declaration_Command is new Base_Command with null record;

   overriding
   function Create
     (Any : not null access LSP.Structures.LSPAny_Vector)
      return Declaration_Command;
   --  Reads Any and creates a new Command

   overriding
   procedure Refactor
     (Self    : Declaration_Command;
      Handler : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Edits   : out LAL_Refactor.Refactoring_Edits);
   --  Executes Self by computing the necessary refactorings

   for Declaration_Command'External_Tag use
     "als-refactor-sort-case-declaration";

end LSP.Ada_Handlers.Refactor.Sort_Case;

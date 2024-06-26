------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2021-2023, AdaCore                  --
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
--  Implementation of the refactoring command to change parameters modes

private with VSS.Strings;

with Libadalang.Analysis;
with Libadalang.Common;

with LAL_Refactor.Subprogram_Signature; use LAL_Refactor.Subprogram_Signature;

with LSP.Server_Jobs;

package LSP.Ada_Handlers.Refactor.Change_Parameter_Mode is

   type Command is new LSP.Ada_Handlers.Refactor.Command with private;

      overriding function Name (Self : Command) return String
   is
      ("Change Parameter Mode");

   procedure Append_Code_Action
     (Self               : in out Command;
      Context            : LSP.Ada_Context_Sets.Context_Access;
      Commands_Vector    : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Target_Subp        : Libadalang.Analysis.Basic_Decl;
      Parameters_Indices : Parameter_Indices_Range_Type;
      New_Mode           : Libadalang.Common.Ada_Mode);
   --  Initializes 'Self' and appends it to 'Commands_Vector'

private

   type Command is new LSP.Ada_Handlers.Refactor.Command with record
      Context           : VSS.Strings.Virtual_String;
      Where             : LSP.Structures.TextDocumentPositionParams;
      First_Param_Index : Integer;
      Last_Param_Index  : Integer;
      New_Mode          : VSS.Strings.Virtual_String;
   end record;

   overriding
   function Create
     (Any : not null access LSP.Structures.LSPAny_Vector)
      return Command;
   --  Reads JS and creates a new Command

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
     (Self              : in out Command'Class;
      Context           : LSP.Ada_Context_Sets.Context_Access;
      Where             : LSP.Structures.TextDocumentPositionParams;
      First_Param_Index : Integer;
      Last_Param_Index  : Integer;
      New_Mode          : VSS.Strings.Virtual_String);
   --  Initializes Self

   function Write_Command (Self : Command) return LSP.Structures.LSPAny_Vector;
   --  Writes the command to Any_Vector

   for Command'External_Tag use "als-refactor-change-parameter-mode";

end LSP.Ada_Handlers.Refactor.Change_Parameter_Mode;

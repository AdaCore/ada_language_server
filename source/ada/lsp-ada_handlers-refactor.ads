------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
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
--  Base package for all the refactoring commands available via the ALS

with LAL_Refactor;

with LSP.Ada_Commands;
with LSP.Errors;

package LSP.Ada_Handlers.Refactor is

   type Command is abstract new LSP.Ada_Commands.Command with private;

   function Name (Self : Command) return String is abstract;

   overriding procedure Execute
     (Self    : Command;
      Handler : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Error   : in out LSP.Errors.ResponseError_Optional);

   procedure Refactor
     (Self    : Command;
      Handler : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Edits   : out LAL_Refactor.Refactoring_Edits) is abstract;
   --  Abstract procedure used to compute the refactoring edits.

private

   type Command is abstract new LSP.Ada_Commands.Command with null record;

end LSP.Ada_Handlers.Refactor;

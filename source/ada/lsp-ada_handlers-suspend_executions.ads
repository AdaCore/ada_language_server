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
--  Implementation of the command to suspend execution for debugging purposes.

with LSP.Client_Message_Receivers;
with LSP.Commands;
with LSP.Errors;

package LSP.Ada_Handlers.Suspend_Executions is

   type Suspend_Execution is new LSP.Commands.Command with private;

private
   type Suspend_Execution is new LSP.Commands.Command with record
      Input_Queue_Length : Natural;
   end record;

   overriding function Create
     (Any : not null access LSP.Structures.LSPAny_Vector)
      return Suspend_Execution;

   overriding procedure Execute
     (Self    : Suspend_Execution;
      Handler : not null access
        LSP.Server_Notification_Receivers.Server_Notification_Receiver'Class;
      Sender  : not null access LSP.Client_Message_Receivers.
        Client_Message_Receiver'Class;
      Id      : LSP.Structures.Integer_Or_Virtual_String;
      Error   : in out LSP.Errors.ResponseError_Optional);

   for Suspend_Execution'External_Tag use "als-suspend-execution";

end LSP.Ada_Handlers.Suspend_Executions;

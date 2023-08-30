------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2021, AdaCore                       --
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
--  Implementation of the command to switch from .adb to .ads file and back.

with LSP.Client_Message_Receivers;
with LSP.Commands;
with LSP.Errors;

package LSP.Ada_Handlers.Other_File_Commands is

   type Command is new LSP.Commands.Command with private;

   procedure Initialize
     (Self : in out Command'Class;
      URI  : LSP.Structures.DocumentUri);

private

   type Command is new LSP.Commands.Command with record
      URI : LSP.Structures.DocumentUri;
   end record;

   overriding function Create
     (Any : not null access LSP.Structures.LSPAny_Vector)
       return Command;

   overriding procedure Execute
     (Self    : Command;
      Handler : not null access
        LSP.Server_Notification_Receivers.Server_Notification_Receiver'Class;
      Sender  : not null access LSP.Client_Message_Receivers.
        Client_Message_Receiver'Class;
      Id      : LSP.Structures.Integer_Or_Virtual_String;
      Error   : in out LSP.Errors.ResponseError_Optional);

   for Command'External_Tag use "als-other-file";

end LSP.Ada_Handlers.Other_File_Commands;

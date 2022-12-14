------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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
--  Implementation of the refactoring command to replace a type

with Ada.Streams;

with LSP.Client_Message_Receivers;
with LSP.Commands;
with LSP.Errors;
with LSP.JSON_Streams;

private with VSS.Strings;

package LSP.Ada_Handlers.Refactor_Replace_Type is

   type Command is new LSP.Commands.Command with private;

   procedure Append_Code_Action
     (Self            : in out Command;
      Context         : Context_Access;
      Commands_Vector : in out LSP.Messages.CodeAction_Vector;
      Where           : LSP.Messages.Location);
   --  Initializes Self and appends it to Commands_Vector

private

   type Command is new LSP.Commands.Command with record
      Context_Id : VSS.Strings.Virtual_String;
      New_Type   : VSS.Strings.Virtual_String;
      Where      : LSP.Messages.Location;
   end record;

   overriding
   function Create
     (JS : not null access LSP.JSON_Streams.JSON_Stream'Class)
      return Command;
   --  Reads JS and creates a new Command

   overriding
   procedure Execute
     (Self    : Command;
      Handler : not null access
        LSP.Server_Notification_Receivers.Server_Notification_Receiver'Class;
      Client  : not null access
        LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Error   : in out LSP.Errors.Optional_ResponseError);
   --  Executes Self by computing the necessary refactorings

   procedure Initialize
     (Self    : in out Command'Class;
      Context : LSP.Ada_Contexts.Context;
      Where   : LSP.Messages.Location);
   --  Initializes Self

   procedure Write_Command
     (S : access Ada.Streams.Root_Stream_Type'Class;
      C : Command);
   --  Writes C to S

   for Command'Write use Write_Command;
   for Command'External_Tag use "als-refactor-replace-type";

end LSP.Ada_Handlers.Refactor_Replace_Type;

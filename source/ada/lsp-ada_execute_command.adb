------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2024, AdaCore                       --
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

with Ada.Tags.Generic_Dispatching_Constructor;
with Ada.Unchecked_Deallocation;

with VSS.Strings.Conversions;

with LSP.Ada_Commands;
with LSP.Ada_Handlers;
with LSP.Ada_Request_Jobs;
with LSP.Client_Message_Receivers;
with LSP.Enumerations;
with LSP.Errors;
with LSP.Server_Requests.ExecuteCommand;
with LSP.Structures;

package body LSP.Ada_Execute_Command is

   type Command_Access is access LSP.Ada_Commands.Command'Class;

   procedure Free is new Ada.Unchecked_Deallocation
     (LSP.Ada_Commands.Command'Class, Command_Access);

   type Ada_Execute_Command_Job
     (Parent : not null access constant Execute_Command_Handler) is limited
   new LSP.Ada_Request_Jobs.Ada_Request_Job (Priority => LSP.Server_Jobs.Low)
     with record
        Command : Command_Access;
     end record;

   overriding function Priority
     (Self : Ada_Execute_Command_Job) return LSP.Server_Jobs.Job_Priority is
       (if Self.Request.Canceled then LSP.Server_Jobs.Immediate
        elsif Self.Command = null then LSP.Server_Jobs.Low
        else Self.Command.Priority);
   --  Use command priority when we have a command

   overriding procedure Execute_Ada_Request
     (Self   : in out Ada_Execute_Command_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   function Create_Command is new Ada.Tags.Generic_Dispatching_Constructor
     (T           => LSP.Ada_Commands.Command,
      Parameters  => LSP.Structures.LSPAny_Vector,
      Constructor => LSP.Ada_Commands.Create);

   ----------------
   -- Create_Job --
   ----------------

   overriding function Create_Job
     (Self    : Execute_Command_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
      return LSP.Server_Jobs.Server_Job_Access
   is
      use type Ada.Tags.Tag;

      Request : LSP.Server_Requests.ExecuteCommand.Request
        renames LSP.Server_Requests.ExecuteCommand.Request (Message.all);

      Params : LSP.Structures.ExecuteCommandParams renames Request.Params;

      Tag : constant Ada.Tags.Tag :=
        (if Params.command.Is_Empty then Ada.Tags.No_Tag
         else Ada.Tags.Internal_Tag
           (VSS.Strings.Conversions.To_UTF_8_String (Params.command)));

      Command : constant Command_Access :=
        (if Tag = Ada.Tags.No_Tag then null
         else new LSP.Ada_Commands.Command'Class'
           (Create_Command (Tag, Params.arguments'Unrestricted_Access)));

      Result : constant LSP.Server_Jobs.Server_Job_Access :=
        new Ada_Execute_Command_Job'
          (Parent  => Self'Unchecked_Access,
           Command => Command,
           Request => LSP.Ada_Request_Jobs.Request_Access (Message));
   begin
      return Result;
   end Create_Job;

   -------------------------
   -- Execute_Ada_Request --
   -------------------------

   overriding procedure Execute_Ada_Request
     (Self   : in out Ada_Execute_Command_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is

      Handler : constant not null access
        LSP.Ada_Handlers.Message_Handler'Class :=
          LSP.Ada_Handlers.Message_Handler'Class
            (Self.Parent.Context.all)'Access;

      Message : LSP.Server_Requests.ExecuteCommand.Request
        renames LSP.Server_Requests.ExecuteCommand.Request (Self.Message.all);

      Response : LSP.Structures.LSPAny_Or_Null;
      Error    : LSP.Errors.ResponseError_Optional;

   begin
      Status := LSP.Server_Jobs.Done;

      if Self.Command = null then
         Client.On_Error_Response
           (Message.Id,
            (code    => LSP.Enumerations.InternalError,
             message => "Unknown command"));

      else
         Self.Command.Execute
           (Handler  => Handler,
            Response => Response,
            Error    => Error);

         if Error.Is_Set then
            Client.On_Error_Response (Message.Id, Error.Value);
         else
            Client.On_ExecuteCommand_Response (Message.Id, Response);
         end if;

         Free (Self.Command);
      end if;
   end Execute_Ada_Request;

end LSP.Ada_Execute_Command;

------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with Ada.Streams;

with LSP.Client_Notifications;
with LSP.Message_Handlers;
with LSP.Messages;
with LSP.Server_Notifications;
with LSP.Types;

private with Ada.Strings.Unbounded;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

package LSP.Servers is

   type Server is limited
     new LSP.Client_Notifications.Client_Notification_Handler with private;

   procedure Initialize
     (Self         : in out Server;
      Stream       : access Ada.Streams.Root_Stream_Type'Class;
      Request      : not null LSP.Message_Handlers.Request_Handler_Access;
      Notification : not null
        LSP.Server_Notifications.Server_Notification_Handler_Access);
   --  Initialize a server

   procedure Finalize (Self : in out Server);
   --  Clean up memory, file handles, tasks, etc.

   procedure Run (Self  : in out Server);
   --  Run the server

   procedure Stop (Self  : in out Server);
   --  Ask server to stop after processing current message

   procedure Workspace_Apply_Edit
     (Self     : in out Server;
      Params   : LSP.Messages.ApplyWorkspaceEditParams;
      Applied  : out Boolean;
      Error    : out LSP.Messages.Optional_ResponseError);
   --  ??? Needs doc
   --  ??? In the current implementation, Applied is always True and
   --  Error is always unset.

private

   -------------------------
   --  Tasking in the ALS --
   -------------------------

   --  The server has 3 tasks:
   --    The input task (currently: the main thread)
   --         This reads input coming from stdin, forms requests, and places
   --         them on the requests queue.
   --    The processing task:
   --         This is the task where libadalang lives. This task receives
   --         requests from the request queue, processes them, and returns
   --         the responses from them on the output queue.
   --    The output task:
   --         This task reads the responses coming from the output queue,
   --         and writes them to the standard output.

   type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;

   package Unbounded_String_Queue_Interface is new
     Ada.Containers.Synchronized_Queue_Interfaces
       (Ada.Strings.Unbounded.Unbounded_String);
   package Requests_Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues
       (Unbounded_String_Queue_Interface);
   package Output_Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues
       (Unbounded_String_Queue_Interface);

   type Requests_Queue_Access is access Requests_Queues.Queue;
   type Output_Queue_Access is access Output_Queues.Queue;

   --  The processing task
   task type Processing_Task_Type is
      entry Start
        (In_Queue     : Requests_Queue_Access;
         Out_Queue    : Output_Queue_Access;
         Request      : not null LSP.Message_Handlers.Request_Handler_Access;
         Notification : not null
           LSP.Server_Notifications.Server_Notification_Handler_Access);
      entry Stop;
      --  Clean shutdown of the task
   end Processing_Task_Type;

   --  The output task
   task type Output_Task_Type is
      entry Start (Queue         : Output_Queue_Access;
                   Output_Stream : Stream_Access);
      --  Start the task. Should be called once, with the stream to output to.

      entry Stop;
      --  Clean shutdown of the task. Can only be called after Start.
   end Output_Task_Type;

   type Server is limited
     new LSP.Client_Notifications.Client_Notification_Handler with
   record
      Initialized : Boolean;
      Stop        : Boolean := False;
      --  Mark Server as uninitialized until get 'initalize' request
      Stream        : access Ada.Streams.Root_Stream_Type'Class;
      Last_Request  : LSP.Types.LSP_Number := 1;
      Vector        : Ada.Strings.Unbounded.Unbounded_String;

      --  Queues and tasks used for asynchronous processing, see doc above
      Requests_Queue : Requests_Queue_Access;
      Output_Queue   : Output_Queue_Access;
      Processing_Task : Processing_Task_Type;
      Output_Task     : Output_Task_Type;
   end record;

   procedure Send_Notification
     (Self  : in out Server;
      Value : in out LSP.Messages.NotificationMessage'Class);
   --  Send given notification to client

   overriding procedure Show_Message
     (Self   : in out Server;
      Params : LSP.Messages.ShowMessageParams);

   overriding procedure Log_Message
     (Self   : in out Server;
      Params : LSP.Messages.LogMessageParams);

   overriding procedure Publish_Diagnostics
     (Self   : in out Server;
      Params : LSP.Messages.PublishDiagnosticsParams);

end LSP.Servers;

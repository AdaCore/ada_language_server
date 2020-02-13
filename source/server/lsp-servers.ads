------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2020, AdaCore                     --
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
--  This package provides a basic LSP server implementation.
--
--  The Server object is initializad with Request and Notification handlers,
--  that actually implements message processing.

with Ada.Containers.Indefinite_Holders;
with Ada.Streams;
with Ada.Exceptions;

with LSP.Client_Message_Receivers;
with LSP.Message_Loggers;
with LSP.Messages.Client_Requests;
with LSP.Messages.Server_Requests;
with LSP.Messages;
with LSP.Server_Backends;
with LSP.Server_Notification_Receivers;
with LSP.Server_Request_Handlers;
with LSP.Types;

with GNATCOLL.Traces;

private with Ada.Strings.Unbounded;
private with Ada.Containers.Hashed_Sets;
private with Ada.Containers.Synchronized_Queue_Interfaces;
private with Ada.Containers.Unbounded_Synchronized_Queues;
private with GNAT.Semaphores;
private with System;
private with LSP.Messages.Server_Notifications;

package LSP.Servers is

   type Server is limited
     new LSP.Client_Message_Receivers.Client_Message_Receiver
       with private;
   --  The representation of LSP server.
   --  Use methods of Client_Message_Receiver to send notifications and
   --  requests to the LSP client.

   procedure Initialize
     (Self   : in out Server;
      Stream : access Ada.Streams.Root_Stream_Type'Class);
   --  Initialize a server by providing input/output Stream.

   procedure Finalize (Self : in out Server);
   --  Clean up memory, file handles, tasks, etc.

   type Uncaught_Exception_Handler is access
     procedure (E : Ada.Exceptions.Exception_Occurrence);

   procedure Run
     (Self         : in out Server;
      Request      : not null
        LSP.Server_Request_Handlers.Server_Request_Handler_Access;
      Notification : not null
        LSP.Server_Notification_Receivers.Server_Notification_Receiver_Access;
      Server       : not null LSP.Server_Backends.Server_Backend_Access;
      On_Error     : not null Uncaught_Exception_Handler;
      Server_Trace : GNATCOLL.Traces.Trace_Handle;
      In_Trace     : GNATCOLL.Traces.Trace_Handle;
      Out_Trace    : GNATCOLL.Traces.Trace_Handle);
   --  Run the server using given Request and Notification handler.
   --  Server_Trace - main trace for the LSP.
   --  In_Trace and Out_Trace - traces that logs all input & output for
   --  debugging purposes. Call On_Error in case of uncaught exceptions.

   procedure Stop (Self : in out Server);
   --  Ask server to stop

   function Has_Look_Ahead_Message (Self : Server) return Boolean;
   --  Return True if there's a message in the look-ahead queue.

   function Look_Ahead_Message (Self : Server)
                                return LSP.Messages.Message'Class;
   --  Get next nessage in the queue if any. Only request/notification
   --  handlers are allowed to call this function.
   --  This call is valid only if Has_Look_Ahead_Message.

   function Input_Queue_Length (Self : Server) return Natural;
   --  Return number of messages pending in Input_Queue.
   --  For debug purposes only!

   function Request_Cancelled
     (Self    : Server;
      Request : LSP.Messages.Server_Requests.Server_Request'Class)
      return Boolean;
   --  Return True if the server has received the information that Request
   --  should be cancelled.

   overriding procedure On_Show_Message
     (Self   : access Server;
      Params : LSP.Messages.ShowMessageParams);

   overriding procedure On_Log_Message
     (Self   : access Server;
      Params : LSP.Messages.LogMessageParams);

   overriding procedure On_Publish_Diagnostics
     (Self   : access Server;
      Params : LSP.Messages.PublishDiagnosticsParams);

   overriding procedure On_Progress
     (Self   : access Server;
      Params : LSP.Messages.Progress_Params);

   overriding procedure On_ShowMessage_Request
     (Self    : access Server;
      Message : LSP.Messages.Client_Requests.ShowMessage_Request);

   overriding procedure On_Workspace_Apply_Edit_Request
     (Self    : access Server;
      Message : LSP.Messages.Client_Requests.Workspace_Apply_Edit_Request);

   overriding procedure On_Workspace_Configuration_Request
     (Self    : access Server;
      Message : LSP.Messages.Client_Requests.Workspace_Configuration_Request);

   function Has_Pending_Work (Self : Server) return Boolean;
   --  Return True if the server has work in the queue, other than the
   --  notification/request it's currently processing. This should only be
   --  called from the processing task.

private

   -------------------------
   --  Tasking in the ALS --
   -------------------------

   --  The server has 3 tasks:
   --    The input task
   --         This reads input coming from stdin, forms requests, and places
   --         them on the requests queue. It also destroys processed requests.
   --    The processing task:
   --         This is the task where libadalang lives. This task receives
   --         requests from the request queue, processes them, and returns
   --         the responses from them on the output queue.
   --    The output task:
   --         This task reads the responses coming from the output queue,
   --         and writes them to the standard output.
   --
   --  There are next flows of messages:
   --  * Notifications created by Input_Tast are processed and destroyed by
   --    Processing_Task.
   --  * Requests created by Input_Tast, processed by Processing_Task, but
   --    destroyed by Input_Task.
   --  * Messages created by Processing_Task are destroyed by Output_Task.
   --
   --  Because Input_Task controls life time of any request, it is able to
   --  mark it canceled

   type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;

   type Request_Access is
     access all LSP.Messages.Server_Requests.Server_Request'Class;

   type Notification_Access is
     access all LSP.Messages.Server_Notifications.Server_Notification'Class;

   use type LSP.Types.LSP_Number_Or_String;
   package Request_Id_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => LSP.Types.LSP_Number_Or_String,  --  Request id
      Hash                => LSP.Types.Hash,
      Equivalent_Elements => "=");

   package Message_Holders is new Ada.Containers.Indefinite_Holders
     (LSP.Messages.Message'Class,
      "=" => LSP.Messages."=");

   package Message_Queue_Interface is new
     Ada.Containers.Synchronized_Queue_Interfaces
       (Message_Holders.Holder);

   package Message_Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues
       (Message_Queue_Interface);

   type Input_Queue_Access is access Message_Queues.Queue;
   type Output_Queue_Access is access Message_Queues.Queue;

   --  The processing task
   task type Processing_Task_Type
     (Server : access LSP.Servers.Server)
   is
      entry Start
        (Request      : not null LSP.Server_Request_Handlers
           .Server_Request_Handler_Access;
         Notification : not null LSP.Server_Notification_Receivers
         .Server_Notification_Receiver_Access;
         Server       : not null LSP.Server_Backends.Server_Backend_Access);
      entry Stop;
      --  Clean shutdown of the task
   end Processing_Task_Type;

   --  The output task
   task type Output_Task_Type
     (Server : access LSP.Servers.Server)
   is
      entry Start;
      --  Start the task. Should be called once.

      entry Stop;
      --  Clean shutdown of the task. Can only be called after Start.
   end Output_Task_Type;

   --  The input task
   task type Input_Task_Type
     (Server : access LSP.Servers.Server)
   is
      entry Start;
      --  Start the task. Should be called once.

      entry Stop;
      --  Clean shutdown of the task. Can only be called after Start.
   end Input_Task_Type;

   type Server is limited
     new LSP.Client_Message_Receivers.Client_Message_Receiver with
   record
      Stop          : GNAT.Semaphores.Binary_Semaphore
                          (Initially_Available => False,
                           Ceiling => System.Default_Priority);
      --  Signal to main task to stop server. Released on "exit" message or
      --  on end of input stream.
      Stream        : access Ada.Streams.Root_Stream_Type'Class;
      Last_Request  : LSP.Types.LSP_Number := 1;
      Vector        : Ada.Strings.Unbounded.Unbounded_String;

      --  Queues and tasks used for asynchronous processing, see doc above
      Input_Queue     : Message_Queues.Queue;
      Look_Ahead      : Message_Holders.Holder;
      --  One message look-ahead buffer for Input_Queue
      Output_Queue    : Message_Queues.Queue;
      Processing_Task : Processing_Task_Type (Server'Unchecked_Access);
      Output_Task     : Output_Task_Type (Server'Unchecked_Access);
      Input_Task      : Input_Task_Type (Server'Unchecked_Access);

      Cancelled_Requests : Request_Id_Sets.Set;
      --  The IDs for the requests that have been cancelled

      Destroy_Queue   : Message_Queues.Queue;

      Server_Trace    : GNATCOLL.Traces.Trace_Handle;
      In_Trace        : GNATCOLL.Traces.Trace_Handle;
      Out_Trace       : GNATCOLL.Traces.Trace_Handle;
      Logger          : aliased LSP.Message_Loggers.Message_Logger;
      On_Error        : Uncaught_Exception_Handler;
   end record;

   Unknown_Method : exception;
   --  This exception is raised by message decoder when it's unable to decode
   --  an unknown request

   function Has_Look_Ahead_Message (Self : Server) return Boolean is
     (not Self.Look_Ahead.Is_Empty);

   function Request_Cancelled
     (Self    : Server;
      Request : LSP.Messages.Server_Requests.Server_Request'Class)
      return Boolean is
     (Self.Cancelled_Requests.Contains (Request.id));

end LSP.Servers;

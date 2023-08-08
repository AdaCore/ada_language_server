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
--  This package provides a basic LSP server implementation.
--
--  The Server object is initializad with Request and Notification handlers,
--  that actually implements message processing.

with Ada.Streams;

with LSP.Client_Message_Receivers;
with LSP.Server_Message_Visitors;

with LSP.Server_Messages;
with LSP.Tracers;

private with LSP.Client_Message_Factories;
private with LSP.Client_Messages;

private with Ada.Containers.Synchronized_Queue_Interfaces;
private with Ada.Containers.Unbounded_Synchronized_Queues;
private with GNAT.Semaphores;
private with System;
private with LSP.Server_Notifications;
private with LSP.Server_Requests;
private with VSS.Stream_Element_Vectors;

package LSP.Servers is

   type Server is limited
     new LSP.Client_Message_Receivers.Client_Message_Receiver
       with private;
   --  The representation of LSP server.
   --  Use methods of Client_Message_Receiver to send notifications, requests
   --  and responses to the LSP client.

   procedure Initialize
     (Self   : in out Server;
      Stream : access Ada.Streams.Root_Stream_Type'Class);
   --  Initialize a server by providing input/output Stream.

   procedure Finalize (Self : in out Server);
   --  Clean up memory, file handles, tasks, etc.

   type Server_Message_Visitor_Access is access all
     LSP.Server_Message_Visitors.Server_Message_Visitor'Class
       with Storage_Size => 0;

   procedure Run
     (Self         : in out Server;
      Handler      : not null Server_Message_Visitor_Access;
      Tracer       : not null LSP.Tracers.Tracer_Access);
   --  Run the server using given Request and Notification handler.
   --  Tracer object provides tracing/logging capabilities for the main trace,
   --  all input & output traces for debugging purposes.
   --  Call On_Error in case of uncaught exceptions.

   procedure Stop (Self : in out Server);
   --  Ask server to stop

   subtype Server_Message_Access is LSP.Server_Messages.Server_Message_Access;
   --  Message send by a client to a server

   function Look_Ahead_Message (Self : Server) return Server_Message_Access;
   --  Get next message in the queue if any. Only request/notification
   --  handlers are allowed to call this function.

   function Input_Queue_Length (Self : Server) return Natural;
   --  Return number of messages pending in Input_Queue.
   --  For debug purposes only!

   function Has_Pending_Work (Self : Server) return Boolean;
   --  Return True if the server has work in the queue, other than the
   --  notification/request it's currently processing. This should only be
   --  called from the processing task.

private

   -------------------------
   --  Tasking in the ALS --
   -------------------------

   --  The server has 4 tasks:
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
   --    The filesystem monitoring task:
   --         This tasks monitors the filesystem for any changes in source
   --         files
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
     access all LSP.Server_Requests.Server_Request'Class;

   type Notification_Access is
     access all LSP.Server_Notifications.Server_Notification'Class;

   package Input_Message_Queue_Interface is new
     Ada.Containers.Synchronized_Queue_Interfaces (Server_Message_Access);

   package Input_Message_Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues
       (Input_Message_Queue_Interface);

   type Input_Queue_Access is access Input_Message_Queues.Queue;

   subtype Client_Message_Access is LSP.Client_Messages.Client_Message_Access;
   --  Message send by a server to a client

   package Output_Message_Queue_Interface is new
     Ada.Containers.Synchronized_Queue_Interfaces (Client_Message_Access);

   package Output_Message_Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues
       (Output_Message_Queue_Interface);
   type Output_Queue_Access is access Output_Message_Queues.Queue;

   Processing_Task_Stack_Size : constant := 32 * 1_024 * 1_024;
   --  Size of the stack for request processing task. Set it to high enough
   --  value to prevent crashes on deep nesting calls inside LAL.

   --  The processing task
   task type Processing_Task_Type
     (Server : access LSP.Servers.Server)
     with Storage_Size => Processing_Task_Stack_Size
   is
      entry Start (Handler : not null Server_Message_Visitor_Access);
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

   ------------
   -- Server --
   ------------

   type Server is limited
     new LSP.Client_Message_Factories.Client_Message_Factory with
   record
      Stop_Signal   : GNAT.Semaphores.Binary_Semaphore
                          (Initially_Available => False,
                           Ceiling => System.Default_Priority);
      --  Signal to main task to stop server. Released on "exit" message or
      --  on end of input stream.
      Stream        : access Ada.Streams.Root_Stream_Type'Class;
      Last_Request  : Positive := 1;
      Vector        : VSS.Stream_Element_Vectors.Stream_Element_Vector;

      --  Queues and tasks used for asynchronous processing, see doc above
      Input_Queue     : Input_Message_Queues.Queue;
      Look_Ahead      : Server_Message_Access;
      --  One message look-ahead buffer for Input_Queue
      Output_Queue    : Output_Message_Queues.Queue;
      Processing_Task : Processing_Task_Type (Server'Unchecked_Access);
      Output_Task     : Output_Task_Type (Server'Unchecked_Access);
      Input_Task      : Input_Task_Type (Server'Unchecked_Access);
      Destroy_Queue   : Input_Message_Queues.Queue;

      Tracer          : LSP.Tracers.Tracer_Access;
   end record;

   overriding procedure On_Message
     (Self    : in out Server;
      Message : LSP.Client_Messages.Client_Message_Access);

   Unknown_Method : exception renames Program_Error;
   --  This exception is raised by message decoder when it's unable to decode
   --  an unknown request
end LSP.Servers;

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

with Ada.Characters.Latin_1;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding;
with Ada.Unchecked_Deallocation;
with GNAT.Traceback.Symbolic;    use GNAT.Traceback.Symbolic;

with LSP.JSON_Streams;
with LSP.Messages.Client_Notifications;
with LSP.Messages.Server_Notifications;
with LSP.Servers.Decode_Notification;
with LSP.Servers.Decode_Request;
with LSP.Servers.Handle_Request;

with GNATCOLL.JSON;

with Libadalang.Common;         use Libadalang.Common;

package body LSP.Servers is

   New_Line : constant String :=
     (Ada.Characters.Latin_1.CR, Ada.Characters.Latin_1.LF);

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
      return LSP.Types.LSP_String renames
       LSP.Types.To_LSP_String;

   procedure Process_One_Message
     (Self        : in out Server'Class;
      Initialized : in out Boolean;
      EOF         : in out Boolean);
   --  Read data from stdin and create a message if there is enough data.
   --  Then put the message into Self.Input_Queue.
   --  Handle initialization logic by tracking 'initialize' request, set
   --  Initialized parameter when the request arrives.
   --  Set EOF at end of stream or an "exit" notification.

   procedure Read_Number_Or_String
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out LSP.Types.LSP_Number_Or_String)
       renames LSP.Types.Read_Number_Or_String;

   procedure Append
     (Vector : in out Ada.Strings.Unbounded.Unbounded_String;
      Buffer : Ada.Streams.Stream_Element_Array);

   function To_Unbounded_String
    (Stream : in out LSP.JSON_Streams.JSON_Stream)
      return Ada.Strings.Unbounded.Unbounded_String;

   function To_Stream_Element_Array
     (Vector : Ada.Strings.Unbounded.Unbounded_String)
      return Ada.Streams.Stream_Element_Array;

   type Response_Access is access all LSP.Messages.ResponseMessage'Class;

   procedure Send_Response
     (Self       : in out Server'Class;
      Response   : in out Response_Access;
      Request_Id : LSP.Types.LSP_Number_Or_String);
   --  Complete Response and send it to the output queue. Response will be
   --  deleted by Output_Task

   procedure Send_Notification
     (Self  : in out Server'Class;
      Value : in out Message_Access);
   --  Send given notification to client. The Notification will be deleted by
   --  Output_Task

   procedure Send_Exception_Response
     (Self       : in out Server'Class;
      E          : Exception_Occurrence;
      Trace_Text : String;
      Request_Id : LSP.Types.LSP_Number_Or_String;
      Code       : LSP.Messages.ErrorCodes := LSP.Messages.InternalError);
   --  Send a response to the stream representing the exception. This
   --  should be called whenever an exception occurred while processing
   --  a request.
   --  Trace_Text is the additional info to write in the traces, and
   --  Request_Id is the id of the request we were trying to process.
   --  Use given Code in the response.

   procedure Send_Not_Initialized
     (Self       : in out Server'Class;
      Request_Id : LSP.Types.LSP_Number_Or_String);
   --  Send "not initialized" response

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => LSP.Messages.Message'Class,
      Name   => Message_Access);

   ------------
   -- Append --
   ------------

   procedure Append
     (Vector : in out Ada.Strings.Unbounded.Unbounded_String;
      Buffer : Ada.Streams.Stream_Element_Array) is
   begin
      for X of Buffer loop
         Ada.Strings.Unbounded.Append (Vector, Character'Val (X));
      end loop;
   end Append;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self         : in out Server;
      Stream       : access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Self.Stream := Stream;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Server) is
      use type Ada.Containers.Count_Type;

   begin
      --  The server has been asked to close. This could be (in particular in
      --  the case of the testsuite) because the input pipe has been closed.
      --  Wait here until all the requests have been consumed, and all the
      --  outputs have been flushed.
      while Self.Input_Queue.Current_Use > 0
        or else Self.Output_Queue.Current_Use > 0
      loop
         delay 0.1;
      end loop;

      Self.Processing_Task.Stop;
      Self.Output_Task.Stop;

      select
         --  Input task can be waiting reading from stream and won't accept
         --  Stop entry call. Let's wait a little and terminate process.
         Self.Input_Task.Stop;
      or
         delay 0.1;
      end select;
   end Finalize;

   -----------------
   -- Log_Message --
   -----------------

   overriding procedure On_Log_Message
     (Self   : access Server;
      Params : LSP.Messages.LogMessageParams)
   is
      Message : Message_Access :=
        new LSP.Messages.Client_Notifications.LogMessage_Notification'
           (method  => +"window/logMessage",
            params  => Params,
            jsonrpc => <>);
   begin
      Self.Send_Notification (Message);
   end On_Log_Message;

   -------------------------
   -- Process_One_Message --
   -------------------------

   procedure Process_One_Message
     (Self        : in out Server'Class;
      Initialized : in out Boolean;
      EOF         : in out Boolean)
   is
      use type Ada.Streams.Stream_Element_Count;

      procedure Parse_Header
        (Length : out Ada.Streams.Stream_Element_Count;
         Vector : in out Ada.Strings.Unbounded.Unbounded_String);
      --  Read lines from Vector and after it from Self.Stream until empty
      --  lines is found. For each non-empty line call Parse_Line.
      --  Return any unprocessed bytes in Vector.

      procedure Parse_Line
        (Line   : String;
         Length : in out Ada.Streams.Stream_Element_Count);
      --  If given Line is "Content-Length:" header then read Length from it.

      procedure Parse_JSON (Vector : Ada.Strings.Unbounded.Unbounded_String);
      --  Process Vector as complete JSON document.

      Buffer_Size : constant := 512;

      ------------------
      -- Parse_Header --
      ------------------

      procedure Parse_Header
        (Length : out Ada.Streams.Stream_Element_Count;
         Vector : in out Ada.Strings.Unbounded.Unbounded_String)
      is
         Buffer : Ada.Streams.Stream_Element_Array (1 .. Buffer_Size);
         Last   : Ada.Streams.Stream_Element_Count :=
           Ada.Streams.Stream_Element_Count
             (Ada.Strings.Unbounded.Length (Vector));
         Line   : String (1 .. 80) := (others => ' ');
         Char   : Character;
         Index  : Natural := 0;
         Empty  : Boolean := False;  --  We've just seen CR, LF
      begin
         if Last > 0 then
            --  Copy unprocessed bytes to Buffer
            Buffer (1 .. Last) := To_Stream_Element_Array (Vector);
            Vector := Ada.Strings.Unbounded.Null_Unbounded_String;
         end if;

         Length := 0;

         --  Process any unprocessed bytes in the loop reading data as needed.
         loop
            --  Collect line characters into Line (1 .. Index)
            for J in 1 .. Last loop
               Char := Character'Val (Buffer (J));

               if Char not in Ada.Characters.Latin_1.CR
                                | Ada.Characters.Latin_1.LF
               then
                  Empty := False;  --  No CR, LF yet
               end if;

               if Index = Line'Last then
                  --  Too long line, drop it keeping last character
                  Line (1) := Line (Line'Last);
                  Index := 2;
               else
                  Index := Index + 1;
               end if;

               Line (Index) := Char;

               if Index > 1 and then Line (Index - 1 .. Index) = New_Line then
                  if Empty then
                     --  Put any unprocessed bytes back into Vector and exit
                     Append (Vector, Buffer (J + 1 .. Last));
                     return;
                  end if;

                  Empty := True;
                  Parse_Line (Line (1 .. Index - 2), Length);
               end if;
            end loop;

            --  We have processed whole Buffer, so read next data into it.
            Self.Stream.Read (Buffer, Last);
         end loop;
      end Parse_Header;

      ----------------
      -- Parse_JSON --
      ----------------

      procedure Parse_JSON (Vector : Ada.Strings.Unbounded.Unbounded_String) is
         use type LSP.Types.LSP_String;
      begin
         if Is_Active (In_Trace) then
            --  Avoid expensive convertion to string when trace is off
            Trace (In_Trace, To_String (Vector));
         end if;

         declare
            --  Parse JSON now
            Document : constant GNATCOLL.JSON.JSON_Value :=
              GNATCOLL.JSON.Read (Vector);

            Message    : Message_Access;
            JS         : aliased LSP.JSON_Streams.JSON_Stream;
            JSON_Array : GNATCOLL.JSON.JSON_Array;
            Version    : LSP.Types.LSP_String;
            Method     : LSP.Types.Optional_String;
            Request_Id : LSP.Types.LSP_Number_Or_String;
            Error      : LSP.Messages.Optional_ResponseError;
         begin
            --  Read request id and method if any
            GNATCOLL.JSON.Append (JSON_Array, Document);
            JS.Set_JSON_Document (JSON_Array);
            JS.Start_Object;
            Read_Number_Or_String (JS, +"id", Request_Id);
            LSP.Types.Read_String (JS, +"jsonrpc", Version);
            LSP.Types.Read_Optional_String (JS, +"method", Method);

            --  Decide if this is a request, response or notification

            if not Method.Is_Set then
               --  TODO: Process client responses here.

               JS.Key ("error");
               LSP.Messages.Optional_ResponseError'Read (JS'Access, Error);

               if Error.Is_Set then
                  --  We have got error from LSP client. Save it in the trace:
                  Server_Trace.Trace ("Got Error response:");

                  Server_Trace.Trace
                    (LSP.Types.To_UTF_8_String (Error.Value.message));
               end if;

               return;

            elsif LSP.Types.Assigned (Request_Id) then  --  This is a request

               if not Initialized then
                  if Method.Value = +"initialize" then
                     Initialized := True;
                  else
                     Send_Not_Initialized (Self, Request_Id);
                     return;
                  end if;
               end if;

               begin
                  Message := new LSP.Messages.RequestMessage'Class'
                    (LSP.Servers.Decode_Request (Document));
               exception
                  when E : others =>
                     --  If we reach this exception handler, this means the
                     --  request could not be decoded.
                     Send_Exception_Response
                       (Self, E,
                        To_String (Vector),
                        Request_Id,
                        LSP.Messages.InvalidParams);

                     return;
               end;

            elsif Initialized
              or else Method.Value = +"exit"
            then
               --  This is a notification
               Message := new LSP.Messages.NotificationMessage'Class'
                 (LSP.Servers.Decode_Notification (Document));

            else
               --  Ignore any notification (except 'exit') until initialization
               return;

            end if;

            --  Now we have a message to process. Push it to the processing
            --  task
            Self.Input_Queue.Enqueue (Message);

            if Message.all in
              LSP.Messages.Server_Notifications.Exit_Notification
            then
               --  After "exit" notification don't read any further input.
               EOF := True;
            end if;
         exception
            when E : others =>
               --  Something goes wrong after JSON parsing

               GNATCOLL.Traces.Trace
                 (LSP.Server_Trace,
                  "Unexpected exception when processing a message:");
               Server_Trace.Trace (Symbolic_Traceback (E));

         end;

      exception
         when E : others =>
            --  If we reach this exception handler, this means we are unable
            --  to parse text as JSON.

            GNATCOLL.Traces.Trace
              (LSP.Server_Trace,
               "Unable to parse JSON message:" & To_String (Vector));
            Server_Trace.Trace (Symbolic_Traceback (E));

      end Parse_JSON;

      ----------------
      -- Parse_Line --
      ----------------

      procedure Parse_Line
        (Line   : String;
         Length : in out Ada.Streams.Stream_Element_Count)
      is
         Content_Length : constant String := "Content-Length:";
      begin
         if Line'Length > Content_Length'Length and then
           Line (Content_Length'Range) = Content_Length
         then
            Length := Ada.Streams.Stream_Element_Count'Value
              (Line (Content_Length'Length + 2 - Line'First .. Line'Last));
         end if;
      end Parse_Line;

      Vector : Ada.Strings.Unbounded.Unbounded_String := Self.Vector;
      Length : Ada.Streams.Stream_Element_Count := 0;  --  Message length
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Buffer_Size);
      Last   : Ada.Streams.Stream_Element_Count;  --  Index the Buffer
   begin
      Parse_Header (Length, Vector);  --  Find Length out of headers

      --  Populate Buffer with Vector content
      Last := Ada.Streams.Stream_Element_Count
        (Ada.Strings.Unbounded.Length (Vector));
      Buffer (1 .. Last) := To_Stream_Element_Array (Vector);
      Vector := Ada.Strings.Unbounded.Null_Unbounded_String;

      loop
         if Last <= Length then
            --  Part of message or exact one message
            Append (Vector, Buffer (1 .. Last));
            Length := Length - Last;
            Last := 0;
         else
            --  Complete message and some extra data after it
            Append (Vector, Buffer (1 .. Length));
            Last := Last - Length;  --  Extra bytes
            Buffer (1 .. Last) := Buffer (Length + 1 .. Length + Last);
            Length := 0;
         end if;

         if Length = 0 then
            --  Complete message is ready in the Vector
            --  Copy extra data if any into Vector and exit
            Self.Vector := Ada.Strings.Unbounded.Null_Unbounded_String;
            Append (Self.Vector, Buffer (1 .. Last));
            Parse_JSON (Vector);
            Vector := Self.Vector;
            exit;
         else
            Self.Stream.Read (Buffer, Last);
         end if;
      end loop;

   exception
      when Ada.IO_Exceptions.End_Error =>
         EOF := True;

      when E : others =>
         --  Catch-all case: make sure no exception in output writing
         --  can cause an exit of the task loop.
         GNATCOLL.Traces.Trace
           (LSP.Server_Trace,
            "Exception when reading input:" & ASCII.LF
            & Exception_Name (E) & " - " &  Exception_Message (E));
         Server_Trace.Trace (Symbolic_Traceback (E));
   end Process_One_Message;

   -------------------------
   -- Publish_Diagnostics --
   -------------------------

   overriding procedure On_Publish_Diagnostics
     (Self   : access Server;
      Params : LSP.Messages.PublishDiagnosticsParams)
   is
      Message : Message_Access :=
        new LSP.Messages.Client_Notifications.PublishDiagnostics_Notification'
          (jsonrpc => <>,
           method  => +"textDocument/publishDiagnostics",
           params  => Params);
   begin
      Self.Send_Notification (Message);
   end On_Publish_Diagnostics;

   ---------
   -- Run --
   ---------

   procedure Run
     (Self         : in out Server;
      Request      : not null
        LSP.Server_Request_Handlers.Server_Request_Handler_Access;
      Notification : not null
        LSP.Server_Notification_Receivers.Server_Notification_Receiver_Access)
   is
   begin
      Self.Processing_Task.Start (Request, Notification);
      Self.Output_Task.Start;
      Self.Input_Task.Start;

      --  Wait for stop signal
      Self.Stop.Seize;
   end Run;

   -----------------------------
   -- Send_Exception_Response --
   -----------------------------

   procedure Send_Exception_Response
     (Self       : in out Server'Class;
      E          : Exception_Occurrence;
      Trace_Text : String;
      Request_Id : LSP.Types.LSP_Number_Or_String;
      Code       : LSP.Messages.ErrorCodes := LSP.Messages.InternalError)
   is
      Exception_Text : constant String :=
        Exception_Name (E) & ASCII.LF & Symbolic_Traceback (E);
      Response       : Response_Access :=
        new LSP.Messages.ResponseMessage'
          (Is_Error => True,
           jsonrpc  => <>,  --  we will set this latter
           id       => <>,  --  we will set this latter
           error    =>
             (Is_Set => True,
              Value =>
                (code => Code,
                 data => GNATCOLL.JSON.Create_Object,
                 message => LSP.Types.To_LSP_String
                   (Exception_Text))));
   begin
      --  Send the response to the output stream
      Send_Response (Self, Response, Request_Id);

      --  Log details in the traces
      GNATCOLL.Traces.Trace
        (LSP.Server_Trace,
         "Exception when processing request:" & ASCII.LF
         & Trace_Text & ASCII.LF
         & Exception_Text);
      Server_Trace.Trace (Symbolic_Traceback (E));
   end Send_Exception_Response;

   --------------------------
   -- Send_Not_Initialized --
   --------------------------

   procedure Send_Not_Initialized
     (Self       : in out Server'Class;
      Request_Id : LSP.Types.LSP_Number_Or_String)
   is
      Response : Response_Access := new LSP.Messages.ResponseMessage'
        (Is_Error => True,
         jsonrpc  => <>,  --  we will set this latter
         id       => <>,  --  we will set this latter
         error    =>
           (Is_Set => True,
            Value  => (code    => LSP.Messages.ServerNotInitialized,
                       message => +"No initialize request was received",
                       others  => <>)));
   begin
      Send_Response (Self, Response, Request_Id);
   end Send_Not_Initialized;

   -----------------------
   -- Send_Notification --
   -----------------------

   procedure Send_Notification
     (Self  : in out Server'Class;
      Value : in out Message_Access)
   is
   begin
      Value.jsonrpc := +"2.0";
      Self.Output_Queue.Enqueue (Value);
      Value := null;
   end Send_Notification;

   -------------------
   -- Send_Response --
   -------------------

   procedure Send_Response
     (Self       : in out Server'Class;
      Response   : in out Response_Access;
      Request_Id : LSP.Types.LSP_Number_Or_String) is
   begin
      Response.jsonrpc := +"2.0";
      Response.id := Request_Id;
      Self.Output_Queue.Enqueue (Message_Access (Response));
      Response := null;
   end Send_Response;

   ------------------
   -- Show_Message --
   ------------------

   overriding procedure On_Show_Message
     (Self   : access Server;
      Params : LSP.Messages.ShowMessageParams)
   is
      Message : Message_Access :=
        new LSP.Messages.Client_Notifications.ShowMessage_Notification'
          (jsonrpc => <>,
           method  => +"window/showMessage",
           params  => Params);
   begin
      Self.Send_Notification (Message);
   end On_Show_Message;

   ----------
   -- Stop --
   ----------

   procedure Stop (Self : in out Server) is
   begin
      Self.Stop.Release;
   end Stop;

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------

   function To_Stream_Element_Array
     (Vector : Ada.Strings.Unbounded.Unbounded_String)
      return Ada.Streams.Stream_Element_Array
   is
      Last : constant Ada.Streams.Stream_Element_Count :=
        Ada.Streams.Stream_Element_Count
          (Ada.Strings.Unbounded.Length (Vector));
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Last);
   begin
      for J in 1 .. Last loop
         Buffer (J) := Character'Pos
           (Ada.Strings.Unbounded.Element (Vector, Positive (J)));
      end loop;

      return Buffer;
   end To_Stream_Element_Array;

   -------------------------
   -- To_Unbounded_String --
   -------------------------

   function To_Unbounded_String
    (Stream : in out LSP.JSON_Streams.JSON_Stream)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      Document    : constant GNATCOLL.JSON.JSON_Array :=
        Stream.Get_JSON_Document;
      JSON_Object : constant GNATCOLL.JSON.JSON_Value :=
        GNATCOLL.JSON.Get (Document, 1);
   begin
      return GNATCOLL.JSON.Write (JSON_Object);
   end To_Unbounded_String;

   ---------------------
   -- Input_Task_Type --
   ---------------------

   task body Input_Task_Type is
      Initialized : Boolean := False;
      EOF : Boolean := False;
   begin
      accept Start;

      loop
         select
            accept Stop;
            exit;
         else
            Server.Process_One_Message (Initialized, EOF);
            --  This call can block reading from stream

            if EOF then
               --  Signal main task to stop the server
               LSP.Servers.Stop (Server.all);

               accept Stop;
               exit;
            end if;
         end select;
      end loop;
   end Input_Task_Type;

   ----------------------
   -- Output_Task_Type --
   ----------------------

   task body Output_Task_Type is
      Message : Message_Access;
      Stream : access Ada.Streams.Root_Stream_Type'Class renames Server.Stream;

      Output_Queue : Output_Queues.Queue renames Server.Output_Queue;

      procedure Write_JSON_RPC
        (Stream : access Ada.Streams.Root_Stream_Type'Class;
         Vector : Ada.Strings.Unbounded.Unbounded_String);
      --  Format Vector into a protocol string including the header,
      --  and send it to Stream.

      procedure Write_JSON_RPC
        (Stream : access Ada.Streams.Root_Stream_Type'Class;
         Vector : Ada.Strings.Unbounded.Unbounded_String)
      is
         Image  : constant String := Positive'Image
           (Ada.Strings.Unbounded.Length (Vector));
         Header : constant String := "Content-Length:" & Image
           & New_Line & New_Line;
      begin
         String'Write (Stream, Header);
         String'Write (Stream, Ada.Strings.Unbounded.To_String (Vector));

         if Is_Active (Out_Trace) then
            --  Avoid expensive convertion to string when trace is off
            Trace (Out_Trace, To_String (Vector));
         end if;
      end Write_JSON_RPC;

   begin
      accept Start;

      loop
         select
            --  Process all available outputs before acceptiong Stop
            Output_Queue.Dequeue (Message);

            declare
               Out_Stream : aliased LSP.JSON_Streams.JSON_Stream;
               Output     : Ada.Strings.Unbounded.Unbounded_String;
            begin
               LSP.Messages.Message'Class'Write
                 (Out_Stream'Access, Message.all);
               Free (Message);

               Output := To_Unbounded_String (Out_Stream);
               --  Send the output to the stream
               Write_JSON_RPC (Stream, Output);
            exception
               when E : others =>
                  --  Catch-all case: make sure no exception in output writing
                  --  can cause an exit of the task loop.
                  GNATCOLL.Traces.Trace
                    (LSP.Server_Trace,
                     "Exception when writing output:" & ASCII.LF
                     & To_String (Output) & ASCII.LF
                     & Exception_Name (E) & " - " &  Exception_Message (E));
                  Server_Trace.Trace (Symbolic_Traceback (E));

            end;
         or
            delay 0.1;

            --  If no output during some timeout, then check for Stop signal

            select
               accept Stop;
               exit;
            else
               null;
            end select;
         end select;
      end loop;
   end Output_Task_Type;

   --------------------------
   -- Processing_Task_Type --
   --------------------------

   task body Processing_Task_Type is
      Request : Message_Access;

      Req_Handler : LSP.Server_Request_Handlers.Server_Request_Handler_Access;

      Notif_Handler :
        LSP.Server_Notification_Receivers.Server_Notification_Receiver_Access;

      Input_Queue   : Input_Queues.Queue renames Server.Input_Queue;
      Output_Queue  : Output_Queues.Queue renames Server.Output_Queue;

      procedure Initialize
        (Request      : not null LSP.Server_Request_Handlers
           .Server_Request_Handler_Access;
         Notification : not null LSP.Server_Notification_Receivers
           .Server_Notification_Receiver_Access);
      --  Initializes internal data structures

      procedure Process_Message (Message : Message_Access);

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Request      : not null LSP.Server_Request_Handlers
           .Server_Request_Handler_Access;
         Notification : not null LSP.Server_Notification_Receivers
           .Server_Notification_Receiver_Access)
      is
      begin
         Req_Handler := Request;
         Notif_Handler := Notification;
      end Initialize;

      ---------------------
      -- Process_Message --
      ---------------------

      procedure Process_Message (Message : Message_Access) is
      begin
         if Message.all in
           LSP.Messages.Server_Notifications.Server_Notification'Class
         then
            --  This is a notification
            LSP.Messages.Server_Notifications.Server_Notification'Class
              (Message.all).Visit (Notif_Handler);

            return;
         end if;

         declare
            --  This is a request
            Request    : LSP.Messages.RequestMessage'Class renames
              LSP.Messages.RequestMessage'Class (Message.all);
         begin
            --  Nest a declare block so we can catch specifically an
            --  exception raised during the processing of a request.
            declare
               Response : constant Message_Access :=
                 new LSP.Messages.ResponseMessage'Class'
                   (LSP.Servers.Handle_Request (Req_Handler, Request));
            begin
               Output_Queue.Enqueue (Response);
               --  Response will be deleted by Output_Task
            end;

         exception
            when E : others =>
               --  If we reach this exception handler, this means an exception
               --  was raised when processing the request.
               Send_Exception_Response
                 (Server.all, E, "To_String (Vector)", Request.id);
         end;
      end Process_Message;

   begin
      --  Perform initialization
      accept Start
        (Request      : not null LSP.Server_Request_Handlers
           .Server_Request_Handler_Access;
         Notification : not null LSP.Server_Notification_Receivers
           .Server_Notification_Receiver_Access)
      do
         Initialize (Request, Notification);
      end Start;

      loop
         select
            --  Process all available requests before acceptiong Stop
            Input_Queue.Dequeue (Request);

            --  Process the request
            begin
               Process_Message (Request);
               Free (Request);
            exception
               when Ada.IO_Exceptions.End_Error =>
                  Server_Trace.Trace ("Received EOF.");
                  exit;

               when E : others =>
                  --  Catch-all case: make sure no exception in any request
                  --  processing can cause an exit of the task main loop.

                  --  Send the response to the stream. If we reach this
                  --  exception handler, this means the request could be
                  --  decoded, but an exception was raised when processing it.
                  Send_Exception_Response
                    (Server.all, E,
                     "To_String (Request)",
                     (Is_Number => False,
                      String    => LSP.Types.Empty_LSP_String));

                  --  Property errors are expected to happen in the normal flow
                  --  of events in LAL. However, for any other error than a
                  --  property error, we want to reload the context.
                  if Exception_Identity (E) /= Property_Error'Identity then
                     Req_Handler.Handle_Error;
                  end if;
                  --  ... and log this in the traces
            end;
         or
            delay 0.1;

            --  If no request during some timeout, then check for Stop signal

            select
               accept Stop;
               exit;
            else
               null;
            end select;

         end select;
      end loop;
   end Processing_Task_Type;

end LSP.Servers;

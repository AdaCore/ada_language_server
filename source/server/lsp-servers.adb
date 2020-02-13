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

with Ada.Characters.Latin_1;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding;
with Ada.Tags;
with Ada.Task_Identification;
with Ada.Unchecked_Deallocation;
with GNAT.Traceback.Symbolic;    use GNAT.Traceback.Symbolic;

with LSP.Errors;
with LSP.JSON_Streams;
with LSP.Messages.Client_Notifications;
with LSP.Servers.Decode_Notification;
with LSP.Servers.Decode_Request;
with LSP.Servers.Handle_Request;

with GNATCOLL.JSON;
with GNATCOLL.Traces;           use GNATCOLL.Traces;

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

   type Client_Request_Access is
     access all LSP.Messages.Client_Requests.Client_Request'Class;

   procedure Send_Request
     (Self   : in out Server'Class;
      Method : String;
      Value  : LSP.Messages.Client_Requests.Client_Request'Class);
   --  Assign Method to the request and send it to the client.

   procedure Send_Exception_Response
     (Self       : in out Server'Class;
      E          : Exception_Occurrence;
      Trace_Text : String;
      Request_Id : LSP.Types.LSP_Number_Or_String;
      Code       : LSP.Messages.ErrorCodes := LSP.Errors.InternalError);
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

   procedure Send_Canceled_Request
     (Self       : in out Server'Class;
      Request_Id : LSP.Types.LSP_Number_Or_String);
   --  Send RequestCancelled response

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

      procedure Process_JSON_Document
        (Document : GNATCOLL.JSON.JSON_Value;
         Vector   : Ada.Strings.Unbounded.Unbounded_String);
      --  Process one JSON message. Vector is corresponding text for traces.

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

      ---------------------------
      -- Process_JSON_Document --
      ---------------------------

      procedure Process_JSON_Document
        (Document : GNATCOLL.JSON.JSON_Value;
         Vector   : Ada.Strings.Unbounded.Unbounded_String)
      is
         use type LSP.Types.LSP_String;

         Message      : Message_Access;
         Request      : Request_Access;
         Notification : Notification_Access;

         Is_Exit_Notification : Boolean;

         JS         : aliased LSP.JSON_Streams.JSON_Stream (True);
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
               Self.Server_Trace.Trace ("Got Error response:");

               Self.Server_Trace.Trace
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
               Request :=
                 new LSP.Messages.Server_Requests.Server_Request'Class'
                   (LSP.Servers.Decode_Request (Document));
            exception
               when UR : Unknown_Method =>
                  Send_Exception_Response
                    (Self, UR,
                     To_String (Vector),
                     Request_Id,
                     LSP.Errors.MethodNotFound);
                  return;

               when E : others =>
                  --  If we reach this exception handler, this means the
                  --  request could not be decoded.
                  Send_Exception_Response
                    (Self, E,
                     To_String (Vector),
                     Request_Id,
                     LSP.Errors.InvalidParams);
                  return;
            end;

            Self.Request_Map.Include (Request_Id, Request);

            Message := Message_Access (Request);

         elsif Initialized
           or else Method.Value = +"exit"
         then
            --  This is a notification
            begin
               Notification :=
                 new Messages.Server_Notifications.Server_Notification'Class'
                   (LSP.Servers.Decode_Notification (Document));
            exception
               when E : Unknown_Method =>
                  Self.Server_Trace.Trace
                    ("Unable to decode notification: "
                     & Symbolic_Traceback (E));
                  return;
            end;

            --  Process '$/cancelRequest' notification
            if Notification.all in
              LSP.Messages.Server_Notifications.Cancel_Notification
            then
               Request_Id :=
                 LSP.Messages.Server_Notifications.Cancel_Notification
                   (Notification.all).params.id;

               if Self.Request_Map.Contains (Request_Id) then
                  Self.Request_Map (Request_Id).Canceled := True;
               end if;
            end if;

            Message := Message_Access (Notification);
         else
            --  Ignore any notification (except 'exit') until initialization
            return;

         end if;

         Self.Logger.Visit (Message.all);

         --  Check whether this was an exit notification. Note: this must be
         --  done *before* the call to Enqueue, since we're not guaranteed
         --  that the memory for Message is still allocated after this call.

         Is_Exit_Notification :=  Message.all in
           LSP.Messages.Server_Notifications.Exit_Notification;

         --  Now we have a message to process. Push it to the processing
         --  task
         Self.Input_Queue.Enqueue (Message);

         if Is_Exit_Notification then
            --  After "exit" notification don't read any further input.
            EOF := True;
         end if;
      end Process_JSON_Document;

      ----------------
      -- Parse_JSON --
      ----------------

      procedure Parse_JSON (Vector : Ada.Strings.Unbounded.Unbounded_String) is
      begin
         if Self.In_Trace.Is_Active then
            --  Avoid expensive convertion to string when trace is off
            Self.In_Trace.Trace (To_String (Vector));
         end if;

         declare
            --  Parse JSON now
            Document : constant GNATCOLL.JSON.JSON_Value :=
              GNATCOLL.JSON.Read (Vector);

         begin
            Process_JSON_Document (Document, Vector);
         exception
            when E : others =>
               --  Something goes wrong after JSON parsing

               Self.Server_Trace.Trace
                 ("Unexpected exception when processing a message:");
               Self.Server_Trace.Trace (Symbolic_Traceback (E));

         end;

      exception
         when E : others =>
            --  If we reach this exception handler, this means we are unable
            --  to parse text as JSON.

            Self.Server_Trace.Trace
              ("Unable to parse JSON message:" & To_String (Vector));
            Self.Server_Trace.Trace (Symbolic_Traceback (E));

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
         Self.Server_Trace.Trace
           ("Exception when reading input:" & ASCII.LF
            & Exception_Name (E) & " - " &  Exception_Message (E));
         Self.Server_Trace.Trace (Symbolic_Traceback (E));
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

   -----------------
   -- On_Progress --
   -----------------

   overriding procedure On_Progress
     (Self   : access Server;
      Params : LSP.Messages.Progress_Params)
   is
      Message : Message_Access :=
        new LSP.Messages.Client_Notifications.Progress_Notification'
          (jsonrpc => <>,
           method  => +"$/progress",
           params  => Params);
   begin
      Self.Send_Notification (Message);
   end On_Progress;

   ----------------------------
   -- On_ShowMessage_Request --
   ----------------------------

   overriding procedure On_ShowMessage_Request
     (Self    : access Server;
      Message : LSP.Messages.Client_Requests.ShowMessage_Request) is
   begin
      Self.Send_Request ("window/showMessageRequest", Message);
   end On_ShowMessage_Request;

   -------------------------------------
   -- On_Workspace_Apply_Edit_Request --
   -------------------------------------

   overriding procedure On_Workspace_Apply_Edit_Request
     (Self    : access Server;
      Message : LSP.Messages.Client_Requests.Workspace_Apply_Edit_Request) is
   begin
      Self.Send_Request ("workspace/applyEdit", Message);
   end On_Workspace_Apply_Edit_Request;

   ----------------------------------------
   -- On_Workspace_Configuration_Request --
   ----------------------------------------

   overriding procedure On_Workspace_Configuration_Request
     (Self    : access Server;
      Message : LSP.Messages.Client_Requests.Workspace_Configuration_Request)
   is
   begin
      Self.Send_Request ("workspace/configuration", Message);
   end On_Workspace_Configuration_Request;

   ---------
   -- Run --
   ---------

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
      Out_Trace    : GNATCOLL.Traces.Trace_Handle)
   is
   begin
      Self.Server_Trace := Server_Trace;
      Self.In_Trace     := In_Trace;
      Self.Out_Trace    := Out_Trace;
      Self.On_Error     := On_Error;

      Self.Logger.Initialize (Server_Trace);

      Self.Processing_Task.Start (Request, Notification, Server);
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
      Code       : LSP.Messages.ErrorCodes := LSP.Errors.InternalError)
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
                (code    => Code,
                 data    => LSP.Types.Empty,
                 message => LSP.Types.To_LSP_String (Exception_Text))));
   begin
      --  Send the response to the output stream
      Send_Response (Self, Response, Request_Id);

      --  Log details in the traces
      Self.Server_Trace.Trace
        ("Exception when processing request:" & ASCII.LF
         & Trace_Text & ASCII.LF
         & Exception_Text);
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
            Value  => (code    => LSP.Errors.ServerNotInitialized,
                       message => +"No initialize request was received",
                       others  => <>)));
   begin
      Send_Response (Self, Response, Request_Id);
   end Send_Not_Initialized;

   procedure Send_Canceled_Request
     (Self       : in out Server'Class;
      Request_Id : LSP.Types.LSP_Number_Or_String)
   is
      Response : Response_Access := new LSP.Messages.ResponseMessage'
        (Is_Error => True,
         jsonrpc  => <>,  --  we will set this latter
         id       => <>,  --  we will set this latter
         error    =>
           (Is_Set => True,
            Value  => (code    => LSP.Errors.RequestCancelled,
                       message => +"Request was canceled",
                       others  => <>)));
   begin
      Send_Response (Self, Response, Request_Id);
   end Send_Canceled_Request;

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

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Self   : in out Server'Class;
      Method : String;
      Value  : LSP.Messages.Client_Requests.Client_Request'Class)
   is
      Message : constant Client_Request_Access :=
        new LSP.Messages.Client_Requests.Client_Request'Class'(Value);
      --  The Message will be deleted by Output_Task
   begin
      Message.jsonrpc := +"2.0";
      Self.Last_Request := Self.Last_Request + 1;
      Message.id := (Is_Number => True, Number => Self.Last_Request);
      Message.method := LSP.Types.To_LSP_String (Method);
      Self.Output_Queue.Enqueue (Message_Access (Message));
   end Send_Request;

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

   ------------------------
   -- Input_Queue_Length --
   ------------------------

   function Input_Queue_Length (Self : Server) return Natural is
      Result : Natural := Natural (Self.Input_Queue.Current_Use);
   begin
      if Self.Look_Ahead /= null then
         Result := Result + 1;  --  One extra message in the look ahead buffer
      end if;

      return Result;
   end Input_Queue_Length;

   ---------------------
   -- Input_Task_Type --
   ---------------------

   task body Input_Task_Type is
      Initialized : Boolean := False;
      EOF         : Boolean := False;
      Message     : Message_Access;
   begin
      accept Start;

      loop
         loop
            --  Destroy any processed request
            select
               --  Process all available outputs before acceptiong Stop
               Server.Destroy_Queue.Dequeue (Message);
               Server.Request_Map.Delete (Request_Access (Message).id);
               Free (Message);

            else
               exit;
            end select;
         end loop;

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

      --  Memory cleanup: remove everything from Destroy_Queue before
      --  leaving this task.
      while Natural (Server.Destroy_Queue.Current_Use) > 0 loop
         Server.Destroy_Queue.Dequeue (Message);
         Server.Request_Map.Delete (Request_Access (Message).id);
         Free (Message);
      end loop;
   end Input_Task_Type;

   ----------------------
   -- Output_Task_Type --
   ----------------------

   task body Output_Task_Type is
      Message : Message_Access;
      Stream : access Ada.Streams.Root_Stream_Type'Class renames Server.Stream;

      Output_Queue : Message_Queues.Queue renames Server.Output_Queue;

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

         if Server.Out_Trace.Is_Active then
            --  Avoid expensive convertion to string when trace is off
            Server.Out_Trace.Trace (To_String (Vector));
         end if;
      end Write_JSON_RPC;

   begin
      accept Start;

      loop
         select
            --  Process all available outputs before acceptiong Stop
            Output_Queue.Dequeue (Message);
            Server.Logger.Visit (Message.all);

            declare
               Out_Stream : aliased LSP.JSON_Streams.JSON_Stream (True);
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
                  Server.Server_Trace.Trace
                    ("Exception when writing output:" & ASCII.LF
                     & To_String (Output) & ASCII.LF
                     & Exception_Name (E) & " - " &  Exception_Message (E));
                  Server.Server_Trace.Trace (Symbolic_Traceback (E));

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

      Req_Handler : LSP.Server_Request_Handlers.Server_Request_Handler_Access;

      Notif_Handler :
        LSP.Server_Notification_Receivers.Server_Notification_Receiver_Access;
      Server_Backend : LSP.Server_Backends.Server_Backend_Access;

      Input_Queue   : Message_Queues.Queue renames Server.Input_Queue;
      Output_Queue  : Message_Queues.Queue renames Server.Output_Queue;

      procedure Initialize
        (Request      : not null LSP.Server_Request_Handlers
           .Server_Request_Handler_Access;
         Notification : not null LSP.Server_Notification_Receivers
           .Server_Notification_Receiver_Access;
         Server       : not null LSP.Server_Backends.Server_Backend_Access);
      --  Initializes internal data structures

      procedure Process_Message (Message : in out Message_Access);

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Request      : not null LSP.Server_Request_Handlers
           .Server_Request_Handler_Access;
         Notification : not null LSP.Server_Notification_Receivers
           .Server_Notification_Receiver_Access;
         Server       : not null LSP.Server_Backends.Server_Backend_Access)
      is
      begin
         Req_Handler := Request;
         Notif_Handler := Notification;
         Server_Backend := Server;
      end Initialize;

      ---------------------
      -- Process_Message --
      ---------------------

      procedure Process_Message (Message : in out Message_Access) is
      begin
         if Message.all in
           LSP.Messages.Server_Notifications.Server_Notification'Class
         then
            --  This is a notification

            Server_Backend.Before_Work (Message.all);
            LSP.Messages.Server_Notifications.Server_Notification'Class
              (Message.all).Visit (Notif_Handler);
            Server_Backend.After_Work (Message.all);

            Free (Message);

            return;
         end if;

         declare
            --  This is a request
            Request : LSP.Messages.Server_Requests.Server_Request'Class renames
              LSP.Messages.Server_Requests.Server_Request'Class (Message.all);
         begin

            if Request.Canceled then
               --  The request has been canceled
               Server.Send_Canceled_Request (Request.id);
               Server.Destroy_Queue.Enqueue (Message);
               --  Request will be deleted by Input_Task
               return;
            end if;

            Server_Backend.Before_Work (Message.all);
            declare
               Response : constant Message_Access :=
                 new LSP.Messages.ResponseMessage'Class'
                   (LSP.Servers.Handle_Request (Req_Handler, Request));
            begin
               Output_Queue.Enqueue (Response);
               --  Response will be deleted by Output_Task
               Server.Destroy_Queue.Enqueue (Message);
               --  Request will be deleted by Input_Task
            end;
            Server_Backend.After_Work (Message.all);

         exception
            --  If we reach this exception handler, this means an exception
            --  was raised when processing the request.
            --
            when E : others =>
               Send_Exception_Response
                 (Server.all, E,
                  Ada.Tags.External_Tag (Message'Tag), Request.id);
               Server.Destroy_Queue.Enqueue (Message);
         end;

      exception
         --  Catch-all case: make sure no exception in any message
         --  processing can cause an exit of the task main loop.
         --
         --  Property errors are expected to happen in the normal flow
         --  of events in LAL. However, for any other error than a
         --  property error, we want to reload the context.
         when E : Property_Error =>
            --  ... and log this in the traces
            Server.Server_Trace.Trace
              ("Exception when processing notification:" & ASCII.LF
               & Ada.Tags.External_Tag (Message'Tag) & ASCII.LF
               & Exception_Name (E) & ASCII.LF &
                 Symbolic_Traceback (E));

         when E : others =>
            --  ... and log this in the traces
            Server.Server_Trace.Trace
              ("Exception when processing notification" & ASCII.LF
               & Ada.Tags.External_Tag (Message'Tag));
            --  The symbolic traceback will be printed by On_Error
            Server.On_Error (E);
      end Process_Message;

      Request : Message_Access;
   begin
      --  Perform initialization
      accept Start
        (Request      : not null LSP.Server_Request_Handlers
           .Server_Request_Handler_Access;
         Notification : not null LSP.Server_Notification_Receivers
           .Server_Notification_Receiver_Access;
         Server       : not null LSP.Server_Backends.Server_Backend_Access)
      do
         Initialize (Request, Notification, Server);
      end Start;

      loop
         --  Process all messages in the Input_Queue
         declare
            Continue : Boolean := True;
         begin
            while Continue loop
               Request := Server.Look_Ahead;

               select
                  Input_Queue.Dequeue (Server.Look_Ahead);

               else
                  --  No more message in the queue
                  Server.Look_Ahead := null;

                  Continue := False;
               end select;

               if Request /= null then
                  Process_Message (Request);
               end if;
            end loop;
         end;

         --  Now there are no messages in the queue and Look_Ahead is empty.
         --  Wait for some time and then check for Stop signal
         select
            Input_Queue.Dequeue (Server.Look_Ahead);
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

      while Natural (Input_Queue.Current_Use) > 0 loop
         declare
            X : Message_Access;
         begin
            Input_Queue.Dequeue (X);
            Free (X);
         end;
      end loop;
      if Server.Look_Ahead /= null then
         Free (Server.Look_Ahead);
      end if;
   end Processing_Task_Type;

   ------------------------
   -- Look_Ahead_Message --
   ------------------------

   function Look_Ahead_Message (Self : Server) return Message_Access is
      use type Ada.Task_Identification.Task_Id;
   begin
      pragma Assert
        (Ada.Task_Identification.Current_Task = Self.Processing_Task'Identity);

      return Self.Look_Ahead;
   end Look_Ahead_Message;

   ----------------------
   -- Has_Pending_Work --
   ----------------------

   function Has_Pending_Work (Self : Server) return Boolean is
      use type Ada.Task_Identification.Task_Id;
   begin
      pragma Assert
        (Ada.Task_Identification.Current_Task = Self.Processing_Task'Identity);

      return Self.Input_Queue_Length > 0;
   end Has_Pending_Work;

end LSP.Servers;

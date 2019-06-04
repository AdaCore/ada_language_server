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
with GNAT.Traceback.Symbolic;    use GNAT.Traceback.Symbolic;

with LSP.JSON_Streams;

with GNATCOLL.JSON;

package body LSP.Servers is

   New_Line : constant String :=
     (Ada.Characters.Latin_1.CR, Ada.Characters.Latin_1.LF);

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
      return LSP.Types.LSP_String renames
       LSP.Types.To_LSP_String;

   procedure Process_One_Message
     (Self : in out Server'Class;
      EOF  : in out Boolean);
   --  Read data from stdin and create a message if there is enough data.
   --  Then put the message into Self.Requests_Queue.
   --  Set EOF at end of stream.

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
      while Self.Requests_Queue.Current_Use > 0
        or Self.Output_Queue.Current_Use > 0
      loop
         delay 0.1;
      end loop;

      Self.Processing_Task.Stop;
      Self.Output_Task.Stop;
      Self.Input_Task.Stop;
   end Finalize;

   -----------------
   -- Log_Message --
   -----------------

   overriding procedure Log_Message
     (Self   : in out Server;
      Params : LSP.Messages.LogMessageParams)
   is
      Message : LSP.Messages.Notifications.LogMessage_Notification;
   begin
      Message.method := +"window/logMessage";
      Message.params := Params;

      Self.Send_Notification (Message);
   end Log_Message;

   -------------------------
   -- Process_One_Message --
   -------------------------

   procedure Process_One_Message
     (Self : in out Server'Class;
      EOF  : in out Boolean)
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
      begin
         Trace (In_Trace, To_String (Vector));
         Self.Requests_Queue.Enqueue (Vector);
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

   overriding procedure Publish_Diagnostics
     (Self   : in out Server;
      Params : LSP.Messages.PublishDiagnosticsParams)
   is
      Message : LSP.Messages.Notifications.PublishDiagnostics_Notification;
   begin
      Message.method := +"textDocument/publishDiagnostics";
      Message.params := Params;

      Self.Send_Notification (Message);
   end Publish_Diagnostics;

   ---------
   -- Run --
   ---------

   procedure Run
     (Self         : in out Server;
      Request      : not null
        LSP.Messages.Requests.Server_Request_Handler_Access;
      Notification : not null
        LSP.Messages.Notifications.Server_Notification_Handler_Access) is
   begin
      Self.Processing_Task.Start (Request, Notification);
      Self.Output_Task.Start;
      Self.Input_Task.Start;

      --  Wait for stop signal
      Self.Stop.Seize;
   end Run;

   -----------------------
   -- Send_Notification --
   -----------------------

   procedure Send_Notification
     (Self  : in out Server;
      Value : in out LSP.Messages.NotificationMessage'Class)
   is
      JSON_Stream    : aliased LSP.JSON_Streams.JSON_Stream;
      Element_Vector : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Value.jsonrpc := +"2.0";
      LSP.Messages.NotificationMessage'Write (JSON_Stream'Access, Value);
      Element_Vector := To_Unbounded_String (JSON_Stream);
      Self.Output_Queue.Enqueue (Element_Vector);
   end Send_Notification;

   ------------------
   -- Show_Message --
   ------------------

   overriding procedure Show_Message
     (Self   : in out Server;
      Params : LSP.Messages.ShowMessageParams)
   is
      Message : LSP.Messages.Notifications.ShowMessage_Notification;
   begin
      Message.method := +"window/showMessage";
      Message.params := Params;

      Self.Send_Notification (Message);
   end Show_Message;

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
      EOF : Boolean := False;
   begin
      accept Start;

      loop
         select
            accept Stop;
            exit;
         else
            Server.Process_One_Message (EOF);
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
      Vector : Ada.Strings.Unbounded.Unbounded_String;
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

         Trace (Out_Trace, To_String (Vector));
      end Write_JSON_RPC;

   begin
      accept Start;

      loop
         select
            --  Process all available outputs before acceptiong Stop
            Output_Queue.Dequeue (Vector);

            begin
               --  Send the output to the stream
               Write_JSON_RPC (Stream, Vector);
            exception
               when E : others =>
                  --  Catch-all case: make sure no exception in output writing
                  --  can cause an exit of the task loop.
                  GNATCOLL.Traces.Trace
                    (LSP.Server_Trace,
                     "Exception when writing output:" & ASCII.LF
                     & To_String (Vector) & ASCII.LF
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
      Request : Ada.Strings.Unbounded.Unbounded_String;

      Req_Handler   : LSP.Messages.Requests.Server_Request_Handler_Access;
      Notif_Handler :
      LSP.Messages.Notifications.Server_Notification_Handler_Access;
      Initialized   : Boolean;

      Requests_Queue : Requests_Queues.Queue renames Server.Requests_Queue;
      Output_Queue   : Output_Queues.Queue renames Server.Output_Queue;

      procedure Initialize
        (Request      : not null
           LSP.Messages.Requests.Server_Request_Handler_Access;
         Notification : not null
           LSP.Messages.Notifications.Server_Notification_Handler_Access);
      --  Initializes internal data structures

      procedure Process_Message_From_Stream
        (Vector     : Ada.Strings.Unbounded.Unbounded_String;
         Result     : out LSP.Types.LSP_Any;
         Error      : out LSP.Messages.Optional_ResponseError);

      procedure Send_Response
        (Response   : in out LSP.Messages.ResponseMessage'Class;
         Request_Id : LSP.Types.LSP_Number_Or_String);
      --  Complete Response and send it to the output queue

      procedure Send_Exception_Response
        (E          : Exception_Occurrence;
         Traces_Msg : String;
         Request_Id : LSP.Types.LSP_Number_Or_String);
      --  Send a response to the stream representing the exception. This
      --  should be called whenever an exception occurred while processing
      --  a request.
      --  Traces_Msg is the additional info to write in the traces, and
      --  Request_Id is the id of the request we were trying to process,
      --  if any.

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Request      : not null
           LSP.Messages.Requests.Server_Request_Handler_Access;
         Notification : not null
           LSP.Messages.Notifications.Server_Notification_Handler_Access)
      is
      begin
         Req_Handler := Request;
         Notif_Handler := Notification;
         Initialized := False;  --  Block request until 'initialize' request
      end Initialize;

      -------------------
      -- Send_Response --
      -------------------

      procedure Send_Response
        (Response   : in out LSP.Messages.ResponseMessage'Class;
         Request_Id : LSP.Types.LSP_Number_Or_String)
      is
         Out_Stream : aliased LSP.JSON_Streams.JSON_Stream;
         Output     : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Response.jsonrpc := +"2.0";
         Response.id := Request_Id;
         LSP.Messages.ResponseMessage'Class'Write
           (Out_Stream'Access, Response);
         Output := To_Unbounded_String (Out_Stream);
         Output_Queue.Enqueue (Output);
      end Send_Response;

      -----------------------------
      -- Send_Exception_Response --
      -----------------------------

      procedure Send_Exception_Response
        (E          : Exception_Occurrence;
         Traces_Msg : String;
         Request_Id : LSP.Types.LSP_Number_Or_String)
      is
         Exception_Text : constant String :=
           Exception_Name (E) & ASCII.LF & Symbolic_Traceback (E);
         Response       : LSP.Messages.ResponseMessage'Class :=
           LSP.Messages.ResponseMessage'
             (Is_Error => True,
              jsonrpc  => <>,
              id       => <>,
              error    =>
                (Is_Set => True,
                 Value =>
                   (code => LSP.Messages.InternalError,
                    data => GNATCOLL.JSON.Create_Object,
                    message => LSP.Types.To_LSP_String
                      (Exception_Text))));
      begin
         --  Send the response to the stream
         Send_Response (Response, Request_Id);

         --  Log details in the traces
         GNATCOLL.Traces.Trace
           (LSP.Server_Trace,
            "Exception when processing request:" & ASCII.LF
            & Traces_Msg & ASCII.LF
            & Exception_Text);
         Server_Trace.Trace (Symbolic_Traceback (E));
      end Send_Exception_Response;

      ---------------------------------
      -- Process_Message_From_Stream --
      ---------------------------------

      procedure Process_Message_From_Stream
        (Vector     : Ada.Strings.Unbounded.Unbounded_String;
         Result     : out LSP.Types.LSP_Any;
         Error      : out LSP.Messages.Optional_ResponseError)
      is
         use type LSP.Types.LSP_String;
         procedure Send_Not_Initialized
           (Request_Id : LSP.Types.LSP_Number_Or_String);

         --------------------------
         -- Send_Not_Initialized --
         --------------------------

         procedure Send_Not_Initialized
           (Request_Id : LSP.Types.LSP_Number_Or_String)
         is
            Response : LSP.Messages.ResponseMessage :=
              (Is_Error => True,
               jsonrpc  => <>,
               id       => <>,
               error    =>
                 (Is_Set => True,
                  Value  => (code    => LSP.Messages.MethodNotFound,
                             message => +"No such method",
                             others  => <>)));
         begin
            Send_Response (Response, Request_Id);
         end Send_Not_Initialized;

         JS             : aliased LSP.JSON_Streams.JSON_Stream;
         Document       : GNATCOLL.JSON.JSON_Value;
         JSON_Array     : GNATCOLL.JSON.JSON_Array;

         Version    : LSP.Types.LSP_String;
         Method     : LSP.Types.Optional_String;
         Request_Id : LSP.Types.LSP_Number_Or_String;

      begin
         Document := GNATCOLL.JSON.Read (Vector);
         GNATCOLL.JSON.Append (JSON_Array, Document);
         JS.Set_JSON_Document (JSON_Array);
         JS.Start_Object;
         LSP.Types.Read_String (JS, +"jsonrpc", Version);
         LSP.Types.Read_Optional_String (JS, +"method", Method);
         Read_Number_Or_String (JS, +"id", Request_Id);

         if not Method.Is_Set then
            JS.Key ("result");
            Result := JS.Read;
            JS.Key ("error");
            LSP.Messages.Optional_ResponseError'Read (JS'Access, Error);
            --  We have got error from LSP client. Save it in the trace:
            Server_Trace.Trace ("Got Error response:");

            if Error.Is_Set then
               Server_Trace.Trace
                 (LSP.Types.To_UTF_8_String (Error.Value.message));
            end if;

            return;
         elsif LSP.Types.Assigned (Request_Id) then
            if not Initialized then
               if Method.Value /= +"initialize" then
                  Send_Not_Initialized (Request_Id);
                  return;
               else
                  Initialized := True;
               end if;
            end if;
         else
            if Initialized then
               --  This is a notification
               Notif_Handler.Handle_Notification
                 (LSP.Messages.Notifications.Decode_Notification (Document));
            end if;

            return;
         end if;

         declare
            Out_Stream : aliased LSP.JSON_Streams.JSON_Stream;
            Output     : Ada.Strings.Unbounded.Unbounded_String;
            Request    : constant LSP.Messages.RequestMessage'Class
              := LSP.Messages.Requests.Decode_Request (Document);
         begin
            --  Nest a declare block so we can catch specifically an
            --  exception raised during the processing of a request.
            declare
               Response   : constant LSP.Messages.ResponseMessage'Class :=
                 Req_Handler.Handle_Request (Request);
            begin
               LSP.Messages.ResponseMessage'Class'Write
                 (Out_Stream'Access, Response);
               Output := To_Unbounded_String (Out_Stream);
               Output_Queue.Enqueue (Output);
            end;
         exception
            when E : others =>
               --  If we reach this exception handler, this means the request
               --  could be decoded, but an exception was raised when
               --  processing it.
               Send_Exception_Response (E, To_String (Vector), Request.id);
         end;
      end Process_Message_From_Stream;

   begin
      --  Perform initialization
      accept Start
        (Request      : not null
           LSP.Messages.Requests.Server_Request_Handler_Access;
         Notification : not null
           LSP.Messages.Notifications.Server_Notification_Handler_Access)
      do
         Initialize (Request, Notification);
      end Start;

      loop
         select
            --  Process all available requests before acceptiong Stop
            Requests_Queue.Dequeue (Request);

            --  Process the request
            declare
               Result     : LSP.Types.LSP_Any;
               Error      : LSP.Messages.Optional_ResponseError;
            begin
               Process_Message_From_Stream (Request, Result, Error);
               --  ??? Should we do something with Results, Error?
            exception
               when E : others =>
                  --  Catch-all case: make sure no exception in any request
                  --  processing can cause an exit of the task main loop.

                  --  Send the response to the stream. If we reach this
                  --  exception handler, this means the request could be
                  --  decoded, but an exception was raised when processing it.
                  Send_Exception_Response
                    (E,
                     To_String (Request),
                     (Is_Number => False,
                      String    => LSP.Types.Empty_LSP_String));

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

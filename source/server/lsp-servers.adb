------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2024, AdaCore                     --
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
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;

with VSS.JSON.Pull_Readers.Simple;
with VSS.JSON.Push_Writers;
with VSS.JSON.Streams;
with VSS.Strings;
with VSS.Text_Streams.Memory_UTF8_Input;
with VSS.Text_Streams.Memory_UTF8_Output;

with LSP.Client_Message_Writers;
with LSP.Enumerations;
with LSP.Errors;
with LSP.Known_Requests;
with LSP.Lifecycle_Checkers;
with LSP.Server_Notification_Readers;
with LSP.Server_Request_Readers;
with LSP.Register_Fallbacks;

package body LSP.Servers is

   New_Line : constant String :=
     (Ada.Characters.Latin_1.CR, Ada.Characters.Latin_1.LF);

   procedure Process_One_Message
     (Self    : in out Server'Class;
      Checker : in out LSP.Lifecycle_Checkers.Lifecycle_Checker;
      Map     : in out LSP.Known_Requests.Known_Request_Map;
      Logger  : Server_Message_Visitor_Access;
      EOF     : in out Boolean);
   --  Read data from stdin and create a message if there is enough data.
   --  Then put the message into Self.Input_Queue.
   --  Handle initialization logic by tracking 'initialize' request using
   --  Checker.
   --  Set EOF at end of stream or an "exit" notification.

   procedure Append
     (Vector : in out VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Buffer : Ada.Streams.Stream_Element_Array);

   procedure Send_Exception_Response
     (Self       : in out Server'Class;
      E          : Ada.Exceptions.Exception_Occurrence;
      Message    : VSS.Strings.Virtual_String;
      Request    : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Request_Id : LSP.Structures.Integer_Or_Virtual_String;
      Code       : LSP.Enumerations.ErrorCodes :=
        LSP.Enumerations.InternalError);
   --  Send a response representing the exception to the client. This
   --  should be called whenever an exception occurred while processing
   --  a request.
   --  Message is the additional info to write in the traces, and
   --  Request_Id is the id of the request we were trying to process.
   --  Use given Code in the response.

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => LSP.Server_Messages.Server_Message'Class,
      Name   => Server_Message_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => LSP.Client_Messages.Client_Message'Class,
      Name   => Client_Message_Access);

   -------------------------
   -- Allocate_Request_Id --
   -------------------------

   function Allocate_Request_Id
     (Self : in out Server'Class)
      return LSP.Structures.Integer_Or_Virtual_String is
   begin
      Self.Last_Request := @ + 1;

      return (Is_Integer => True, Integer => Self.Last_Request);
   end Allocate_Request_Id;

   ------------
   -- Append --
   ------------

   procedure Append
     (Vector : in out VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Buffer : Ada.Streams.Stream_Element_Array)
   is
      use type Ada.Streams.Stream_Element_Count;
   begin
      Vector.Set_Capacity (Vector.Length + Buffer'Length);

      for Byte of Buffer loop
         Vector.Append (Byte);
      end loop;
   end Append;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue
     (Self : in out Server'Class;
      Job  : in out LSP.Server_Jobs.Server_Job_Access) is
   begin
      Self.Scheduler.Enqueue (Job);
      Job := null;
   end Enqueue;

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : in out Server;
      Stream : access Ada.Streams.Root_Stream_Type'Class) is
   begin
      Self.Stream := Stream;

      LSP.Register_Fallbacks
        (Self.Scheduler, Self.Default_Handler'Unchecked_Access);
   end Initialize;

   -------------------------
   -- Process_One_Message --
   -------------------------

   procedure Process_One_Message
     (Self    : in out Server'Class;
      Checker : in out LSP.Lifecycle_Checkers.Lifecycle_Checker;
      Map     : in out LSP.Known_Requests.Known_Request_Map;
      Logger  : Server_Message_Visitor_Access;
      EOF     : in out Boolean)
   is
      use type Ada.Streams.Stream_Element_Count;

      procedure Parse_Header
        (Length : out Ada.Streams.Stream_Element_Count;
         Vector : in out VSS.Stream_Element_Vectors.Stream_Element_Vector);
      --  Read lines from Vector and after it from Self.Stream until empty
      --  lines is found. For each non-empty line call Parse_Line.
      --  Return any unprocessed bytes in Vector.

      procedure Parse_Line
        (Line   : String;
         Length : in out Ada.Streams.Stream_Element_Count);
      --  If given Line is "Content-Length:" header then read Length from it.

      procedure Parse_JSON
        (Vector : VSS.Stream_Element_Vectors.Stream_Element_Vector);
      --  Process Vector as complete JSON document.

      procedure Process_JSON_Document
        (Vector : VSS.Stream_Element_Vectors.Stream_Element_Vector);
      --  Process one JSON message. Vector is corresponding text for traces.

      Buffer_Size : constant := 512;
      Empty_Vector : VSS.Stream_Element_Vectors.Stream_Element_Vector;

      ------------------
      -- Parse_Header --
      ------------------

      procedure Parse_Header
        (Length : out Ada.Streams.Stream_Element_Count;
         Vector : in out VSS.Stream_Element_Vectors.Stream_Element_Vector)
      is
         Buffer : Ada.Streams.Stream_Element_Array (1 .. Buffer_Size);
         Last   : Ada.Streams.Stream_Element_Count := Vector.Length;
         Line   : String (1 .. 80) := (others => ' ');
         Char   : Character;
         Index  : Natural := 0;
         Empty  : Boolean := False;  --  We've just seen CR, LF
      begin
         --  Copy unprocessed bytes to Buffer
         for J in 1 .. Vector.Length loop
            Buffer (J) := Vector (J);
         end loop;

         if not Vector.Is_Empty then
            Vector := Empty_Vector;
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
                     Vector.Set_Capacity (Last - J);

                     for Byte of Buffer (J + 1 .. Last) loop
                        Vector.Append (Byte);
                     end loop;

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
        (Vector : VSS.Stream_Element_Vectors.Stream_Element_Vector)
      is
         use type VSS.Strings.Virtual_String;

         Memory : aliased
           VSS.Text_Streams.Memory_UTF8_Input.Memory_UTF8_Input_Stream;

         procedure Decode_JSON_RPC_Headers
           (Request_Id : out LSP.Structures.Integer_Or_Virtual_String;
            Version    : out VSS.Strings.Virtual_String;
            Method     : out VSS.Strings.Virtual_String;
            Error      : out LSP.Errors.ResponseError_Optional);

         procedure Decode_JSON_RPC_Headers
           (Request_Id : out LSP.Structures.Integer_Or_Virtual_String;
            Version    : out VSS.Strings.Virtual_String;
            Method     : out VSS.Strings.Virtual_String;
            Error      : out LSP.Errors.ResponseError_Optional)
         is
            use all type VSS.JSON.Streams.JSON_Stream_Element_Kind;

            R  : aliased VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;

         begin
            R.Set_Stream (Memory'Unchecked_Access);
            R.Read_Next;
            pragma Assert (R.Is_Start_Document);
            R.Read_Next;
            pragma Assert (R.Is_Start_Object);
            R.Read_Next;
            while not R.Is_End_Object loop
               pragma Assert (R.Is_Key_Name);
               declare
                  Key : constant VSS.Strings.Virtual_String := R.Key_Name;
               begin
                  R.Read_Next;

                  if Key = "id" then
                     case R.Element_Kind is
                        when String_Value =>
                           Request_Id :=
                             (Is_Integer     => False,
                              Virtual_String => R.String_Value);
                        when Number_Value =>
                           Request_Id :=
                             (Is_Integer => True,
                              Integer    => Integer
                                (R.Number_Value.Integer_Value));
                        when others =>
                           raise Constraint_Error;
                     end case;
                     R.Read_Next;

                  elsif Key = "jsonrpc" then
                     pragma Assert (R.Is_String_Value);
                     Version := R.String_Value;
                     R.Read_Next;

                  elsif Key = "method" then
                     pragma Assert (R.Is_String_Value);
                     Method := R.String_Value;
                     R.Read_Next;

                  elsif Key = "error" then
                     --  TODO: Optional_ResponseError'Read (Error);
                     Error := (Is_Set => True, Value => <>);

                  else
                     R.Skip_Current_Value;
                  end if;
               end;
            end loop;

            Memory.Rewind;
         exception
            when E : others =>
               Self.Tracer.Trace_Exception (E, "JSON decoding error");
         end Decode_JSON_RPC_Headers;

         function Assigned
           (Id : LSP.Structures.Integer_Or_Virtual_String) return Boolean is
             (Id.Is_Integer or else not Id.Virtual_String.Is_Null);

         Message      : Server_Message_Access;
         Request      : Request_Access;
         Notification : Notification_Access;

         Ok : Boolean;
         Is_Exit_Notification : Boolean;

         Version    : VSS.Strings.Virtual_String;
         Method     : VSS.Strings.Virtual_String;
         Request_Id : LSP.Structures.Integer_Or_Virtual_String :=
           (False, VSS.Strings.Empty_Virtual_String);
         Error      : LSP.Errors.ResponseError_Optional;

      begin
         Memory.Set_Data (Vector);

         --  Read request id and method if any
         Decode_JSON_RPC_Headers (Request_Id, Version, Method, Error);

         --  Decide if this is a request, response or notification

         if Method.Is_Null then
            --  TODO: Process client responses here.

            if Error.Is_Set then
               Self.Tracer.Trace ("Got Error response:");
               Self.Tracer.Trace_Text (Error.Value.message);
            end if;

            return;

         elsif Assigned (Request_Id) then  --  This is a request

            declare
               R : VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;

            begin
               R.Set_Stream (Memory'Unchecked_Access);
               R.Read_Next;
               pragma Assert (R.Is_Start_Document);
               R.Read_Next;

               Request :=
                 new LSP.Server_Requests.Server_Request'Class'
                   (LSP.Server_Request_Readers.Read_Request
                      (R, Method));

               if not R.Is_End_Document then
                  Self.Tracer.Trace ("Request decoding failed:");
                  Self.Tracer.Trace (Vector);
                  Self.On_Error_Response
                    (Request_Id,
                     (code    => LSP.Enumerations.InvalidParams,
                      message => "Unable to decode request."));

                  return;
               end if;
            exception
               when UR : Unknown_Method =>
                  Send_Exception_Response
                    (Self, UR, "Unknown method.",
                     Vector,
                     Request_Id,
                     LSP.Enumerations.MethodNotFound);
                  return;

               when E : others =>
                  --  If we reach this exception handler, this means the
                  --  request could not be decoded.
                  Send_Exception_Response
                    (Self, E, "Request decoding fails:",
                     Vector,
                     Request_Id,
                     LSP.Enumerations.InvalidParams);
                  return;
            end;

            Message := Server_Message_Access (Request);

         else
            --  This is a notification
            declare
               R : VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;

            begin
               R.Set_Stream (Memory'Unchecked_Access);
               R.Read_Next;
               pragma Assert (R.Is_Start_Document);
               R.Read_Next;

               Notification :=
                 new LSP.Server_Notifications.Server_Notification'Class'
                   (LSP.Server_Notification_Readers.Read_Notification
                      (R, Method));

               if not R.Is_End_Document then
                  Self.Tracer.Trace ("Notification decoding failed:");
                  Self.Tracer.Trace (Vector);
               end if;
            end;

            Message := Server_Message_Access (Notification);
         end if;

         if Logger /= null then
            Message.Visit_Server_Message_Visitor (Logger.all);
         end if;

         Checker.Check_Message (Self, Message.all, Ok, Is_Exit_Notification);
         --  Check initialization status and send a response if this is a
         --  request before initialization.
         --
         --  Check whether this was an exit notification. Note: this must be
         --  done *before* the call to Enqueue, since we're not guaranteed
         --  that the memory for Message is still allocated after this call.

         if Ok then
            --  Now we have a message to process. Push it to the processing
            --  task
            Map.Process_Message (Message.all);
            Self.Input_Queue.Enqueue (Message);
         end if;

         if Is_Exit_Notification then
            --  After "exit" notification don't read any further input.
            EOF := True;
         end if;
      end Process_JSON_Document;

      ----------------
      -- Parse_JSON --
      ----------------

      procedure Parse_JSON
        (Vector : VSS.Stream_Element_Vectors.Stream_Element_Vector) is
      begin
         Self.Tracer.Trace_Input (Vector);
         Process_JSON_Document (Vector);
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

      Vector : VSS.Stream_Element_Vectors.Stream_Element_Vector := Self.Vector;
      Length : Ada.Streams.Stream_Element_Count := 0;  --  Message length
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Buffer_Size);
      Last   : Ada.Streams.Stream_Element_Count;  --  Index the Buffer
   begin
      Parse_Header (Length, Vector);  --  Find Length out of headers

      --  Populate Buffer with Vector content
      Last := Vector.Length;
      for J in 1 .. Vector.Length loop
         Buffer (J) := Vector (J);
      end loop;

      Vector := Empty_Vector;

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
            Self.Vector := Empty_Vector;
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
   end Process_One_Message;

   ----------------------
   -- Register_Handler --
   ----------------------

   procedure Register_Handler
     (Self    : in out Server'Class;
      Tag     : Ada.Tags.Tag;
      Handler : LSP.Server_Message_Handlers.Server_Message_Handler_Access) is
   begin
      Self.Scheduler.Register_Handler (Tag, Handler);
   end Register_Handler;

   ---------
   -- Run --
   ---------

   procedure Run
     (Self         : in out Server;
      Handler      : not null Server_Message_Visitor_Access;
      Tracer       : not null LSP.Tracers.Tracer_Access;
      In_Logger    : Server_Message_Visitor_Access;
      Out_Logger   : Client_Message_Visitor_Access) is
   begin
      Self.Tracer := Tracer;
      Self.Default_Handler.Initialize (Handler);

      Self.Processing_Task.Start (Handler);
      Self.Output_Task.Start (Out_Logger);
      Self.Input_Task.Start (In_Logger);

      --  Wait for stop signal
      Self.Stop_Signal.Seize;
   end Run;

   ----------------
   -- On_Message --
   ----------------

   overriding procedure On_Message
     (Self    : in out Server;
      Message : LSP.Client_Messages.Client_Message_Access)
   is
   begin
      Self.Output_Queue.Enqueue (Message);
   end On_Message;

   -----------------------------
   -- Send_Exception_Response --
   -----------------------------

   procedure Send_Exception_Response
     (Self       : in out Server'Class;
      E          : Ada.Exceptions.Exception_Occurrence;
      Message    : VSS.Strings.Virtual_String;
      Request    : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Request_Id : LSP.Structures.Integer_Or_Virtual_String;
      Code       : LSP.Enumerations.ErrorCodes :=
        LSP.Enumerations.InternalError) is
   begin
      Self.Tracer.Trace_Exception (E, Message);
      Self.Tracer.Trace (Request);
      Self.On_Error_Response
        (Request_Id,
         (code    => Code,
          message => Message));
   end Send_Exception_Response;

   ----------
   -- Stop --
   ----------

   procedure Stop (Self : in out Server) is
   begin
      Self.Stop_Signal.Release;
   end Stop;

   ------------------------
   -- Input_Queue_Length --
   ------------------------

   function Input_Queue_Length (Self : Server) return Natural is
      use type Server_Message_Access;
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
      EOF     : Boolean := False;
      Message : Server_Message_Access;
      Map     : LSP.Known_Requests.Known_Request_Map;
      Checker : LSP.Lifecycle_Checkers.Lifecycle_Checker;
      Logger  : Server_Message_Visitor_Access;
   begin
      accept Start (In_Logger : Server_Message_Visitor_Access) do
         Logger := In_Logger;
      end Start;

      loop
         loop
            --  Destroy any processed request
            select
               --  Process all available outputs before acceptiong Stop
               Server.Destroy_Queue.Dequeue (Message);
               Map.Remove_Request (Message.all);
               Free (Message);

            else
               exit;
            end select;
         end loop;

         select
            accept Stop;
            exit;
         else
            Server.Process_One_Message (Checker, Map, Logger, EOF);
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
         Map.Remove_Request (Message.all);
         Free (Message);
      end loop;

   exception
      when E : others =>
         Server.Tracer.Trace_Exception (E, "Input_Task died");
         Server.Stop;  --  Ask server to stop
   end Input_Task_Type;

   ----------------------
   -- Output_Task_Type --
   ----------------------

   task body Output_Task_Type is
      Logger : Client_Message_Visitor_Access;

      Message : Client_Message_Access;

      Output_Queue : Output_Message_Queues.Queue renames Server.Output_Queue;

      procedure Write_JSON_RPC
        (Stream : access Ada.Streams.Root_Stream_Type'Class;
         Vector : VSS.Stream_Element_Vectors.Stream_Element_Vector);
      --  Format Vector into a protocol string including the header,
      --  and send it to Stream.

      --------------------
      -- Write_JSON_RPC --
      --------------------

      procedure Write_JSON_RPC
        (Stream : access Ada.Streams.Root_Stream_Type'Class;
         Vector : VSS.Stream_Element_Vectors.Stream_Element_Vector)
      is
         Image  : constant String := Ada.Streams.Stream_Element_Count'Image
           (Vector.Length);
         Header : constant String := "Content-Length:" & Image
           & New_Line & New_Line;

      begin
         Server.Tracer.Trace_Output (Vector);
         String'Write (Stream, Header);
         VSS.Stream_Element_Vectors.Stream_Element_Vector'Write
           (Stream, Vector);

      exception
         when E : others =>
            Server.Tracer.Trace_Exception (E, "Can't write JSON to stdout");
            raise;
      end Write_JSON_RPC;

   begin
      accept Start (Out_Logger : Client_Message_Visitor_Access) do
         Logger := Out_Logger;
      end Start;

      loop
         select
            --  Process all available outputs before acceptiong Stop
            Output_Queue.Dequeue (Message);

            if Logger /= null then
               Message.Visit_Client_Message_Visitor (Logger.all);
            end if;

            declare
               Stream : aliased VSS.Text_Streams.Memory_UTF8_Output
                 .Memory_UTF8_Output_Stream;
               Writer : aliased VSS.JSON.Push_Writers.JSON_Simple_Push_Writer;

               Visitor : LSP.Client_Message_Writers.Client_Message_Writer
                 (Writer'Unchecked_Access);

            begin
               Writer.Set_Stream (Stream'Unchecked_Access);
               Writer.Start_Document;

               Message.Visit_Client_Message_Visitor (Visitor);
               Free (Message);
               Writer.End_Document;

               --  Send the output to the stream
               Write_JSON_RPC (Server.Stream, Stream.Buffer);
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

   exception
      when E : others =>
         Server.Tracer.Trace_Exception (E, "Output_Task died");
         Server.Stop;  --  Ask server to stop
   end Output_Task_Type;

   --------------------------
   -- Processing_Task_Type --
   --------------------------

   task body Processing_Task_Type is

      Input_Queue : Input_Message_Queues.Queue renames Server.Input_Queue;

      procedure Process_Message (Message : in out Server_Message_Access);
      --  Create a job for the message and execute all highest priority jobs

      procedure Execute_Jobs (Message : in out Server_Message_Access);
      --  Execute low priority jobs (if any) till a new message arrive

      ------------------
      -- Execute_Jobs --
      ------------------

      procedure Execute_Jobs (Message : in out Server_Message_Access) is
      begin
         loop
            select
               Input_Queue.Dequeue (Message);

               exit;

            else
               declare
                  Waste : Server_Message_Access;

               begin
                  --  Call Process_Job at least once to complete a fenced
                  --  job if any.
                  Server.Scheduler.Process_Job (Server.all, Waste);

                  if Waste.Assigned then
                     Server.Destroy_Queue.Enqueue (Waste);
                  end if;

                  exit when not Server.Scheduler.Has_Jobs;
               end;
            end select;
         end loop;
      end Execute_Jobs;

      ---------------------
      -- Process_Message --
      ---------------------

      procedure Process_Message (Message : in out Server_Message_Access) is
      begin
         Server.Scheduler.Create_Job (Message);

         if Message.Assigned then
            --  Scheduler wasn't able to process message, destroy it
            Server.Destroy_Queue.Enqueue (Message);
         end if;

         loop
            declare
               Waste : Server_Message_Access;
            begin
               Server.Scheduler.Process_High_Priority_Job (Server.all, Waste);

               exit when not Waste.Assigned;

               Server.Destroy_Queue.Enqueue (Waste);
            end;
         end loop;

      exception
         when E : others =>
            --  Message handler should never raise any exception
            Server.Tracer.Trace_Exception (E, "Message handler raised error!");
      end Process_Message;

      Request : Server_Message_Access;
   begin
      --  Perform initialization
      accept Start
        (Handler : not null Server_Message_Visitor_Access)
      do
         pragma Unreferenced (Handler);
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

               if Request.Assigned then
                  Process_Message (Request);
               end if;
            end loop;
         end;

         --  Now there are no messages in the queue and Look_Ahead is empty.
         --  Wait for messages executing jobs, then check Stop signal

         Execute_Jobs (Server.Look_Ahead);

         if not Server.Look_Ahead.Assigned then
            --  there is no jobs any more, just wait for input messages

            select
               Input_Queue.Dequeue (Server.Look_Ahead);

            or
               delay 0.1;

               --  no request during some timeout, check for Stop signal

               select
                  accept Stop;
                  exit;
               else
                  null;
               end select;

            end select;
         end if;
      end loop;

      while Natural (Input_Queue.Current_Use) > 0 loop
         declare
            X : Server_Message_Access;
         begin
            Input_Queue.Dequeue (X);
            Free (X);
         end;
      end loop;

      if Server.Look_Ahead.Assigned then
         Free (Server.Look_Ahead);
      end if;

   exception
      when E : others =>
         Server.Tracer.Trace_Exception (E, "Processing_Task died");
         Server.Stop;  --  Ask server to stop
   end Processing_Task_Type;

end LSP.Servers;

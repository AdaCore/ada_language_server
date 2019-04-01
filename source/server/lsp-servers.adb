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
with Ada.Strings.UTF_Encoding;

with LSP.Servers.Handlers;
with LSP.JSON_Streams;

with GNATCOLL.JSON;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body LSP.Servers is

   New_Line : constant String :=
     (Ada.Characters.Latin_1.CR, Ada.Characters.Latin_1.LF);

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
      return LSP.Types.LSP_String renames
       LSP.Types.To_LSP_String;

   procedure Process_Message_Loop
     (Self       : in out Server'Class;
      Request_Id : LSP.Types.LSP_Number;
      Result     : out LSP.Types.LSP_Any;
      Error      : out LSP.Messages.Optional_ResponseError);

   procedure Process_Message_From_Stream
     (Self       : in out Server'Class;
      Stream     : access Ada.Streams.Root_Stream_Type'Class;
      Request_Id : out LSP.Types.LSP_Number_Or_String;
      Result     : out LSP.Types.LSP_Any;
      Error      : out LSP.Messages.Optional_ResponseError);

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

   procedure Write_JSON_RPC
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Vector : Ada.Strings.Unbounded.Unbounded_String);

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
      Stream       : access Ada.Streams.Root_Stream_Type'Class;
      Request      : not null LSP.Message_Handlers.Request_Handler_Access;
      Notification : not null
        LSP.Server_Notifications.Server_Notification_Handler_Access)
   is
      type Request_Info is record
         Name   : LSP.Types.LSP_String;
         Action : LSP.Request_Dispatchers.Parameter_Handler_Access;
      end record;

      Request_List : constant array (Positive range <>) of Request_Info :=
        ((+"initialize", Handlers.Do_Initialize'Access),
         (+"shutdown", Handlers.Do_Shutdown'Access),
         (+"textDocument/willSaveWaitUntil", Handlers.Do_Not_Found'Access),
         (+"textDocument/completion", Handlers.Do_Completion'Access),
         (+"completionItem/resolve", Handlers.Do_Not_Found'Access),
         (+"textDocument/hover", Handlers.Do_Hover'Access),
         (+"textDocument/signatureHelp", Handlers.Do_Signature_Help'Access),
         (+"textDocument/definition", Handlers.Do_Definition'Access),
         (+"textDocument/references", Handlers.Do_References'Access),
         (+"textDocument/documentHighlight", Handlers.Do_Highlight'Access),
         (+"textDocument/documentSymbol", Handlers.Do_Document_Symbol'Access),
         (+"workspace/symbol", Handlers.Do_Workspace_Symbol'Access),
         (+"textDocument/codeAction", Handlers.Do_Code_Action'Access),
         (+"textDocument/codeLens", Handlers.Do_Not_Found'Access),
         (+"codeLens/resolve", Handlers.Do_Not_Found'Access),
         (+"textDocument/documentLink", Handlers.Do_Not_Found'Access),
         (+"documentLink/resolve", Handlers.Do_Not_Found'Access),
         (+"textDocument/formatting", Handlers.Do_Not_Found'Access),
         (+"textDocument/rangeFormatting", Handlers.Do_Not_Found'Access),
         (+"textDocument/onTypeFormatting", Handlers.Do_Not_Found'Access),
         (+"textDocument/rename", Handlers.Do_Not_Found'Access),
         (+"workspace/executeCommand", Handlers.Do_Execute_Command'Access),
         (+"", Handlers.Do_Not_Found'Access));

      type Notification_Info is record
         Name   : LSP.Types.LSP_String;
         Action : LSP.Notification_Dispatchers.Parameter_Handler_Access;
      end record;

      type Notification_Info_Array is
        array (Positive range <>) of Notification_Info;

      Notification_List : constant Notification_Info_Array :=
        ((+"exit", Handlers.Do_Exit'Access),
         (+"textDocument/didChange", Handlers.DidChangeTextDocument'Access),
         (+"textDocument/didClose", Handlers.DidCloseTextDocument'Access),
         (+"textDocument/didOpen", Handlers.DidOpenTextDocument'Access),
         (+"textDocument/didSave", Handlers.DidSaveTextDocument'Access),
         (+"workspace/didChangeConfiguration",
          Handlers.DidChangeConfiguration'Access),
         (+"", Handlers.Ignore_Notification'Access));

   begin
      Self.Stream := Stream;
      Self.Req_Handler := Request;
      Self.Notif_Handler := Notification;
      Self.Initilized := False;  --  Block request until 'initialize' request

      for Request of Request_List loop
         Self.Requests.Register (Request.Name, Request.Action);
      end loop;

      for Notification of Notification_List loop
         Self.Notifications.Register (Notification.Name, Notification.Action);
      end loop;
   end Initialize;

   -----------------
   -- Log_Message --
   -----------------

   overriding procedure Log_Message
     (Self   : in out Server;
      Params : LSP.Messages.LogMessageParams)
   is
      Message : LSP.Messages.LogMessage_Notification;
   begin
      Message.method := +"window/logMessage";
      Message.params := Params;

      Self.Send_Notification (Message);
   end Log_Message;

   ---------------------------------
   -- Process_Message_From_Stream --
   ---------------------------------

   procedure Process_Message_From_Stream
     (Self       : in out Server'Class;
      Stream     : access Ada.Streams.Root_Stream_Type'Class;
      Request_Id : out LSP.Types.LSP_Number_Or_String;
      Result     : out LSP.Types.LSP_Any;
      Error      : out LSP.Messages.Optional_ResponseError)
   is
      use type LSP.Types.LSP_String;
      procedure Send_Not_Initialized
        (Request_Id : LSP.Types.LSP_Number_Or_String);

      procedure Send_Response
        (Response   : in out LSP.Messages.ResponseMessage'Class;
         Request_Id : LSP.Types.LSP_Number_Or_String);

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
         Write_JSON_RPC (Self.Stream, Output);
      end Send_Response;

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (Stream.all);

      Not_Set    : constant LSP.Types.LSP_Number_Or_String :=
        (Is_Number => False, String => LSP.Types.Empty_LSP_String);
      Version    : LSP.Types.LSP_String;
      Method     : LSP.Types.Optional_String;
   begin
      JS.Start_Object;
      LSP.Types.Read_String (JS, +"jsonrpc", Version);
      LSP.Types.Read_Optional_String (JS, +"method", Method);
      Read_Number_Or_String (JS, +"id", Request_Id);

      if not Method.Is_Set then
         JS.Key ("result");
         Result := JS.Read;
         JS.Key ("error");
         LSP.Messages.Optional_ResponseError'Read (Stream, Error);
         --  We have got error from LSP client. Save it in the trace:
         Server_Trace.Trace ("Got Error response:");

         if Error.Is_Set then
            Server_Trace.Trace
              (LSP.Types.To_UTF_8_String (Error.Value.message));
         end if;

         return;
      elsif LSP.Types.Assigned (Request_Id) then
         if not Self.Initilized then
            if Method.Value /= +"initialize" then
               Send_Not_Initialized (Request_Id);
               Request_Id := Not_Set;
               return;
            else
               Self.Initilized := True;
            end if;
         end if;
      else
         if Self.Initilized then
            JS.Key ("params");
            Self.Notifications.Dispatch
              (Method  => Method.Value,
               Stream  => Stream,
               Handler => Self.Notif_Handler);
         end if;

         Request_Id := Not_Set;
         return;
      end if;

      JS.Key ("params");
      declare
         Out_Stream : aliased LSP.JSON_Streams.JSON_Stream;
         Output     : Ada.Strings.Unbounded.Unbounded_String;
         Response   : LSP.Messages.ResponseMessage'Class :=
           Self.Requests.Dispatch
             (Method  => Method.Value,
              Stream  => Stream,
              Handler => Self.Req_Handler);
      begin
         Response.jsonrpc := +"2.0";
         Response.id := Request_Id;
         LSP.Messages.ResponseMessage'Class'Write
           (Out_Stream'Access, Response);
         Output := To_Unbounded_String (Out_Stream);
         Write_JSON_RPC (Self.Stream, Output);
         Request_Id := Not_Set;
      end;
   end Process_Message_From_Stream;

   --------------------------
   -- Process_Message_Loop --
   --------------------------

   procedure Process_Message_Loop
     (Self       : in out Server'Class;
      Request_Id : LSP.Types.LSP_Number;
      Result     : out LSP.Types.LSP_Any;
      Error      : out LSP.Messages.Optional_ResponseError)
   is
      use type Ada.Streams.Stream_Element_Count;

      procedure Parse_Header
        (Length : out Ada.Streams.Stream_Element_Count;
         Vector : in out Ada.Strings.Unbounded.Unbounded_String);
      procedure Parse_JSON
        (Vector : Ada.Strings.Unbounded.Unbounded_String;
         Stop   : in out Boolean);
      procedure Parse_Line
        (Line   : String;
         Length : in out Ada.Streams.Stream_Element_Count);

      ------------------
      -- Parse_Header --
      ------------------

      procedure Parse_Header
        (Length : out Ada.Streams.Stream_Element_Count;
         Vector : in out Ada.Strings.Unbounded.Unbounded_String)
      is
         Buffer : Ada.Streams.Stream_Element_Array (1 .. 512);
         Last   : Ada.Streams.Stream_Element_Count :=
           Ada.Streams.Stream_Element_Count
             (Ada.Strings.Unbounded.Length (Vector));
         Line   : String (1 .. 80) := (others => ' ');
         Char   : Character;
         Index  : Natural := 0;
         Empty  : Boolean := False;  --  We've just seen CR, LF
      begin
         if Last > 0 then
            Buffer (1 .. Last) := To_Stream_Element_Array (Vector);
            Vector := Ada.Strings.Unbounded.Null_Unbounded_String;
         end if;

         Length := 0;

         loop
            for J in 1 .. Last loop
               Char := Character'Val (Buffer (J));

               if Char not in Ada.Characters.Latin_1.CR
                                | Ada.Characters.Latin_1.LF
               then
                  Empty := False;
               end if;

               if Index = Line'Last then
                  --  Too long line drop it keeping last character
                  Line (1) := Line (Line'Last);
                  Index := 2;
               else
                  Index := Index + 1;
               end if;

               Line (Index) := Char;

               if Index > 1 and then Line (Index - 1 .. Index) = New_Line then
                  if Empty then
                     Append (Vector, Buffer (J + 1 .. Last));
                     return;
                  end if;

                  Empty := True;
                  Parse_Line (Line (1 .. Index - 2), Length);
               end if;
            end loop;

            Self.Stream.Read (Buffer, Last);
         end loop;
      end Parse_Header;

      ----------------
      -- Parse_JSON --
      ----------------

      procedure Parse_JSON
        (Vector : Ada.Strings.Unbounded.Unbounded_String;
         Stop   : in out Boolean)
      is
         In_Stream      : aliased LSP.JSON_Streams.JSON_Stream;
         Document       : GNATCOLL.JSON.JSON_Value;
         JSON_Array     : GNATCOLL.JSON.JSON_Array;
         Message_Id     : LSP.Types.LSP_Number_Or_String;
      begin
         Trace (In_Trace, To_String (Vector));
         Document := GNATCOLL.JSON.Read (Vector);
         GNATCOLL.JSON.Append (JSON_Array, Document);
         In_Stream.Set_JSON_Document (JSON_Array);

         Self.Process_Message_From_Stream
           (In_Stream'Access, Message_Id, Result, Error);

         if Message_Id.Is_Number and then Message_Id.Number = Request_Id then
            Stop := True;
         end if;
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

      Stop   : Boolean := False;
      Vector : Ada.Strings.Unbounded.Unbounded_String :=
        Self.Vector;
      Length : Ada.Streams.Stream_Element_Count := 0;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 512);
      Last   : Ada.Streams.Stream_Element_Count;
   begin
      while not (Self.Stop or Stop) loop
         Parse_Header (Length, Vector);
         Last := Ada.Streams.Stream_Element_Count
           (Ada.Strings.Unbounded.Length (Vector));
         Buffer (1 .. Last) := To_Stream_Element_Array (Vector);
         Vector := Ada.Strings.Unbounded.Null_Unbounded_String;

         loop
            if Last <= Length then
               Append (Vector, Buffer (1 .. Last));
               Length := Length - Last;
               Last := 0;
            else
               Append (Vector, Buffer (1 .. Length));
               Last := Last - Length;
               Buffer (1 .. Last) := Buffer (Length + 1 .. Length + Last);
               Length := 0;
            end if;

            if Length = 0 then
               Self.Vector := Ada.Strings.Unbounded.Null_Unbounded_String;
               Append (Self.Vector, Buffer (1 .. Last));
               Parse_JSON (Vector, Stop);
               Vector := Self.Vector;
               exit;
            else
               Self.Stream.Read (Buffer, Last);
            end if;
         end loop;
      end loop;
   end Process_Message_Loop;

   -------------------------
   -- Publish_Diagnostics --
   -------------------------

   overriding procedure Publish_Diagnostics
     (Self   : in out Server;
      Params : LSP.Messages.PublishDiagnosticsParams)
   is
      Message : LSP.Messages.PublishDiagnostics_Notification;
   begin
      Message.method := +"textDocument/publishDiagnostics";
      Message.params := Params;

      Self.Send_Notification (Message);
   end Publish_Diagnostics;

   ---------
   -- Run --
   ---------

   procedure Run (Self  : in out Server) is
      Result     : LSP.Types.LSP_Any;
      Error      : LSP.Messages.Optional_ResponseError;
   begin
      Self.Process_Message_Loop (0, Result, Error);
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
      Write_JSON_RPC (Self.Stream, Element_Vector);
   end Send_Notification;

   ------------------
   -- Show_Message --
   ------------------

   overriding procedure Show_Message
     (Self   : in out Server;
      Params : LSP.Messages.ShowMessageParams)
   is
      Message : LSP.Messages.ShowMessage_Notification;
   begin
      Message.method := +"window/showMessage";
      Message.params := Params;

      Self.Send_Notification (Message);
   end Show_Message;

   ----------
   -- Stop --
   ----------

   procedure Stop (Self  : in out Server) is
   begin
      Self.Stop := True;
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

   --------------------------
   -- Workspace_Apply_Edit --
   --------------------------

   procedure Workspace_Apply_Edit
     (Self     : in out Server;
      Params   : LSP.Messages.ApplyWorkspaceEditParams;
      Applied  : out Boolean;
      Error    : out LSP.Messages.Optional_ResponseError)
   is
      Request        : constant LSP.Messages.ApplyWorkspaceEdit_Request :=
        (jsonrpc => +"2.0",
         id      => (Is_Number => True, Number => Self.Last_Request),
         method  => +"workspace/applyEdit",
         params  => Params);
      JSON_Stream    : aliased LSP.JSON_Streams.JSON_Stream;
      Element_Vector : Ada.Strings.Unbounded.Unbounded_String;
      Result         : LSP.Types.LSP_Any;
   begin
      Self.Last_Request := Self.Last_Request + 1;
      LSP.Messages.ApplyWorkspaceEdit_Request'Write
        (JSON_Stream'Access, Request);
      Element_Vector := To_Unbounded_String (JSON_Stream);
      Write_JSON_RPC (Self.Stream, Element_Vector);
      Self.Process_Message_Loop
        (Request_Id => Request.id.Number,
         Result     => Result,
         Error      => Error);

      if Error.Is_Set then
         Applied := False;
      else
         Applied := Result.Get ("applied").Get;
      end if;
   end Workspace_Apply_Edit;

   --------------------
   -- Write_JSON_RPC --
   --------------------

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

end LSP.Servers;

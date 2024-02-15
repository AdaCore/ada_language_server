------------------------------------------------------------------------------
--                         Language Client Protocol                         --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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

--  Client-to-server and server-to-client message loggers that uses 'Image to
--  dump most of messages, but reduces output for other messages to avoid too
--  large dumps or leaking edited code.

with VSS.Text_Streams;

with LSP.Client_Message_Visitors;
with LSP.Client_Notification_Loggers;
with LSP.Client_Notifications;
with LSP.Client_Request_Loggers;
with LSP.Client_Requests;
with LSP.Client_Response_Loggers;
with LSP.Client_Responses;
with LSP.Server_Message_Visitors;
with LSP.Server_Notification_Loggers;
with LSP.Server_Notifications;
with LSP.Server_Request_Loggers;
with LSP.Server_Requests;
with LSP.Server_Response_Loggers;
with LSP.Server_Responses;

with LSP.Structures;

package LSP.Secure_Message_Loggers is
   pragma Preelaborate;

   type Client_Response_Logger is
     new LSP.Client_Response_Loggers.Client_Response_Logger
       with null record;

   overriding procedure On_Completion_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Completion_Result);

   overriding procedure On_DocumentSymbol_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentSymbol_Result);

   overriding procedure On_FoldingRange_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.FoldingRange_Vector_Or_Null);

   overriding procedure On_References_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Location_Vector_Or_Null);

   overriding procedure On_Tokens_Full_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokens_Or_Null);

   overriding procedure On_CodeAction_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Command_Or_CodeAction_Vector_Or_Null);

   overriding procedure On_Symbol_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Symbol_Result);

   type Server_Notification_Logger is
     new LSP.Server_Notification_Loggers.Server_Notification_Logger
       with null record;

   overriding procedure On_DidChange_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.DidChangeTextDocumentParams);

   overriding procedure On_DidOpen_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.DidOpenTextDocumentParams);

   type Client_Logger
     (Output : not null access VSS.Text_Streams.Output_Text_Stream'Class)
   is new LSP.Client_Message_Visitors.Client_Message_Visitor with record
      Request  : LSP.Client_Request_Loggers.Client_Request_Logger (Output);
      Response : Client_Response_Logger (Output);

      Notification :
        LSP.Client_Notification_Loggers.Client_Notification_Logger (Output);
   end record;

   overriding procedure On_Client_Notification
     (Self  : in out Client_Logger;
      Value : LSP.Client_Notifications.Client_Notification'Class);

   overriding procedure On_Client_Request
     (Self  : in out Client_Logger;
      Value : LSP.Client_Requests.Client_Request'Class);

   overriding procedure On_Client_Response
     (Self  : in out Client_Logger;
      Value : LSP.Client_Responses.Client_Response'Class);

   type Server_Logger
     (Output : not null access VSS.Text_Streams.Output_Text_Stream'Class)
   is new LSP.Server_Message_Visitors.Server_Message_Visitor with record
      Request  : LSP.Server_Request_Loggers.Server_Request_Logger (Output);
      Response : LSP.Server_Response_Loggers.Server_Response_Logger (Output);

      Notification : Server_Notification_Logger (Output);
   end record;

   overriding procedure On_Server_Notification
     (Self  : in out Server_Logger;
      Value : LSP.Server_Notifications.Server_Notification'Class);

   overriding procedure On_Server_Request
     (Self  : in out Server_Logger;
      Value : LSP.Server_Requests.Server_Request'Class);

   overriding procedure On_Server_Response
     (Self  : in out Server_Logger;
      Value : LSP.Server_Responses.Server_Response'Class);

end LSP.Secure_Message_Loggers;

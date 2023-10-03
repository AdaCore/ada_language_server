------------------------------------------------------------------------------
--                         Language Server Protocol                         --
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

with LSP.Client_Message_Receivers;
with LSP.Server_Message_Visitors;
with LSP.Server_Messages;
with LSP.Server_Notification_Receivers;
with LSP.Server_Notifications;
with LSP.Server_Request_Receivers;
with LSP.Server_Requests;
with LSP.Structures;

package LSP.Lifecycle_Checkers is

   type Lifecycle_Checker is tagged limited private;
   --  Type to keep state of initialization and to verify message flow

   procedure Check_Message
     (Self    : in out Lifecycle_Checker;
      Client  : in out LSP.Client_Message_Receivers
                         .Client_Message_Receiver'Class;
      Message : LSP.Server_Messages.Server_Message'Class;
      Is_Ok   : out Boolean;
      Is_Exit : out Boolean);
   --  Verify if given message is allowed in the current lifycycle state.
   --  If it's allowed return Is_Ok = True, else send response using Client
   --  object if needed and return Is_Ok = False.
   --  Set Is_Exit = True if Message is then exits notification.

private

   type Visitor (Parent : not null access Lifecycle_Checker) is
     limited new LSP.Server_Message_Visitors.Server_Message_Visitor
       and LSP.Server_Notification_Receivers.Server_Notification_Receiver
       and LSP.Server_Request_Receivers.Server_Request_Receiver
       with null record;

   overriding procedure On_Initialize_Request
     (Self  : in out Visitor;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InitializeParams);

   overriding procedure On_Exits_Notification (Self : in out Visitor);

   overriding procedure On_Server_Notification
     (Self  : in out Visitor;
      Value : LSP.Server_Notifications.Server_Notification'Class);

   overriding procedure On_Server_Request
     (Self  : in out Visitor;
      Value : LSP.Server_Requests.Server_Request'Class);

   type Lifecycle_Checker is tagged limited record
      Visitor : LSP.Lifecycle_Checkers.Visitor
        (Lifecycle_Checker'Unchecked_Access);

      Initialized     : Boolean := False;
      Is_Exit         : Boolean;

      Request_Id      : LSP.Structures.Integer_Or_Virtual_String_Optional;
      Client          : access constant
        LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
   end record;

end LSP.Lifecycle_Checkers;

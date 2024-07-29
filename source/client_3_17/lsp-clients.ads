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

private with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Unbounded;

with VSS.Strings;

limited with LSP.Client_Notification_Receivers;
limited with LSP.Client_Request_Receivers;
limited with LSP.Client_Response_Receivers;
with LSP.Raw_Clients;
with LSP.Server_Message_Consumers;
private with LSP.Server_Message_Factories;
private with LSP.Server_Messages;
limited with LSP.Server_Notification_Receivers;
limited with LSP.Server_Request_Receivers;
limited with LSP.Server_Response_Receivers;
with LSP.Structures;
private with LSP.Structures.Hashes;

package LSP.Clients is

   type Client is new LSP.Raw_Clients.Raw_Client
     and LSP.Server_Message_Consumers.Server_Message_Consumer
       with private;
   --  Client object to send/recieve request and notification to/from
   --  the LSP server

   procedure Initialize (Self : in out Client'Class);
   --  Initialize Client to correct state

   function Send_Notification (Self : in out Client'Class)
      return not null access
        LSP.Server_Notification_Receivers.Server_Notification_Receiver'Class;

   function Send_Request (Self : in out Client'Class)
      return not null access
        LSP.Server_Request_Receivers.Server_Request_Receiver'Class;

   function Send_Response (Self : in out Client'Class)
     return not null access
       LSP.Server_Response_Receivers.Server_Response_Receiver'Class;

   procedure Set_Response_Handler
     (Self : in out Client'Class;
      To   : not null access
        LSP.Client_Response_Receivers.Client_Response_Receiver'Class);
   --  Set response handler

   procedure Set_Request_Handler
     (Self : in out Client'Class;
      To   : not null access
        LSP.Client_Request_Receivers.Client_Request_Receiver'Class);
   --  Set request handler

   procedure Set_Notification_Handler
     (Self : in out Client'Class;
      To   : not null access
        Client_Notification_Receivers.Client_Notification_Receiver'Class);
   --  Set notification handler

   function Allocate_Request_Id
     (Self : in out Client'Class)
      return LSP.Structures.Integer_Or_Virtual_String;
   --  Allocates request id.

   function Request_Id_Prefix
     (Self : Client) return VSS.Strings.Virtual_String;
   --  Prefix to generate request id in form "prefix-id".

private

   package Request_Maps is
     new Ada.Containers.Hashed_Maps
     (Key_Type        => LSP.Structures.Integer_Or_Virtual_String,
      Element_Type    => VSS.Strings.Virtual_String,
      Hash            => LSP.Structures.Hashes.Hash,
      Equivalent_Keys => LSP.Structures."=",
      "="             => VSS.Strings."=");

   type Client is new LSP.Raw_Clients.Raw_Client
     and LSP.Server_Message_Consumers.Server_Message_Consumer
   with record
      Request_Id       : Standard.Integer := 0;  --  Id of prev request
      Request_Map      : Request_Maps.Map;       --  issued requests
      Response_Handler : access
        LSP.Client_Response_Receivers.Client_Response_Receiver'Class;
      Request_Handler  : access
        LSP.Client_Request_Receivers.Client_Request_Receiver'Class;
      Notification     : access
        LSP.Client_Notification_Receivers.Client_Notification_Receiver'Class;
      Error_Message    : VSS.Strings.Virtual_String;

      Server_Factory   : aliased
        LSP.Server_Message_Factories.Server_Message_Factory
          (Client'Unchecked_Access);
   end record;

   overriding procedure On_Message
     (Self    : in out Client;
      Message : not null LSP.Server_Messages.Server_Message_Access);

   overriding procedure On_Raw_Message
     (Self    : in out Client;
      Data    : Ada.Strings.Unbounded.Unbounded_String;
      Success : in out Boolean);

   overriding function Error_Message
     (Self : Client) return VSS.Strings.Virtual_String;

   function Send_Notification (Self : in out Client'Class)
     return not null access
       LSP.Server_Notification_Receivers.Server_Notification_Receiver'Class is
      (Self.Server_Factory'Unchecked_Access);

   function Send_Request (Self : in out Client'Class)
     return not null access
       LSP.Server_Request_Receivers.Server_Request_Receiver'Class is
      (Self.Server_Factory'Unchecked_Access);

   function Send_Response (Self : in out Client'Class)
     return not null access
       LSP.Server_Response_Receivers.Server_Response_Receiver'Class is
      (Self.Server_Factory'Unchecked_Access);

end LSP.Clients;

------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2022-2024, AdaCore                     --
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

private with LSP.Server_Message_Visitors;
with LSP.Server_Messages;
private with LSP.Server_Notification_Receivers;
limited private with LSP.Server_Notifications;
limited private with LSP.Server_Requests;
private with LSP.Structures.Hashes;

package LSP.Known_Requests is

   type Known_Request_Map is tagged limited private;

   procedure Process_Message
     (Self    : in out Known_Request_Map;
      Message : in out LSP.Server_Messages.Server_Message'Class);
   --  If Message is a request, then keep the request for futher cancelation.
   --  If Message is a cancel request notification, then find the corresponding
   --  request and mark it as canceled.

   procedure Remove_Request
     (Self    : in out Known_Request_Map;
      Message : LSP.Server_Messages.Server_Message'Class);
   --  Remove request with given Id from the map

private

   type Visitor (Parent : not null access Known_Request_Map) is
     limited new LSP.Server_Message_Visitors.Server_Message_Visitor
       and LSP.Server_Notification_Receivers.Server_Notification_Receiver
       with null record;

   overriding procedure On_Server_Notification
     (Self  : in out Visitor;
      Value : LSP.Server_Notifications.Server_Notification'Class);

   overriding procedure On_Server_Request
     (Self  : in out Visitor;
      Value : LSP.Server_Requests.Server_Request'Class);

   overriding procedure On_CancelRequest_Notification
     (Self  : in out Visitor;
      Value : LSP.Structures.CancelParams);

   type Request_Access is
     access all LSP.Server_Requests.Server_Request'Class;

   package Request_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => LSP.Structures.Integer_Or_Virtual_String,  --  id
      Element_Type    => Request_Access,
      Hash            => LSP.Structures.Hashes.Hash,
      Equivalent_Keys => LSP.Structures."=",
      "="             => "=");

   type Known_Request_Map is tagged limited record
      Visitor  : LSP.Known_Requests.Visitor
        (Known_Request_Map'Unchecked_Access);

      Map      : Request_Maps.Map;
      Removing : Boolean;
   end record;

end LSP.Known_Requests;

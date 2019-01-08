------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
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

with Ada.Streams;

with LSP.Messages;
with LSP.Message_Handlers;
with LSP.Notification_Handlers;
with LSP.Types;

private with LSP.Notification_Dispatchers;
private with LSP.Request_Dispatchers;

private with Ada.Strings.Unbounded;

package LSP.Servers is
--   pragma Preelaborate;

   type Server is tagged limited private;

   not overriding procedure Initialize
     (Self         : in out Server;
      Stream       : access Ada.Streams.Root_Stream_Type'Class;
      Request      : not null LSP.Message_Handlers.Request_Handler_Access;
      Notification : not null LSP.Notification_Handlers.
        Notification_Handler_Access);

   not overriding procedure Send_Notification
     (Self  : in out Server;
      Value : in out LSP.Messages.NotificationMessage'Class);

   not overriding procedure Run (Self  : in out Server);

   not overriding procedure Stop (Self  : in out Server);
   --  Ask server to stop after processing current message

   not overriding procedure Workspace_Apply_Edit
     (Self     : in out Server;
      Params   : LSP.Messages.ApplyWorkspaceEditParams;
      Applied  : out Boolean;
      Error    : out LSP.Messages.Optional_ResponseError);

private

   type Server is tagged limited record
      Initilized : Boolean;
      Stop       : Boolean := False;
      --  Mark Server as uninitialized until get 'initalize' request
      Stream        : access Ada.Streams.Root_Stream_Type'Class;
      Req_Handler   : LSP.Message_Handlers.Request_Handler_Access;
      Notif_Handler : LSP.Notification_Handlers.Notification_Handler_Access;
      Requests      : aliased LSP.Request_Dispatchers.Request_Dispatcher;
      Notifications : aliased LSP.Notification_Dispatchers
        .Notification_Dispatcher;
      Last_Request  : LSP.Types.LSP_Number := 1;
      Vector        : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end LSP.Servers;

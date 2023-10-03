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

with Ada.Unchecked_Conversion;

package body LSP.Known_Requests is

   type Request_Constant_Access is
     access constant LSP.Server_Requests.Server_Request'Class;

   function Cast is new Ada.Unchecked_Conversion
     (Request_Constant_Access, Request_Access);

   -----------------------------------
   -- On_CancelRequest_Notification --
   -----------------------------------

   overriding procedure On_CancelRequest_Notification
     (Self  : in out Visitor;
      Value : LSP.Structures.CancelParams)
   is
      Cursor : constant Request_Maps.Cursor := Self.Parent.Map.Find (Value.id);
   begin
      if Request_Maps.Has_Element (Cursor) then
         Request_Maps.Element (Cursor).Canceled := True;
      end if;
   end On_CancelRequest_Notification;

   ----------------------------
   -- On_Server_Notification --
   ----------------------------

   overriding procedure On_Server_Notification
     (Self  : in out Visitor;
      Value : LSP.Server_Notifications.Server_Notification'Class) is
   begin
      Value.Visit_Server_Receiver (Self);
   end On_Server_Notification;

   -----------------------
   -- On_Server_Request --
   -----------------------

   overriding procedure On_Server_Request
     (Self  : in out Visitor;
      Value : LSP.Server_Requests.Server_Request'Class) is
   begin
      if Self.Parent.Removing then
         Self.Parent.Map.Delete (Value.Id);
      else
         Self.Parent.Map.Insert (Value.Id, Cast (Value'Unchecked_Access));
      end if;
   end On_Server_Request;

   ---------------------
   -- Process_Message --
   ---------------------

   procedure Process_Message
     (Self    : in out Known_Request_Map;
      Message : in out LSP.Server_Messages.Server_Message'Class) is
   begin
      Self.Removing := False;
      Message.Visit_Server_Message_Visitor (Self.Visitor);
   end Process_Message;

   --------------------
   -- Remove_Request --
   --------------------

   procedure Remove_Request
     (Self    : in out Known_Request_Map;
      Message : LSP.Server_Messages.Server_Message'Class) is
   begin
      Self.Removing := True;
   end Remove_Request;

end LSP.Known_Requests;

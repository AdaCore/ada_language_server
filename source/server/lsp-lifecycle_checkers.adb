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

with LSP.Constants;

package body LSP.Lifecycle_Checkers is

   -------------------
   -- Check_Message --
   -------------------

   procedure Check_Message
     (Self    : in out Lifecycle_Checker;
      Client  : in out LSP.Client_Message_Receivers
                         .Client_Message_Receiver'Class;
      Message : LSP.Server_Messages.Server_Message'Class;
      Is_Ok   : out Boolean;
      Is_Exit : out Boolean)
   is
   begin
      Self.Is_Exit := False;
      Self.Request_Id := (Is_Set => False);
      Self.Client := Client'Unchecked_Access;

      Message.Visit_Server_Message_Visitor (Self.Visitor);

      Is_Exit := Self.Is_Exit;
      Is_Ok := Self.Initialized or else Is_Exit;

      if not Is_Ok and then Self.Request_Id.Is_Set then
         Client.On_Error_Response
           (Id    => Self.Request_Id.Value,
            Value =>
              (code    => LSP.Constants.ServerNotInitialized,
               message => "No initialize request has been received"));
      end if;
   end Check_Message;

   ---------------------------
   -- On_Exits_Notification --
   ---------------------------

   overriding procedure On_Exits_Notification (Self : in out Visitor) is
   begin
      Self.Parent.Is_Exit := True;
   end On_Exits_Notification;

   ---------------------------
   -- On_Initialize_Request --
   ---------------------------

   overriding procedure On_Initialize_Request
     (Self  : in out Visitor;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InitializeParams) is
   begin
      Self.Parent.Initialized := True;
   end On_Initialize_Request;

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
      Self.Parent.Request_Id := (True, Value.Id);
      Value.Visit_Server_Receiver (Self);
   end On_Server_Request;

end LSP.Lifecycle_Checkers;

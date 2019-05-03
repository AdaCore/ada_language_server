------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Ada.Strings.UTF_Encoding;
with LSP.Types; use LSP.Types;

package body LSP.Messages.Common_Writers is

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
      return LSP.Types.LSP_String renames
       LSP.Types.To_LSP_String;

   --------------------------
   -- Write_Request_Prefix --
   --------------------------

   procedure Write_Request_Prefix
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RequestMessage'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      Write_String (JS, +"jsonrpc", V.jsonrpc);

      if V.id.Is_Number then
         Write_Number (JS, +"id", V.id.Number);
      elsif not Is_Empty (V.id.String) then
         Write_String (JS, +"id", V.id.String);
      end if;

      Write_String (JS, +"method", V.method);
   end Write_Request_Prefix;

   -------------------------------
   -- Write_Notification_Prefix --
   -------------------------------

   procedure Write_Notification_Prefix
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.NotificationMessage'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      Write_String (JS, +"jsonrpc", V.jsonrpc);
      Write_String (JS, +"method", V.method);
   end Write_Notification_Prefix;

   -------------------------------
   -- Set_Common_Request_Fields --
   -------------------------------

   procedure Set_Common_Request_Fields
     (R  : in out RequestMessage'Class;
      JS : in out LSP.JSON_Streams.JSON_Stream'Class)
   is
      Version    : LSP.Types.LSP_String;
      Method     : LSP.Types.LSP_String;
      Request_Id : LSP.Types.LSP_Number_Or_String;
   begin
      LSP.Types.Read_String (JS, +"jsonrpc", Version);
      LSP.Types.Read_String (JS, +"method", Method);
      Read_Number_Or_String (JS, +"id", Request_Id);
      R.jsonrpc := Version;
      R.method := Method;
      R.id := Request_Id;
   end Set_Common_Request_Fields;

   ------------------------------------
   -- Set_Common_Notification_Fields --
   ------------------------------------

   procedure Set_Common_Notification_Fields
     (R  : in out NotificationMessage'Class;
      JS : in out LSP.JSON_Streams.JSON_Stream'Class)
   is
      Version    : LSP.Types.LSP_String;
      Method     : LSP.Types.LSP_String;
   begin
      LSP.Types.Read_String (JS, +"jsonrpc", Version);
      LSP.Types.Read_String (JS, +"method", Method);
      R.jsonrpc := Version;
      R.method := Method;
   end Set_Common_Notification_Fields;

end LSP.Messages.Common_Writers;

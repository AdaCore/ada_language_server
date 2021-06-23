------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
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

with LSP.Types; use LSP.Types;

with LSP.JSON_Streams;

package body LSP.Messages.Common_Writers is

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
      Write_String (JS, "jsonrpc", V.jsonrpc);

      if V.id.Is_Number then
         Write_Number (JS, "id", V.id.Number);
      elsif not V.id.String.Is_Empty then
         Write_String (JS, "id", V.id.String);
      end if;

      Write_String (JS, "method", V.method);
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
      Write_String (JS, "jsonrpc", V.jsonrpc);
      Write_String (JS, "method", V.method);
   end Write_Notification_Prefix;

end LSP.Messages.Common_Writers;

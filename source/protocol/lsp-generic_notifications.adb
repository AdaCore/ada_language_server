------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2020, AdaCore                     --
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

with Magic.JSON.Streams.Readers;
with Magic.Strings.Conversions;

with LSP.Types;
with LSP.Messages.Common_Writers;

package body LSP.Generic_Notifications is

   ------------
   -- Decode --
   ------------

   function Decode
     (JS : not null access LSP.JSON_Streams.JSON_Stream)
        return Notification is
   begin
      return V : Notification do
         Read (JS, V);
      end return;
   end Decode;

   ----------
   -- Read --
   ----------

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Notification)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
               Magic.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;

            if Key = "jsonrpc" then
               LSP.Types.LSP_String'Read (S, V.jsonrpc);
            elsif Key = "method" then
               LSP.Types.LSP_String'Read (S, V.method);
            elsif Key = "params" then
               T'Read (S, V.params);
            else
               JS.R.Read_Next;  --  Skip corresponding value
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read;

   -----------
   -- Visit --
   -----------

   procedure Visit
     (Self    : Notification;
      Handler : access Visitor) is
   begin
      raise Program_Error;
   end Visit;

   -----------
   -- Write --
   -----------

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Notification)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      LSP.Messages.Common_Writers.Write_Notification_Prefix (S, V);
      JS.Key ("params");
      T'Write (S, V.params);
      JS.End_Object;
   end Write;

end LSP.Generic_Notifications;

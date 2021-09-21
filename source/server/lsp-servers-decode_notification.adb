------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

with Ada.Tags; use Ada.Tags;
with Ada.Tags.Generic_Dispatching_Constructor;

with VSS.JSON.Pull_Readers.Simple;

with LSP.JSON_Streams;
with LSP.Messages.Server_Notifications; use LSP.Messages.Server_Notifications;

function LSP.Servers.Decode_Notification
   (Document : VSS.Text_Streams.Input_Text_Stream_Access;
    Method   : VSS.Strings.Virtual_String)
    return LSP.Messages.Server_Notifications.Server_Notification'Class
is

   function Constructor is new Ada.Tags.Generic_Dispatching_Constructor
     (T           => LSP.Messages.Server_Notifications.Server_Notification,
      Parameters  => LSP.JSON_Streams.JSON_Stream,
      Constructor => LSP.Messages.Server_Notifications.Decode);

   R   : aliased VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
   JS  : aliased LSP.JSON_Streams.JSON_Stream
     (Is_Server_Side => True, R => R'Unchecked_Access);
   Tag : constant Ada.Tags.Tag :=
     LSP.Messages.Server_Notifications.Method_To_Tag (Method);

begin
   R.Set_Stream (Document);
   JS.R.Read_Next;
   pragma Assert (JS.R.Is_Start_Document);
   JS.R.Read_Next;

   if Tag in Ada.Tags.No_Tag then
      raise Unknown_Method;
   else
      return Constructor (Tag, JS'Access);
   end if;
end LSP.Servers.Decode_Notification;

------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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
--
--  This package provides a base class to implement custom commands.
--  The concrete LSP server provides the set of commands by inheriting from
--  this base class. Read_XXX procedures declared in LSP.Messages construct
--  corresponding object using generic dispatching constructor on the server
--  side. On the client side any command is an opaque object and will always be
--  represented as a JSON object.

with Ada.Tags;

with VSS.String_Vectors;

with LSP.Errors;
with LSP.JSON_Streams;
limited with LSP.Client_Message_Receivers;
limited with LSP.Server_Notification_Receivers;
with GNATCOLL.Refcount;

package LSP.Commands is

   type Command is abstract tagged null record;
   type Command_Access is access all Command'Class with Storage_Size => 0;

   function Create
     (Stream : not null access LSP.JSON_Streams.JSON_Stream'Class)
       return Command is abstract;

   procedure Execute
     (Self    : Command;
      Handler : not null access
        LSP.Server_Notification_Receivers.Server_Notification_Receiver'Class;
      Client  : not null access
        LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Error  : in out LSP.Errors.Optional_ResponseError) is abstract;
   --  Execute given command and return Error is something went wrong.
   --  Use Client object to send requests and notifications to the client.
   --  Commands are executed on the server side only.
   --  The Handler is the access to the message handler executing the command.

   procedure Register (Value : Ada.Tags.Tag);
   --  Register a new command type. The type should be in Command'Class

   function All_Commands return VSS.String_Vectors.Virtual_String_Vector;
   --  Return all registered command names.

   package Command_Shared_Pointers is
     new GNATCOLL.Refcount.Shared_Pointers (Command'Class);

   type Command_Pointer is new Command_Shared_Pointers.Ref with null record;
   --  Smart pointer to Command'Class for garbage collection.

end LSP.Commands;

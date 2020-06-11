------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding;

with Magic.Stream_Element_Buffers;

with Spawn.Environments;
with Spawn.Processes;
with Spawn.String_Vectors;

package LSP.Raw_Clients is
   type Raw_Client is tagged limited private;

   procedure On_Error
     (Self  : in out Raw_Client;
      Error : String) is null;
   --  Callback to be called on LSP server termination.

   procedure On_Standard_Error_Message
     (Self : in out Raw_Client;
      Text : String);
   --  Callback to be called when stderror has data.

   procedure On_Exception
     (Self       : in out Raw_Client;
      Occurrence : Ada.Exceptions.Exception_Occurrence) is null;
   --  Called when an exception is raised by the underlying listener

   procedure On_Raw_Message
     (Self : in out Raw_Client;
      Data : Ada.Strings.Unbounded.Unbounded_String) is null;
   --  Callback to be called on new message from LSP server.

   procedure On_Started (Self : in out Raw_Client) is null;
   --  Callback to be called on successful startup of the server process.

   procedure On_Finished (Self : in out Raw_Client) is null;
   --  Callback to be called on finish of server process.

   procedure Set_Arguments
     (Self      : in out Raw_Client'Class;
      Arguments : Spawn.String_Vectors.UTF_8_String_Vector);
   --  LSP server command line arguments

   procedure Set_Environment
     (Self        : in out Raw_Client'Class;
      Environment : Spawn.Environments.Process_Environment);
   --  LSP server process environment

   procedure Set_Working_Directory
     (Self      : in out Raw_Client'Class;
      Directory : Ada.Strings.UTF_Encoding.UTF_8_String);
   --  LSP server working directory

   procedure Set_Program
     (Self    : in out Raw_Client'Class;
      Program : Ada.Strings.UTF_Encoding.UTF_8_String);
   --  LSP server executables name

   procedure Start (Self : in out Raw_Client'Class);
   --  Start LSP server

   function Is_Server_Running (Self : Raw_Client'Class) return Boolean;
   --  Check is LSP server is running

   procedure Stop (Self : in out Raw_Client'Class);
   --  Stop LSP server

   function Exit_Code (Self : Raw_Client'Class) return Integer;
   --  Check is LSP server is running

   procedure Send_Message
     (Self : in out Raw_Client'Class;
      Text : Ada.Strings.Unbounded.Unbounded_String);
   --  Send a request to LSP server. Text should contain valid JSON in
   --  UTF-8 encoding.

   procedure Send_Buffer
     (Self : in out Raw_Client'Class;
      Text : Magic.Stream_Element_Buffers.Stream_Element_Buffer);
   --  Send a request to LSP server. Text should contain valid JSON in
   --  UTF-8 encoding.

   function Can_Send_Message (Self : Raw_Client'Class) return Boolean;
   --  Return True when server's process is running and send queue is empty,
   --  thus send operation can start immidiately.

private
   type Listener (Client : access Raw_Client'Class) is limited
     new Spawn.Processes.Process_Listener with null record;

   overriding procedure Error_Occurred
    (Self          : in out Listener;
     Process_Error : Integer);

   overriding procedure Standard_Output_Available (Self : in out Listener);
   overriding procedure Standard_Input_Available (Self : in out Listener);
   overriding procedure Standard_Error_Available (Self : in out Listener);
   overriding procedure Started (Self : in out Listener);
   overriding procedure Finished
     (Self      : in out Listener;
      Exit_Code : Integer);

   overriding procedure Exception_Occurred
     (Self       : in out Listener;
      Occurrence : Ada.Exceptions.Exception_Occurrence);

   type Raw_Client is tagged limited record
      Server    : Spawn.Processes.Process;
      Listener  : aliased Raw_Clients.Listener (Raw_Client'Unchecked_Access);

      Standard_Input_Available : Boolean := False;

      To_Write  : Ada.Strings.Unbounded.Unbounded_String; --  Output data
      Written   : Natural := 0;  --  How much we have written from To_Write
      To_Read   : Natural := 0;
      --  How much we should read in the Buffer to get complete JSON
      --  Zero means we should read protocol headers
      Buffer    : Ada.Strings.Unbounded.Unbounded_String;
      --  Part of input
   end record;

end LSP.Raw_Clients;

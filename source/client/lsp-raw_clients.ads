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

with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding;

with Spawn.Environments;
with Spawn.Processes;
with Spawn.String_Vectors;

package LSP.Raw_Clients is
   type Raw_Client is tagged limited private;

   not overriding procedure On_Error
     (Self  : in out Raw_Client;
      Error : String) is null;
   --  Callback to be called on LSP server termination.

   not overriding procedure On_Raw_Message
     (Self : in out Raw_Client;
      Data : Ada.Strings.Unbounded.Unbounded_String) is null;
   --  Callback to be called on new message from LSP server.

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

   procedure Send_Request
     (Self : in out Raw_Client'Class;
      Text : Ada.Strings.Unbounded.Unbounded_String);
   --  Send a request to LSP server. Text should contain valid JSON in
   --  UTF-8 encoding.

private
   type Listener (Client : access Raw_Client'Class) is limited
     new Spawn.Processes.Process_Listener with null record;

   overriding procedure Error_Occurred
    (Self          : in out Listener;
     Process_Error : Integer);

   overriding procedure Standard_Output_Available (Self : in out Listener);
   overriding procedure Standard_Input_Available (Self : in out Listener);
   overriding procedure Standard_Error_Available (Self : in out Listener);

   type Raw_Client is tagged limited record
      Server    : Spawn.Processes.Process;
      Listener  : aliased Raw_Clients.Listener (Raw_Client'Unchecked_Access);
      Can_Write : Boolean := False;
      To_Write  : Ada.Strings.Unbounded.Unbounded_String;
      Written   : Natural := 0;
      To_Read   : Natural := 0;
      --  How much we should read in the Buffer to get complete JSON
      --  Zero means we should read protocol headers
      Buffer    : Ada.Strings.Unbounded.Unbounded_String;
      --  Part of input
   end record;

end LSP.Raw_Clients;

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

--  Asynchronous API with listener pattern

with Ada.Streams;

with Spawn.Environments;
with Spawn.String_Vectors;

private with Ada.Finalization;
private with Ada.Strings.Unbounded;
private with Interfaces.C;

package Spawn.Processes is

   type Process_Listener is limited interface;
   type Process_Listener_Access is access all Process_Listener'Class;

   not overriding procedure Standard_Output_Available
    (Self : in out Process_Listener) is null;
   --  Called once when it's possible to read data again.

   not overriding procedure Standard_Error_Available
    (Self : in out Process_Listener) is null;
   --  Called once when it's possible to read data again.

   not overriding procedure Standard_Input_Available
    (Self : in out Process_Listener) is null;
   --  Called once when it's possible to write data again.

   not overriding procedure Started (Self : in out Process_Listener) is null;

   not overriding procedure Finished
    (Self      : in out Process_Listener;
     Exit_Code : Integer) is null;

   not overriding procedure Error_Occurred
    (Self          : in out Process_Listener;
     Process_Error : Integer) is null;

   type Process_Error is (Failed_To_Start);

   type Process_Status is
    (Not_Running,
     Starting,
     Running);

   type Process is tagged limited private;

   function Arguments (Self : Process'Class)
     return Spawn.String_Vectors.UTF_8_String_Vector;
   procedure Set_Arguments
     (Self      : in out Process'Class;
      Arguments : Spawn.String_Vectors.UTF_8_String_Vector)
        with Pre => Self.Status = Not_Running;
   --  Command line arguments

   function Environment (Self : Process'Class)
     return Spawn.Environments.Process_Environment;

   procedure Set_Environment
     (Self        : in out Process'Class;
      Environment : Spawn.Environments.Process_Environment)
        with Pre => Self.Status = Not_Running;
   --  Process environment

   function Working_Directory (Self : Process'Class) return UTF_8_String;
   procedure Set_Working_Directory
     (Self      : in out Process'Class;
      Directory : UTF_8_String)
        with Pre => Self.Status = Not_Running;
   --  Working directory

   function Program (Self : Process'Class) return UTF_8_String;
   procedure Set_Program
     (Self    : in out Process'Class;
      Program : UTF_8_String)
        with Pre => Self.Status = Not_Running;
   --  Executables name

   procedure Start (Self : in out Process'Class)
     with Pre => Self.Status = Not_Running;

   function Status (Self : Process'Class) return Process_Status;

   function Exit_Code (Self : Process'Class) return Integer
     with Pre => Self.Status = Not_Running;

   function Listener (Self : Process'Class) return Process_Listener_Access;
   procedure Set_Listener
     (Self     : in out Process'Class;
      Listener : Process_Listener_Access)
        with Pre => Self.Status = Not_Running;
   --  Process's events listener

   procedure Close_Standard_Input (Self : in out Process'Class);
   --  Do nothing if Self.Status /= Running

   procedure Write_Standard_Input
     (Self : in out Process'Class;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   --  Do nothing if Self.Status /= Running. If no data was written you will
   --  get Standard_Input_Available notification latter.

   procedure Close_Standard_Output (Self : in out Process'Class);
   --  Do nothing if Self.Status /= Running

   procedure Read_Standard_Output
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   --  Returns available data received throgh standard output stream. If no
   --  data was read you will get Standard_Output_Available notification latter

   procedure Close_Standard_Error (Self : in out Process'Class);
   --  Do nothing if Self.Status /= Running

   procedure Read_Standard_Error
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

private

   type Pipe_Kinds is (Stdin, Stdout, Stderr, Launch);
   --  Launch is an extra pipe to report starting errors
   subtype Standard_Pipe is Pipe_Kinds range Stdin .. Stderr;

   type Pipe_Array is array (Pipe_Kinds) of Interfaces.C.int;
   --  File descriptors array

   type Index_Array is array (Pipe_Kinds) of Natural;
   --  Index in poll for each descriptors array

   type Process is new Ada.Finalization.Limited_Controlled with record
      Arguments   : Spawn.String_Vectors.UTF_8_String_Vector;
      Environment : Spawn.Environments.Process_Environment :=
        Spawn.Environments.System_Environment;
      Exit_Code   : Integer := -1;
      Status      : Process_Status := Not_Running;
      Listener    : Process_Listener_Access;
      Program     : Ada.Strings.Unbounded.Unbounded_String;
      Directory   : Ada.Strings.Unbounded.Unbounded_String;

      pid   : Interfaces.C.int := 0;
      pipe  : Pipe_Array := (others => 0);
      Index : Index_Array := (others => 0);
   end record;

   overriding procedure Finalize (Self : in out Process);

   type Process_Access is access all Process'Class;

end Spawn.Processes;

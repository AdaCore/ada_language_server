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
--
--  This is a test to check calls in unexpected order, such as
--  * write to standard input while process has not been started
--  * write to standard input while process has not finished
--
--  To be portable this test launch itself with "-slave" option.

with Ada.Command_Line;
with Ada.Directories;
with Ada.Streams;
with Ada.Text_IO;

with Spawn.Processes;
with Spawn.Processes.Monitor_Loop;
with Spawn.String_Vectors;

procedure Spawn_Unexpected is

   procedure Write_Standard_Input
     (Process : in out Spawn.Processes.Process;
      Sample  : Character);
   --  Write some data to Process's Standard_Input.

   package Listeners is
      type Listener is limited new Spawn.Processes.Process_Listener with record
         P       : Spawn.Processes.Process;
         Stopped : Boolean := False;
      end record;

      overriding procedure Standard_Input_Available
        (Self : in out Listener);
      --  Called once when it's possible to write data again.

      overriding procedure Started (Self : in out Listener);

      overriding procedure Finished
        (Self      : in out Listener;
         Exit_Code : Integer);

      overriding procedure Error_Occurred
        (Self          : in out Listener;
         Process_Error : Integer);

   end Listeners;

   package body Listeners is

      overriding procedure Standard_Input_Available
        (Self : in out Listener)
      is
         pragma Unreferenced (Self);
      begin
         Ada.Text_IO.Put_Line ("Standard_Input_Available");
      end Standard_Input_Available;

      overriding procedure Started (Self : in out Listener) is
         pragma Unreferenced (Self);
      begin
         Ada.Text_IO.Put_Line ("Started");
      end Started;

      overriding procedure Finished
        (Self      : in out Listener;
         Exit_Code : Integer) is
      begin
         Ada.Text_IO.Put_Line ("Finished" & (Exit_Code'Img));
         Self.Stopped := True;
      end Finished;

      overriding procedure Error_Occurred
        (Self          : in out Listener;
         Process_Error : Integer)
      is
         pragma Unreferenced (Self);
      begin
         Ada.Text_IO.Put_Line ("Error_Occurred:" & (Process_Error'Img));
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end Error_Occurred;

   end Listeners;

   --------------------------
   -- Write_Standard_Input --
   --------------------------

   procedure Write_Standard_Input
     (Process : in out Spawn.Processes.Process;
      Sample  : Character)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Chunk : constant Ada.Streams.Stream_Element_Array :=
        (1 .. 10 => Character'Pos (Sample));
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Process.Write_Standard_Input (Chunk, Last);
      pragma Assert (Last = Chunk'Length);
   end Write_Standard_Input;

   use all type Spawn.Processes.Process_Status;

   Cmd  : constant String :=
     Ada.Directories.Full_Name (Ada.Command_Line.Command_Name);
   Args : Spawn.String_Vectors.UTF_8_String_Vector;
   L    : aliased Listeners.Listener;
begin
   if Ada.Command_Line.Argument_Count >= 1
     and then Ada.Command_Line.Argument (1) = "-slave"
   then
      --  This is a subprocess, exit.
      return;
   end if;

   Args.Append ("-slave");
   L.P.Set_Program (Cmd);
   L.P.Set_Arguments (Args);
   L.P.Set_Working_Directory (Ada.Directories.Current_Directory);
   L.P.Set_Listener (L'Unchecked_Access);

   Write_Standard_Input (L.P, Sample => '1');

   L.P.Start;

   while not (L.Stopped and L.P.Status = Not_Running) loop
      Spawn.Processes.Monitor_Loop (1);
   end loop;

   Write_Standard_Input (L.P, Sample => '2');
end Spawn_Unexpected;

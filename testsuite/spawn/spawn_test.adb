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

with Ada.Text_IO;
with Ada.Streams;

with Spawn.Processes;
with Spawn.Processes.Monitor_Loop;
with Spawn.String_Vectors;

procedure Spawn_Test is

   P : Spawn.Processes.Process;

   package Listeners is
      type Listener is new Spawn.Processes.Process_Listener with record
         Stopped : Boolean := False;
      end record;

      overriding procedure Standard_Output_Available
        (Self : in out Listener);

      overriding procedure Standard_Error_Available
        (Self : in out Listener);

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

      overriding procedure Standard_Output_Available
        (Self : in out Listener)
      is
         pragma Unreferenced (Self);
         Data : Ada.Streams.Stream_Element_Array (1 .. 16);
         Last : Ada.Streams.Stream_Element_Count;
      begin
         Ada.Text_IO.Put_Line ("Standard_Output_Available");
         P.Read_Standard_Output (Data, Last);

         for X of Data (1 .. Last) loop
            Ada.Text_IO.Put (Character'Val (X));
         end loop;
      end Standard_Output_Available;

      overriding procedure Standard_Error_Available
        (Self : in out Listener)
      is
         pragma Unreferenced (Self);
      begin
         Ada.Text_IO.Put_Line ("Standard_Error_Available");
      end Standard_Error_Available;

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
      end Error_Occurred;

   end Listeners;

   Args : Spawn.String_Vectors.UTF_8_String_Vector;
   L    : aliased Listeners.Listener;
begin
   Args.Append ("Hello World");
   P.Set_Program ("/bin/echo");
   P.Set_Arguments (Args);
   P.Set_Working_Directory ("/tmp");
   P.Set_Listener (L'Unchecked_Access);
   P.Start;
   while not L.Stopped loop
      Spawn.Processes.Monitor_Loop (1);
   end loop;
end Spawn_Test;

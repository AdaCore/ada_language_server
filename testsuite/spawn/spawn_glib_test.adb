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
with Spawn.String_Vectors;

with Glib.Application;

procedure Spawn_Glib_Test is

   procedure Activate_Callback
     (Application : access Glib.Application.Gapplication_Record'Class);

   package Listeners is
      type Listener is new Spawn.Processes.Process_Listener with record
         Stopped : Boolean := False;
         App     : Glib.Application.Gapplication;
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

   P : Spawn.Processes.Process;
   L : aliased Listeners.Listener;

   package body Listeners is

      overriding procedure Standard_Output_Available
        (Self : in out Listener)
      is
         pragma Unreferenced (Self);
         use type Ada.Streams.Stream_Element_Offset;
         Data : Ada.Streams.Stream_Element_Array (1 .. 16);
         Last : Ada.Streams.Stream_Element_Count;
      begin
         Ada.Text_IO.Put_Line ("Standard_Output_Available");
         loop
            P.Read_Standard_Output (Data, Last);

            exit when Last in 0;

            for X of Data (1 .. Last) loop
               Ada.Text_IO.Put (Character'Val (X));
            end loop;

            if Last >= 2
              and then Character'Val (Data (1)) = 'O'
              and then Character'Val (Data (2)) = 'K'
            then
               P.Close_Standard_Input;
            end if;
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
         Data : constant Ada.Streams.Stream_Element_Array :=
           (1 => Character'Pos ('O'),
            2 => Character'Pos ('K'),
            3 => 10);
         Last : Ada.Streams.Stream_Element_Count;
      begin
         Ada.Text_IO.Put_Line ("Standard_Input_Available");
         P.Write_Standard_Input (Data, Last);
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
         Self.App.Release;
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

   -----------------------
   -- Activate_Callback --
   -----------------------

   procedure Activate_Callback
     (Application : access Glib.Application.Gapplication_Record'Class)
   is
      Args : Spawn.String_Vectors.UTF_8_String_Vector;
   begin
      L.App := Application.all'Access;
      Application.Hold;
      Args.Append ("/tmp/aaa.txt");
--      P.Set_Program ("/bin/echo");
      P.Set_Program ("/usr/bin/tee");
      P.Set_Arguments (Args);
      P.Set_Working_Directory ("/tmp");
      P.Set_Listener (L'Unchecked_Access);
      P.Start;
   end Activate_Callback;

   App  : constant Glib.Application.Gapplication :=
     Glib.Application.Gapplication_New
       (Flags => Glib.Application.G_Application_Flags_None);
   X : Glib.Gint;
   pragma Unreferenced (X);
begin
   App.On_Activate (Activate_Callback'Unrestricted_Access);
   X := App.Run;
end Spawn_Glib_Test;

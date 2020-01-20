------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2020, AdaCore                       --
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

with Ada.Command_Line;
with Ada.Streams;
with Ada.Text_IO;

with Glib.Application;

with Spawn.Processes;
with Spawn.String_Vectors;

procedure Spawn_Glib_Args_Test is

   package Listeners is

      type Listener is new Spawn.Processes.Process_Listener with record
         App     : Glib.Application.Gapplication;
         Process : access Spawn.Processes.Process'Class;
      end record;

      overriding procedure Standard_Output_Available
        (Self : in out Listener);

      overriding procedure Standard_Error_Available
        (Self : in out Listener);

      overriding procedure Finished
        (Self      : in out Listener;
         Exit_Code : Integer);

      overriding procedure Error_Occurred
        (Self          : in out Listener;
         Process_Error : Integer);

   end Listeners;

   procedure Activate_Callback
     (Application : access Glib.Application.Gapplication_Record'Class);

   package body Listeners is

      --------------------
      -- Error_Occurred --
      --------------------

      overriding procedure Error_Occurred
        (Self          : in out Listener;
         Process_Error : Integer) is
      begin
         Ada.Text_IO.Put_Line ("Error_Occurred:" & (Process_Error'Img));
         Self.App.Release;
      end Error_Occurred;

      --------------
      -- Finished --
      --------------

      overriding procedure Finished
        (Self      : in out Listener;
         Exit_Code : Integer) is
      begin
         Ada.Text_IO.Put_Line ("Finished" & (Exit_Code'Img));
         Self.App.Release;
      end Finished;

      ------------------------------
      -- Standard_Error_Available --
      ------------------------------

      overriding procedure Standard_Error_Available
        (Self : in out Listener)
      is
         Data : Ada.Streams.Stream_Element_Array (1 .. 256);
         Last : Ada.Streams.Stream_Element_Count;

      begin
         Self.Process.Read_Standard_Error (Data, Last);

         for X of Data (1 .. Last) loop
            Ada.Text_IO.Put (Character'Val (X));
         end loop;
      end Standard_Error_Available;

      -------------------------------
      -- Standard_Output_Available --
      -------------------------------

      overriding procedure Standard_Output_Available
        (Self : in out Listener)
      is
         use type Ada.Streams.Stream_Element_Offset;

         Data : Ada.Streams.Stream_Element_Array (1 .. 256);
         Last : Ada.Streams.Stream_Element_Count;

      begin
         Self.Process.Read_Standard_Output (Data, Last);

         for X of Data (1 .. Last) loop
            Ada.Text_IO.Put (Character'Val (X));
         end loop;
      end Standard_Output_Available;

   end Listeners;

   P : aliased Spawn.Processes.Process;
   L : aliased Listeners.Listener;

   -----------------------
   -- Activate_Callback --
   -----------------------

   procedure Activate_Callback
     (Application : access Glib.Application.Gapplication_Record'Class)
   is
      Args : Spawn.String_Vectors.UTF_8_String_Vector;

   begin
      L.App := Application.all'Access;
      L.Process := P'Access;

      Application.Hold;

      P.Set_Program ("./spawn_glib_args_test.exe");
      Args.Append ("Hello, world with spaces!");
      P.Set_Arguments (Args);
      P.Set_Working_Directory ("/tmp");
      P.Set_Listener (L'Unchecked_Access);
      P.Start;
   end Activate_Callback;

   App   : constant Glib.Application.Gapplication :=
     Glib.Application.Gapplication_New
       (Flags => Glib.Application.G_Application_Flags_None);
   Dummy : Glib.Gint;

begin
   if Ada.Command_Line.Argument_Count = 0 then
      App.On_Activate (Activate_Callback'Unrestricted_Access);
      Dummy := App.Run;

   elsif Ada.Command_Line.Argument_Count /= 1 then
      raise Program_Error;
   end if;
end Spawn_Glib_Args_Test;

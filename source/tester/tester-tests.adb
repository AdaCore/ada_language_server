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
with GNAT.OS_Lib;

with Spawn.String_Vectors;
with Spawn.Processes.Monitor_Loop;

package body Tester.Tests is

   type Command_Kind is (Start, Stop);

   procedure Do_Start
     (Self    : in out Test'Class;
      Command : GNATCOLL.JSON.JSON_Value);

   procedure Do_Stop
     (Self    : in out Test'Class;
      Command : GNATCOLL.JSON.JSON_Value);

   --------------
   -- Do_Abort --
   --------------

   not overriding procedure Do_Abort (Self : Test) is
   begin
      GNAT.OS_Lib.OS_Exit (1);
   end Do_Abort;

   -------------
   -- Do_Fail --
   -------------

   not overriding procedure Do_Fail (Self : Test; Message : String) is
      pragma Unreferenced (Self);
   begin
      Ada.Text_IO.Put_Line ("Test failed:" & Message);
   end Do_Fail;

   --------------
   -- Do_Start --
   --------------

   procedure Do_Start
     (Self    : in out Test'Class;
      Command : GNATCOLL.JSON.JSON_Value)
   is
      Cmd : constant GNATCOLL.JSON.JSON_Array := Command.Get ("cmd");
      Args : Spawn.String_Vectors.UTF_8_String_Vector;
   begin
      for J in 2 .. GNATCOLL.JSON.Length (Cmd) loop
         Args.Append (GNATCOLL.JSON.Get (Cmd, J).Get);
      end loop;

      Self.Server.Set_Program (GNATCOLL.JSON.Get (Cmd, 1).Get);
      Self.Server.Set_Arguments (Args);
      Self.Server.Set_Listener (Self.Listener'Unchecked_Access);
      Self.Server.Start;

      loop
         Spawn.Processes.Monitor_Loop (Timeout => 1);
         exit when Self.Server.Status in Spawn.Processes.Running;
      end loop;
   end Do_Start;

   -------------
   -- Do_Stop --
   -------------

   procedure Do_Stop
     (Self    : in out Test'Class;
      Command : GNATCOLL.JSON.JSON_Value)
   is
      Exit_Code : constant Integer := Command.Get ("exit_code").Get;
   begin
      Self.Server.Close_Standard_Input;

      loop
         Spawn.Processes.Monitor_Loop (Timeout => 1);
         exit when Self.Server.Status in Spawn.Processes.Not_Running;
      end loop;

      if Self.Server.Exit_Code /= Exit_Code then
         Self.Do_Fail ("Unexpected exit code:" & (Self.Server.Exit_Code'Img));
      end if;
   end Do_Stop;

   --------------------
   -- Error_Occurred --
   --------------------

   overriding procedure Error_Occurred
    (Self          : in out Listener;
     Process_Error : Integer)
   is
   begin
      Ada.Text_IO.Put_Line ("Error on server start:" & (Process_Error'Img));
      Ada.Text_IO.Put ("   ");
      Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message (Process_Error));
      Self.Test.Do_Abort;
   end Error_Occurred;

   ---------------------
   -- Execute_Command --
   ---------------------

   not overriding procedure Execute_Command
     (Self    : in out Test;
      Command : GNATCOLL.JSON.JSON_Value)
   is
      procedure Execute (Name : String; Value : GNATCOLL.JSON.JSON_Value);

      -------------
      -- Execute --
      -------------

      procedure Execute (Name : String; Value : GNATCOLL.JSON.JSON_Value) is
         Kind : constant Command_Kind := Command_Kind'Value (Name);
      begin
         Self.Index := Self.Index + 1;

         case Kind is
            when Start =>
               Self.Do_Start (Value);
            when Stop =>
               Self.Do_Stop (Value);
         end case;
      end Execute;

   begin
      Command.Map_JSON_Object (Execute'Access);
   end Execute_Command;

   ---------
   -- Run --
   ---------

   not overriding procedure Run
     (Self     : in out Test;
      Commands : GNATCOLL.JSON.JSON_Array)
   is
   begin
      while Self.Index <= GNATCOLL.JSON.Length (Commands) loop
         declare
            Command : constant GNATCOLL.JSON.JSON_Value :=
              GNATCOLL.JSON.Get (Commands, Self.Index);
         begin
            Self.Execute_Command (Command);
         end;
      end loop;
   end Run;

end Tester.Tests;

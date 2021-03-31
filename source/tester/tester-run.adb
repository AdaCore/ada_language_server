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

with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNATCOLL.JSON;

with Tester.Macros;
with Tester.Tests;

with Spawn.Environments;

procedure Tester.Run is
   Env  : constant Spawn.Environments.Process_Environment :=
     Spawn.Environments.System_Environment;

   JSON : GNATCOLL.JSON.JSON_Value;
begin
   if not (Ada.Command_Line.Argument_Count = 1
     or else (Ada.Command_Line.Argument_Count = 2
              and then Ada.Command_Line.Argument (1) = "--debug"))
   then
      Ada.Text_IO.Put_Line ("Usage:");
      Ada.Text_IO.Put_Line
        ("   " & Ada.Command_Line.Command_Name & " [options] test.json");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Options are:");
      Ada.Text_IO.Put_Line
        ("  --debug  disable timeouts and pause after server start");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   declare
      File  : constant String := Ada.Command_Line.Argument
        (Ada.Command_Line.Argument_Count);
      Input : Ada.Text_IO.File_Type;
      Text  : Ada.Strings.Unbounded.Unbounded_String;
   begin
      if not Ada.Directories.Exists (File) then
         Ada.Text_IO.Put_Line ("No such file: " & File);
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      end if;

      Ada.Text_IO.Open (Input, Ada.Text_IO.In_File, File);

      while not Ada.Text_IO.End_Of_File (Input) loop
         Ada.Strings.Unbounded.Append
           (Text, Ada.Text_IO.Get_Line (Input));
      end loop;

      Ada.Text_IO.Close (Input);
      JSON := GNATCOLL.JSON.Read (Text, File);
      Tester.Macros.Expand (JSON, Env, File);

      declare
         Test : Tester.Tests.Test;
      begin
         Test.Run (JSON.Get, Debug => Ada.Command_Line.Argument_Count = 2);
      end;
   end;
end Tester.Run;

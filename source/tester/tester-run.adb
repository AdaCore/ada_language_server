------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
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

with VSS.Command_Line;
with VSS.Strings;
with VSS.Strings.Conversions;
with VSS.String_Vectors;

with Spawn.Environments;

with Tester.Macros;
with Tester.Tests;

procedure Tester.Run is

   package Options is
      --  Command line options and arguments

      Debug : constant VSS.Command_Line.Binary_Option :=
        (Short_Name  => "d",
         Long_Name   => "debug",
         Description => "disable timeouts then pause after server start");

      File  : constant VSS.Command_Line.Positional_Option :=
        (Name  => "test.json",
         Description => "JSON test script");
   end Options;

   Env  : constant Spawn.Environments.Process_Environment :=
     Spawn.Environments.System_Environment;

   JSON : GNATCOLL.JSON.JSON_Value;
begin
   VSS.Command_Line.Add_Option (Options.Debug);
   VSS.Command_Line.Add_Option (Options.File);
   VSS.Command_Line.Process;  --  This terminates process on option's error

   if not Options.File.Is_Specified then
      declare
         use type VSS.Strings.Virtual_String;
         Usage : VSS.String_Vectors.Virtual_String_Vector;
      begin
         Usage.Append ("Usage:");
         Usage.Append
           ("   tester-run [options] " & Options.File.Name);
         Usage.Append ("");
         Usage.Append ("Options are:");
         Usage.Append
           ("  --" & Options.Debug.Long_Name
            & " (-" & Options.Debug.Short_Name & ")"
            & "  " & Options.Debug.Description);
         VSS.Command_Line.Report_Error (Usage.Join_Lines (VSS.Strings.LF));
      end;
   end if;

   declare
      File  : constant String := VSS.Strings.Conversions.To_UTF_8_String
        (Options.File.Value);
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
         Test.Run (JSON.Get, Debug => Options.Debug.Is_Specified);
      end;
   end;
end Tester.Run;

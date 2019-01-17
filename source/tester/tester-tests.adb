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

with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.OS_Lib;

with Spawn.String_Vectors;
with Spawn.Processes.Monitor_Loop;

package body Tester.Tests is

   type Command_Kind is (Start, Stop, Send);

   procedure Do_Start
     (Self    : in out Test'Class;
      Command : GNATCOLL.JSON.JSON_Value);

   procedure Do_Stop
     (Self    : in out Test'Class;
      Command : GNATCOLL.JSON.JSON_Value);

   procedure Do_Send
     (Self    : in out Test'Class;
      Command : GNATCOLL.JSON.JSON_Value);

   Is_Windows : constant Boolean := GNAT.OS_Lib.Directory_Separator = '\';

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
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Do_Fail;

   -------------
   -- Do_Send --
   -------------

   procedure Do_Send
     (Self    : in out Test'Class;
      Command : GNATCOLL.JSON.JSON_Value)
   is
      Request : constant GNATCOLL.JSON.JSON_Value := Command.Get ("request");
      Wait    : constant GNATCOLL.JSON.JSON_Array := Command.Get ("wait").Get;
      Text    : constant Ada.Strings.Unbounded.Unbounded_String :=
        Request.Write;
   begin
      Self.Waits := Wait;
      Self.Send_Message (Text);

      loop
         Spawn.Processes.Monitor_Loop (Timeout => 1);
         exit when GNATCOLL.JSON.Length (Self.Waits) = 0;
      end loop;
   end Do_Send;

   --------------
   -- Do_Start --
   --------------

   procedure Do_Start
     (Self    : in out Test'Class;
      Command : GNATCOLL.JSON.JSON_Value)
   is
      function Program_Name (Path : String) return String;
      --  Return full path to an exacutable designated by Path

      ------------------
      -- Program_Name --
      ------------------

      function Program_Name (Path : String) return String is
      begin
         if Is_Windows then
            return Ada.Directories.Full_Name (Path & ".exe");
         else
            return Ada.Directories.Full_Name (Path);
         end if;
      end Program_Name;

      Cmd  : constant GNATCOLL.JSON.JSON_Array := Command.Get ("cmd");
      Args : Spawn.String_Vectors.UTF_8_String_Vector;
   begin
      for J in 2 .. GNATCOLL.JSON.Length (Cmd) loop
         Args.Append (GNATCOLL.JSON.Get (Cmd, J).Get);
      end loop;

      Self.Set_Program (Program_Name (GNATCOLL.JSON.Get (Cmd, 1).Get));
      Self.Set_Arguments (Args);
      Self.Start;

      loop
         Spawn.Processes.Monitor_Loop (Timeout => 1);
         exit when Self.Is_Server_Running;
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
      Self.Stop;

      loop
         Spawn.Processes.Monitor_Loop (Timeout => 1);
         exit when not Self.Is_Server_Running;
      end loop;

      if Self.Exit_Code /= Exit_Code then
         Self.Do_Fail ("Unexpected exit code:" & (Self.Exit_Code'Img));
      end if;
   end Do_Stop;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error
     (Self  : in out Test;
      Error : String) is
   begin
      Ada.Text_IO.Put_Line ("Error on server:" & Error);
      Self.Do_Abort;
   end On_Error;

   --------------------
   -- On_Raw_Message --
   --------------------

   overriding procedure On_Raw_Message
     (Self : in out Test;
      Data : Ada.Strings.Unbounded.Unbounded_String)
   is
      use type GNATCOLL.JSON.JSON_Value_Type;

      procedure Sweep_Waits (JSON : GNATCOLL.JSON.JSON_Value);
      --  Find matching wait if any and delete it from Test.Waits

      function Match (Left, Right : GNATCOLL.JSON.JSON_Value) return Boolean
        with Pre => Left.Kind = GNATCOLL.JSON.JSON_Object_Type
                      and Right.Kind = GNATCOLL.JSON.JSON_Object_Type;
      --  Check if Left has all properties from Right.

      -----------
      -- Match --
      -----------

      function Match (Left, Right : GNATCOLL.JSON.JSON_Value) return Boolean is
         procedure Match_Proerty
           (Name  : String;
            Value : GNATCOLL.JSON.JSON_Value);
         --  Match one property in JSON object

         Success : Boolean := True;

         -------------------
         -- Match_Proerty --
         -------------------

         procedure Match_Proerty
           (Name  : String;
            Value : GNATCOLL.JSON.JSON_Value)
         is
            use type GNATCOLL.JSON.JSON_Value;
         begin
            if Left.Has_Field (Name) then
               declare
                  Prop : constant GNATCOLL.JSON.JSON_Value := Left.Get (Name);
               begin
                  if Prop.Kind /= Value.Kind then
                     Success := False;
                     return;
                  end if;

                  case Prop.Kind is
                     when GNATCOLL.JSON.JSON_Object_Type =>
                        if not Match (Prop, Value) then
                           Success := False;
                        end if;

                     when GNATCOLL.JSON.JSON_Array_Type =>
                        declare
                           L : constant GNATCOLL.JSON.JSON_Array := Prop.Get;
                           R : constant GNATCOLL.JSON.JSON_Array := Value.Get;
                           Len : constant Natural := GNATCOLL.JSON.Length (L);
                        begin
                           if Len /= GNATCOLL.JSON.Length (R) then
                              Success := False;
                              return;
                           end if;

                           for J in 1 .. Len loop
                              if not Match
                                (GNATCOLL.JSON.Get (L, J),
                                 GNATCOLL.JSON.Get (R, J))
                              then
                                 Success := False;
                                 return;
                              end if;
                           end loop;
                        end;

                     when others =>
                        if Prop /= Value then
                           Success := False;
                        end if;
                  end case;
               end;
            else
               Success := False;
            end if;
         end Match_Proerty;

      begin
         Right.Map_JSON_Object (Match_Proerty'Access);

         return Success;
      end Match;

      -----------------
      -- Sweep_Waits --
      -----------------

      procedure Sweep_Waits (JSON : GNATCOLL.JSON.JSON_Value) is
         Found : Natural := 0;
      begin
         for J in 1 .. GNATCOLL.JSON.Length (Self.Waits) loop
            declare
               Wait : constant GNATCOLL.JSON.JSON_Value :=
                 GNATCOLL.JSON.Get (Self.Waits, J);
            begin
               if Match (JSON, Wait) then
                  Found := J;
                  exit;
               end if;
            end;
         end loop;

         if Found /= 0 then
            declare
               Copy : GNATCOLL.JSON.JSON_Array;
            begin
               for J in 1 .. GNATCOLL.JSON.Length (Self.Waits) loop
                  if J /= Found then
                     declare
                        Wait : constant GNATCOLL.JSON.JSON_Value :=
                          GNATCOLL.JSON.Get (Self.Waits, J);
                     begin
                        GNATCOLL.JSON.Append (Copy, Wait);
                     end;
                  end if;
               end loop;

               Self.Waits := Copy;
            end;
         end if;
      end Sweep_Waits;

      JSON : constant GNATCOLL.JSON.JSON_Value :=
        GNATCOLL.JSON.Read (Data);
   begin
      Sweep_Waits (JSON);
   end On_Raw_Message;

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
            when Send =>
               Self.Do_Send (Value);
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

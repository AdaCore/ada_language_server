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
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib;

with Spawn.Processes.Monitor_Loop;

package body Tester.Tests is

   Max_Wait : constant := 2_000;
   --  Max number of milliseconds to wait on a given snippet

   type Command_Kind is (Start, Stop, Send, Comment);

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

   procedure Do_Abort (Self : Test) is
   begin
      GNAT.OS_Lib.OS_Exit (1);
   end Do_Abort;

   -------------
   -- Do_Fail --
   -------------

   procedure Do_Fail
     (Self : Test;
      Text : Spawn.String_Vectors.UTF_8_String_Vector)
   is
      pragma Unreferenced (Self);
   begin
      Ada.Text_IO.Put_Line ("Test failed!");

      for Line of Text loop
         Ada.Text_IO.Put_Line (Line);
      end loop;

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
      Sort    : constant GNATCOLL.JSON.JSON_Value := Command.Get ("sortReply");
      Text    : constant Ada.Strings.Unbounded.Unbounded_String :=
        Request.Write;

      Total_Milliseconds_Waited : Integer := 0;
      Timeout : constant := 100;
   begin
      Self.Waits := Wait;
      Self.Sort_Reply := Sort;
      Self.Send_Message (Text);

      loop
         Spawn.Processes.Monitor_Loop (Timeout => Timeout);
         exit when GNATCOLL.JSON.Length (Self.Waits) = 0;

         Total_Milliseconds_Waited := Total_Milliseconds_Waited + Timeout;
         if Total_Milliseconds_Waited > Max_Wait then
            declare
               Text : Spawn.String_Vectors.UTF_8_String_Vector;
            begin
               Text.Append ("Timed out waiting for the answer to:");
               Text.Append (GNATCOLL.JSON.Write (Request, False));
               Text.Append ("");
               Text.Append ("Remaining waits:");
               Text.Append
                 (GNATCOLL.JSON.Write
                    (GNATCOLL.JSON.Create (Self.Waits), False));
               Text.Append ("");
               Text.Append ("Last Message from server:");
               Text.Append (GNATCOLL.JSON.Write (Self.Last_Message, False));
               Self.Do_Fail (Text);
               exit;
            end;
         end if;
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
         declare
            Text : Spawn.String_Vectors.UTF_8_String_Vector;
         begin
            Text.Append ("Unexpected exit code:" & (Self.Exit_Code'Img));
            Self.Do_Fail (Text);
         end;
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

      function Match (Left, Right : GNATCOLL.JSON.JSON_Value) return Boolean;
      --  For JSON Objects check if Left has all properties from Right.
      --  For JSON Array check length and each item recursively.
      --  For JSON simple type check if Reft and Right are equal.

      procedure Sort_Reply
        (Name  : String;
         Value : GNATCOLL.JSON.JSON_Value);
      --  Let Name be field name in the reply to be sorted, Value is
      --  JSON string with a key. Then JSON[Name] should be array.
      --  Each element in the array should have key field. Sort this array.

      -----------
      -- Match --
      -----------

      function Match (Left, Right : GNATCOLL.JSON.JSON_Value) return Boolean is

         use type GNATCOLL.JSON.JSON_Value;

         procedure Match_Property
           (Name  : String;
            Value : GNATCOLL.JSON.JSON_Value);
         --  Match one property in JSON object

         Success : Boolean := True;

         --------------------
         -- Match_Property --
         --------------------

         procedure Match_Property
           (Name  : String;
            Value : GNATCOLL.JSON.JSON_Value) is
         begin
            if Success and Left.Has_Field (Name) then
               declare
                  Prop : constant GNATCOLL.JSON.JSON_Value := Left.Get (Name);
               begin
                  Success := Match (Prop, Value);
               end;
            else
               Success := False;
            end if;
         end Match_Property;

      begin
         if Left.Kind /= Right.Kind then
            return False;
         end if;

         case Left.Kind is
            when GNATCOLL.JSON.JSON_Object_Type =>

               Right.Map_JSON_Object (Match_Property'Access);

               return Success;

            when GNATCOLL.JSON.JSON_Array_Type =>
               declare
                  L : constant GNATCOLL.JSON.JSON_Array := Left.Get;
                  R : constant GNATCOLL.JSON.JSON_Array := Right.Get;
                  Len : constant Natural := GNATCOLL.JSON.Length (L);
               begin
                  if Len /= GNATCOLL.JSON.Length (R) then
                     return False;
                  end if;

                  for J in 1 .. Len loop
                     if not Match
                       (GNATCOLL.JSON.Get (L, J),
                        GNATCOLL.JSON.Get (R, J))
                     then
                        return False;
                     end if;
                  end loop;

                  return True;
               end;

            when GNATCOLL.JSON.JSON_String_Type =>
               if String'(GNATCOLL.JSON.Get (Right)) = "<ANY>" then
                  return True;
               else
                  return Left = Right;
               end if;

            when others =>

               return Left = Right;
         end case;
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

      ----------------
      -- Sort_Reply --
      ----------------

      procedure Sort_Reply
        (Name  : String;
         Value : GNATCOLL.JSON.JSON_Value)
      is
         function Less (Left, Right : GNATCOLL.JSON.JSON_Value) return Boolean;

         function Less
           (Left, Right : GNATCOLL.JSON.JSON_Value) return Boolean
         is
            Key : constant String := Value.Get;
            Left_Value  : constant GNATCOLL.JSON.JSON_Value := Left.Get (Key);
            Right_Value : constant GNATCOLL.JSON.JSON_Value := Right.Get (Key);
         begin
            if Left_Value.Kind /= Right_Value.Kind then
               return Left_Value.Kind < Right_Value.Kind;
            end if;

            case Left_Value.Kind is
               when GNATCOLL.JSON.JSON_String_Type =>
                  declare
                     Left_String : constant String := Left_Value.Get;
                     Right_String : constant String := Right_Value.Get;
                  begin
                     return Left_String < Right_String;
                  end;

               when others =>
                  raise Program_Error with "Not implemented";
            end case;
         end Less;

         List : GNATCOLL.JSON.JSON_Value := JSON.Get (Name);
      begin
         if List.Kind /= GNATCOLL.JSON.JSON_Array_Type then
            return;
         end if;

         List := JSON.Get (Name);
         GNATCOLL.JSON.Sort (List, Less'Access);
         JSON.Set_Field (Name, List);
      end Sort_Reply;

   begin
      Self.Last_Message := JSON;

      if not Self.Sort_Reply.Is_Empty then
         Self.Sort_Reply.Map_JSON_Object (Sort_Reply'Access);
      end if;

      Sweep_Waits (JSON);
   end On_Raw_Message;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command
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
            when Comment =>
               null;  --  Do nothing on comments
         end case;
      end Execute;

   begin
      Command.Map_JSON_Object (Execute'Access);
   end Execute_Command;

   ---------
   -- Run --
   ---------

   procedure Run
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

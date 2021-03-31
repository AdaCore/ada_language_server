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
with GNAT.OS_Lib;           use GNAT.OS_Lib;

with GNATCOLL.Utils; use GNATCOLL.Utils;
with GNATCOLL.JSON;  use GNATCOLL.JSON;

with Spawn.Processes.Monitor_Loop;

package body Tester.Tests is

   Max_Wait : constant := 4_000;
   --  Max number of milliseconds to wait on a given snippet

   type Command_Kind is (Start, Stop, Send, Comment);

   procedure Do_Start
     (Self    : in out Test'Class);

   procedure Do_Stop
     (Self    : in out Test'Class;
      Command : GNATCOLL.JSON.JSON_Value);

   procedure Do_Send
     (Self    : in out Test'Class;
      Command : GNATCOLL.JSON.JSON_Value);

   function Wait_Factor return Integer;
   --  Return the factor to multiply the delays with - useful for valgrind
   --  runs. This is an integer read from the environment variable
   --  $ALS_WAIT_FACTOR if it is defined.

   function Is_Has_Pattern (List : GNATCOLL.JSON.JSON_Array) return Boolean;
   --  Check if List in form of ["<HAS>", item1, item2, ...]

   Is_Windows : constant Boolean := Directory_Separator = '\';

   --------------------
   -- Is_Has_Pattern --
   --------------------

   function Is_Has_Pattern (List : GNATCOLL.JSON.JSON_Array) return Boolean is
      Len : constant Natural := GNATCOLL.JSON.Length (List);
   begin
      if Len > 1 then
         declare
            First : constant GNATCOLL.JSON.JSON_Value :=
              GNATCOLL.JSON.Get (List, 1);
         begin
            return First.Kind = GNATCOLL.JSON.JSON_String_Type
              and then String'(GNATCOLL.JSON.Get (First)) = "<HAS>";
         end;
      else
         return False;
      end if;
   end Is_Has_Pattern;

   --------------
   -- Do_Abort --
   --------------

   procedure Do_Abort (Self : Test) is
   begin
      OS_Exit (1);
   end Do_Abort;

   -------------
   -- Do_Fail --
   -------------

   procedure Do_Fail
     (Self : in out Test;
      Text : Spawn.String_Vectors.UTF_8_String_Vector)
   is
      pragma Unreferenced (Self);
   begin
      Ada.Text_IO.Put_Line ("Test failed!");

      for Line of Text loop
         Ada.Text_IO.Put_Line (Line);
      end loop;

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      --  Prevent execution any other commands in the script:
      Self.Index := Integer'Last;
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
         if Total_Milliseconds_Waited > Max_Wait * Wait_Factor
            and then not Self.In_Debug
         then
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
               Text.Append ("Full output from server:");
               Text.Append
                 (GNATCOLL.JSON.Write
                    (GNATCOLL.JSON.Create (Self.Full_Server_Output), False));
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
     (Self    : in out Test'Class)
   is
      function Program_Name (Path : String) return String;
      --  Return full path to an exacutable designated by Path

      ------------------
      -- Program_Name --
      ------------------

      function Program_Name (Path : String) return String is
      begin
         if Is_Windows
           and then not Ends_With (Path, ".exe")
         then
            return Ada.Directories.Full_Name (Path & ".exe");
         else
            return Ada.Directories.Full_Name (Path);
         end if;
      end Program_Name;

      Command_Line : constant GNAT.OS_Lib.String_Access := Getenv ("ALS");

      Args : Spawn.String_Vectors.UTF_8_String_Vector;
   begin
      if Command_Line = null or else Command_Line.all = "" then
         raise Program_Error with
           "You must specify the language server command line in $ALS";
      end if;

      declare
         Splits : constant Unbounded_String_Array :=
           Split (Command_Line.all, ' ');
      begin
         Self.Set_Program (Program_Name (To_String (Splits (Splits'First))));

         for J in Splits'First + 1 .. Splits'Last loop
            Args.Append (To_String (Splits (J)));
         end loop;
      end;

      Self.Set_Arguments (Args);
      Self.Start;

      loop
         Spawn.Processes.Monitor_Loop (Timeout => 1);
         exit when Self.Is_Server_Running;
      end loop;

      if Self.In_Debug then
         declare
            Ignore : Integer;
         begin
            Ada.Text_IO.Put_Line
             ("Language server is running. You can attach it with GDB.");
            Ada.Text_IO.Put_Line ("Press ENTER to continue.");

            --  Wait for ENTER:
            Ignore := Ada.Text_IO.Get_Line'Length;
         end;
      end if;
   end Do_Start;

   -------------
   -- Do_Stop --
   -------------

   procedure Do_Stop
     (Self    : in out Test'Class;
      Command : GNATCOLL.JSON.JSON_Value)
   is
      Exit_Code : constant Integer := Command.Get ("exit_code").Get;

      Stop_Client : constant GNATCOLL.JSON.JSON_Value :=
        Command.Get ("close_stdin");
   begin
      if Stop_Client.Kind not in GNATCOLL.JSON.JSON_Boolean_Type
        or else Stop_Client.Get = True
      then
         Self.Stop;
      end if;

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
     (Self    : in out Test;
      Data    : Ada.Strings.Unbounded.Unbounded_String;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);

      procedure Sweep_Waits (JSON : GNATCOLL.JSON.JSON_Value);
      --  Find matching wait if any and delete it from Test.Waits

      function Match (Left, Right : GNATCOLL.JSON.JSON_Value) return Boolean;
      --  For JSON Objects check if Left has all properties from Right.
      --  For JSON Array check length and each item recursively.
      --  For JSON simple type check if Reft and Right are equal.

      procedure Sort_Reply_In_JSON
        (JSON      : GNATCOLL.JSON.JSON_Value;
         Parameter : GNATCOLL.JSON.JSON_Value);

      -----------
      -- Match --
      -----------

      function Match (Left, Right : GNATCOLL.JSON.JSON_Value) return Boolean is

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
            if Success
              and then Value.Kind = GNATCOLL.JSON.JSON_String_Type
              and then String'(GNATCOLL.JSON.Get (Value)) = "<ABSENT>"
            then

               Success := not Left.Has_Field (Name);

            elsif Success and then Left.Has_Field (Name) then

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
                  if Is_Has_Pattern (R) then
                     --  Found: "<HAS>", item1, item2. Check all item1, item2,
                     --  etc... in the Left
                     for R_Index in 2 .. GNATCOLL.JSON.Length (R) loop
                        declare
                           R_Item : constant GNATCOLL.JSON.JSON_Value :=
                             GNATCOLL.JSON.Get (R, R_Index);
                        begin
                           if (for all J in 1 .. Len =>
                                 not Match (GNATCOLL.JSON.Get (L, J), R_Item))
                           then
                              return False;
                           end if;
                        end;
                     end loop;

                     return True;
                  elsif Len /= GNATCOLL.JSON.Length (R) then
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

      ------------------------
      -- Sort_Reply_In_JSON --
      ------------------------

      procedure Sort_Reply_In_JSON
        (JSON      : GNATCOLL.JSON.JSON_Value;
         Parameter : GNATCOLL.JSON.JSON_Value)
      is
         procedure Sort_Reply
           (Name  : String;
            Value : GNATCOLL.JSON.JSON_Value);
         --  Let Name be field name in the reply to be sorted, Value is
         --  JSON string with a key. Then JSON[Name] should be array.
         --  Each element in the array should have key field. Sort this array.

         ----------------
         -- Sort_Reply --
         ----------------

         procedure Sort_Reply
           (Name  : String;
            Value : GNATCOLL.JSON.JSON_Value)
         is
            function Less
              (Left, Right : GNATCOLL.JSON.JSON_Value) return Boolean;

            function Less
              (Left, Right : GNATCOLL.JSON.JSON_Value) return Boolean
            is
               function Key_Count return Natural;
               function Get_Key (Index : Positive) return String;

               ---------------
               -- Key_Count --
               ---------------

               function Key_Count return Natural is
               begin
                  case Value.Kind is
                     when GNATCOLL.JSON.JSON_String_Type =>
                        return 1;
                     when GNATCOLL.JSON.JSON_Array_Type =>
                        return GNATCOLL.JSON.Length (Value.Get);
                     when others =>
                        raise Program_Error;
                  end case;
               end Key_Count;

               -------------
               -- Get_Key --
               -------------

               function Get_Key (Index : Positive) return String is
               begin
                  case Value.Kind is
                     when GNATCOLL.JSON.JSON_String_Type =>
                        return Value.Get;
                     when GNATCOLL.JSON.JSON_Array_Type =>
                        return GNATCOLL.JSON.Get (Value.Get, Index).Get;
                     when others =>
                        raise Program_Error;
                  end case;
               end Get_Key;

            begin
               for J in 1 .. Key_Count loop
                  declare
                     Key         : constant String := Get_Key (J);
                     Left_Value  : constant GNATCOLL.JSON.JSON_Value :=
                       Left.Get (Key);

                     Right_Value : constant GNATCOLL.JSON.JSON_Value :=
                       Right.Get (Key);
                  begin
                     if Left_Value.Kind /= Right_Value.Kind then
                        return Left_Value.Kind < Right_Value.Kind;
                     end if;

                     case Left_Value.Kind is
                        when GNATCOLL.JSON.JSON_String_Type =>
                           declare
                              L : constant String := Left_Value.Get;
                              R : constant String := Right_Value.Get;
                           begin
                              if L /= R then
                                 return L < R;
                              end if;
                           end;

                        when others =>
                           raise Program_Error with "Not implemented";
                     end case;
                  end;
               end loop;

               return False;
            end Less;

            List : GNATCOLL.JSON.JSON_Value;
         begin
            if JSON.Is_Empty then
               return;
            end if;

            List := JSON.Get (Name);
            if Value.Kind = GNATCOLL.JSON.JSON_Object_Type then
               Sort_Reply_In_JSON (List, Value);
            elsif List.Kind = GNATCOLL.JSON.JSON_Array_Type then
               GNATCOLL.JSON.Sort (List, Less'Access);
               JSON.Set_Field (Name, List);
            end if;
         end Sort_Reply;

      begin
         Parameter.Map_JSON_Object (Sort_Reply'Access);
      end Sort_Reply_In_JSON;

      JSON : constant GNATCOLL.JSON.JSON_Value :=
        GNATCOLL.JSON.Read (Data);

   begin
      GNATCOLL.JSON.Append (Self.Full_Server_Output, JSON);

      if not Self.Sort_Reply.Is_Empty then
         Sort_Reply_In_JSON (JSON, Self.Sort_Reply);
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
               Self.Do_Start;
            when Stop =>
               Self.Do_Stop (Value);
            when Send =>
               Self.Do_Send (Value);
            when Comment =>
               null;  --  Do nothing on comments
         end case;
      end Execute;

      task Watch_Dog is
         entry Cancel;
      end Watch_Dog;

      task body Watch_Dog is
      begin
         select
            accept Cancel;
         or
            delay 20.0 * Wait_Factor;

            Ada.Text_IO.Put_Line ("Timeout on command:");
            Ada.Text_IO.Put_Line (Command.Write);
            OS_Exit (1);
         end select;
      end Watch_Dog;

   begin
      if Self.In_Debug then
         Watch_Dog.Cancel;  --  Don't use watchdog under debug
         Command.Map_JSON_Object (Execute'Access);
      else
         Command.Map_JSON_Object (Execute'Access);
         Watch_Dog.Cancel;
      end if;
   end Execute_Command;

   ---------
   -- Run --
   ---------

   procedure Run
     (Self     : in out Test;
      Commands : GNATCOLL.JSON.JSON_Array;
      Debug    : Boolean) is
   begin
      Self.In_Debug := Debug;

      while Self.Index <= GNATCOLL.JSON.Length (Commands) loop
         declare
            Command : constant GNATCOLL.JSON.JSON_Value :=
              GNATCOLL.JSON.Get (Commands, Self.Index);
         begin
            Self.Execute_Command (Command);
         end;
      end loop;
   end Run;

   -----------------
   -- Wait_Factor --
   -----------------

   function Wait_Factor return Integer is
      Factor : constant GNAT.OS_Lib.String_Access
        := Getenv ("ALS_WAIT_FACTOR");
   begin
      if Factor = null or else Factor.all = "" then
         return 1;
      else
         return Integer'Value (Factor.all);
      end if;
   end Wait_Factor;

end Tester.Tests;

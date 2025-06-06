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

with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Streams;
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C_Streams;
with GNAT.OS_Lib;

with GNATCOLL.Utils; use GNATCOLL.Utils;
with GNATCOLL.JSON;  use GNATCOLL.JSON;

with VSS.String_Vectors;
with VSS.Strings.Conversions;

with Spawn.Processes.Monitor_Loop;
with Spawn.Process_Listeners;

with Tester.Macros;

package body Tester.Tests is

   Max_Wait : constant := 16.0;
   --  Max number of seconds to wait on a given snippet

   type Command_Kind is (Start, Stop, Send, Shell, Prepend_To_Env, Comment);

   procedure Do_Prepend_To_Env
     (Self    : in out Test'Class;
      Command : GNATCOLL.JSON.JSON_Value);
   --  Implementation of `prepend_to_env` command

   procedure Do_Start
     (Self    : in out Test'Class;
      Command : GNATCOLL.JSON.JSON_Value);
   --  Implementation of `start` command

   procedure Do_Stop
     (Self    : in out Test'Class;
      Command : GNATCOLL.JSON.JSON_Value);
   --  Implementation of `stop` command

   procedure Do_Send
     (Self    : in out Test'Class;
      Command : GNATCOLL.JSON.JSON_Value);
   --  Implementation of `send` command

   procedure Do_Shell
     (Self    : in out Test'Class;
      Command : GNATCOLL.JSON.JSON_Value);
   --  Implementation of `shell` command

   procedure Do_Test_Hanged (Self : Test'Class);
   --  Launch `On_Hang` script if configured.

   function Wait_Factor
     (Self    : Test'Class;
      Command : GNATCOLL.JSON.JSON_Value) return Integer;
   --  Return the factor to multiply the delays with - useful for valgrind runs
   --  or commands that take longer time.
   --  This is an integer read from either the "waitFactor" field of Command
   --  (if any) or the $ALS_WAIT_FACTOR environment variable (if any).

   function Is_Has_Pattern (List : GNATCOLL.JSON.JSON_Array) return Boolean;
   --  Check if List in form of ["<HAS>", item1, item2, ...]

   function Is_Does_Not_Have_Pattern
     (List : GNATCOLL.JSON.JSON_Array)
      return Boolean;
   --  Check if List in form of ["<DOES_NOT_HAVE>", item1, item2, ...]

   function Generate_Diff
     (Left, Right : GNATCOLL.JSON.JSON_Value;
      Indent      : Natural;
      Minimal     : Boolean)
      return Unbounded_String;
   --  Result is a json like representation of the diff between Left and Right.
   --  Indent represents the current indentation level.
   --  Minimal will only show the different lines.

   function Find_Matching_Request
     (Request      : GNATCOLL.JSON.JSON_Value;
      Search_Array : GNATCOLL.JSON.JSON_Array;
      Request_ID   : out Unbounded_String)
      return GNATCOLL.JSON.JSON_Value;
   --  Search for the matching Request in Search_Array. Return JSON_Null if
   --  Request was not matched.

   procedure Generate_Diff_Internal
     (Left, Right : GNATCOLL.JSON.JSON_Value;
      Indent  : Natural;
      Minimal : Boolean;
      Result  : in out Unbounded_String);
   --  Internal recursive implementation of Generate_Diff. Don't call directly.

   procedure On_Failed
     (Self    : in out Test'Class;
      Request : GNATCOLL.JSON.JSON_Value);
   --  React differently depending on On_Failed_Out

   type Process_Listener is limited new
     Spawn.Process_Listeners.Process_Listener with
   record
      Command : VSS.Strings.Virtual_String;
      Process : Spawn.Processes.Process;
   end record;

   procedure Initialize
     (Self : in out Process_Listener'Class;
      Cmd  : String;
      Args : Spawn.String_Vectors.UTF_8_String_Vector;
      Env  : Spawn.Environments.Process_Environment);

   overriding procedure Standard_Output_Available
    (Self : in out Process_Listener);

   overriding procedure Standard_Error_Available
    (Self : in out Process_Listener);

   overriding procedure Error_Occurred
    (Self          : in out Process_Listener;
     Process_Error : Integer);

   overriding procedure Finished
     (Self        : in out Process_Listener;
      Exit_Status : Spawn.Processes.Process_Exit_Status;
      Exit_Code   : Spawn.Processes.Process_Exit_Code);

   Is_Windows : constant Boolean := GNAT.OS_Lib.Directory_Separator = '\';

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

   ------------------------------
   -- Is_Does_Not_Have_Pattern --
   ------------------------------

   function Is_Does_Not_Have_Pattern
     (List : GNATCOLL.JSON.JSON_Array)
      return Boolean
   is
      Len : constant Natural := GNATCOLL.JSON.Length (List);
   begin
      if Len > 1 then
         declare
            First : constant GNATCOLL.JSON.JSON_Value :=
              GNATCOLL.JSON.Get (List, 1);
         begin
            return First.Kind = GNATCOLL.JSON.JSON_String_Type
              and then String'(GNATCOLL.JSON.Get (First)) = "<DOES_NOT_HAVE>";
         end;
      else
         return False;
      end if;
   end Is_Does_Not_Have_Pattern;

   --------------
   -- Do_Abort --
   --------------

   procedure Do_Abort (Self : Test) is
   begin
      GNAT.OS_Lib.OS_Exit (1);
   end Do_Abort;

   -----------------------
   -- Do_Prepend_To_Env --
   -----------------------

   procedure Do_Prepend_To_Env
     (Self    : in out Test'Class;
      Command : GNATCOLL.JSON.JSON_Value)
   is
      procedure On_Key (Name : String; Value : GNATCOLL.JSON.JSON_Value);

      ------------
      -- On_Key --
      ------------

      procedure On_Key (Name : String; Value : GNATCOLL.JSON.JSON_Value) is
      begin
         if Self.Environment.Contains (Name) then
            Self.Environment.Insert
              (Name,
               Value.Get
               & GNAT.OS_Lib.Path_Separator &
               Self.Environment.Value (Name));
         else
            Self.Environment.Insert (Name, Value.Get);
         end if;
      end On_Key;
   begin
      Command.Map_JSON_Object (On_Key'Access);
   end Do_Prepend_To_Env;

   -------------
   -- Do_Fail --
   -------------

   procedure Do_Fail
     (Self : in out Test;
      Text : Spawn.String_Vectors.UTF_8_String_Vector) is
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
      use type Ada.Calendar.Time;

      procedure Check_Unique_Id (Request_Id : GNATCOLL.JSON.JSON_Value);
      --  Check if Request_Id is unique over all request ids

      ---------------------
      -- Check_Unique_Id --
      ---------------------

      procedure Check_Unique_Id (Request_Id : GNATCOLL.JSON.JSON_Value) is
         Id : VSS.Strings.Virtual_String;
      begin
         case Request_Id.Kind is
            when GNATCOLL.JSON.JSON_String_Type =>
               Id := VSS.Strings.Conversions.To_Virtual_String
                 (String'(Request_Id.Get));
            when GNATCOLL.JSON.JSON_Int_Type =>
               Id := VSS.Strings.Conversions.To_Virtual_String
                 (Integer'Image (Request_Id.Get));
            when others =>
               raise Program_Error with "Unexpected 'id' type!";
         end case;

         if Self.Known_Ids.Contains (Id) then
            declare
               Text : Spawn.String_Vectors.UTF_8_String_Vector;
            begin
               Text.Append ("Duplicated request id:");
               Text.Append (VSS.Strings.Conversions.To_UTF_8_String (Id));
               Self.Do_Fail (Text);
            end;
         else
            Self.Known_Ids.Insert (Id);
         end if;
      end Check_Unique_Id;

      Request : constant GNATCOLL.JSON.JSON_Value := Command.Get ("request");
      Wait    : constant GNATCOLL.JSON.JSON_Array := Get (Command, "wait");
      Sort    : constant GNATCOLL.JSON.JSON_Value := Command.Get ("sortReply");

      Text    : constant Ada.Strings.Unbounded.Unbounded_String :=
        Request.Write;

      Timeout : constant Duration := Max_Wait * Self.Wait_Factor (Command);
   begin
      Clear (Self.Recent_Server_Output);

      if Request.Has_Field ("id") and Request.Has_Field ("method") then
         Check_Unique_Id (Request.Get ("id"));
      end if;

      Self.Started := Ada.Calendar.Clock;
      Self.Waits := Wait;
      Self.Sort_Reply := Sort;
      Self.Send_Message (Text);

      loop
         Spawn.Processes.Monitor_Loop (Timeout => 0.1);
         exit when GNATCOLL.JSON.Length (Self.Waits) = 0;

         if Ada.Calendar.Clock - Self.Started > Timeout
            and then not Self.In_Debug
         then
            Self.Do_Test_Hanged;
            Self.On_Failed (Request);
            exit;
         end if;
      end loop;
   end Do_Send;

   procedure Do_Shell
     (Self    : in out Test'Class;
      Command : GNATCOLL.JSON.JSON_Value)
   is
      use type Spawn.Process_Status;

      List  : constant GNATCOLL.JSON.JSON_Array := Command.Get;
      Cmd   : constant String := GNATCOLL.JSON.Get (List, 1).Get;
      Args  : Spawn.String_Vectors.UTF_8_String_Vector;

      Listener : aliased Process_Listener;
   begin
      for J in 2 .. GNATCOLL.JSON.Length (List) loop
         Args.Append (GNATCOLL.JSON.Get (List, J).Get);
      end loop;

      Listener.Initialize (Cmd, Args, Self.Environment);
      Listener.Process.Start;

      loop
         Spawn.Processes.Monitor_Loop (Timeout => 0.01);
         exit when Listener.Process.Status = Spawn.Not_Running;
      end loop;
   end Do_Shell;

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
         if Is_Windows
           and then not Ends_With (Path, ".exe")
         then
            return Ada.Directories.Full_Name (Path & ".exe");
         else
            return Ada.Directories.Full_Name (Path);
         end if;
      end Program_Name;

      Command_Line : constant JSON_Array := Get (Command.Get ("cmd"));
      ALS_Exe      : constant String := Self.Environment.Value ("ALS");
      Args         : Spawn.String_Vectors.UTF_8_String_Vector;
   begin
      if ALS_Exe = "" then
         raise Program_Error with
           "You must specify the language server command line in $ALS";
      end if;

      --  Set the program using $ALS env variable
      Self.Set_Program (Program_Name (ALS_Exe));

      --  Set the arguments using the 'cmd' field. Skip the first one, since
      --  it's "${ALS}".
      for J in
        GNATCOLL.JSON.Array_First (Command_Line) + 1 ..
        GNATCOLL.JSON.Length (Command_Line)
      loop
         Args.Append (GNATCOLL.JSON.Get (Command_Line, J).Get);
      end loop;

      Self.Set_Arguments (Args);
      Self.Set_Environment (Self.Environment);
      Self.Start;

      loop
         Spawn.Processes.Monitor_Loop (Timeout => 0.001);
         exit when Self.Is_Server_Running;
      end loop;

      if Self.In_Debug then
         declare
            Ignore : Integer;
         begin
            Ada.Text_IO.Put_Line
             ("Language server is running. You can attach it with GDB:");
            Ada.Text_IO.Put_Line ("gdb -p " & Self.Server_PID);
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
      use type Spawn.Processes.Process_Exit_Code;

      Exit_Code   : constant Spawn.Processes.Process_Exit_Code :=
        Spawn.Processes.Process_Exit_Code
          (Integer'(Get (Get (Command, "exit_code"))));
      Stop_Client : constant GNATCOLL.JSON.JSON_Value :=
        Command.Get ("close_stdin");

   begin
      if Stop_Client.Kind not in GNATCOLL.JSON.JSON_Boolean_Type
        or else Stop_Client.Get = True
      then
         Self.Stop;
      end if;

      loop
         Spawn.Processes.Monitor_Loop (Timeout => 0.001);
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

   --------------------
   -- Do_Test_Hanged --
   --------------------

   procedure Do_Test_Hanged (Self : Test'Class) is
      use type Spawn.Process_Status;
      use type VSS.Strings.Virtual_String;

      Listener : aliased Process_Listener;
      Args     : Spawn.String_Vectors.UTF_8_String_Vector;
      List     : constant VSS.String_Vectors.Virtual_String_Vector :=
        Self.On_Hang.Split (' ');

   begin
      if Self.On_Hang.Is_Empty then
         return;
      end if;

      for J in 2 .. List.Length loop
         if List (J) = "<ALS_PID>" then
            Args.Append (Self.Server_PID);
         else
            Args.Append (VSS.Strings.Conversions.To_UTF_8_String (List (J)));
         end if;
      end loop;

      Listener.Initialize
        (VSS.Strings.Conversions.To_UTF_8_String (List (1)),
         Args,
         Self.Environment);

      Listener.Process.Start;

      loop
         Spawn.Processes.Monitor_Loop (Timeout => 0.01);
         exit when Listener.Process.Status = Spawn.Not_Running;
      end loop;
   end Do_Test_Hanged;

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
      --  Sort JSON inplace taking Parameter as sorting key(s) definition.

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

                  elsif Is_Does_Not_Have_Pattern (R) then
                     --  Found: "<DOES_NOT_HAVE>", item1, item2. Check all
                     --  item1, item2, etc... are not in the Left
                     for R_Index in 2 .. GNATCOLL.JSON.Length (R) loop
                        declare
                           R_Item : constant GNATCOLL.JSON.JSON_Value :=
                             GNATCOLL.JSON.Get (R, R_Index);
                        begin
                           if (for some J in 1 .. Len =>
                                 Match (GNATCOLL.JSON.Get (L, J), R_Item))
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
         --  Let Name be field name in the reply (JSON) to be sorted, Value is
         --  the rest of sorting key(s) definition. Then JSON[Name] should be
         --  array (if presents). Sort this array according to Value.

         ----------------
         -- Sort_Reply --
         ----------------

         procedure Sort_Reply
           (Name  : String;
            Value : GNATCOLL.JSON.JSON_Value)
         is
            function Less
              (Left, Right : GNATCOLL.JSON.JSON_Value) return Boolean;
            --  Compare two JSON values using key definition given in Value

            function Less
              (Left, Right : GNATCOLL.JSON.JSON_Value) return Boolean
            is
               function Key_Count
                 (Value  : GNATCOLL.JSON.JSON_Value) return Natural;
               --  Count number of keys in given Value (keys definition)

               function Get_Key_Value
                 (Object : GNATCOLL.JSON.JSON_Value;
                  Value  : GNATCOLL.JSON.JSON_Value;
                  Index  : Positive) return GNATCOLL.JSON.JSON_Value;
               --  Object is a JSON object under inspection.
               --  Value is a key(s) definition.
               --  Index is a key number (for compound keys could be > 1).
               --  Return JSON value that corresponds to key definition.

               ---------------
               -- Key_Count --
               ---------------

               function Key_Count
                  (Value  : GNATCOLL.JSON.JSON_Value) return Natural is
               begin
                  case Value.Kind is
                     when GNATCOLL.JSON.JSON_String_Type =>
                        return 1;
                     when GNATCOLL.JSON.JSON_Array_Type =>
                        return GNATCOLL.JSON.Length (Value.Get);
                     when GNATCOLL.JSON.JSON_Object_Type =>
                        declare
                           procedure On_Field
                             (Ignore : UTF8_String; Value : JSON_Value);
                           --  Fetch Value into Child variable

                           Child     : GNATCOLL.JSON.JSON_Value;

                           procedure On_Field
                             (Ignore : UTF8_String; Value : JSON_Value) is
                           begin
                              Child := Value;
                           end On_Field;
                        begin
                           --  Just one field expected
                           Value.Map_JSON_Object (On_Field'Access);
                           return Key_Count (Child);
                        end;
                     when others =>
                        raise Program_Error;
                  end case;
               end Key_Count;

               -------------------
               -- Get_Key_Value --
               -------------------

               function Get_Key_Value
                 (Object : GNATCOLL.JSON.JSON_Value;
                  Value  : GNATCOLL.JSON.JSON_Value;
                  Index  : Positive) return GNATCOLL.JSON.JSON_Value is
               begin
                  case Value.Kind is
                     when GNATCOLL.JSON.JSON_String_Type =>
                        return Object.Get (Value.Get);
                     when GNATCOLL.JSON.JSON_Array_Type =>
                        return Object.Get
                          (GNATCOLL.JSON.Get (Value.Get, Index).Get);
                     when GNATCOLL.JSON.JSON_Object_Type =>
                        declare
                           procedure On_Field
                             (Name : UTF8_String; Value : JSON_Value);
                           --  Fetch Object[Name] into Child variable and
                           --  Value into Child_Key variable.

                           Child     : GNATCOLL.JSON.JSON_Value;
                           Child_Key : GNATCOLL.JSON.JSON_Value;

                           procedure On_Field
                             (Name : UTF8_String; Value : JSON_Value) is
                           begin
                              Child := Object.Get (Name);
                              Child_Key := Value;
                           end On_Field;
                        begin
                           --  Just one field expected
                           Value.Map_JSON_Object (On_Field'Access);
                           return Get_Key_Value (Child, Child_Key, Index);
                        end;
                     when others =>
                        raise Program_Error;
                  end case;
               end Get_Key_Value;

            begin
               for J in 1 .. Key_Count (Value) loop
                  declare
                     Left_Value  : constant GNATCOLL.JSON.JSON_Value :=
                        Get_Key_Value (Left, Value, J);

                     Right_Value : constant GNATCOLL.JSON.JSON_Value :=
                        Get_Key_Value (Right, Value, J);
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

                        when GNATCOLL.JSON.JSON_Int_Type =>
                           declare
                              L : constant Integer := Left_Value.Get;
                              R : constant Integer := Right_Value.Get;
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

            if List.Kind = GNATCOLL.JSON.JSON_Null_Type then
               return;  --  No such field, do nothing
            elsif List.Kind = GNATCOLL.JSON.JSON_Array_Type then
               GNATCOLL.JSON.Sort (List, Less'Access);
               JSON.Set_Field (Name, List);
            elsif Value.Kind = GNATCOLL.JSON.JSON_Object_Type then
               Sort_Reply_In_JSON (List, Value);
            else
               raise Program_Error;
            end if;
         end Sort_Reply;

      begin
         Parameter.Map_JSON_Object (Sort_Reply'Access);
      end Sort_Reply_In_JSON;

      JSON : constant GNATCOLL.JSON.JSON_Value :=
        GNATCOLL.JSON.Read (Data);

   begin
      if not Self.In_Debug and Self.On_Hang.Is_Empty then
         --  Reset watchdog and timer on each message
         Self.Watch_Dog.Restart;
      end if;

      Self.Started := Ada.Calendar.Clock;

      GNATCOLL.JSON.Append (Self.Full_Server_Output, JSON);

      if not Self.Sort_Reply.Is_Empty then
         Sort_Reply_In_JSON (JSON, Self.Sort_Reply);
      end if;

      GNATCOLL.JSON.Append (Self.Recent_Server_Output, JSON);

      Sweep_Waits (JSON);
   end On_Raw_Message;

   -------------------
   -- Generate_Diff --
   -------------------

   function Generate_Diff
     (Left, Right : GNATCOLL.JSON.JSON_Value;
      Indent      : Natural;
      Minimal     : Boolean)
      return Unbounded_String
   is
      Result : Unbounded_String;
   begin
      if Left.Kind = Right.Kind
        and then Left.Kind = GNATCOLL.JSON.JSON_Array_Type
      then
         --  At this point L which contains one expected object and possibly
         --  multiple answers from the server. Some answer like logMessage or
         --  diagnostics can have leaked in the recent output.

         declare
            L : constant GNATCOLL.JSON.JSON_Array := Left.Get;
            R : constant GNATCOLL.JSON.JSON_Array := Right.Get;
         begin
            for J in 1 .. GNATCOLL.JSON.Length (R) loop
               --  For each request, try to find the request with the same ID
               declare
                  Expected         : constant GNATCOLL.JSON.JSON_Value :=
                    GNATCOLL.JSON.Get (R, J);
                  Expected_ID      : Unbounded_String;
                  Matching_Request : constant GNATCOLL.JSON.JSON_Value :=
                    Find_Matching_Request
                      (Request      => Expected,
                       Search_Array => L,
                       Request_ID   => Expected_ID);
               begin
                  if Matching_Request /= GNATCOLL.JSON.JSON_Null then
                     Generate_Diff_Internal
                       (Left    => Matching_Request,
                        Right   => Expected,
                        Indent  => Indent,
                        Minimal => Minimal,
                        Result  => Result);
                  else
                     if Expected_ID = Null_Unbounded_String then
                        --  Alert the user we failed to find the request
                        Result.Append
                          ("Failed to find result for request:"
                           & Expected_ID);
                        Result.Append (Ada.Characters.Latin_1.LF);
                        Result.Append
                          ("Either the result was never received or the id "
                           & "defined in test.json doesn't match the request");
                        Result.Append (Ada.Characters.Latin_1.LF);
                     end if;
                  end if;
               end;
            end loop;
         end;
      else
         --  This case should happen rarely, most objects are arrays.
         Generate_Diff_Internal
           (Left    => Left,
            Right   => Right,
            Indent  => Indent,
            Minimal => Minimal,
            Result  => Result);
      end if;

      return Result;
   end Generate_Diff;

   ---------------------------
   -- Find_Matching_Request --
   ---------------------------

   function Find_Matching_Request
     (Request      : GNATCOLL.JSON.JSON_Value;
      Search_Array : GNATCOLL.JSON.JSON_Array;
      Request_ID   : out Unbounded_String)
      return GNATCOLL.JSON.JSON_Value
   is
      function Get_ID
        (Value : GNATCOLL.JSON.JSON_Value) return Unbounded_String;

      ------------
      -- Get_ID --
      ------------

      function Get_ID
        (Value : GNATCOLL.JSON.JSON_Value) return Unbounded_String is
      begin
         if Value.Kind = JSON_Object_Type and then Value.Has_Field ("id") then
            declare
               ID : constant GNATCOLL.JSON.JSON_Value := Value.Get ("id");
            begin
               if ID.Kind = JSON_Int_Type then
                  declare
                     ID_Value : constant Integer := Value.Get ("id");
                  begin
                     return To_Unbounded_String (ID_Value'Image);
                  end;
               elsif ID.Kind = JSON_String_Type then
                  declare
                     ID_Value : constant String := Value.Get ("id");
                  begin
                     return To_Unbounded_String (ID_Value);
                  end;
               end if;
            end;
         end if;
         return Null_Unbounded_String;
      end Get_ID;

   begin
      Request_ID := Get_ID (Request);
      if Request_ID = Null_Unbounded_String then
         --  No ID, so left was not a request object
         return GNATCOLL.JSON.JSON_Null;
      end if;

      for J in 1 .. GNATCOLL.JSON.Length (Search_Array) loop
         if Get_ID (GNATCOLL.JSON.Get (Search_Array, J)) = Request_ID then
            return GNATCOLL.JSON.Get (Search_Array, J);
         end if;
      end loop;

      return GNATCOLL.JSON.JSON_Null;
   end Find_Matching_Request;

   ----------------------------
   -- Generate_Diff_Internal --
   ----------------------------

   procedure Generate_Diff_Internal
     (Left, Right : GNATCOLL.JSON.JSON_Value;
      Indent  : Natural;
      Minimal : Boolean;
      Result  : in out Unbounded_String)
   is
      Added_Prefix     : constant String := "+ ";
      Expected_Prefix  : constant String := "- ";
      No_Change_Prefix : constant String := "";

      procedure Add_Indent (Lvl : Natural);
      --  Indent to Lvl

      procedure Add_To_Result
        (Data, Prefix : String;
         New_Line     : Boolean := True);
      --  Directly add Data to result

      procedure Diff (L, R : GNATCOLL.JSON.JSON_Value);
      --  Check if L and R are different and update Result

      procedure Add_Issue
        (Val    : GNATCOLL.JSON.JSON_Value;
         Reason : String);
      --  Log an issue related to Val.
      --  Reason should explain why Val is wrong.

      procedure Add_Unchanged (S : String; New_Line : Boolean := False);
      --  Add unchanged output

      procedure Match_Property
        (Name  : String;
         Value : GNATCOLL.JSON.JSON_Value);
      --  Match one property in JSON object

      ----------------
      -- Add_Indent --
      ----------------

      procedure Add_Indent (Lvl : Natural) is
      begin
         Add_Unchanged ((1 .. 2 * Lvl => ' '));
      end Add_Indent;

      -------------------
      -- Add_To_Result --
      -------------------

      procedure Add_To_Result
        (Data, Prefix : String;
         New_Line     : Boolean := True) is
      begin
         Result.Append (Prefix);
         for C of Data loop
            Result.Append (C);
            if C = Ada.Characters.Latin_1.LF then
               Result.Append (Prefix);
            end if;
         end loop;
         if New_Line then
            Result.Append (Ada.Characters.Latin_1.LF);
         end if;
      end Add_To_Result;

      ----------
      -- Diff --
      ----------

      procedure Diff (L, R : GNATCOLL.JSON.JSON_Value) is
      begin
         if L.Kind /= R.Kind or else L /= R then
            Add_Unchanged ("", New_Line => True);
            Add_To_Result
              (Data   => Write (L, False),
               Prefix => Added_Prefix);
            Add_To_Result
              (Data   => Write (R, False),
               Prefix => Expected_Prefix);
         else
            Add_Unchanged (Write (L, False));
         end if;
      end Diff;

      -----------------
      -- Add_Removed --
      -----------------

      procedure Add_Issue
        (Val    : GNATCOLL.JSON.JSON_Value;
         Reason : String) is
      begin
         Add_To_Result
           (Data   => Reason,
            Prefix => Expected_Prefix);
         Add_To_Result
           (Data   => Write (Val, False),
            Prefix => Added_Prefix);
      end Add_Issue;

      --------------------
      -- Match_Property --
      --------------------

      procedure Match_Property
        (Name  : String;
         Value : GNATCOLL.JSON.JSON_Value) is
      begin
         if Value.Kind = GNATCOLL.JSON.JSON_String_Type
           and then String'(GNATCOLL.JSON.Get (Value)) = "<ABSENT>"
         then
            if Left.Has_Field (Name) then
               Add_Issue
                 (Val    => Left,
                  Reason => "Should not have the field: " & Name);
            end if;

         elsif Left.Has_Field (Name) then

            declare
               Prop : constant GNATCOLL.JSON.JSON_Value := Left.Get (Name);
            begin
               Add_Indent (Indent + 1);
               Add_Unchanged (Name & ": ");
               Generate_Diff_Internal
                 (Prop, Value, Indent + 1, Minimal, Result);
               Add_Unchanged ("", New_Line => True);
            end;
         end if;
      end Match_Property;

      -------------------
      -- Add_Unchanged --
      -------------------

      procedure Add_Unchanged (S : String; New_Line : Boolean := False) is
      begin
         if not Minimal then
            Add_To_Result (S, No_Change_Prefix, New_Line => New_Line);
         end if;
      end Add_Unchanged;

   begin
      if Left.Kind /= Right.Kind then
         Diff (Left, Right);
         return;
      end if;

      case Left.Kind is
         when GNATCOLL.JSON.JSON_Object_Type =>
            Add_Unchanged ("{", New_Line => True);
            Right.Map_JSON_Object (Match_Property'Access);

            Add_Indent (Indent);
            Add_Unchanged ("}");
         when GNATCOLL.JSON.JSON_Array_Type =>
            Add_Unchanged ("[", New_Line => True);
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
                        Found  : Boolean := False;
                     begin
                        for J in 1 .. Len loop
                           declare
                              L_Item : constant GNATCOLL.JSON.JSON_Value :=
                                GNATCOLL.JSON.Get (L, J);
                           begin
                              if L_Item = R_Item then
                                 Found := True;
                                 exit;
                              end if;
                           end;
                        end loop;

                        if not Found then
                           Add_Issue
                             (Val      => Left,
                              Reason   =>
                                "Failed <HAS> condition"
                                & Ada.Characters.Latin_1.LF
                                & GNATCOLL.JSON.Write (R_Item, False));
                        end if;
                     end;
                  end loop;

               elsif Is_Does_Not_Have_Pattern (R) then
                  --  Found: "<DOES_NOT_HAVE>", item1, item2. Check all
                  --  item1, item2, etc... are not in the Left
                  for R_Index in 2 .. GNATCOLL.JSON.Length (R) loop
                     declare
                        R_Item : constant GNATCOLL.JSON.JSON_Value :=
                          GNATCOLL.JSON.Get (R, R_Index);
                        Found  : Boolean := False;
                     begin
                        for J in 1 .. Len loop
                           declare
                              L_Item : constant GNATCOLL.JSON.JSON_Value :=
                                GNATCOLL.JSON.Get (L, J);
                           begin
                              if L_Item = R_Item then
                                 Found := True;
                                 exit;
                              end if;
                           end;
                        end loop;

                        if Found then
                           Add_Issue
                             (Val    => Left,
                              Reason =>
                                "Failed <DOES_NOT_HAVE> condition"
                                & Ada.Characters.Latin_1.LF
                              & GNATCOLL.JSON.Write (R_Item, False));
                        end if;
                     end;
                  end loop;

               elsif Len /= GNATCOLL.JSON.Length (R) then
                  Add_Issue
                    (Val    => Left,
                     Reason =>
                       "Received array doesn't match the size"
                     & Ada.Characters.Latin_1.LF
                     & GNATCOLL.JSON.Write (Right, False));
                  return;
               else
                  for J in 1 .. Len loop
                     Add_Indent (Indent + 1);
                     Generate_Diff_Internal
                       (GNATCOLL.JSON.Get (L, J),
                        GNATCOLL.JSON.Get (R, J),
                        Indent + 1,
                        Minimal,
                        Result);
                     if J /= Len then
                        Add_Unchanged (",");
                     end if;
                  end loop;
               end if;
            end;

            Add_Unchanged ("", New_Line => True);
            Add_Indent (Indent);
            Add_Unchanged ("]");
         when GNATCOLL.JSON.JSON_String_Type =>
            Diff (Left, Right);

         when others =>
            Diff (Left, Right);
      end case;
   end Generate_Diff_Internal;

   ---------------
   -- On_Failed --
   ---------------

   procedure On_Failed
     (Self    : in out Test'Class;
      Request : GNATCOLL.JSON.JSON_Value)
   is
      Text          : Spawn.String_Vectors.UTF_8_String_Vector;
      Waits         : constant GNATCOLL.JSON.JSON_Value :=
        GNATCOLL.JSON.Create (Self.Waits);
      Output        : constant GNATCOLL.JSON.JSON_Value :=
        GNATCOLL.JSON.Create (Self.Recent_Server_Output);
      On_Failed_Val : constant String :=
        VSS.Strings.Conversions.To_UTF_8_String (Self.Output_Format);
      Waits_Output  : constant String :=
        GNATCOLL.JSON.Write (Waits, False);
      Full_Output   : constant String :=
        GNATCOLL.JSON.Write
          (GNATCOLL.JSON.Create (Self.Full_Server_Output), False);

   begin
      Text.Append ("Timed out waiting for the answer to:");
      Text.Append (GNATCOLL.JSON.Write (Request, False));
      Text.Append ("");

      if On_Failed_Val = "verbose" then
         Text.Append ("Remaining waits:");
         Text.Append (Waits_Output);
         Text.Append ("");
         Text.Append ("Full output from server:");
         Text.Append (Full_Output);

      elsif On_Failed_Val = "min_diff" then
         declare
            Diff : Unbounded_String;
         begin
            Diff := Generate_Diff
              (Left    => Output,
               Right   => Waits,
               Indent  => 0,
               Minimal => True);
            Text.Append ("Diff:");
            Text.Append (To_String (Diff));
         end;

      elsif On_Failed_Val = "recent" then
         Text.Append ("Remaining waits:");
         Text.Append (GNATCOLL.JSON.Write (Waits, False));
         Text.Append ("");
         Text.Append ("Recent output from server:");
         Text.Append (GNATCOLL.JSON.Write (Output, False));

      else
         if On_Failed_Val /= "" and then On_Failed_Val /= "diff" then
            Text.Append
              ("Unrecognized value for Format: """
               & On_Failed_Val
               & """ reverting to ""diff""");
         end if;
         declare
            Diff : Unbounded_String;
         begin
            Diff := Generate_Diff
              (Left    => Output,
               Right   => Waits,
               Indent  => 0,
               Minimal => False);
            Text.Append ("Diff:");
            Text.Append (To_String (Diff));
         end;
      end if;

      Text.Append ("");

      --  Always log the full output from the server, the python layer will
      --  hide it.
      Text.Append ("Log:");
      Text.Append ("Remaining waits:");
      Text.Append (Waits_Output);
      Text.Append ("");
      Text.Append ("Full output from server:");
      Text.Append (Full_Output);

      Self.Do_Fail (Text);
   end On_Failed;

   --------------------
   -- Error_Occurred --
   --------------------

   overriding procedure Error_Occurred
    (Self          : in out Process_Listener;
     Process_Error : Integer) is
   begin
      Ada.Text_IO.Put ("Fail to run '");
      Ada.Text_IO.Put (VSS.Strings.Conversions.To_UTF_8_String (Self.Command));
      Ada.Text_IO.Put ("' error ");
      Ada.Text_IO.Put_Line (Process_Error'Image);
   end Error_Occurred;

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
            when Prepend_To_Env =>
               Self.Do_Prepend_To_Env (Value);
            when Shell =>
               Self.Do_Shell (Value);
            when Comment =>
               null;  --  Do nothing on comments
         end case;
      end Execute;

      Copy : GNATCOLL.JSON.JSON_Value := Command;
      --  Make a copy of the command to exand macros in its properties
   begin
      Tester.Macros.Expand (Copy, Self.Environment, Self.File);

      if Self.In_Debug or not Self.On_Hang.Is_Empty then
         Copy.Map_JSON_Object (Execute'Access);
      else
         Self.Watch_Dog.Start
           (Timeout => (Max_Wait + 1.0) * Self.Wait_Factor (Copy),
            Command => Copy.Write);

         Copy.Map_JSON_Object (Execute'Access);

         Self.Watch_Dog.Cancel;
      end if;
   end Execute_Command;

   --------------
   -- Finished --
   --------------

   overriding procedure Finished
     (Self        : in out Process_Listener;
      Exit_Status : Spawn.Processes.Process_Exit_Status;
      Exit_Code   : Spawn.Processes.Process_Exit_Code)
   is
      use type Spawn.Processes.Process_Exit_Code;

   begin
      if Exit_Code /= 0 then
         Ada.Text_IO.Put ("Process '");
         Ada.Text_IO.Put
           (VSS.Strings.Conversions.To_UTF_8_String (Self.Command));
         Ada.Text_IO.Put ("' finished with code ");
         Ada.Text_IO.Put_Line (Exit_Code'Image);
      end if;
   end Finished;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Process_Listener'Class;
      Cmd  : String;
      Args : Spawn.String_Vectors.UTF_8_String_Vector;
      Env  : Spawn.Environments.Process_Environment)
   is
      function To_Program (Name : String) return String is
        (if Ada.Directories.Simple_Name (Name) = Name
         then Env.Search_Path (Name)
         else Ada.Directories.Full_Name (Name));
      --  Return full path of the command with given Name.
      --  If Name is a full path, then return Name as is.
      --  If Name is a relative path, then cast it into a full path.
      --  If Name is a base name, then find it on the PATH.

      Process : Spawn.Processes.Process renames Self.Process;
   begin
      Self.Command := VSS.Strings.Conversions.To_Virtual_String (Cmd);

      for Item of Args loop
         Self.Command.Append (' ');
         Self.Command.Append
           (VSS.Strings.Conversions.To_Virtual_String (Item));
      end loop;

      Process.Set_Arguments (Args);
      Process.Set_Environment (Env);
      Process.Set_Listener (Self'Unchecked_Access);
      Process.Set_Program (To_Program (Cmd));
   end Initialize;

   ---------
   -- Run --
   ---------

   procedure Run
     (Self          : in out Test;
      File          : VSS.Strings.Virtual_String;
      Commands      : GNATCOLL.JSON.JSON_Array;
      On_Hang       : VSS.Strings.Virtual_String;
      Debug         : Boolean;
      Output_Format : VSS.Strings.Virtual_String) is
   begin
      Self.File := File;
      Self.In_Debug := Debug;
      Self.On_Hang := On_Hang;
      Self.Output_Format := Output_Format;

      while Self.Index <= GNATCOLL.JSON.Length (Commands) loop
         declare
            Command : constant GNATCOLL.JSON.JSON_Value :=
              GNATCOLL.JSON.Get (Commands, Self.Index);
         begin
            Self.Execute_Command (Command);
         end;
      end loop;
   end Run;

   -------------------------------
   -- Standard_Error_Available --
   -------------------------------

   overriding procedure Standard_Error_Available
     (Self : in out Process_Listener)
   is
      use type Ada.Streams.Stream_Element_Count;
      Data    : Ada.Streams.Stream_Element_Array (1 .. 128);
      Last    : Ada.Streams.Stream_Element_Count;
      Success : Boolean := True;
      Ignore  : Interfaces.C_Streams.size_t;

   begin
      loop
         Self.Process.Read_Standard_Error (Data, Last, Success);

         exit when Last = 0 or not Success;

         Ignore := Interfaces.C_Streams.fwrite
           (Data'Address,
            Interfaces.C_Streams.size_t (Last),
            1,
            Interfaces.C_Streams.stderr);
      end loop;
   end Standard_Error_Available;

   -------------------------------
   -- Standard_Output_Available --
   -------------------------------

   overriding procedure Standard_Output_Available
     (Self : in out Process_Listener)
   is
      use type Ada.Streams.Stream_Element_Count;
      Data    : Ada.Streams.Stream_Element_Array (1 .. 128);
      Last    : Ada.Streams.Stream_Element_Count;
      Success : Boolean := True;
      Ignore  : Interfaces.C_Streams.size_t;

   begin
      loop
         Self.Process.Read_Standard_Output (Data, Last, Success);

         exit when Last = 0 or not Success;

         Ignore := Interfaces.C_Streams.fwrite
           (Data'Address,
            Interfaces.C_Streams.size_t (Last),
            1,
            Interfaces.C_Streams.stdout);
      end loop;
   end Standard_Output_Available;

   -----------------
   -- Wait_Factor --
   -----------------

   function Wait_Factor
     (Self    : Test'Class;
      Command : GNATCOLL.JSON.JSON_Value) return Integer
   is
      Command_Factor : constant String :=
        (if Command.Has_Field ("waitFactor")
         then Command.Get ("waitFactor")
         else "");

      Env_Factor     : constant String :=
        Self.Environment.Value ("ALS_WAIT_FACTOR", "1");

      Factor         : constant String :=
        (if Command_Factor /= "" then Command_Factor else Env_Factor);

   begin
      return Integer'Value (Factor);
   end Wait_Factor;

   --------------------
   -- Watch_Dog_Task --
   --------------------

   task body Watch_Dog_Task is
      Left : Duration;
      Cmd  : Ada.Strings.Unbounded.Unbounded_String;
   begin
      loop
         select
            accept Start
              (Timeout : Duration;
               Command : Ada.Strings.Unbounded.Unbounded_String)
            do
               Cmd := Command;
               Left := Timeout;
            end Start;
         or
            terminate;
         end select;

         Watch_Command_Execution :
         loop
            select
               accept Cancel;
               exit Watch_Command_Execution;
            or
               accept Restart;
            or
               delay Left;

               Ada.Text_IO.Put_Line ("Timeout on command:");
               Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (Cmd));
               GNAT.OS_Lib.OS_Exit (1);
            end select;
         end loop Watch_Command_Execution;
      end loop;
   end Watch_Dog_Task;

end Tester.Tests;

------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

with Ada.Streams;
with GNAT.OS_Lib;

with VSS.Stream_Element_Vectors;
with VSS.Strings.Conversions;
with VSS.Strings.Converters.Decoders;
with VSS.Characters.Latin;
with VSS.Regular_Expressions;

with Spawn.Processes;
with Spawn.Processes.Monitor_Loop;
with Spawn.Process_Listeners;
with Spawn.String_Vectors;

package body LSP.Ada_Handlers.Alire is

   type Process_Listener is limited
     new Spawn.Process_Listeners.Process_Listener with record
      Process : Spawn.Processes.Process;
      Stdout  : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Stderr  : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Error   : Integer := 0;  --  Error_Occurred argument
      Text    : VSS.Strings.Virtual_String;  --  Stdout as a text
     end record;

   overriding procedure Standard_Output_Available
     (Self : in out Process_Listener);

   overriding procedure Standard_Error_Available
     (Self : in out Process_Listener);

   overriding procedure Error_Occurred
     (Self  : in out Process_Listener;
      Error : Integer);

   procedure Start_Alire
     (Listener : in out Process_Listener'Class;
      ALR      : String;
      Option_1 : String;
      Option_2 : String;
      Root     : String);

   --------------------
   -- Error_Occurred --
   --------------------

   overriding procedure Error_Occurred
     (Self  : in out Process_Listener;
      Error : Integer) is
   begin
      Self.Error := Error;
   end Error_Occurred;

   ---------------
   -- Run_Alire --
   ---------------

   procedure Run_Alire
     (Root        : String;
      Has_Alire   : out Boolean;
      Error       : out VSS.Strings.Virtual_String;
      Project     : out VSS.Strings.Virtual_String;
      Environment : in out GPR2.Environment.Object)
   is
      use type GNAT.OS_Lib.String_Access;
      use type Spawn.Process_Exit_Code;
      use type Spawn.Process_Exit_Status;
      use type Spawn.Process_Status;
      use all type VSS.Regular_Expressions.Match_Option;

      ALR : GNAT.OS_Lib.String_Access :=
        GNAT.OS_Lib.Locate_Exec_On_Path ("alr");

      Crate_Pattern : constant VSS.Regular_Expressions.Regular_Expression :=
        VSS.Regular_Expressions.To_Regular_Expression ("^([^=]+)=");

      Project_Pattern : constant VSS.Regular_Expressions.Regular_Expression :=
        VSS.Regular_Expressions.To_Regular_Expression
          (" +Project_File: ([^\n]+)");

      Export_Pattern : constant VSS.Regular_Expressions.Regular_Expression :=
        VSS.Regular_Expressions.To_Regular_Expression
          ("export ([^=]+)=""([^\n]+)""");

      Anchored : constant VSS.Regular_Expressions.Match_Options :=
        (VSS.Regular_Expressions.Anchored_Match => True);

      List     : array (1 .. 2) of aliased Process_Listener;
      Lines    : VSS.String_Vectors.Virtual_String_Vector;
      Text     : VSS.Strings.Virtual_String;
      Decoder  : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
   begin
      Project.Clear;
      Has_Alire := ALR /= null;

      if ALR = null then
         Error := "No alr in the PATH";
         return;
      end if;

      Start_Alire (List (1), ALR.all, "--non-interactive", "show", Root);
      Start_Alire (List (2), ALR.all, "--non-interactive", "printenv", Root);

      loop
         Spawn.Processes.Monitor_Loop (0.1);

         exit when
           (for all Item of List => Item.Process.Status = Spawn.Not_Running);
      end loop;

      Decoder.Initialize ("utf-8");
      GNAT.OS_Lib.Free (ALR);

      --  Decode output and check errors
      for Item of List loop
         Decoder.Reset_State;
         Item.Text := Decoder.Decode (Item.Stdout);

         if Item.Process.Exit_Status /= Spawn.Normal
           or else Item.Process.Exit_Code /= 0
           or else Decoder.Has_Error
           or else Item.Error /= 0
         then
            Error := "'alr";

            for Arg of Item.Process.Arguments loop
               Error.Append (" ");
               Error.Append (VSS.Strings.Conversions.To_Virtual_String (Arg));
            end loop;

            Error.Append ("' failed:");
            Error.Append (VSS.Characters.Latin.Line_Feed);

            if Decoder.Has_Error then
               Error.Append (Decoder.Error_Message);
            else
               Error.Append (Item.Text);
            end if;

            Error.Append (VSS.Characters.Latin.Line_Feed);
            Decoder.Reset_State;
            Text := Decoder.Decode (Item.Stderr);

            if Decoder.Has_Error then
               Error.Append (Decoder.Error_Message);
            else
               Error.Append (Text);
            end if;

            if Item.Error /= 0 then
               Error.Append
                 (VSS.Strings.Conversions.To_Virtual_String
                   (GNAT.OS_Lib.Errno_Message (Item.Error)));
            end if;

            return;
         end if;
      end loop;

      --  Find project file in `alr show` output
      Lines := List (1).Text.Split_Lines;

      declare
         First : constant VSS.Strings.Virtual_String := Lines (1);
         --  We should keep copy of regexp subject string while we have a match
         Match : constant VSS.Regular_Expressions.Regular_Expression_Match :=
           Crate_Pattern.Match (First);
      begin
         if Match.Has_Match then
            Project := Match.Captured (1);
            Project.Append (".gpr");
         end if;
      end;

      for Line of Lines loop
         declare
            Match : constant VSS.Regular_Expressions.Regular_Expression_Match
              := Project_Pattern.Match (Line, Anchored);
         begin
            if Match.Has_Match then
               Project := Match.Captured (1);
               exit;
            end if;
         end;
      end loop;

      --  Find variables in `alr printenv` output
      Lines := List (2).Text.Split_Lines;

      for Line of Lines loop
         declare
            Match : constant VSS.Regular_Expressions.Regular_Expression_Match
              := Export_Pattern.Match (Line, Anchored);
         begin
            if Match.Has_Match then
               Environment.Insert
                 (Key   => VSS.Strings.Conversions.To_UTF_8_String
                             (Match.Captured (1)),
                  Value => VSS.Strings.Conversions.To_UTF_8_String
                             (Match.Captured (2)));
            end if;
         end;
      end loop;

      if Project.Is_Empty then
         Error.Append ("No project file is found by alire");
      end if;
   end Run_Alire;

   ---------------
   -- Run_Alire --
   ---------------

   procedure Run_Alire
     (Root        : String;
      Has_Alire   : out Boolean;
      Error       : out VSS.Strings.Virtual_String;
      Environment : in out GPR2.Environment.Object)
   is
      Ignore : VSS.Strings.Virtual_String;
   begin
      --  TODO: optimization: don't run second alire process
      Run_Alire (Root, Has_Alire, Error, Ignore, Environment);
   end Run_Alire;

   -------------------
   -- Spawn_Process --
   -------------------

   procedure Start_Alire
     (Listener : in out Process_Listener'Class;
      ALR      : String;
      Option_1 : String;
      Option_2 : String;
      Root     : String)
   is
      Process : Spawn.Processes.Process renames Listener.Process;
      Options : Spawn.String_Vectors.UTF_8_String_Vector;
   begin
      Options.Append (Option_1);
      Options.Append (Option_2);
      Process.Set_Arguments (Options);
      Process.Set_Working_Directory (Root);
      Process.Set_Program (ALR);
      Process.Set_Listener (Listener'Unchecked_Access);
      Process.Start;
   end Start_Alire;

   ------------------------------
   -- Standard_Error_Available --
   ------------------------------

   overriding procedure Standard_Error_Available
     (Self : in out Process_Listener)
   is
      use type Ada.Streams.Stream_Element_Count;

      Data    : Ada.Streams.Stream_Element_Array (1 .. 256);
      Last    : Ada.Streams.Stream_Element_Count := 1;
      Success : Boolean := True;

   begin
      while Last > 0 loop
         Self.Process.Read_Standard_Error (Data, Last, Success);

         for Item of Data (1 .. Last) loop
            Self.Stderr.Append (Item);
         end loop;
      end loop;
   end Standard_Error_Available;

   -------------------------------
   -- Standard_Output_Available --
   -------------------------------

   overriding procedure Standard_Output_Available
     (Self : in out Process_Listener)
   is
      use type Ada.Streams.Stream_Element_Count;

      Data    : Ada.Streams.Stream_Element_Array (1 .. 256);
      Last    : Ada.Streams.Stream_Element_Count := 1;
      Success : Boolean := True;

   begin
      while Last > 0 loop
         Self.Process.Read_Standard_Output (Data, Last, Success);

         for Item of Data (1 .. Last) loop
            Self.Stdout.Append (Item);
         end loop;
      end loop;
   end Standard_Output_Available;

end LSP.Ada_Handlers.Alire;

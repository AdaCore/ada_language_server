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
with GNATCOLL.Traces;
with GNATCOLL.VFS;

with LSP.GNATCOLL_Tracers;
with VSS.Stream_Element_Vectors;
with VSS.Strings.Conversions;
with VSS.Strings.Converters.Decoders;
with VSS.String_Vectors;
with VSS.Characters.Latin;
with VSS.Regular_Expressions;

with Spawn.Environments;
with Spawn.Processes;
with Spawn.Processes.Monitor_Loop;
with Spawn.Process_Listeners;
with Spawn.String_Vectors;
with VSS.Strings.Formatters.Strings;
with VSS.Strings.Templates;

package body LSP.Alire is

   Trace : constant GNATCOLL_Tracers.Tracer :=
     GNATCOLL_Tracers.Create ("ALS.ALIRE", GNATCOLL.Traces.On);

   Alire_Verbose : constant GNATCOLL_Tracers.Tracer :=
     GNATCOLL_Tracers.Create ("ALS.ALIRE.VERBOSE", GNATCOLL.Traces.From_Config);

   type Process_Listener is limited
     new Spawn.Process_Listeners.Process_Listener
   with record
      Process : Spawn.Processes.Process;
      Stdout  : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Stderr  : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Error   : Integer := 0;  --  Error_Occurred argument
      Text    : VSS.Strings.Virtual_String;  --  Stdout as a text
   end record;

   overriding
   procedure Standard_Output_Available (Self : in out Process_Listener);

   overriding
   procedure Standard_Error_Available (Self : in out Process_Listener);

   overriding
   procedure Error_Occurred (Self : in out Process_Listener; Error : Integer);

   procedure Start_Alire
     (Options : VSS.String_Vectors.Virtual_String_Vector;
      Root    : String;
      Error   : out VSS.Strings.Virtual_String;
      Lines   : out VSS.String_Vectors.Virtual_String_Vector);

   Anchored : constant VSS.Regular_Expressions.Match_Options :=
     (VSS.Regular_Expressions.Anchored_Match => True);

   Crate_Pattern : constant VSS.Regular_Expressions.Regular_Expression :=
     VSS.Regular_Expressions.To_Regular_Expression ("^([^= ]+)=");

   Project_Pattern : constant VSS.Regular_Expressions.Regular_Expression :=
     VSS.Regular_Expressions.To_Regular_Expression
       (" +Project_File: ([^\n]+)");

   Export_Pattern : constant VSS.Regular_Expressions.Regular_Expression :=
     VSS.Regular_Expressions.To_Regular_Expression
       ("export ([^=]+)=""([^\n]+)""");

   --------------------
   -- Error_Occurred --
   --------------------

   overriding
   procedure Error_Occurred (Self : in out Process_Listener; Error : Integer)
   is
   begin
      Self.Error := Error;
   end Error_Occurred;

   -----------------------------
   -- Conservative_Alire_Sync --
   -----------------------------

   procedure Conservative_Alire_Sync
     (Root : String; Error : out VSS.Strings.Virtual_String)
   is
      Lines : VSS.String_Vectors.Virtual_String_Vector;
   begin
      Start_Alire
        (Options => ["--non-interactive", "build", "--stop-after=generation"],
         Root    => Root,
         Error   => Error,
         Lines   => Lines);
   end Conservative_Alire_Sync;

   ---------------
   -- Run_Alire --
   ---------------

   procedure Determine_Alire_Project
     (Root    : String;
      Error   : out VSS.Strings.Virtual_String;
      Project : out VSS.Strings.Virtual_String)
   is
      Lines : VSS.String_Vectors.Virtual_String_Vector;
   begin
      Project.Clear;

      Start_Alire
        (Options => ["--non-interactive", "show"],
         Root    => Root,
         Error   => Error,
         Lines   => Lines);

      if not Error.Is_Empty then
         return;
      end if;

      --  Find project file in `alr show` output. There are several cases to cover.
      --
      --  If alire.toml contains a "project-files" entry providing a list of
      --  projects, `alr show` prints one line per project of the pattern:
      --  Project_File: ...
      --
      --  Otherwise `alr show` doesn't print a Project_File line and we can expect
      --  there to be a project named identically to the crate name.
      --
      --  So the strategy below is to first use the crate name as a project
      --  name, and then override it if a Project_File line is found (the first
      --  one is taken).

      --  When `alr show` is called in a directory where /alire/ and /config/
      --  don't exist, the command auto-generates those directories and prints a
      --  few lines of output before the actual crate information. So we can't
      --  assume that the crate name will be on the first line.
      for Line of Lines loop
         declare
            --  We should keep copy of regexp subject string while we have a match
            Match :
              constant VSS.Regular_Expressions.Regular_Expression_Match :=
                Crate_Pattern.Match (Line);
         begin
            if Match.Has_Match then
               Project := Match.Captured (1);
               Project.Append (".gpr");
               exit;
            end if;
         end;
      end loop;

      --  Next check if there is a Project_File line, take the first one.
      for Line of Lines loop
         declare
            Match :
              constant VSS.Regular_Expressions.Regular_Expression_Match :=
                Project_Pattern.Match (Line, Anchored);
         begin
            if Match.Has_Match then
               Project := Match.Captured (1);
               exit;
            end if;
         end;
      end loop;

      if Project.Is_Empty then
         Error.Append
           ("No project file could be determined from the output of `alr show`:");
         Error.Append (Lines.Join_Lines (VSS.Strings.LF));
      end if;

   end Determine_Alire_Project;

   ---------------
   -- Run_Alire --
   ---------------

   procedure Setup_Alire_Env
     (Root        : String;
      Error       : out VSS.Strings.Virtual_String;
      Environment : in out GPR2.Environment.Object)
   is
      use VSS.Strings.Conversions;

      Lines : VSS.String_Vectors.Virtual_String_Vector;
   begin

      Start_Alire (["--non-interactive", "printenv"], Root, Error, Lines);

      if not Error.Is_Empty then
         return;
      end if;

      --  Find variables in `alr printenv` output

      for Line of Lines loop
         declare
            Match :
              constant VSS.Regular_Expressions.Regular_Expression_Match :=
                Export_Pattern.Match (Line, Anchored);
         begin
            if Match.Has_Match then
               Environment.Insert
                 (Key   => To_UTF_8_String (Match.Captured (1)),
                  Value => To_UTF_8_String (Match.Captured (2)));
            end if;
         end;
      end loop;
   end Setup_Alire_Env;

   -----------------
   -- Start_Alire --
   -----------------

   procedure Start_Alire
     (Options : VSS.String_Vectors.Virtual_String_Vector;
      Root    : String;
      Error   : out VSS.Strings.Virtual_String;
      Lines   : out VSS.String_Vectors.Virtual_String_Vector)
   is
      use type Spawn.Process_Exit_Code;
      use type Spawn.Process_Exit_Status;
      use type Spawn.Process_Status;
      use type VSS.Strings.Virtual_String;
      use VSS.Strings.Formatters.Strings;
      use VSS.Strings.Conversions;

      Item       : aliased Process_Listener;
      Process    : Spawn.Processes.Process renames Item.Process;
      Full_Options : VSS.String_Vectors.Virtual_String_Vector := Options;
      Sp_Options : Spawn.String_Vectors.UTF_8_String_Vector;
      Decoder    : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      Text       : VSS.Strings.Virtual_String;
   begin

      declare
         use type GNAT.OS_Lib.String_Access;
         ALR : GNAT.OS_Lib.String_Access :=
           GNAT.OS_Lib.Locate_Exec_On_Path ("alr");
      begin
         if ALR = null then
            Error := "Alire executable ('alr') not found in PATH";
            return;
         end if;

         Process.Set_Program (ALR.all);
         GNAT.OS_Lib.Free (ALR);
      end;

      if Alire_Verbose.Is_Active then
         Full_Options.Prepend ("-v");
      end if;

      for Op of Full_Options loop
         Sp_Options.Append (To_UTF_8_String (Op));
      end loop;

      Process.Set_Arguments (Sp_Options);
      Process.Set_Working_Directory (Root);
      Process.Set_Listener (Item'Unchecked_Access);

      if Trace.Is_Active then
         declare
            Template : VSS.Strings.Templates.Virtual_String_Template :=
              "(in {}) {} {}";
         begin
            Trace.Trace_Text
              (Template.Format
                 (Image (To_Virtual_String (Process.Working_Directory)),
                  Image (To_Virtual_String (Process.Program)),
                  Image (Full_Options.Join (" "))));
         end;
      end if;

      Process.Start;

      loop
         Spawn.Processes.Monitor_Loop (0.1);

         exit when Item.Process.Status = Spawn.Not_Running;
      end loop;

      if Trace.Is_Active then
         Trace.Trace
           ("Alire exit code "
            & Item.Process.Exit_Code'Image
            & " with output:");

         if not Item.Stdout.Is_Empty then
            Trace.Trace (Item.Stdout);
         end if;

         if not Item.Stderr.Is_Empty then
            Trace.Trace (Item.Stderr);
         end if;
      end if;

      Decoder.Initialize ("utf-8");

      --  Decode output and check errors
      Decoder.Reset_State;
      Item.Text := Decoder.Decode (Item.Stdout);

      if Item.Process.Exit_Status = Spawn.Normal
        and then Item.Process.Exit_Code = 0
        and then not Decoder.Has_Error
        and then Item.Error = 0
      then

         Lines := Item.Text.Split_Lines;

      else
         Error := "'alr";

         for Arg of Item.Process.Arguments loop
            Error.Append (" ");
            Error.Append (To_Virtual_String (Arg));
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
              (To_Virtual_String (GNAT.OS_Lib.Errno_Message (Item.Error)));
         end if;
      end if;

   end Start_Alire;

   ------------------------------
   -- Standard_Error_Available --
   ------------------------------

   overriding
   procedure Standard_Error_Available (Self : in out Process_Listener) is
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

   overriding
   procedure Standard_Output_Available (Self : in out Process_Listener) is
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

   --------------------
   -- Is_Alire_Crate --
   --------------------

   function Is_Alire_Crate
     (Client : LSP.Ada_Client_Capabilities.Client_Capability) return Boolean
   is
      Alire_TOML : constant GNATCOLL.VFS.Virtual_File :=
        (if Client.Root.Is_Empty then GNATCOLL.VFS.No_File
         else Client.Root_Directory.Create_From_Dir ("alire.toml"));
   begin
      return Alire_TOML.Is_Regular_File;
   end Is_Alire_Crate;

   ----------------------------
   -- Should_Setup_Alire_Env --
   ----------------------------

   function Should_Setup_Alire_Env
     (Client : LSP.Ada_Client_Capabilities.Client_Capability) return Boolean is
   begin
      return
        Is_Alire_Crate (Client)
        and Spawn.Environments.System_Environment.Value ("ALIRE") /= "True";
   end Should_Setup_Alire_Env;

end LSP.Alire;

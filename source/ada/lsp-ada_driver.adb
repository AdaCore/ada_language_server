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
--
--  This is driver to run LSP server for Ada language.

with Ada.Characters.Latin_1;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;
with GNAT.Strings;

pragma Warnings (Off, "is an internal GNAT unit");
with Gnatformat.Configuration;
with LSP.Ada_Handlers.Open_ALS_Log_File_Commands;
with System.Soft_Links;
with System.Secondary_Stack;

with VSS.Application;
with VSS.Command_Line;
with VSS.Standard_Paths;
with VSS.Strings.Conversions;

with GNATCOLL.JSON;
with GNATCOLL.Memory;         use GNATCOLL.Memory;
with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GNATCOLL.VFS;            use GNATCOLL.VFS;
with GNATCOLL.Utils;

with LSP.Ada_Commands;
with LSP.Ada_Definition;
with LSP.Ada_Declaration;
with LSP.Ada_Document_Symbol;
with LSP.Ada_Did_Change_Configurations;
with LSP.Ada_Did_Change_Document;
with LSP.Ada_Execute_Command;
with LSP.Ada_Folding_Range;
with LSP.Ada_Hover;
with LSP.Ada_Prepare_Type_Hierarchy;
with LSP.Ada_References;
with LSP.Ada_Handlers;
with LSP.Ada_Handlers.Executables_Commands;
with LSP.Ada_Handlers.Mains_Commands;
with LSP.Ada_Handlers.Named_Parameters_Commands;
with LSP.Ada_Handlers.Object_Dir_Commands;
with LSP.Ada_Handlers.Other_File_Commands;
with LSP.Ada_Handlers.Open_Project_File_Commands;
with LSP.Ada_Handlers.Project_File_Commands;
with LSP.Ada_Handlers.Project_Reload_Commands;
with LSP.Ada_Handlers.Refactor.Add_Parameter;
with LSP.Ada_Handlers.Refactor.Change_Parameter_Mode;
with LSP.Ada_Handlers.Refactor.Change_Parameters_Default_Value;
with LSP.Ada_Handlers.Refactor.Change_Parameters_Type;
with LSP.Ada_Handlers.Refactor.Extract_Subprogram;
with LSP.Ada_Handlers.Refactor.Auto_Import;
with LSP.Ada_Handlers.Refactor.Introduce_Parameter;
with LSP.Ada_Handlers.Refactor.Move_Parameter;
with LSP.Ada_Handlers.Refactor.Pull_Up_Declaration;
with LSP.Ada_Handlers.Refactor.Remove_Parameter;
with LSP.Ada_Handlers.Refactor.Replace_Type;
with LSP.Ada_Handlers.Refactor.Sort_Dependencies;
with LSP.Ada_Handlers.Refactor.Suppress_Seperate;
with LSP.Ada_Handlers.Show_Dependencies_Commands;
with LSP.Ada_Handlers.Source_Dirs_Commands;
with LSP.Ada_Handlers.Suspend_Executions;
with LSP.Ada_Tokens_Full;
with LSP.Ada_Tokens_Range;
with LSP.Ada_Type_Hierarchy_Subtypes;
with LSP.Ada_Type_Hierarchy_Supertypes;
with LSP.Default_Message_Handlers;
with LSP.GNATCOLL_Trace_Streams;
with LSP.GNATCOLL_Tracers;
with LSP.GPR_Handlers;
with LSP.GPR_External_Tools;
with LSP.GPR_Did_Change_Document;
with LSP.Memory_Statistics;
with LSP.Predefined_Completion;
with LSP.Secure_Message_Loggers;
with LSP.Server_Jobs;
with LSP.Server_Notifications.DidChange;
with LSP.Server_Notifications.DidChangeConfiguration;
with LSP.Server_Notifications.DidChangeWorkspaceFolders;
with LSP.Server_Notifications.DidClose;
with LSP.Server_Notifications.DidOpen;
with LSP.Server_Notifications.Exits;
with LSP.Server_Requests.Declaration;
with LSP.Server_Requests.Definition;
with LSP.Server_Requests.DocumentSymbol;
with LSP.Server_Requests.ExecuteCommand;
with LSP.Server_Requests.FoldingRange;
with LSP.Server_Requests.Hover;
with LSP.Server_Requests.Initialize;
with LSP.Server_Requests.PrepareTypeHierarchy;
with LSP.Server_Requests.References;
with LSP.Server_Requests.Subtypes;
with LSP.Server_Requests.Supertypes;
with LSP.Server_Requests.Tokens_Full;
with LSP.Server_Requests.Tokens_Range;
with LSP.Servers;
with LSP.Stdio_Streams;

--------------------
-- LSP.Ada_Driver --
--------------------

procedure LSP.Ada_Driver is

   use type VSS.Strings.Virtual_String;

   procedure Register_Commands;
   --  Register all known commands

   procedure Remove_Old_Log_files
     (Dir                 : GNATCOLL.VFS.Virtual_File;
      Prefix              : String;
      Max_Nb_Of_Log_Files : Integer);
   --  Remove old log files with the given prefix in the given log
   --  directory when it exceeds the number specified in Max_Nb_Of_Log_Files.

      --------------------------
      -- Remove_Old_Log_files --
      --------------------------

   procedure Remove_Old_Log_files
     (Dir                 : GNATCOLL.VFS.Virtual_File;
      Prefix              : String;
      Max_Nb_Of_Log_Files : Integer)
   is
      Files   : File_Array_Access := Read_Dir (Dir, Files_Only);
      Success : Boolean;
      Counted : Natural := 0;
      Traces_File_Suffix : constant String := ".cfg";
   begin
      Sort (Files.all);

      --  Browse the log files in reverse timestamp order
      for J in reverse Files'Range loop
         if GNATCOLL.Utils.Starts_With (+Files (J).Base_Name, Prefix)
           and then not GNATCOLL.Utils.Ends_With
             (+Files (J).Base_Name, Traces_File_Suffix)
         then
            Counted := Counted + 1;

            --  When we've counted all the files we wanted to keep, delete
            --  the older ones.
            if Counted > Max_Nb_Of_Log_Files then
               Delete (Files (J), Success);
            end if;
         end if;
      end loop;

      Unchecked_Free (Files);
   end Remove_Old_Log_files;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands is
   begin
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Other_File_Commands.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Suspend_Executions.Suspend_Execution'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Project_Reload_Commands.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Open_Project_File_Commands.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Open_ALS_Log_File_Commands.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Show_Dependencies_Commands.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Source_Dirs_Commands.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Executables_Commands.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Mains_Commands.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Project_File_Commands.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Object_Dir_Commands.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Named_Parameters_Commands.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Refactor.Auto_Import.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Refactor.Suppress_Seperate.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Refactor.Extract_Subprogram.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Refactor.Introduce_Parameter.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Refactor.Pull_Up_Declaration.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Refactor.Replace_Type.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Refactor.Sort_Dependencies.Command'Tag);

      --  Refactoring - Change Subprogram Signature Commands
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Refactor.Add_Parameter.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Refactor.Remove_Parameter.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Refactor.Move_Parameter.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Refactor.Change_Parameter_Mode.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Refactor.Change_Parameters_Type.Command'Tag);
      LSP.Ada_Commands.Register
        (LSP.Ada_Handlers.Refactor.Change_Parameters_Default_Value.
           Command'Tag);
   end Register_Commands;

   Server_Trace : constant LSP.GNATCOLL_Tracers.Tracer :=
     LSP.GNATCOLL_Tracers.Create ("ALS.MAIN", From_Config);
   --  Main trace for the LSP.

   Server_Logger_Trace : constant LSP.GNATCOLL_Tracers.Tracer :=
     LSP.GNATCOLL_Tracers.Create ("ALS.MAIN.LOGGER", Off);
   --  Trace that logs all the requests/responses parameters.

   In_Trace  : constant LSP.GNATCOLL_Tracers.Tracer :=
     LSP.GNATCOLL_Tracers.Create ("ALS.IN", Off);
   Out_Trace : constant LSP.GNATCOLL_Tracers.Tracer :=
     LSP.GNATCOLL_Tracers.Create ("ALS.OUT", Off);
   --  Traces that logs all input & output. For debugging purposes.
   Tracer    : aliased LSP.GNATCOLL_Tracers.Server_Tracer;

   In_Stream  : aliased LSP.GNATCOLL_Trace_Streams.Output_Text_Stream;
   --  Output stream for logging input messages into the trace
   In_Logger  : aliased LSP.Secure_Message_Loggers.Server_Logger
    (In_Stream'Unchecked_Access);
   --  Logger for logging input messages
   Out_Stream : aliased LSP.GNATCOLL_Trace_Streams.Output_Text_Stream;
   --  Output stream for logging output messages into the trace
   Out_Logger : aliased LSP.Secure_Message_Loggers.Client_Logger
    (Out_Stream'Unchecked_Access);
   --  Logger for logging output messages

   Server      : aliased LSP.Servers.Server;
   Stream      : aliased LSP.Stdio_Streams.Stdio_Stream;
   Ada_Handler : aliased LSP.Ada_Handlers.Message_Handler
     (Server'Access, Server'Access, Tracer'Unchecked_Access);
   GPR_Handler : aliased LSP.GPR_Handlers.Message_Handler
     (Server'Access, Tracer'Unchecked_Access);

   --  Job handlers
   Ada_Did_Change_Handler : aliased
     LSP.Ada_Did_Change_Configurations.Ada_Did_Change_Handler
       (Ada_Handler'Unchecked_Access);

   Ada_Did_Change_Doc_Handler : aliased
     LSP.Ada_Did_Change_Document.Ada_Did_Change_Handler
       (Ada_Handler'Unchecked_Access);

   Ada_References_Handler : aliased LSP.Ada_References.Ada_References_Handler
     (Ada_Handler'Unchecked_Access);

   Ada_Hover_Handler      : aliased LSP.Ada_Hover.Ada_Hover_Handler
     (Ada_Handler'Unchecked_Access);

   Ada_Definition_Handler : aliased LSP.Ada_Definition.Ada_Definition_Handler
     (Ada_Handler'Unchecked_Access);

   Ada_Declaration_Handler : aliased
     LSP.Ada_Declaration.Ada_Declaration_Handler
       (Ada_Handler'Unchecked_Access);

   Ada_Document_Symbol_Handler : aliased
     LSP.Ada_Document_Symbol.Ada_Document_Symbol_Handler
       (Ada_Handler'Unchecked_Access);

   Ada_Execute_Command_Handler : aliased
     LSP.Ada_Execute_Command.Execute_Command_Handler
       (Ada_Handler'Unchecked_Access);

   Ada_Folding_Range_Handler : aliased
     LSP.Ada_Folding_Range.Ada_Folding_Range_Handler
       (Ada_Handler'Unchecked_Access);

   Ada_Tokens_Full_Handler : aliased
     LSP.Ada_Tokens_Full.Ada_Tokens_Full_Handler
       (Ada_Handler'Unchecked_Access);

   Ada_Tokens_Range_Handler : aliased
     LSP.Ada_Tokens_Range.Ada_Tokens_Range_Handler
       (Ada_Handler'Unchecked_Access);

   Ada_Prepare_Type_Hierarchy_Handler : aliased
     LSP.Ada_Prepare_Type_Hierarchy.Ada_Prepare_Type_Hierarchy_Handler
       (Ada_Handler'Unchecked_Access);

   Ada_Type_Hierarchy_Subtypes_Handler : aliased
     LSP.Ada_Type_Hierarchy_Subtypes.Ada_Type_Hierarchy_Subtype_Handler
       (Ada_Handler'Unchecked_Access);

   Ada_Type_Hierarchy_Supertypes_Handler : aliased
     LSP.Ada_Type_Hierarchy_Supertypes.Ada_Type_Hierarchy_Supertype_Handler
       (Ada_Handler'Unchecked_Access);

   Ada_Fence_Message_Handler : aliased
     LSP.Default_Message_Handlers.Default_Message_Handler;
   --  A shared handler with Fense priority

   GPR_Did_Change_Doc_Handler : aliased
     LSP.GPR_Did_Change_Document.GPR_Did_Change_Handler
       (GPR_Handler'Unchecked_Access);

   Fuzzing_Activated      : constant Boolean :=
     not VSS.Application.System_Environment.Value ("ALS_FUZZING").Is_Empty;
   pragma Unreferenced (Fuzzing_Activated);

   ALS_Home               : constant VSS.Strings.Virtual_String :=
     VSS.Application.System_Environment.Value ("ALS_HOME");
   GPR_Path               : constant VSS.Strings.Virtual_String :=
     VSS.Application.System_Environment.Value ("GPR_PROJECT_PATH");
   Path                   : constant VSS.Strings.Virtual_String :=
     VSS.Application.System_Environment.Value ("PATH");
   Home_Dir               : constant Virtual_File :=
     Create_From_UTF8
       (VSS.Strings.Conversions.To_UTF_8_String
          ((if ALS_Home.Is_Empty
              then VSS.Standard_Paths.Writable_Location
                     (VSS.Standard_Paths.Home_Location)
              else ALS_Home)));
   ALS_Dir                : Virtual_File := Home_Dir / ".als";
   Clean_ALS_Dir          : Boolean := False;
   GNATdebug              : constant Virtual_File := Create_From_Base
     (".gnatdebug");

   Traces_File : Virtual_File;

   Trace_File_Option      : constant VSS.Command_Line.Value_Option :=
     (Short_Name  => "",
      Long_Name   => "tracefile",
      Description => "Full path to a file containing traces configuration",
      Value_Name  => "ARG");

   Config_Description     : constant VSS.Strings.Virtual_String :=
     "Full path to a JSON file containing initialization "
     & "options for the server (i.e: all the settings that can be specified "
     & "through LSP 'initialize' request's initializattionOptions)";

   Config_File_Option     : constant VSS.Command_Line.Value_Option :=
     (Short_Name  => "",
      Long_Name   => "config",
      Description => Config_Description,
      Value_Name  => "ARG");

   Language_GPR_Option    : constant VSS.Command_Line.Binary_Option :=
     (Short_Name  => "",
      Long_Name   => "language-gpr",
      Description => "Handle GPR language instead of Ada");

   Version_Option         : constant VSS.Command_Line.Binary_Option :=
     (Short_Name  => "",
      Long_Name   => "version",
      Description => "Display the program version");

   Ada_Log_File_Prefix    : constant String := "ada_ls";
   GPR_Log_File_Prefix    : constant String := "gpr_ls";

   Config_File            : Virtual_File;

   Memory_Monitor_Enabled : Boolean;

   Exception_When_Parsing_Traces : Exception_Occurrence_Access;
   --  Used to save any exception that might occur when parsing non-valid
   --  traces configuration files.

   procedure Free is new
     Ada.Unchecked_Deallocation
       (Exception_Occurrence,
        Exception_Occurrence_Access);

begin
   --  Handle the command line

   VSS.Command_Line.Add_Option (Trace_File_Option);
   VSS.Command_Line.Add_Option (Config_File_Option);
   VSS.Command_Line.Add_Option (Language_GPR_Option);
   VSS.Command_Line.Add_Option (Version_Option);
   VSS.Command_Line.Add_Help_Option;

   VSS.Command_Line.Process;  --  Will exit if errors or help requested.

   if VSS.Command_Line.Is_Specified (Version_Option) then
      Ada.Text_IO.Put_Line
         ("ALS version: " & $VERSION & " (" & $BUILD_DATE & ")");
      GNAT.OS_Lib.OS_Exit (0);
   end if;

   declare
      Default_Traces_File_Contents : constant String :=
        ">"
        & (if VSS.Command_Line.Is_Specified (Language_GPR_Option)
           then GPR_Log_File_Prefix
           else Ada_Log_File_Prefix)
        & "_log.$T.log:buffer_size=0:buffer_size=0"
        & Ada.Characters.Latin_1.LF
        & "ALS.MAIN=yes"
        & Ada.Characters.Latin_1.LF
        & "ALS.IN=no"
        & Ada.Characters.Latin_1.LF
        & "ALS.OUT=no"
        & Ada.Characters.Latin_1.LF;
   begin
      --  Look for a traces file, in this order:
      --     - passed on the command line via --tracefile,
      --     - in a .gnatdebug file locally
      --     - in "traces.cfg" in the ALS home directory
      if VSS.Command_Line.Is_Specified (Trace_File_Option) then
         Traces_File :=
           Create_From_UTF8
             (VSS.Strings.Conversions.To_UTF_8_String
                (VSS.Command_Line.Value (Trace_File_Option)));
         if not Traces_File.Is_Regular_File then
            Ada.Text_IO.Put_Line ("Could not find the specified traces file");
            GNAT.OS_Lib.OS_Exit (1);
         end if;

      elsif GNATdebug.Is_Regular_File then
         Traces_File := GNATdebug;
      else
         --  No $HOME/.als directory: create one first
         if not ALS_Dir.Is_Directory then
            begin
               Make_Dir (ALS_Dir);

            exception
               --  We have caught an exception when trying to create the .als
               --  directory: warn the user.
               when GNATCOLL.VFS.VFS_Directory_Error =>
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     "warning: Could not create default ALS log directory at '"
                     & ALS_Dir.Display_Full_Name
                     & "'"
                     & Ada.Characters.Latin_1.LF
                     & "Please make sure the parent directory is writable or "
                     & "specify another parent directory via the ALS_HOME "
                     & "environment variable.");
                  ALS_Dir := GNATCOLL.VFS.No_File;
            end;
         end if;

         --  If the ALS directory is valid, parse any existing trace file or
         --  create a default one if needed.

         if ALS_Dir.Is_Directory then
            Traces_File :=
              Create_From_Dir
                (Dir       => ALS_Dir,
                 Base_Name =>
                   +(if VSS.Command_Line.Is_Specified (Language_GPR_Option)
                     then GPR_Log_File_Prefix
                     else Ada_Log_File_Prefix)
                   & "_traces.cfg");

            --  No default traces file found: create one if we can
            if not Traces_File.Is_Regular_File and then ALS_Dir.Is_Writable
            then
               declare
                  W_Traces_File : Writable_File;
               begin
                  W_Traces_File := Traces_File.Write_File;
                  Write (W_Traces_File, Default_Traces_File_Contents);
                  Close (W_Traces_File);
               end;
            end if;
         end if;

         Clean_ALS_Dir := True;
      end if;

      --  Parse the traces config file. Fallback to the default traces' configuration
      --  if we fail to parse the user's traces config file.
      --  In fallback mode, the logs will be produced in the ALS's current directory:
      --  user can still access them easily via the 'als-open-log-file' command.
      begin
         Parse_Config_File (Traces_File);
      exception
         when E : Constraint_Error =>
            Exception_When_Parsing_Traces :=
              Ada.Exceptions.Save_Occurrence (E);
            Parse_Config (Config => Default_Traces_File_Contents);
      end;
   end;

   --  Look for a config file, that contains the configuration for the server
   --  (i.e: the configuration that can be specified through the 'initialize'
   --  request initializationOptions).

   if VSS.Command_Line.Is_Specified (Config_File_Option) then
      Config_File := Create_From_UTF8
        (VSS.Strings.Conversions.To_UTF_8_String
           (VSS.Command_Line.Value (Config_File_Option)));
      if not Config_File.Is_Regular_File then
         Ada.Text_IO.Put_Line ("Could not find the specified config file");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      declare
         JSON_Contents : GNAT.Strings.String_Access := Config_File.Read_File;
         Parse_Result  : GNATCOLL.JSON.Read_Result;
      begin
         Parse_Result := GNATCOLL.JSON.Read (JSON_Contents.all);
         GNAT.Strings.Free (JSON_Contents);

         if not Parse_Result.Success then
            Ada.Text_IO.Put_Line
              ("Error when parsing config file at "
               & GNATCOLL.Utils.Image (Parse_Result.Error.Line, 1)
               & ":"
               & GNATCOLL.Utils.Image (Parse_Result.Error.Column, 1));
            Ada.Text_IO.Put_Line
              (Ada.Strings.Unbounded.To_String (Parse_Result.Error.Message));
            GNAT.OS_Lib.OS_Exit (1);
         end if;
      end;
   end if;

   In_Stream.Initialize (Trace_Handle (Server_Logger_Trace));
   Out_Stream.Initialize (Trace_Handle (Server_Logger_Trace));

   Tracer.Initialize (Server_Trace, In_Trace, Out_Trace);
   Tracer.Trace ("ALS version: " & $VERSION & " (" & $BUILD_DATE & ")");

   Tracer.Trace ("Initializing server ...");

   Tracer.Trace
     ("GPR PATH: " & VSS.Strings.Conversions.To_UTF_8_String (GPR_Path));
   Tracer.Trace
     ("PATH: " & VSS.Strings.Conversions.To_UTF_8_String (Path));

   --  Log any exception that occured while parsing the user's traces
   --  configuration file.
   if Exception_When_Parsing_Traces /= null then
      Tracer.Trace_Exception
        (Exception_When_Parsing_Traces.all,
         "Exception while parsing traces file:");
      Free (Exception_When_Parsing_Traces);
   end if;

   --  Start monitoring the memory if the memory monitor trace is active

   Memory_Monitor_Enabled := Create ("DEBUG.ADA_MEMORY").Is_Active;

   if Memory_Monitor_Enabled then
      GNATCOLL.Memory.Configure (Activate_Monitor => True);
   end if;

   Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Error);
   --  Protect stdout from pollution by accidental Put_Line calls

   Gnatformat.Configuration.Elaborate_GPR2;

   declare
      Allow_Incremental_Text_Changes : constant GNATCOLL.Traces.Trace_Handle
        := GNATCOLL.Traces.Create ("ALS.ALLOW_INCREMENTAL_TEXT_CHANGES",
                                   GNATCOLL.Traces.On);
      --  Trace to activate the support for incremental text changes.

   begin
      Ada_Handler.Initialize
        (Incremental_Text_Changes => Allow_Incremental_Text_Changes.Is_Active,
         Config_File => VSS.Command_Line.Value (Config_File_Option));
   end;

   Server.Initialize (Stream'Unchecked_Access);

   begin
      if VSS.Command_Line.Is_Specified (Language_GPR_Option) then

         LSP.GPR_External_Tools.Initialize_Extra_Packages_Attributes;

         Server.Register_Handler
           (LSP.Server_Notifications.DidChange.Notification'Tag,
            GPR_Did_Change_Doc_Handler'Unchecked_Access);

         Server.Run
           (GPR_Handler'Unchecked_Access,
            Tracer'Unchecked_Access,
            In_Logger  => (if In_Trace.Is_Active
                           then In_Logger'Unchecked_Access else null),
            Out_Logger => (if Out_Trace.Is_Active
                           then Out_Logger'Unchecked_Access else null));

      else
         Register_Commands;

         --  Load predefined completion items
         LSP.Predefined_Completion.Load_Predefined_Completion_Db
           (Trace_Handle (Server_Trace));

         Ada_Fence_Message_Handler.Initialize
           (Handler  => Ada_Handler'Unchecked_Access,
            Priority => LSP.Server_Jobs.Fence);

         Server.Register_Handler
           (LSP.Server_Requests.Initialize.Request'Tag,
            Ada_Fence_Message_Handler'Unchecked_Access);

         Server.Register_Handler
           (LSP.Server_Notifications.DidOpen.Notification'Tag,
            Ada_Fence_Message_Handler'Unchecked_Access);

         Server.Register_Handler
           (LSP.Server_Notifications.DidClose.Notification'Tag,
            Ada_Fence_Message_Handler'Unchecked_Access);

         Server.Register_Handler
           (LSP.Server_Notifications.DidChangeWorkspaceFolders.Notification'Tag,
            Ada_Fence_Message_Handler'Unchecked_Access);

         Server.Register_Handler
           (LSP.Server_Notifications.Exits.Notification'Tag,
            Ada_Fence_Message_Handler'Unchecked_Access);

         Server.Register_Handler
           (LSP.Server_Notifications.DidChangeConfiguration.Notification'Tag,
            Ada_Did_Change_Handler'Unchecked_Access);

         Server.Register_Handler
           (LSP.Server_Notifications.DidChange.Notification'Tag,
            Ada_Did_Change_Doc_Handler'Unchecked_Access);

         Server.Register_Handler
           (LSP.Server_Requests.Hover.Request'Tag,
            Ada_Hover_Handler'Unchecked_Access);

         Server.Register_Handler
           (LSP.Server_Requests.Definition.Request'Tag,
            Ada_Definition_Handler'Unchecked_Access);

         Server.Register_Handler
           (LSP.Server_Requests.Declaration.Request'Tag,
            Ada_Declaration_Handler'Unchecked_Access);

         Server.Register_Handler
           (LSP.Server_Requests.DocumentSymbol.Request'Tag,
            Ada_Document_Symbol_Handler'Unchecked_Access);

         Server.Register_Handler
           (LSP.Server_Requests.ExecuteCommand.Request'Tag,
            Ada_Execute_Command_Handler'Unchecked_Access);

         Server.Register_Handler
           (LSP.Server_Requests.FoldingRange.Request'Tag,
            Ada_Folding_Range_Handler'Unchecked_Access);

         Server.Register_Handler
           (LSP.Server_Requests.Tokens_Full.Request'Tag,
            Ada_Tokens_Full_Handler'Unchecked_Access);

         Server.Register_Handler
           (LSP.Server_Requests.Tokens_Range.Request'Tag,
            Ada_Tokens_Range_Handler'Unchecked_Access);

         Server.Register_Handler
           (LSP.Server_Requests.PrepareTypeHierarchy.Request'Tag,
            Ada_Prepare_Type_Hierarchy_Handler'Unchecked_Access);

         Server.Register_Handler
           (LSP.Server_Requests.Subtypes.Request'Tag,
            Ada_Type_Hierarchy_Subtypes_Handler'Unchecked_Access);

         Server.Register_Handler
           (LSP.Server_Requests.Supertypes.Request'Tag,
            Ada_Type_Hierarchy_Supertypes_Handler'Unchecked_Access);

         Server.Register_Handler
           (LSP.Server_Requests.References.Request'Tag,
            Ada_References_Handler'Unchecked_Access);

         Server.Run
           (Ada_Handler'Unchecked_Access,
            Tracer'Unchecked_Access,
            In_Logger  => (if In_Trace.Is_Active
                           then In_Logger'Unchecked_Access else null),
            Out_Logger => (if Out_Trace.Is_Active
                           then Out_Logger'Unchecked_Access else null),
            Priority   => LSP.Server_Jobs.Low);
      end if;
   exception
      when E : others =>
         Tracer.Trace_Exception
           (E, "FATAL - Unexpected exception in the main thread:");
   end;

   --  Dump the memory statistics if the memory monitor trace is active
   if Memory_Monitor_Enabled then
      declare
         Memory_Stats : constant String :=
                          LSP.Memory_Statistics.Dump_Memory_Statistics (3);

      begin
         Tracer.Trace (Memory_Stats);
      end;
   end if;

   Server.Finalize;

   if Clean_ALS_Dir then
      --  Remove the logs produced for the GPR language if the '--language-gpr'
      --  option has been specified. Otherwise remove the Ada language logs.
      Remove_Old_Log_files
        (Dir                 => ALS_Dir,
         Prefix              =>
           (if VSS.Command_Line.Is_Specified (Language_GPR_Option) then
            GPR_Log_File_Prefix else Ada_Log_File_Prefix),
         Max_Nb_Of_Log_Files => Ada_Handler.Get_Configuration.Log_Threshold);
   end if;

   --  Clean secondary stack up
   declare
      Stack : System.Secondary_Stack.SS_Stack_Ptr :=
        System.Soft_Links.Get_Sec_Stack.all;
   begin
      System.Secondary_Stack.SS_Free (Stack);
      System.Soft_Links.Set_Sec_Stack (Stack);
   end;
end LSP.Ada_Driver;

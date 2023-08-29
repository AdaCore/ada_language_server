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

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with GNAT.Strings;

pragma Warnings (Off, "is an internal GNAT unit");
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

with LSP.Ada_Handlers;
with LSP.Ada_Handlers.Suspend_Executions;
with LSP.Commands;
with LSP.GNATCOLL_Tracers;
with LSP.Memory_Statistics;
with LSP.Servers;
with LSP.Stdio_Streams;

--------------------
-- LSP.Ada_Driver --
--------------------

procedure LSP.Ada_Driver is

   use type VSS.Strings.Virtual_String;

   Server_Trace : constant Trace_Handle := Create ("ALS.MAIN", From_Config);
   --  Main trace for the LSP.

   In_Trace  : constant Trace_Handle := Create ("ALS.IN", Off);
   Out_Trace : constant Trace_Handle := Create ("ALS.OUT", Off);
   --  Traces that logs all input & output. For debugging purposes.
   Tracer    : aliased LSP.GNATCOLL_Tracers.Tracer;

   Server      : aliased LSP.Servers.Server;
   Stream      : aliased LSP.Stdio_Streams.Stdio_Stream;
   Ada_Handler : aliased LSP.Ada_Handlers.Message_Handler
     (Server'Access, Server'Access, Tracer'Unchecked_Access);
   GPR_Handler : aliased LSP.Ada_Handlers.Message_Handler
     (Server'Access, Server'Access, Tracer'Unchecked_Access);

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
   ALS_Dir                : constant Virtual_File := Home_Dir / ".als";
   Clean_ALS_Dir          : Boolean := False;
   GNATdebug              : constant Virtual_File := Create_From_Base
     (".gnatdebug");

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

   Config_File            : Virtual_File;

   Memory_Monitor_Enabled : Boolean;
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

   --  Look for a traces file, in this order:
   --     - passed on the command line via --tracefile,
   --     - in a .gnatdebug file locally
   --     - in "traces.cfg" in the ALS home directory
   if VSS.Command_Line.Is_Specified (Trace_File_Option) then
      declare
         Traces_File : constant Virtual_File := Create_From_UTF8
           (VSS.Strings.Conversions.To_UTF_8_String
              (VSS.Command_Line.Value (Trace_File_Option)));
      begin
         if not Traces_File.Is_Regular_File then
            Ada.Text_IO.Put_Line ("Could not find the specified traces file");
            GNAT.OS_Lib.OS_Exit (1);
         end if;

         Parse_Config_File (Traces_File);
      end;
   elsif GNATdebug.Is_Regular_File then
      Parse_Config_File (GNATdebug);

   elsif ALS_Dir.Is_Directory then
      Clean_ALS_Dir := True;

      --  Search for custom traces config in traces.cfg
      Parse_Config_File (+Virtual_File'(ALS_Dir / "traces.cfg").Full_Name);

      --  Set log file
      Set_Default_Stream
        (">" & (+Virtual_File'(ALS_Dir / "als").Full_Name) &
           ".$T.$$.log:buffer_size=0");
   end if;

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

   Tracer.Initialize (Server_Trace, In_Trace, Out_Trace);
   Tracer.Trace ("ALS version: " & $VERSION & " (" & $BUILD_DATE & ")");

   Tracer.Trace ("Initializing server ...");

   Tracer.Trace
     ("GPR PATH: " & VSS.Strings.Conversions.To_UTF_8_String (GPR_Path));
   Tracer.Trace
     ("PATH: " & VSS.Strings.Conversions.To_UTF_8_String (Path));

   --  Start monitoring the memory if the memory monitor trace is active

   Memory_Monitor_Enabled := Create ("DEBUG.ADA_MEMORY").Is_Active;

   if Memory_Monitor_Enabled then
      GNATCOLL.Memory.Configure (Activate_Monitor => True);
   end if;

   if not VSS.Command_Line.Is_Specified (Language_GPR_Option) then
      --  Load predefined completion items
      pragma Assert (not VSS.Command_Line.Is_Specified (Language_GPR_Option));
   end if;

   Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Error);
   --  Protect stdout from pollution by accidental Put_Line calls

   declare
      Allow_Incremental_Text_Changes : constant GNATCOLL.Traces.Trace_Handle
        := GNATCOLL.Traces.Create ("ALS.ALLOW_INCREMENTAL_TEXT_CHANGES",
                                   GNATCOLL.Traces.On);
      --  Trace to activate the support for incremental text changes.
   begin
      Ada_Handler.Initialize
        (Incremental_Text_Changes => Allow_Incremental_Text_Changes.Is_Active);
   end;

   Server.Initialize (Stream'Unchecked_Access);

   begin
      if VSS.Command_Line.Is_Specified (Language_GPR_Option) then
         Server.Run (GPR_Handler'Unchecked_Access, Tracer'Unchecked_Access);
      else
         LSP.Commands.Register
           (LSP.Ada_Handlers.Suspend_Executions.Suspend_Execution'Tag);

         Server.Run (Ada_Handler'Unchecked_Access, Tracer'Unchecked_Access);
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
      pragma Assert (Clean_ALS_Dir);
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

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
--
--  This is driver to run LSP server for Ada language.

with Ada.Text_IO;
with Ada.Exceptions;          use Ada.Exceptions;
with GNAT.Command_Line;       use GNAT.Command_Line;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with GNAT.OS_Lib;
with GNAT.Strings;

pragma Warnings (Off, "is an internal GNAT unit");
with System.Soft_Links;
with System.Secondary_Stack;

with GNATCOLL.Memory;         use GNATCOLL.Memory;
with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GNATCOLL.VFS;            use GNATCOLL.VFS;

with LSP.Ada_Handlers;
with LSP.Ada_Handlers.Named_Parameters_Commands;
with LSP.Ada_Handlers.Refactor_Imports_Commands;
with LSP.Commands;
with LSP.Error_Decorators;
with LSP.Fuzz_Decorators;
with LSP.Memory_Statistics;
with LSP.Predefined_Completion;
with LSP.Servers;
with LSP.Stdio_Streams;

--------------------
-- LSP.Ada_Driver --
--------------------

procedure LSP.Ada_Driver is

   function Getenv (Var : String) return String;
   --  Return the value set for the given environment variable

   procedure On_Uncaught_Exception (E : Exception_Occurrence);
   --  Reset LAL contexts in Message_Handler after catching some exception.

   procedure Register_Commands;
   --  Register all known commands

   procedure Die_On_Uncaught (E : Exception_Occurrence);
   --  Quit the process when an uncaught exception reaches this. Used for
   --  fuzzing.

   Server_Trace : constant Trace_Handle := Create ("ALS.MAIN", From_Config);
   --  Main trace for the LSP.

   In_Trace  : constant Trace_Handle := Create ("ALS.IN", Off);
   Out_Trace : constant Trace_Handle := Create ("ALS.OUT", Off);
   --  Traces that logs all input & output. For debugging purposes.

   Server  : aliased LSP.Servers.Server;
   Stream  : aliased LSP.Stdio_Streams.Stdio_Stream;
   Handler : aliased LSP.Ada_Handlers.Message_Handler
     (Server'Access, Server_Trace);

   Error_Decorator : aliased LSP.Error_Decorators.Error_Decorator
     (Server_Trace,
      Handler'Unchecked_Access,
      On_Uncaught_Exception'Unrestricted_Access);
   --  This decorator catches all Property_Error exceptions and provides
   --  default responses for each request. It also reset Libadalang Context
   --  on any other exception.

   ------------
   -- Getenv --
   ------------

   function Getenv (Var : String) return String is
      Str : GNAT.Strings.String_Access := GNAT.OS_Lib.Getenv (Var);
   begin
      return S : constant String := Str.all do
         GNAT.Strings.Free (Str);
      end return;
   end Getenv;

   ---------------------------
   -- On_Uncaught_Exception --
   ---------------------------

   procedure On_Uncaught_Exception (E : Exception_Occurrence) is
   begin
      Trace (Server_Trace,
             "EXCEPTION: " & Exception_Name (E) & ASCII.LF &
               Symbolic_Traceback (E));
      Handler.Handle_Error;
   end On_Uncaught_Exception;

   ---------------------
   -- Die_On_Uncaught --
   ---------------------

   procedure Die_On_Uncaught (E : Exception_Occurrence) is
   begin
      Trace (Server_Trace,
             "EXCEPTION: " & Exception_Name (E) & ASCII.LF &
               Symbolic_Traceback (E));
      --  An exception occurred while fuzzing: make it fatal.
      GNAT.OS_Lib.OS_Exit (42);
   end Die_On_Uncaught;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands is
   begin
      LSP.Commands.Register
        (LSP.Ada_Handlers.Named_Parameters_Commands.Command'Tag);
      LSP.Commands.Register
        (LSP.Ada_Handlers.Refactor_Imports_Commands.Command'Tag);
   end Register_Commands;

   use GNAT.Strings;

   Cmdline                : Command_Line_Configuration;

   Fuzzing_Activated      : constant Boolean := Getenv ("ALS_FUZZING") /= "";

   ALS_Home               : constant String := Getenv ("ALS_HOME");
   Home_Dir               : constant Virtual_File :=
                            (if ALS_Home /= "" then Create (+ALS_Home)
                             else Get_Home_Directory);
   ALS_Dir                : constant Virtual_File := Home_Dir / ".als";
   GNATdebug              : constant Virtual_File := Create_From_Base
     (".gnatdebug");

   Tracefile_Name         : aliased String_Access;
   Config_File            : Virtual_File;
   Help_Arg               : aliased Boolean := False;
   Version_Arg            : aliased Boolean := False;

   Memory_Monitor_Enabled : Boolean;
begin
   --  Handle the command line
   Set_Usage
     (Cmdline,
      Help => "Command line interface for the Ada Language Server");

   Define_Switch
     (Cmdline,
      Output      => Tracefile_Name'Access,
      Long_Switch => "--tracefile=",
      Help        => "Full path to a file containing traces configuration");

   Define_Switch
     (Cmdline,
      Output      => Version_Arg'Access,
      Long_Switch => "--version",
      Help        => "Display the program version");

   Define_Switch
     (Cmdline,
      Output      => Help_Arg'Access,
      Long_Switch => "--help",
      Help        => "Display this help");

   begin
      Getopt (Cmdline);
   exception
      when GNAT.Command_Line.Exit_From_Command_Line =>
         Free (Cmdline);
         GNAT.OS_Lib.OS_Exit (0);
   end;

   Free (Cmdline);

   if Version_Arg then
      Ada.Text_IO.Put_Line ("ALS version: " & $VERSION);
      GNAT.OS_Lib.OS_Exit (0);
   end if;

   --  Look for a traces file, in this order:
   --     - passed on the command line via --tracefile,
   --     - in a .gnatdebug file locally
   --     - in "traces.cfg" in the ALS home directory
   if Tracefile_Name /= null
     and then Tracefile_Name.all /= ""
   then
      Config_File := Create (+Tracefile_Name.all);
      if not Config_File.Is_Regular_File then
         Ada.Text_IO.Put_Line ("Could not find the specified traces file");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
      Parse_Config_File (Config_File);

   elsif GNATdebug.Is_Regular_File then
      Parse_Config_File (GNATdebug);

   elsif ALS_Dir.Is_Directory then
      --  Search for custom traces config in traces.cfg
      Parse_Config_File (+Virtual_File'(ALS_Dir / "traces.cfg").Full_Name);

      --  Set log file
      Set_Default_Stream
        (">" & (+Virtual_File'(ALS_Dir / "als").Full_Name) &
           ".$T.$$.log:buffer_size=0");
   end if;

   if Tracefile_Name /= null then
      Free (Tracefile_Name);
   end if;

   Server_Trace.Trace ("ALS version: " & $VERSION);

   Server_Trace.Trace ("Initializing server ...");

   --  Start monitoring the memory if the memory monitor trace is active

   Memory_Monitor_Enabled := Create ("DEBUG.ADA_MEMORY").Is_Active;

   if Memory_Monitor_Enabled then
      GNATCOLL.Memory.Configure
        (Activate_Monitor => True);
   end if;

   --  Load predefined completion items
   LSP.Predefined_Completion.Load_Predefined_Completion_Db (Server_Trace);

   Server.Initialize (Stream'Unchecked_Access);
   begin
      Register_Commands;
      if Fuzzing_Activated then
         --  Fuzzing mode means registering the fuzzing decorators and
         --  registering Die_On_Uncaught as error handler.
         declare
            Fuzz_Requests : aliased LSP.Fuzz_Decorators.Fuzz_Request_Decorator
              (Server_Trace,
               Error_Decorator'Unchecked_Access,
               Die_On_Uncaught'Unrestricted_Access);
            Fuzz_Notifications : aliased
              LSP.Fuzz_Decorators.Fuzz_Notification_Decorator
                (Server_Trace,
                 Handler'Unchecked_Access,
                 Handler'Unchecked_Access);
         begin
            Server.Run
              (Fuzz_Requests'Unchecked_Access,
               Fuzz_Notifications'Unchecked_Access,
               Server       => Handler'Unchecked_Access,
               On_Error     => Die_On_Uncaught'Unrestricted_Access,
               Server_Trace => Server_Trace,
               In_Trace     => In_Trace,
               Out_Trace    => Out_Trace);
         end;
      else
         Server.Run
           (Error_Decorator'Unchecked_Access,
            Handler'Unchecked_Access,
            Server       => Handler'Unchecked_Access,
            On_Error     => On_Uncaught_Exception'Unrestricted_Access,
            Server_Trace => Server_Trace,
            In_Trace     => In_Trace,
            Out_Trace    => Out_Trace);
      end if;
   exception
      when E : others =>
         Server_Trace.Trace
           ("FATAL - Unexpected exception in the main thread: "
            & Exception_Name (E) & " - " &  Exception_Message (E));
         Server_Trace.Trace (Symbolic_Traceback (E));
   end;
   Server_Trace.Trace ("Shutting server down ...");

   --  Dump the memory statistics if the memory monitor trace is active
   if Memory_Monitor_Enabled then
      declare
         Memory_Stats : constant String :=
                          LSP.Memory_Statistics.Dump_Memory_Statistics (3);

      begin
         Server_Trace.Trace (Memory_Stats);
      end;
   end if;

   Server.Finalize;
   Handler.Cleanup;

   --  Clean secondary stack up
   declare
      Stack : System.Secondary_Stack.SS_Stack_Ptr :=
        System.Soft_Links.Get_Sec_Stack.all;
   begin
      System.Secondary_Stack.SS_Free (Stack);
      System.Soft_Links.Set_Sec_Stack (Stack);
   end;
end LSP.Ada_Driver;

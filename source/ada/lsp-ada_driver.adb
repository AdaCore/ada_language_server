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

with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GNATCOLL.VFS;            use GNATCOLL.VFS;

with LSP.Ada_Handlers;
with LSP.Ada_Handlers.Named_Parameters_Commands;
with LSP.Commands;
with LSP.Error_Decorators;
with LSP.Servers;
with LSP.Stdio_Streams;

--------------------
-- LSP.Ada_Driver --
--------------------

procedure LSP.Ada_Driver is

   function Getenv (Var : String) return String;
   --  Return the value set for the given environment variable

   procedure On_Uncaught_Exception;
   --  Reset LAL contexts in Message_Handler after catching some exception.

   procedure Register_Commands;
   --  Register all known commands

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
       (Server_Trace, Handler'Unchecked_Access, On_Uncaught_Exception'Access);
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

   procedure On_Uncaught_Exception is
   begin
      Handler.Handle_Error;
   end On_Uncaught_Exception;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands is
   begin
      LSP.Commands.Register
        (LSP.Ada_Handlers.Named_Parameters_Commands.Command'Tag);
   end Register_Commands;

   Cmdline   : Command_Line_Configuration;
   ALS_Home  : constant String := Getenv ("ALS_HOME");
   Home_Dir  : constant Virtual_File :=
                 (if ALS_Home /= "" then Create (+ALS_Home)
                  else Get_Home_Directory);
   ALS_Dir   : constant Virtual_File := Home_Dir / ".als";
   GNATdebug : constant Virtual_File := Create_From_Base (".gnatdebug");

   use GNAT.Strings;
   Tracefile_Name : aliased String_Access;
   Config_File    : Virtual_File;
   Help_Arg       : aliased Boolean := False;
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

   Server.Initialize (Stream'Unchecked_Access);
   begin
      Register_Commands;
      Server.Run
        (Error_Decorator'Unchecked_Access,
         Handler'Unchecked_Access,
         Server       => Handler'Unchecked_Access,
         On_Error     => On_Uncaught_Exception'Unrestricted_Access,
         Server_Trace => Server_Trace,
         In_Trace     => In_Trace,
         Out_Trace    => Out_Trace);
   exception
      when E : others =>
         Server_Trace.Trace
           ("FATAL - Unexpected exception in the main thread: "
            & Exception_Name (E) & " - " &  Exception_Message (E));
         Server_Trace.Trace (Symbolic_Traceback (E));
   end;
   Server_Trace.Trace ("Shutting server down ...");
   Server.Finalize;
   Handler.Cleanup;
end LSP.Ada_Driver;

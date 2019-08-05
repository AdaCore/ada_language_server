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

with Ada.Exceptions;          use Ada.Exceptions;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with GNAT.OS_Lib;
with GNAT.Strings;

with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GNATCOLL.VFS;            use GNATCOLL.VFS;

with LSP.Servers;
with LSP.Stdio_Streams;

with LSP.Ada_Contexts;
with LSP.Ada_Handlers;

--------------------
-- LSP.Ada_Driver --
--------------------

procedure LSP.Ada_Driver is

   Server_Trace : constant Trace_Handle := Create ("ALS.MAIN", From_Config);
   --  Main trace for the LSP.

   In_Trace  : constant Trace_Handle := Create ("ALS.IN", Off);
   Out_Trace : constant Trace_Handle := Create ("ALS.OUT", Off);
   --  Traces that logs all input & output. For debugging purposes.

   Server  : aliased LSP.Servers.Server;
   Stream  : aliased LSP.Stdio_Streams.Stdio_Stream;
   Context : aliased LSP.Ada_Contexts.Context (Server_Trace);
   Handler : aliased LSP.Ada_Handlers.Message_Handler
     (Server'Access, Context'Access, Server_Trace);

   function Getenv (Var : String) return String;
   --  Return the value set for the given environment variable

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

   ALS_Home  : constant String := Getenv ("ALS_HOME");
   Home_Dir  : constant Virtual_File :=
                 (if ALS_Home /= "" then Create (+ALS_Home)
                  else Get_Home_Directory);
   ALS_Dir   : constant Virtual_File := Home_Dir / ".als";
   GNATdebug : constant Virtual_File := Create_From_Base (".gnatdebug");

begin

   --  Look for a .gnatdebug file locally; if it exists, use its contents as
   --  traces config file. If not, if the ".als" directory exists in the home
   --  directory, initialize traces there.
   if GNATdebug.Is_Regular_File then
      Parse_Config_File (GNATdebug);

   elsif ALS_Dir.Is_Directory then
      --  Search for custom traces config in traces.cfg
      Parse_Config_File
        (+Virtual_File'(ALS_Dir / "traces.cfg").Full_Name);

      --  Set log file
      Set_Default_Stream
        (">" & (+Virtual_File'(ALS_Dir / "als").Full_Name) &
           ".$T.$$.log:buffer_size=0");
   end if;

   Server_Trace.Trace ("Initializing server ...");

   Server.Initialize (Stream'Unchecked_Access);
   begin
      Server.Run
        (Handler'Unchecked_Access,
         Handler'Unchecked_Access,
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
end LSP.Ada_Driver;

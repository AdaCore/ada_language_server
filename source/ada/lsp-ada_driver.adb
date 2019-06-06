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

with GNATCOLL.Traces;
with GNATCOLL.VFS;

with LSP.Servers;
with LSP.Stdio_Streams;

with LSP.Ada_Contexts;
with LSP.Ada_Handlers;

--------------------
-- LSP.Ada_Driver --
--------------------

procedure LSP.Ada_Driver is

   Server  : aliased LSP.Servers.Server;
   Stream  : aliased LSP.Stdio_Streams.Stdio_Stream;
   Context : aliased LSP.Ada_Contexts.Context;
   Handler : aliased LSP.Ada_Handlers.Message_Handler
     (Server'Access, Context'Access);

   use GNATCOLL.VFS, GNATCOLL.Traces;

   ALS_Dir   : constant Virtual_File := Get_Home_Directory / ".als";
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

   Server.Run (Handler'Unchecked_Access, Handler'Unchecked_Access);
   Server_Trace.Trace ("Shutting server down ...");

   Server.Finalize;
end LSP.Ada_Driver;

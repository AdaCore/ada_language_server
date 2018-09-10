------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
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

with LSP.Servers;
with LSP.Stdio_Streams;

with LSP.Ada_Contexts;
with LSP.Ada_Handlers;

procedure LSP.Ada_Driver is

   Server  : aliased LSP.Servers.Server;
   Stream  : aliased LSP.Stdio_Streams.Stdio_Stream;
   Context : aliased LSP.Ada_Contexts.Context;
   Handler : aliased LSP.Ada_Handlers.Message_Handler
     (Server'Access, Context'Access);
begin
   Server.Initialize
     (Stream'Unchecked_Access,
      Handler'Unchecked_Access,
      Handler'Unchecked_Access);

   Server.Run;
end LSP.Ada_Driver;

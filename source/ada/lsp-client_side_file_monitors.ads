------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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

with GNATCOLL.VFS;

with VSS.Strings;

with LSP.Client_Request_Receivers; use LSP.Client_Request_Receivers;
with LSP.File_Monitors;

package LSP.Client_Side_File_Monitors is

   type File_Monitor (Client : not null access Client_Request_Receiver'Class)
     is limited new LSP.File_Monitors.File_Monitor with private;

private

   type File_Monitor (Client : not null access Client_Request_Receiver'Class)
     is limited new LSP.File_Monitors.File_Monitor with
   record
      Last_Id         : Positive := 1;
      Registration_Id : VSS.Strings.Virtual_String;
   end record;

   overriding procedure Monitor_Directories
     (Self        : access File_Monitor;
      Directories : GNATCOLL.VFS.File_Array);

   overriding procedure Stop_Monitoring_Directories
     (Self : access File_Monitor);

end LSP.Client_Side_File_Monitors;

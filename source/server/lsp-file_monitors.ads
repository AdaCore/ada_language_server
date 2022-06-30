------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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
--  This package provides a file monitoring service interface.

with GNATCOLL.VFS;
with Ada.Unchecked_Deallocation;

package LSP.File_Monitors is

   type File_Monitor is limited interface;

   type File_Monitor_Access is access all File_Monitor'Class;

   function Assigned (Self : access File_Monitor'Class) return Boolean is
     (Self /= null);
   --  Check is the reference has not nul value

   procedure Monitor_Directories
     (Self        : access File_Monitor;
      Directories : GNATCOLL.VFS.File_Array) is abstract;
   --  Set up filesystem monitoring for Directories, and emit
   --  "workspace/didChangeWatchedFiles" notifications
   --  for any modification of any files in these directories.
   --
   --  This cancels any monitoring that was previously set up: only
   --  one set of directories is monitored at a given time.

   procedure Stop_Monitoring_Directories (Self : access File_Monitor) is null;
   --  Stop filesystem monitoring. This is a no-op if no monitoring is
   --  ongoing.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (File_Monitor'Class, File_Monitor_Access);

end LSP.File_Monitors;

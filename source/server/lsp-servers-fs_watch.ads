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
--  This package implements file monitor service over Libfswatch.

with LSP.File_Monitors;
with GNATCOLL.VFS;

private with Libfswatch;

package LSP.Servers.FS_Watch is

   type FS_Watch_Monitor (Server : access LSP.Servers.Server) is
     limited new LSP.File_Monitors.File_Monitor with private;

private

   type LSP_Monitor is new Libfswatch.Root_Event_Monitor with record
      The_Server : access Server;
   end record;
   overriding procedure Callback
     (Self   : in out LSP_Monitor;
      Events : Libfswatch.Event_Vectors.Vector);
   --  The function that is called when files change on disk

   type LSP_Monitor_Access is access LSP_Monitor;

   protected type Data_To_Monitor (Server : access LSP.Servers.Server) is
      --  This is used to share data with the Filesystem_Monitoring_Task

      procedure Stop_Monitor;
      --  Stop the monitoring session

      procedure Set_LSP_Monitor (M : LSP_Monitor_Access);
      --  Store the LSP Monitor that has been created by the task

   private
      Monitor : LSP_Monitor_Access;
      --  A ref to the monitor. This is set when the task starts monitoring.
      --  The task has the ownership of this.
   end Data_To_Monitor;
   type Data_To_Monitor_Access is access Data_To_Monitor;

   task type Monitor_Task is
      entry Start
        (Data_To_Monitor : Data_To_Monitor_Access;
         Directories     : GNATCOLL.VFS.File_Array);
      --  Start watching the data in Data_To_Monitor

      entry Stop;
      --  Stop the task
   end Monitor_Task;
   type Monitor_Task_Access is access Monitor_Task;

   type FS_Watch_Monitor (Server : access LSP.Servers.Server) is
     limited new LSP.File_Monitors.File_Monitor with
   record
      Filesystem_Monitor_Task : Monitor_Task_Access;
      To_Monitor              : Data_To_Monitor_Access;
   end record;

   overriding procedure Monitor_Directories
     (Self        : access FS_Watch_Monitor;
      Directories : GNATCOLL.VFS.File_Array);
   --  Set up filesystem monitoring for Directories, and emit
   --  "workspace/didChangeWatchedFiles" notifications
   --  for any modification of any files in these directories.
   --
   --  This cancels any monitoring that was previously set up: only
   --  one set of directories is monitored at a given time.

   overriding procedure Stop_Monitoring_Directories
     (Self : access FS_Watch_Monitor);

end LSP.Servers.FS_Watch;

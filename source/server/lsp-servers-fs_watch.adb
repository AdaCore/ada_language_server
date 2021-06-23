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

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Libfswatch;                use Libfswatch;

with GNATCOLL.VFS;              use GNATCOLL.VFS;

package body LSP.Servers.FS_Watch is

   Filesystem_Monitoring_Trace : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("ALS.FILESYSTEM_MONITORING",
                             GNATCOLL.Traces.Off);

   --------------
   -- Callback --
   --------------

   overriding procedure Callback
     (Self : in out LSP_Monitor; Events : Libfswatch.Event_Vectors.Vector)
   is
      function Flag_To_FileChangeType
        (X : Event_Flags) return LSP.Messages.FileChangeType;
      --  Utility conversion function

      ----------------------------
      -- Flag_To_FileChangeType --
      ----------------------------

      function Flag_To_FileChangeType
        (X : Event_Flags) return LSP.Messages.FileChangeType is
      begin
         case X is
         when Created | Moved_From =>
            return LSP.Messages.Created;
         when Removed =>
            return LSP.Messages.Deleted;
         when others =>
            return LSP.Messages.Changed;
         end case;
      end Flag_To_FileChangeType;

      File : Virtual_File;

   begin
      --  Look through all events...
      for E of Events loop
         File := Create (+To_String (E.Path), Normalize => True);
         --  A file from the source directories has been modified on disk:
         --  send a message to inform that this should be reloaded. The
         --  server will process it in the processing thread.

         declare
            use LSP.Messages;
            use LSP.Messages.Server_Notifications;
            Message : Message_Access;
            Changes : DidChangeWatchedFilesParams;
            URI     : constant LSP.Messages.DocumentUri :=
              LSP.Types.File_To_URI (File.Display_Full_Name);
         begin
            for F of E.Flags loop
               Changes.changes.Append
                 (FileEvent'(uri => URI,
                             a_type => Flag_To_FileChangeType (F)));
            end loop;
            Message := new DidChangeWatchedFiles_Notification'
              (method  => "workspace/didChangeWatchedFiles",
               jsonrpc => "2.0",
               params  => Changes);

            Self.The_Server.Input_Queue.Enqueue (Message);
         end;
      end loop;
   end Callback;

   ---------------------
   -- Data_To_Monitor --
   ---------------------

   protected body Data_To_Monitor is
      procedure Stop_Monitor is
      begin
         if Monitor /= null then
            Monitor.Stop_Monitor;
         end if;
      end Stop_Monitor;

      ---------------------
      -- Set_LSP_Monitor --
      ---------------------

      procedure Set_LSP_Monitor (M : LSP_Monitor_Access) is
      begin
         Monitor := M;
      end Set_LSP_Monitor;
   end Data_To_Monitor;

   ------------------
   -- Monitor_Task --
   ------------------

   task body Monitor_Task is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (LSP_Monitor, LSP_Monitor_Access);

      Data           : Data_To_Monitor_Access;
      Monitor        : LSP_Monitor_Access;
      Dirs           : GNATCOLL.VFS.File_Array_Access;
      Stop_Requested : Boolean := False;
      Free_Index     : Natural;
      --  Index of the first available spot in Dirs.all
   begin
      loop
         --  Wait until Start or Stop
         select
            accept Start
              (Data_To_Monitor : Data_To_Monitor_Access;
               Directories     : GNATCOLL.VFS.File_Array)
            do
               Data    := Data_To_Monitor;

               Monitor := new LSP_Monitor;
               Monitor.The_Server := Data_To_Monitor.Server;

               Dirs := new File_Array (1 .. Directories'Length);
               Free_Index := 1;
               for Dir of Directories loop
                  if Dir.Is_Directory then
                     Dirs (Free_Index) := Dir;
                     Free_Index := Free_Index + 1;
                  end if;
               end loop;
            end Start;
         or
            accept Stop do
               Stop_Requested := True;
            end Stop;
         end select;

         if Stop_Requested then
            --  Exit the task
            exit;
         else
            if Free_Index > 1 then
               Data.Set_LSP_Monitor (Monitor);

               --  Start monitoring. This call is blocking until
               --  Monitor.Stop_Monitor is called.
               Monitor.Blocking_Monitor
                 (Dirs (1 .. Free_Index - 1),
                  (Updated, Created, Moved_From, Removed, Moved_To));
            else
               Data.Set_LSP_Monitor (null);
            end if;

            --  Deallocate memory
            Unchecked_Free (Dirs);
            Unchecked_Free (Monitor);
         end if;
      end loop;
   end Monitor_Task;

   -------------------------
   -- Monitor_Directories --
   -------------------------

   overriding procedure Monitor_Directories
     (Self        : access FS_Watch_Monitor;
      Directories : GNATCOLL.VFS.File_Array)
   is
   begin
      --  If the trace is deactivated, do nothing, and do not launch the task
      if not Filesystem_Monitoring_Trace.Active then
         return;
      end if;

      --  If the task hasn't started, start it now
      if Self.Filesystem_Monitor_Task = null then
         Self.Filesystem_Monitor_Task := new Monitor_Task;
      end if;

      if Self.To_Monitor /= null then
         --  If we were previously monitoring directories, stop this now
         Self.To_Monitor.Stop_Monitor;
      else
         --  Create the shared data if it didn't exist before
         Self.To_Monitor := new Data_To_Monitor (Self.Server);
      end if;

      --  Tell the task to start monitoring directories
      Self.Filesystem_Monitor_Task.Start
        (Data_To_Monitor => Self.To_Monitor,
         Directories     => Directories);
   end Monitor_Directories;

   ---------------------------------
   -- Stop_Monitoring_Directories --
   ---------------------------------

   overriding procedure Stop_Monitoring_Directories
     (Self : access FS_Watch_Monitor)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Monitor_Task, Monitor_Task_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Data_To_Monitor, Data_To_Monitor_Access);
   begin
      if Self.To_Monitor /= null then
         Self.To_Monitor.Stop_Monitor;
         Unchecked_Free (Self.To_Monitor);
      end if;

      if Self.Filesystem_Monitor_Task /= null then
         Self.Filesystem_Monitor_Task.Stop;
         Unchecked_Free (Self.Filesystem_Monitor_Task);
      end if;
   end Stop_Monitoring_Directories;

end LSP.Servers.FS_Watch;

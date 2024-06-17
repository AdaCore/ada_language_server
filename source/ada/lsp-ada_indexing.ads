------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2023-2024, AdaCore                     --
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

private with Ada.Calendar;
with Ada.Containers.Hashed_Sets;

with GNATCOLL.VFS;

with LSP.Ada_Configurations;
with LSP.Ada_Handlers;
with LSP.Server_Jobs;
limited with LSP.Servers;
private with LSP.Client_Message_Receivers;
private with LSP.Server_Messages;
private with LSP.Structures;

package LSP.Ada_Indexing is

   package File_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => GNATCOLL.VFS.Virtual_File,
      Hash                => GNATCOLL.VFS.Full_Name_Hash,
      Equivalent_Elements => GNATCOLL.VFS."=",
      "="                 => GNATCOLL.VFS."=");

   procedure Schedule_Indexing
     (Server        : not null access LSP.Servers.Server'Class;
      Handler       : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Configuration : LSP.Ada_Configurations.Configuration'Class;
      Project_Stamp : LSP.Ada_Handlers.Project_Stamp;
      Files         : File_Sets.Set);

   type Indexing_Job (<>) is new LSP.Server_Jobs.Server_Job
     with private;

private

   --  Indexing of sources is performed in the background as soon as
   --  requested (typically after a project load), and pre-indexes the
   --  Ada source files, so that subsequent request are fast. The way
   --  the "backgrounding" works is the following:
   --
   --      * each request which should trigger indexing (for instance
   --        project load) compute list of file to index and call
   --        Schedule_Indexing which creates indexing job
   --
   --      * the indexing job takes care of the indexing; it's also
   --        looking at the queue after each indexing to see if there
   --        are requests pending. If a request is pending, it creates and
   --        schedule new indexing job

   type Indexing_Job
     (Handler : not null access LSP.Ada_Handlers.Message_Handler'Class) is
        new LSP.Server_Jobs.Server_Job with
   record
      Files_To_Index       : File_Sets.Set;
      --  Contains any files that need indexing.

      Indexing_Token       : LSP.Structures.ProgressToken;
      --  The token of the current indexing progress sequence

      Total_Files_Indexed  : Natural := 0;
      Total_Files_To_Index : Positive := 1;
      --  These two fields are used to produce a progress bar for the indexing
      --  operations. Total_Files_To_Index starts at 1 so that the progress
      --  bar starts at 0%.

      Progress_Report_Sent : Ada.Calendar.Time;
      --  Time of send of last progress notification.

      Project_Stamp        : LSP.Ada_Handlers.Project_Stamp;
   end record;

   overriding function Priority
     (Self : Indexing_Job) return LSP.Server_Jobs.Job_Priority is
       (LSP.Server_Jobs.Lowest);

   overriding procedure Execute
     (Self   : in out Indexing_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   overriding function Message (Self : Indexing_Job)
     return LSP.Server_Messages.Server_Message_Access is (null);

end LSP.Ada_Indexing;

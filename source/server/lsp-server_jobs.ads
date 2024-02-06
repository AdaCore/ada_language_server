------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2024, AdaCore                     --
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

with LSP.Client_Message_Receivers;
with LSP.Server_Messages;

package LSP.Server_Jobs is
   pragma Preelaborate;

   type Job_Priority is (Low, High, Immediate, Fence);
   --  Job priority to schedule jobs.
   --
   --  @value Low - long running jobs like find-all-references
   --  @value High - fast queries like hover
   --  @value Immediate - urgent queries like cancel-request
   --  @value Fence - ordering messages like didChange
   --
   --  When te server gets an Immediate or Fence job it stops accepting
   --  new messages until the job is done. Server execute each job in its
   --  queue before executing any Fence job.

   subtype Ordinal_Priority is Job_Priority range Low .. High;

   type Server_Job is limited interface;

   function Priority (Self : Server_Job) return Job_Priority is abstract;
   --  Return job's priority

   function Is_Done (Self : Server_Job) return Boolean is abstract;
   --  Return True if job has completed the execution

   procedure Execute
     (Self   : in out Server_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class)
          is abstract;
   --  Spend some time executing the job. Use Client to send messages if
   --  required.

   procedure Complete
     (Self : in out Server_Job;
      Next : LSP.Server_Messages.Server_Message_Access) is null;
   --  Complete message execution. The next message is provided if any.
   --  Currently this is called only for Fence jobs.

   procedure Cancel
     (Self   : in out Server_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class)
          is null;
   --  Cancel job execution. Use Client to send messages if required.

   --  function Progress (Self : Server_Job) return Job_Progress is abstract;
   --  Report job's progress

   function Message (Self : Server_Job)
     return LSP.Server_Messages.Server_Message_Access is abstract;
   --  Message to be destroyed when the job is done

   function Assigned (Self : access Server_Job'Class) return Boolean is
      (Self /= null);

   type Server_Job_Access is access all Server_Job'Class;

end LSP.Server_Jobs;

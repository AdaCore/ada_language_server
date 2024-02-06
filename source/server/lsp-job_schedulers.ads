------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2024, AdaCore                       --
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

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Tags;

with LSP.Client_Message_Receivers;
with LSP.Server_Jobs;
with LSP.Server_Message_Handlers;
with LSP.Server_Messages;

package LSP.Job_Schedulers is
   pragma Preelaborate;

   type Job_Scheduler is tagged limited private;

   procedure Register_Handler
     (Self    : in out Job_Scheduler'Class;
      Tag     : Ada.Tags.Tag;
      Handler : LSP.Server_Message_Handlers.Server_Message_Handler_Access);
   --  Register server message handler per message tag.

   function Has_Jobs (Self : Job_Scheduler'Class) return Boolean;
   --  Return true if there are any jobs in the queue.

   procedure Create_Job
     (Self    : in out Job_Scheduler'Class;
      Message : in out LSP.Server_Messages.Server_Message_Access);
   --  Create a job to process a server message. The scheduler takes ownership
   --  of the message and will return it to the server when the job is done.
   --  If there is no handler for the message, then the scheduler doesn't
   --  accept message and server should destroy it.

   procedure Process_High_Priority_Job
     (Self    : in out Job_Scheduler'Class;
      Client  :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Waste   : out LSP.Server_Messages.Server_Message_Access);
   --  Execute jobs with highest priority (Immediate, Fence).
   --  When a job is done the routine returns (in Waste) the message to be
   --  deallocated by the server. The Client is used to send messages during
   --  the execution of the job.

   procedure Process_Job
     (Self    : in out Job_Scheduler'Class;
      Client  :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Waste   : out LSP.Server_Messages.Server_Message_Access);
   --  Execute jobs with ordinal priority (Low, High).
   --  When a job is done the routine returns (in Waste) the message to be
   --  deallocated by the server. The Client is used to send messages during
   --  the execution of the job.

private

   function Hash (Tag : Ada.Tags.Tag) return Ada.Containers.Hash_Type;

   package Handler_Maps is new Ada.Containers.Hashed_Maps
     (Ada.Tags.Tag,
      LSP.Server_Message_Handlers.Server_Message_Handler_Access,
      Hash,
      Ada.Tags."=",
      LSP.Server_Message_Handlers."=");

   package Job_Lists is new Ada.Containers.Doubly_Linked_Lists
     (LSP.Server_Jobs.Server_Job_Access, LSP.Server_Jobs."=");

   subtype Ordinal_Priority is LSP.Server_Jobs.Job_Priority
     range LSP.Server_Jobs.Low .. LSP.Server_Jobs.High;

   type Job_List_Array is array (Ordinal_Priority) of Job_Lists.List;

   type Job_Scheduler is tagged limited record
      Blocker  : LSP.Server_Jobs.Server_Job_Access;
      --  A job with non-ordinal priority (Immediate, Fence)
      Done     : LSP.Server_Jobs.Server_Job_Access;
      --  A job with Fence priority to be completed
      Handlers : Handler_Maps.Map;
      Jobs     : Job_List_Array;
   end record;

end LSP.Job_Schedulers;

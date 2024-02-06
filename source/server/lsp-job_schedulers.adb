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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Storage_Elements;

package body LSP.Job_Schedulers is

   procedure Free is new Ada.Unchecked_Deallocation
     (LSP.Server_Jobs.Server_Job'Class, LSP.Server_Jobs.Server_Job_Access);

   procedure Complete_Last_Fence_Job
     (Self : in out Job_Scheduler'Class;
      Next : LSP.Server_Messages.Server_Message_Access);
   --  Call Complete on the last done Fence job (if any) and free it

   -----------------------------
   -- Complete_Last_Fence_Job --
   -----------------------------

   procedure Complete_Last_Fence_Job
     (Self : in out Job_Scheduler'Class;
      Next : LSP.Server_Messages.Server_Message_Access) is
   begin
      if Self.Done.Assigned then
         Self.Done.Complete (Next);
         Free (Self.Done);
      end if;
   end Complete_Last_Fence_Job;

   ----------------
   -- Create_Job --
   ----------------

   procedure Create_Job
     (Self    : in out Job_Scheduler'Class;
      Message : in out LSP.Server_Messages.Server_Message_Access)
   is
      Cursor : constant Handler_Maps.Cursor :=
        Self.Handlers.Find (Message'Tag);

      Job : LSP.Server_Jobs.Server_Job_Access;
   begin
      if Handler_Maps.Has_Element (Cursor) then

         Job := Handler_Maps.Element (Cursor).Create_Job (Message);

         if Job.Assigned then
            Message := null;

            if Job.Priority in Self.Jobs'Range then
               Self.Jobs (Job.Priority).Append (Job);
            else
               Self.Blocker := Job;
            end if;
         end if;
      end if;
   end Create_Job;

   --------------
   -- Has_Jobs --
   --------------

   function Has_Jobs (Self : Job_Scheduler'Class) return Boolean is
   begin
      return Self.Blocker.Assigned or else
        (for some List of Self.Jobs => not List.Is_Empty);
   end Has_Jobs;

   ----------
   -- Hash --
   ----------

   function Hash (Tag : Ada.Tags.Tag) return Ada.Containers.Hash_Type is
      function Cast is new Ada.Unchecked_Conversion
        (Ada.Tags.Tag, System.Address);
   begin
      return Ada.Containers.Hash_Type'Mod
        (System.Storage_Elements.To_Integer (Cast (Tag)));
   end Hash;

   -------------
   -- Process --
   -------------

   procedure Process_High_Priority_Job
     (Self   : in out Job_Scheduler'Class;
      Client : in out
        LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Waste  : out LSP.Server_Messages.Server_Message_Access)
   is
      use all type LSP.Server_Jobs.Job_Priority;

      procedure Execute (Job : LSP.Server_Jobs.Server_Job_Access);

      -------------
      -- Execute --
      -------------

      procedure Execute (Job : LSP.Server_Jobs.Server_Job_Access) is
      begin
         Self.Complete_Last_Fence_Job (Job.Message);
         Waste := Job.Message;

         while not Job.Is_Done loop
            Job.Execute (Client);
         end loop;
      end Execute;

      Job : LSP.Server_Jobs.Server_Job_Access renames Self.Blocker;
   begin
      if not Job.Assigned then
         Waste := null;

         return;
      end if;

      if Job.Priority = Fence then
         --  Process other jobs before any Fence job
         while (for some List of Self.Jobs => not List.Is_Empty) loop
            Self.Process_Job (Client, Waste);

            if Waste.Assigned then
               return;
            end if;
         end loop;

         Execute (Job);
         Self.Done := Job;  --  keep Job live till Complete call
         Job := null;
      else
         Execute (Job);
         Free (Job);
      end if;
   end Process_High_Priority_Job;

   -----------------
   -- Process_Job --
   -----------------

   procedure Process_Job
     (Self    : in out Job_Scheduler'Class;
      Client  :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Waste   : out LSP.Server_Messages.Server_Message_Access) is
   begin
      for List of reverse Self.Jobs when not List.Is_Empty loop
         declare
            Job : LSP.Server_Jobs.Server_Job_Access := List.First_Element;
         begin
            List.Delete_First;
            Self.Complete_Last_Fence_Job (Job.Message);
            Job.Execute (Client);

            if Job.Is_Done then
               Waste := Job.Message;
               Free (Job);
            else
               Waste := null;
               List.Append (Job);  --  Push the job back to the queue
            end if;

            exit;
         end;
      end loop;

      Self.Complete_Last_Fence_Job (null);
   end Process_Job;

   ----------------------
   -- Register_Handler --
   ----------------------

   procedure Register_Handler
     (Self    : in out Job_Scheduler'Class;
      Tag     : Ada.Tags.Tag;
      Handler : LSP.Server_Message_Handlers.Server_Message_Handler_Access) is
   begin
      Self.Handlers.Include (Tag, Handler);
   end Register_Handler;

end LSP.Job_Schedulers;

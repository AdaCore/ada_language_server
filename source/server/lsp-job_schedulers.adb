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
with LSP.Server_Jobs;

package body LSP.Job_Schedulers is

   procedure Free is new Ada.Unchecked_Deallocation
     (LSP.Server_Jobs.Server_Job'Class, LSP.Server_Jobs.Server_Job_Access);

   ----------------
   -- Create_Job --
   ----------------

   procedure Create_Job
     (Self    : in out Job_Scheduler'Class;
      Message : in out LSP.Server_Messages.Server_Message_Access) is
   begin
      Self.Message := Message;
      Message := null;
   end Create_Job;

   --------------
   -- Has_Jobs --
   --------------

   function Has_Jobs (Self : Job_Scheduler'Class) return Boolean is
   begin
      return Self.Message.Assigned;
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
      Job : LSP.Server_Jobs.Server_Job_Access;
   begin
      Waste := null;

      --  Process the most recent message if any
      if Self.Message.Assigned then
         declare
            Cursor : constant Handler_Maps.Cursor :=
              Self.Handlers.Find (Self.Message'Tag);
         begin
            if Handler_Maps.Has_Element (Cursor) then
               Job := Handler_Maps.Element (Cursor).Create_Job (Self.Message);
               Self.Message := null;
            else
               Waste := Self.Message;
               Self.Message := null;
               return;
            end if;
         end;
      else
         null;  --  TBD: find next job here
      end if;

      while Job.Assigned loop
         Job.Execute (Client);

         if Job.Is_Done then
            --  TBD: Call complete?
            Waste := Job.Message;
            Free (Job);
            exit;
         else
            raise Program_Error with "Unimplemeted";
            --  TBD: put job back to the queue
         end if;
      end loop;
   end Process_High_Priority_Job;

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

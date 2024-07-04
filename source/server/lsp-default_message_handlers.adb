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

with LSP.Client_Message_Receivers;

package body LSP.Default_Message_Handlers is

   type Sequential_Job is new LSP.Server_Jobs.Server_Job with record
      Handler  : Server_Message_Visitor_Access;
      Priority : LSP.Server_Jobs.Job_Priority;
      Message  : LSP.Server_Messages.Server_Message_Access;
      Is_Done  : Boolean := False;
   end record;

   type Sequential_Job_Access is access all Sequential_Job'Class;

   overriding function Priority
     (Self : Sequential_Job) return LSP.Server_Jobs.Job_Priority is
       (Self.Priority);

   overriding function Message (Self : Sequential_Job)
     return LSP.Server_Messages.Server_Message_Access is (Self.Message);

   overriding procedure Execute
     (Self   : in out Sequential_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   overriding procedure Complete
     (Self : in out Sequential_Job;
      Next : LSP.Server_Messages.Server_Message_Access) is null;

   overriding procedure Cancel
     (Self   : in out Sequential_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class)
          is null;

   ----------------
   -- Create_Job --
   ----------------

   overriding function Create_Job
     (Self    : Default_Message_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
      return LSP.Server_Jobs.Server_Job_Access
   is
      Result : constant Sequential_Job_Access := new Sequential_Job'
        (Handler  => Self.Handler,
         Priority => Self.Priority,
         Message  => Message,
         Is_Done  => False);
   begin
      return LSP.Server_Jobs.Server_Job_Access (Result);
   end Create_Job;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : in out Sequential_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is
      pragma Unreferenced (Client);
      --  Self.Handler knows how to send messages by it-self
   begin
      Self.Message.Visit_Server_Message_Visitor (Self.Handler.all);
      Status := LSP.Server_Jobs.Done;
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : in out Default_Message_Handler'Class;
      Handler : not null access
        LSP.Server_Message_Visitors.Server_Message_Visitor'Class;
      Priority : LSP.Server_Jobs.Job_Priority := LSP.Server_Jobs.Fence) is
   begin
      Self.Handler := Handler;
      Self.Priority := Priority;
   end Initialize;

end LSP.Default_Message_Handlers;

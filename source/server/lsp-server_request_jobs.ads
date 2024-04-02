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

--  This package provides a base type for server requests jobs. It implements
--  request cancelation logic: if the request has been canceled then the job
--  has Immediate priority and its execution just sends the corresponding
--  error code.

with LSP.Client_Message_Receivers;
with LSP.Server_Jobs;
with LSP.Server_Messages;
with LSP.Server_Requests;

package LSP.Server_Request_Jobs is
   pragma Preelaborate;

   type Request_Access is
     access all LSP.Server_Requests.Server_Request'Class;

   type Server_Request_Job (Priority : LSP.Server_Jobs.Job_Priority)
     is abstract limited new LSP.Server_Jobs.Server_Job with
   record
       Request : Request_Access;
   end record;

   procedure Execute_Request
     (Self   : in out Server_Request_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status) is abstract;

   overriding function Priority
     (Self : Server_Request_Job) return LSP.Server_Jobs.Job_Priority is
       (if Self.Request.Canceled then LSP.Server_Jobs.Immediate
        else Self.Priority);

   overriding procedure Execute
     (Self   : in out Server_Request_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   overriding function Message (Self : Server_Request_Job)
     return LSP.Server_Messages.Server_Message_Access is
       (LSP.Server_Messages.Server_Message_Access (Self.Request));

end LSP.Server_Request_Jobs;

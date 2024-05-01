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
with LSP.Server_Jobs;
with LSP.Server_Request_Jobs;

package LSP.Ada_Request_Jobs is
   pragma Preelaborate;

   type Ada_Request_Job is abstract
     new LSP.Server_Request_Jobs.Server_Request_Job with null record;
   --  This base type for Ada request jobs. It catches Property_Error
   --  exceptions to avoid error responses in these cases.

   procedure Execute_Ada_Request
     (Self   : in out Ada_Request_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status) is abstract;

   overriding procedure Execute_Request
     (Self   : in out Ada_Request_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);
   --  A wrapper for Execute_Ada_Request that catches Property_Errors and
   --  returns an empty response

   subtype Request_Access is LSP.Server_Request_Jobs.Request_Access;

end LSP.Ada_Request_Jobs;

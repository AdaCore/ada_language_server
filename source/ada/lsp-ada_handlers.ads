------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
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
--  This package provides requests and notifications handler for Ada
--  language.

with LSP.Client_Message_Receivers;
with LSP.Server_Request_Receivers;
with LSP.Server_Message_Visitors;
with LSP.Server_Requests;
with LSP.Unimplemented_Handlers;
with LSP.Tracers;

package LSP.Ada_Handlers is

   type Message_Handler
     (Client : not null access LSP.Client_Message_Receivers
        .Client_Message_Receiver'Class;
      Tracer : not null LSP.Tracers.Tracer_Access) is limited
   new LSP.Server_Message_Visitors.Server_Message_Visitor
     and LSP.Server_Request_Receivers.Server_Request_Receiver
   with private;

private

   type Message_Handler
     (Client : not null access LSP.Client_Message_Receivers
        .Client_Message_Receiver'Class;
      Tracer : not null LSP.Tracers.Tracer_Access) is limited
   new LSP.Unimplemented_Handlers.Unimplemented_Handler
     and LSP.Server_Message_Visitors.Server_Message_Visitor
   with record
      null;
   end record;

   overriding procedure On_Server_Request
     (Self  : in out Message_Handler;
      Value : LSP.Server_Requests.Server_Request'Class);

   procedure Publish_Diagnostics
     (Self              : access Message_Handler'Class;
      Document          : not null LSP.Ada_Documents.Document_Access;
      Other_Diagnostics : LSP.Messages.Diagnostic_Vector :=
        LSP.Messages.Diagnostic_Vectors.Empty;
      Force             : Boolean := False);
   --  Publish diagnostic messages for given document if needed.
   --  Other_Diagnostics can be used to specify punctual diagnostics not coming
   --  from sources that analyze files when being opened or modified.
   --  When Force is True, the diagnostics will always be sent, not matter if
   --  they have changed or not.

end LSP.Ada_Handlers;

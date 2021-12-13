------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2020, AdaCore                       --
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

--  This package provides a decorator specialized for fuzzing the Ada
--  Language Server, making checks on the requests being processed and
--  intentionally crashing the server when there are inconsistencies.

with GNATCOLL.Traces;
with LSP.Messages;
with LSP.Servers;

with LSP.Server_Request_Handlers;
with LSP.Server_Notification_Receivers;
with LSP.Error_Decorators;
with LSP.Ada_Documents;

package LSP.Fuzz_Decorators is

   type Fuzz_Request_Decorator
     (Trace   : GNATCOLL.Traces.Trace_Handle;
      Handler : not null
        LSP.Server_Request_Handlers.Server_Request_Handler_Access;
      On_Error : not null LSP.Servers.Uncaught_Exception_Handler) is new
     LSP.Error_Decorators.Error_Decorator (Trace, Handler, On_Error)
   with null record;

   type Fuzz_Notification_Decorator
     (Trace   : GNATCOLL.Traces.Trace_Handle;
      Handler : not null
        LSP.Server_Notification_Receivers.Server_Notification_Receiver_Access;
      Doc_Provider : not null LSP.Ada_Documents.Document_Provider_Access)
   is new LSP.Server_Notification_Receivers.Server_Notification_Receiver with
     null record;

   overriding procedure On_Initialized_Notification
     (Self  : access Fuzz_Notification_Decorator);

   overriding procedure On_Exit_Notification
     (Self  : access Fuzz_Notification_Decorator);

   overriding procedure On_DidChangeConfiguration_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DidChangeConfigurationParams);

   overriding procedure On_DidChangeWorkspaceFolders_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DidChangeWorkspaceFoldersParams);

   overriding procedure On_DidChangeWatchedFiles_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DidChangeWatchedFilesParams);

   overriding procedure On_Cancel_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.CancelParams);

   overriding procedure On_DidCreateFiles_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.CreateFilesParams);

   overriding procedure On_DidRenameFiles_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.RenameFilesParams);

   overriding procedure On_DidDeleteFiles_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DeleteFilesParams);

   overriding procedure On_DidOpenTextDocument_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DidOpenTextDocumentParams);

   overriding procedure On_DidChangeTextDocument_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DidChangeTextDocumentParams);

   overriding procedure On_DidSaveTextDocument_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DidSaveTextDocumentParams);

   overriding procedure On_DidCloseTextDocument_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DidCloseTextDocumentParams);

end LSP.Fuzz_Decorators;

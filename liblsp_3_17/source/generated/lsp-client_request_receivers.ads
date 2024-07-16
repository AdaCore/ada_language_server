--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with LSP.Structures;

package LSP.Client_Request_Receivers is
   pragma Preelaborate;

   type Client_Request_Receiver is limited interface;

   procedure On_RegisterCapability_Request
     (Self  : in out Client_Request_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.RegistrationParams) is null;
   --  The `client/registerCapability` request is sent from the server to the
   --  client to register a new capability handler on the client side.

   procedure On_UnregisterCapability_Request
     (Self  : in out Client_Request_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.UnregistrationParams) is null;
   --  The `client/unregisterCapability` request is sent from the server to the
   --  client to unregister a previously registered capability handler on the
   --  client side.

   procedure On_ShowDocument_Request
     (Self  : in out Client_Request_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ShowDocumentParams) is null;
   --  A request to show a document. This request might open an external
   --  program depending on the value of the URI to open. For example a request
   --  to open `https://code.visualstudio.com/` will very likely open the URI
   --  in a WEB browser.
   --
   --  @since 3.16.0

   procedure On_ShowMessageRequest_Request
     (Self  : in out Client_Request_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ShowMessageRequestParams) is null;
   --  The show message request is sent from the server to the client to show a
   --  message and a set of options actions to the user.

   procedure On_Progress_Create_Request
     (Self  : in out Client_Request_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkDoneProgressCreateParams) is null;
   --  The `window/workDoneProgress/create` request is sent from the server to
   --  the client to initiate progress reporting from the server.

   procedure On_ApplyEdit_Request
     (Self  : in out Client_Request_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ApplyWorkspaceEditParams) is null;
   --  A request sent from the server to the client to modified certain
   --  resources.

   procedure On_Code_Lens_Refresh_Request
     (Self : in out Client_Request_Receiver;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is null;
   --  A request to refresh all code actions
   --
   --  @since 3.16.0

   procedure On_Configuration_Request
     (Self  : in out Client_Request_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ConfigurationParams) is null;
   --  The 'workspace/configuration' request is sent from the server to the
   --  client to fetch a certain configuration setting.
   --
   --  This pull model replaces the old push model were the client signaled
   --  configuration change via an event. If the server still needs to
   --  react to configuration changes (since the server caches the result of
   --  `workspace/configuration` requests) the server should register for an
   --  empty configuration change event and empty the cache if such an event
   --  is received.

   procedure On_Diagnostic_Refresh_Request
     (Self : in out Client_Request_Receiver;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is null;
   --  The diagnostic refresh request definition.
   --
   --  @since 3.17.0

   procedure On_Inlay_Refresh_Request
     (Self : in out Client_Request_Receiver;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is null;
   --  @since 3.17.0

   procedure On_Inline_Refresh_Request
     (Self : in out Client_Request_Receiver;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is null;
   --  @since 3.17.0

   procedure On_Tokens_Refresh_Request
     (Self : in out Client_Request_Receiver;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is null;
   --  @since 3.16.0

   procedure On_WorkspaceFolders_Request
     (Self : in out Client_Request_Receiver;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is null;
   --  The `workspace/workspaceFolders` is sent from the server to the client
   --  to fetch the open workspace folders.

end LSP.Client_Request_Receivers;

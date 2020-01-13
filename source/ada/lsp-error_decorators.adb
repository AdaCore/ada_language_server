------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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
with Libadalang.Common;

package body LSP.Error_Decorators is

   generic
      type Request is private;

      type Response (Is_Error : Boolean) is private;

      type Handler is limited interface
        and LSP.Server_Request_Handlers.Server_Request_Handler;

      with function On_Request
        (Self  : not null access Handler;
         Value : Request) return Response is abstract;

   function Generic_Request
     (Self  : not null access Error_Decorator;
      Value : Request) return Response;

   ---------------------
   -- Generic_Request --
   ---------------------

   function Generic_Request
     (Self  : not null access Error_Decorator;
      Value : Request) return Response is
   begin
      return On_Request (Handler'Class (Self.Handler.all)'Access, Value);
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Trace.Trace ("Uncaught Property_Error");
         Self.Trace.Trace (E);
         pragma Warnings (Off, "is read but never assigned");
         return Result : Response (Is_Error => False);
         pragma Warnings (On, "is read but never assigned");
      when others =>
         --  Property errors are expected to happen in the normal flow
         --  of events in LAL. However, for any other error than a
         --  property error, we want to reload the context.
         Self.On_Error.all;
         raise;
   end Generic_Request;

   ---------------------------
   -- On_Initialize_Request --
   ---------------------------

   overriding function On_Initialize_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Initialize_Request)
        return LSP.Messages.Server_Responses.Initialize_Response is
   begin
      --  We don't expect Property_Error here and can't provide any good
      --  response by default, so just forward the request here.
      return Self.Handler.On_Initialize_Request (Request);
   end On_Initialize_Request;

   -------------------------
   -- On_Shutdown_Request --
   -------------------------

   function Shutdown_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Shutdown_Request,
      Response   => LSP.Messages.Server_Responses.Shutdown_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Shutdown_Request);

   overriding function On_Shutdown_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Shutdown_Request)
      return LSP.Messages.Server_Responses.Shutdown_Response
        renames Shutdown_Request;

   ---------------------------
   -- On_CodeAction_Request --
   ---------------------------

   function CodeAction_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.CodeAction_Request,
      Response   => LSP.Messages.Server_Responses.CodeAction_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_CodeAction_Request);

   overriding function On_CodeAction_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.CodeAction_Request)
      return LSP.Messages.Server_Responses.CodeAction_Response
        renames CodeAction_Request;

   ---------------------------
   -- On_Completion_Request --
   ---------------------------

   function Completion_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Completion_Request,
      Response   => LSP.Messages.Server_Responses.Completion_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Completion_Request);

   overriding function On_Completion_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Completion_Request)
      return LSP.Messages.Server_Responses.Completion_Response
        renames Completion_Request;

   ---------------------------
   -- On_Declaration_Request --
   ---------------------------

   function Declaration_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Declaration_Request,
      Response   => LSP.Messages.Server_Responses.Location_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Declaration_Request);

   overriding function On_Declaration_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Declaration_Request)
      return LSP.Messages.Server_Responses.Location_Response
        renames Declaration_Request;

   -------------------------------
   -- On_Implementation_Request --
   -------------------------------

   function Implementation_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Implementation_Request,
      Response   => LSP.Messages.Server_Responses.Location_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Implementation_Request);

   overriding function On_Implementation_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Implementation_Request)
      return LSP.Messages.Server_Responses.Location_Response
        renames Implementation_Request;

   ---------------------------
   -- On_Definition_Request --
   ---------------------------

   function Definition_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Definition_Request,
      Response   => LSP.Messages.Server_Responses.Location_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Definition_Request);

   overriding function On_Definition_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Definition_Request)
      return LSP.Messages.Server_Responses.Location_Response
        renames Definition_Request;

   --------------------------------
   -- On_Type_Definition_Request --
   --------------------------------

   function Type_Definition_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Type_Definition_Request,
      Response   => LSP.Messages.Server_Responses.Location_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Type_Definition_Request);

   overriding function On_Type_Definition_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Type_Definition_Request)
      return LSP.Messages.Server_Responses.Location_Response
        renames Type_Definition_Request;

   --------------------------
   -- On_Highlight_Request --
   --------------------------

   function Highlight_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Highlight_Request,
      Response   => LSP.Messages.Server_Responses.Highlight_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Highlight_Request);

   overriding function On_Highlight_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Highlight_Request)
      return LSP.Messages.Server_Responses.Highlight_Response
        renames Highlight_Request;

   ----------------------
   -- On_Hover_Request --
   ----------------------

   function Hover_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Hover_Request,
      Response   => LSP.Messages.Server_Responses.Hover_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Hover_Request);

   overriding function On_Hover_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Hover_Request)
      return LSP.Messages.Server_Responses.Hover_Response
        renames Hover_Request;

   ---------------------------
   -- On_References_Request --
   ---------------------------

   function References_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.References_Request,
      Response   => LSP.Messages.Server_Responses.Location_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_References_Request);

   overriding function On_References_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.References_Request)
      return LSP.Messages.Server_Responses.Location_Response
        renames References_Request;

   -------------------------------
   -- On_Signature_Help_Request --
   -------------------------------

   function Signature_Help_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Signature_Help_Request,
      Response   => LSP.Messages.Server_Responses.SignatureHelp_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Signature_Help_Request);

   overriding function On_Signature_Help_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Signature_Help_Request)
      return LSP.Messages.Server_Responses.SignatureHelp_Response
        renames Signature_Help_Request;

   -------------------------------
   -- On_Document_Links_Request --
   -------------------------------

   function Document_Links_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Document_Links_Request,
      Response   => LSP.Messages.Server_Responses.Links_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Document_Links_Request);

   overriding function On_Document_Links_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Document_Links_Request)
      return LSP.Messages.Server_Responses.Links_Response
        renames Document_Links_Request;

   ---------------------------------
   -- On_Document_Symbols_Request --
   ---------------------------------

   function Document_Symbols_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Document_Symbols_Request,
      Response   => LSP.Messages.Server_Responses.Symbol_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Document_Symbols_Request);

   overriding function On_Document_Symbols_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Document_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response
        renames Document_Symbols_Request;

   -----------------------
   -- On_Rename_Request --
   -----------------------

   function Rename_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Rename_Request,
      Response   => LSP.Messages.Server_Responses.Rename_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Rename_Request);

   overriding function On_Rename_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Rename_Request)
      return LSP.Messages.Server_Responses.Rename_Response
        renames Rename_Request;

   --------------------------------
   -- On_Execute_Command_Request --
   --------------------------------

   function Execute_Command_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Execute_Command_Request,
      Response   => LSP.Messages.Server_Responses.ExecuteCommand_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Execute_Command_Request);

   overriding function On_Execute_Command_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response
        renames Execute_Command_Request;

   ----------------------------------
   -- On_Workspace_Symbols_Request --
   ----------------------------------

   function Workspace_Symbols_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Workspace_Symbols_Request,
      Response   => LSP.Messages.Server_Responses.Symbol_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Workspace_Symbols_Request);

   overriding function On_Workspace_Symbols_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Workspace_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response
        renames Workspace_Symbols_Request;

   ------------------------------------------
   -- On_Workspace_Execute_Command_Request --
   ------------------------------------------

   function Workspace_Execute_Command_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests
                      .Workspace_Execute_Command_Request,
      Response   => LSP.Messages.Server_Responses.ExecuteCommand_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers
                      .On_Workspace_Execute_Command_Request);

   overriding function On_Workspace_Execute_Command_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Workspace_Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response
        renames Workspace_Execute_Command_Request;

   ------------------------------
   -- On_ALS_Called_By_Request --
   ------------------------------

   function ALS_Called_By_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.ALS_Called_By_Request,
      Response   => LSP.Messages.Server_Responses.ALS_Called_By_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_ALS_Called_By_Request);

   overriding function On_ALS_Called_By_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.ALS_Called_By_Request)
      return LSP.Messages.Server_Responses.ALS_Called_By_Response
        renames ALS_Called_By_Request;

   --------------------------
   -- On_ALS_Debug_Request --
   --------------------------

   overriding function On_ALS_Debug_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.ALS_Debug_Request)
      return LSP.Messages.Server_Responses.ALS_Debug_Response
   is
   begin
      --  We don't expect Property_Error here
      return Self.Handler.On_ALS_Debug_Request (Request);
   end On_ALS_Debug_Request;

end LSP.Error_Decorators;

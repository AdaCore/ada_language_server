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

package body LSP.Property_Error_Decorators is

   ---------------------------
   -- On_Initialize_Request --
   ---------------------------

   overriding function On_Initialize_Request
     (Self    : access Property_Error_Decorator;
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

   overriding function On_Shutdown_Request
     (Self    : access Property_Error_Decorator;
      Request : LSP.Messages.Server_Requests.Shutdown_Request)
      return LSP.Messages.Server_Responses.Shutdown_Response is
   begin
      return Self.Handler.On_Shutdown_Request (Request);
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Trace.Trace ("Uncatched Property_Error");
         Self.Trace.Trace (E);
         return Result : LSP.Messages.Server_Responses.Shutdown_Response
                           (Is_Error => False);
   end On_Shutdown_Request;

   ---------------------------
   -- On_CodeAction_Request --
   ---------------------------

   overriding function On_CodeAction_Request
     (Self    : access Property_Error_Decorator;
      Request : LSP.Messages.Server_Requests.CodeAction_Request)
      return LSP.Messages.Server_Responses.CodeAction_Response is
   begin
      return Self.Handler.On_CodeAction_Request (Request);
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Trace.Trace ("Uncatched Property_Error");
         Self.Trace.Trace (E);
         return Result : LSP.Messages.Server_Responses.CodeAction_Response
                           (Is_Error => False);
   end On_CodeAction_Request;

   ---------------------------
   -- On_Completion_Request --
   ---------------------------

   overriding function On_Completion_Request
     (Self    : access Property_Error_Decorator;
      Request : LSP.Messages.Server_Requests.Completion_Request)
      return LSP.Messages.Server_Responses.Completion_Response
   is
   begin
      return Self.Handler.On_Completion_Request (Request);
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Trace.Trace ("Uncatched Property_Error");
         Self.Trace.Trace (E);
         return Result : LSP.Messages.Server_Responses.Completion_Response
                           (Is_Error => False);
   end On_Completion_Request;

   ---------------------------
   -- On_Definition_Request --
   ---------------------------

   overriding function On_Definition_Request
     (Self    : access Property_Error_Decorator;
      Request : LSP.Messages.Server_Requests.Definition_Request)
      return LSP.Messages.Server_Responses.Location_Response
   is
   begin
      return Self.Handler.On_Definition_Request (Request);
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Trace.Trace ("Uncatched Property_Error");
         Self.Trace.Trace (E);
         return Result : LSP.Messages.Server_Responses.Location_Response
                           (Is_Error => False);
   end On_Definition_Request;

   --------------------------------
   -- On_Type_Definition_Request --
   --------------------------------

   overriding function On_Type_Definition_Request
     (Self    : access Property_Error_Decorator;
      Request : LSP.Messages.Server_Requests.Type_Definition_Request)
      return LSP.Messages.Server_Responses.Location_Response
   is
   begin
      return Self.Handler.On_Type_Definition_Request (Request);
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Trace.Trace ("Uncatched Property_Error");
         Self.Trace.Trace (E);
         return Result : LSP.Messages.Server_Responses.Location_Response
                           (Is_Error => False);
   end On_Type_Definition_Request;

   --------------------------
   -- On_Highlight_Request --
   --------------------------

   overriding function On_Highlight_Request
     (Self    : access Property_Error_Decorator;
      Request : LSP.Messages.Server_Requests.Highlight_Request)
      return LSP.Messages.Server_Responses.Highlight_Response
   is
   begin
      return Self.Handler.On_Highlight_Request (Request);
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Trace.Trace ("Uncatched Property_Error");
         Self.Trace.Trace (E);
         return Result : LSP.Messages.Server_Responses.Highlight_Response
                           (Is_Error => False);
   end On_Highlight_Request;

   ----------------------
   -- On_Hover_Request --
   ----------------------

   overriding function On_Hover_Request
     (Self    : access Property_Error_Decorator;
      Request : LSP.Messages.Server_Requests.Hover_Request)
      return LSP.Messages.Server_Responses.Hover_Response
   is
   begin
      return Self.Handler.On_Hover_Request (Request);
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Trace.Trace ("Uncatched Property_Error");
         Self.Trace.Trace (E);
         return Result : LSP.Messages.Server_Responses.Hover_Response
                           (Is_Error => False);
   end On_Hover_Request;

   ---------------------------
   -- On_References_Request --
   ---------------------------

   overriding function On_References_Request
     (Self    : access Property_Error_Decorator;
      Request : LSP.Messages.Server_Requests.References_Request)
      return LSP.Messages.Server_Responses.Location_Response
   is
   begin
      return Self.Handler.On_References_Request (Request);
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Trace.Trace ("Uncatched Property_Error");
         Self.Trace.Trace (E);
         return Result : LSP.Messages.Server_Responses.Location_Response
                           (Is_Error => False);
   end On_References_Request;

   -------------------------------
   -- On_Signature_Help_Request --
   -------------------------------

   overriding function On_Signature_Help_Request
     (Self    : access Property_Error_Decorator;
      Request : LSP.Messages.Server_Requests.Signature_Help_Request)
      return LSP.Messages.Server_Responses.SignatureHelp_Response
   is
   begin
      return Self.Handler.On_Signature_Help_Request (Request);
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Trace.Trace ("Uncatched Property_Error");
         Self.Trace.Trace (E);
         return Result : LSP.Messages.Server_Responses.SignatureHelp_Response
                           (Is_Error => False);
   end On_Signature_Help_Request;

   ---------------------------------
   -- On_Document_Symbols_Request --
   ---------------------------------

   overriding function On_Document_Symbols_Request
     (Self    : access Property_Error_Decorator;
      Request : LSP.Messages.Server_Requests.Document_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response
   is
   begin
      return Self.Handler.On_Document_Symbols_Request (Request);
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Trace.Trace ("Uncatched Property_Error");
         Self.Trace.Trace (E);
         return Result : LSP.Messages.Server_Responses.Symbol_Response
                           (Is_Error => False);
   end On_Document_Symbols_Request;

   -----------------------
   -- On_Rename_Request --
   -----------------------

   overriding function On_Rename_Request
     (Self    : access Property_Error_Decorator;
      Request : LSP.Messages.Server_Requests.Rename_Request)
      return LSP.Messages.Server_Responses.Rename_Response
   is
   begin
      return Self.Handler.On_Rename_Request (Request);
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Trace.Trace ("Uncatched Property_Error");
         Self.Trace.Trace (E);
         return Result : LSP.Messages.Server_Responses.Rename_Response
                           (Is_Error => False);
   end On_Rename_Request;

   --------------------------------
   -- On_Execute_Command_Request --
   --------------------------------

   overriding function On_Execute_Command_Request
     (Self    : access Property_Error_Decorator;
      Request : LSP.Messages.Server_Requests.Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response
   is
   begin
      return Self.Handler.On_Execute_Command_Request (Request);
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Trace.Trace ("Uncatched Property_Error");
         Self.Trace.Trace (E);
         return Result : LSP.Messages.Server_Responses.ExecuteCommand_Response
                           (Is_Error => False);
   end On_Execute_Command_Request;

   ----------------------------------
   -- On_Workspace_Symbols_Request --
   ----------------------------------

   overriding function On_Workspace_Symbols_Request
     (Self    : access Property_Error_Decorator;
      Request : LSP.Messages.Server_Requests.Workspace_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response
   is
   begin
      return Self.Handler.On_Workspace_Symbols_Request (Request);
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Trace.Trace ("Uncatched Property_Error");
         Self.Trace.Trace (E);
         return Result : LSP.Messages.Server_Responses.Symbol_Response
                           (Is_Error => False);
   end On_Workspace_Symbols_Request;

   ------------------------------------------
   -- On_Workspace_Execute_Command_Request --
   ------------------------------------------

   overriding function On_Workspace_Execute_Command_Request
     (Self    : access Property_Error_Decorator;
      Request : LSP.Messages.Server_Requests.Workspace_Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response
   is
   begin
      return Self.Handler.On_Workspace_Execute_Command_Request (Request);
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Trace.Trace ("Uncatched Property_Error");
         Self.Trace.Trace (E);
         return Result : LSP.Messages.Server_Responses.ExecuteCommand_Response
                           (Is_Error => False);
   end On_Workspace_Execute_Command_Request;

   ------------------------------
   -- On_ALS_Called_By_Request --
   ------------------------------

   overriding function On_ALS_Called_By_Request
     (Self    : access Property_Error_Decorator;
      Request : LSP.Messages.Server_Requests.ALS_Called_By_Request)
      return LSP.Messages.Server_Responses.ALS_Called_By_Response
   is
   begin
      return Self.Handler.On_ALS_Called_By_Request (Request);
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Trace.Trace ("Uncatched Property_Error");
         Self.Trace.Trace (E);
         return Result : LSP.Messages.Server_Responses.ALS_Called_By_Response
                           (Is_Error => False);
   end On_ALS_Called_By_Request;

   --------------------------
   -- On_ALS_Debug_Request --
   --------------------------

   overriding function On_ALS_Debug_Request
     (Self    : access Property_Error_Decorator;
      Request : LSP.Messages.Server_Requests.ALS_Debug_Request)
      return LSP.Messages.Server_Responses.ALS_Debug_Response
   is
   begin
      --  We don't expect Property_Error here
      return Self.Handler.On_ALS_Debug_Request (Request);
   end On_ALS_Debug_Request;

end LSP.Property_Error_Decorators;

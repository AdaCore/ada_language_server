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

with Ada.Strings.UTF_Encoding;

package body LSP.Servers.Handlers is

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
      return LSP.Types.LSP_String renames
         LSP.Types.To_LSP_String;

   ----------------------------
   -- DidChangeConfiguration --
   ----------------------------

   procedure DidChangeConfiguration
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null
        LSP.Notification_Handlers.Notification_Handler_Access)
   is
      Params : LSP.Messages.DidChangeConfigurationParams;
   begin
      LSP.Messages.DidChangeConfigurationParams'Read (Stream, Params);

      Handler.Workspace_Did_Change_Configuration (Params);
   end DidChangeConfiguration;

   ---------------------------
   -- DidChangeTextDocument --
   ---------------------------

   procedure DidChangeTextDocument
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null
        LSP.Notification_Handlers.Notification_Handler_Access)
   is
      Params : LSP.Messages.DidChangeTextDocumentParams;
   begin
      LSP.Messages.DidChangeTextDocumentParams'Read (Stream, Params);

      Handler.Text_Document_Did_Change (Params);
   end DidChangeTextDocument;

   --------------------------
   -- DidCloseTextDocument --
   --------------------------

   procedure DidCloseTextDocument
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null
        LSP.Notification_Handlers.Notification_Handler_Access)
   is
      Params : LSP.Messages.DidCloseTextDocumentParams;
   begin
      LSP.Messages.DidCloseTextDocumentParams'Read (Stream, Params);

      Handler.Text_Document_Did_Close (Params);
   end DidCloseTextDocument;

   -------------------------
   -- DidOpenTextDocument --
   -------------------------

   procedure DidOpenTextDocument
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null
        LSP.Notification_Handlers.Notification_Handler_Access)
   is
      Params : LSP.Messages.DidOpenTextDocumentParams;
   begin
      LSP.Messages.DidOpenTextDocumentParams'Read (Stream, Params);

      Handler.Text_Document_Did_Open (Params);
   end DidOpenTextDocument;

   -------------------------
   -- DidSaveTextDocument --
   -------------------------

   procedure DidSaveTextDocument
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null
        LSP.Notification_Handlers.Notification_Handler_Access)
   is
      Params : LSP.Messages.DidSaveTextDocumentParams;
   begin
      LSP.Messages.DidSaveTextDocumentParams'Read (Stream, Params);

      Handler.Text_Document_Did_Save (Params);
   end DidSaveTextDocument;

   --------------------
   -- Do_Code_Action --
   --------------------

   function Do_Code_Action
    (Stream  : access Ada.Streams.Root_Stream_Type'Class;
     Handler : not null LSP.Message_Handlers.Request_Handler_Access)
      return LSP.Messages.ResponseMessage'Class
   is
      Params   : LSP.Messages.CodeActionParams;
   begin
      LSP.Messages.CodeActionParams'Read (Stream, Params);

      return Response : LSP.Messages.CodeAction_Response do

         Handler.Text_Document_Code_Action_Request
           (Response => Response,
            Value    => Params);
      end return;
   end Do_Code_Action;

   -------------------
   -- Do_Completion --
   -------------------

   function Do_Completion
    (Stream  : access Ada.Streams.Root_Stream_Type'Class;
     Handler : not null LSP.Message_Handlers.Request_Handler_Access)
      return LSP.Messages.ResponseMessage'Class
   is
      Params   : LSP.Messages.TextDocumentPositionParams;
   begin
      LSP.Messages.TextDocumentPositionParams'Read (Stream, Params);

      return Response : LSP.Messages.Completion_Response do

         Handler.Text_Document_Completion_Request
           (Response => Response,
            Value    => Params);
      end return;
   end Do_Completion;

   -------------------
   -- Do_Definition --
   -------------------

   function Do_Definition
    (Stream  : access Ada.Streams.Root_Stream_Type'Class;
     Handler : not null LSP.Message_Handlers.Request_Handler_Access)
      return LSP.Messages.ResponseMessage'Class
   is
      Params   : LSP.Messages.TextDocumentPositionParams;
   begin
      LSP.Messages.TextDocumentPositionParams'Read (Stream, Params);

      return Response : LSP.Messages.Location_Response do

         Handler.Text_Document_Definition_Request
           (Response => Response,
            Value    => Params);
      end return;
   end Do_Definition;

   ------------------------
   -- Do_Document_Symbol --
   ------------------------

   function Do_Document_Symbol
    (Stream  : access Ada.Streams.Root_Stream_Type'Class;
     Handler : not null LSP.Message_Handlers.Request_Handler_Access)
      return LSP.Messages.ResponseMessage'Class
   is
      Params   : LSP.Messages.DocumentSymbolParams;
   begin
      LSP.Messages.DocumentSymbolParams'Read (Stream, Params);

      return Response : LSP.Messages.Symbol_Response do

         Handler.Text_Document_Symbol_Request
           (Response => Response,
            Value    => Params);
      end return;
   end Do_Document_Symbol;

   ------------------------
   -- Do_Execute_Command --
   ------------------------

   function Do_Execute_Command
    (Stream  : access Ada.Streams.Root_Stream_Type'Class;
     Handler : not null LSP.Message_Handlers.Request_Handler_Access)
      return LSP.Messages.ResponseMessage'Class
   is
      Params   : LSP.Messages.ExecuteCommandParams;
   begin
      LSP.Messages.ExecuteCommandParams'Read (Stream, Params);

      return Response : LSP.Messages.ExecuteCommand_Response do

         Handler.Workspace_Execute_Command_Request
           (Response => Response,
            Value    => Params);
      end return;
   end Do_Execute_Command;

   -------------
   -- Do_Exit --
   -------------

   procedure Do_Exit
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null
        LSP.Notification_Handlers.Notification_Handler_Access)
   is
      pragma Unreferenced (Stream);
   begin
      Handler.Exit_Notification;
   end Do_Exit;

   ------------------
   -- Do_Highlight --
   ------------------

   function Do_Highlight
    (Stream  : access Ada.Streams.Root_Stream_Type'Class;
     Handler : not null LSP.Message_Handlers.Request_Handler_Access)
      return LSP.Messages.ResponseMessage'Class
   is
      Params   : LSP.Messages.TextDocumentPositionParams;
   begin
      LSP.Messages.TextDocumentPositionParams'Read (Stream, Params);

      return Response : LSP.Messages.Highlight_Response do

         Handler.Text_Document_Highlight_Request
           (Response => Response,
            Value    => Params);
      end return;
   end Do_Highlight;

   --------------
   -- Do_Hover --
   --------------

   function Do_Hover
    (Stream  : access Ada.Streams.Root_Stream_Type'Class;
     Handler : not null LSP.Message_Handlers.Request_Handler_Access)
      return LSP.Messages.ResponseMessage'Class
   is
      Params   : LSP.Messages.TextDocumentPositionParams;
   begin
      LSP.Messages.TextDocumentPositionParams'Read (Stream, Params);

      return Response : LSP.Messages.Hover_Response do

         Handler.Text_Document_Hover_Request
           (Response => Response,
            Value    => Params);
      end return;
   end Do_Hover;

   -------------------
   -- Do_Initialize --
   -------------------

   function Do_Initialize
    (Stream  : access Ada.Streams.Root_Stream_Type'Class;
     Handler : not null LSP.Message_Handlers.Request_Handler_Access)
       return LSP.Messages.ResponseMessage'Class
   is
      Params   : LSP.Messages.InitializeParams;
   begin
      LSP.Messages.InitializeParams'Read (Stream, Params);

      return Response : LSP.Messages.Initialize_Response do

         Handler.Initialize_Request
           (Response => Response,
            Value    => Params);
      end return;
   end Do_Initialize;

   ------------------
   -- Do_Not_Found --
   ------------------

   function Do_Not_Found
    (Stream  : access Ada.Streams.Root_Stream_Type'Class;
     Handler : not null LSP.Message_Handlers.Request_Handler_Access)
       return LSP.Messages.ResponseMessage'Class
   is
      pragma Unreferenced (Stream, Handler);

   begin
      return Response : LSP.Messages.ResponseMessage do
         Response.error :=
           (Is_Set => True,
            Value  => (code    => LSP.Messages.MethodNotFound,
                       message => +"No such method",
                       others  => <>));
      end return;
   end Do_Not_Found;

   -------------------
   -- Do_References --
   -------------------

   function Do_References
    (Stream  : access Ada.Streams.Root_Stream_Type'Class;
     Handler : not null LSP.Message_Handlers.Request_Handler_Access)
      return LSP.Messages.ResponseMessage'Class
   is
      Params : LSP.Messages.ReferenceParams;
   begin
      LSP.Messages.ReferenceParams'Read (Stream, Params);

      return Response : LSP.Messages.Location_Response do

         Handler.Text_Document_References_Request
           (Response => Response,
            Value    => Params);
      end return;
   end Do_References;

   -----------------
   -- Do_Shutdown --
   -----------------

   function Do_Shutdown
    (Stream  : access Ada.Streams.Root_Stream_Type'Class;
     Handler : not null LSP.Message_Handlers.Request_Handler_Access)
      return LSP.Messages.ResponseMessage'Class
   is
      pragma Unreferenced (Stream);
   begin
      return Response : LSP.Messages.ResponseMessage do

         Handler.Shutdown_Request (Response);
      end return;
   end Do_Shutdown;

   -----------------------
   -- Do_Signature_Help --
   -----------------------

   function Do_Signature_Help
    (Stream  : access Ada.Streams.Root_Stream_Type'Class;
     Handler : not null LSP.Message_Handlers.Request_Handler_Access)
      return LSP.Messages.ResponseMessage'Class
   is
      Params   : LSP.Messages.TextDocumentPositionParams;
   begin
      LSP.Messages.TextDocumentPositionParams'Read (Stream, Params);

      return Response : LSP.Messages.SignatureHelp_Response do

         Handler.Text_Document_Signature_Help_Request
           (Response => Response,
            Value    => Params);
      end return;
   end Do_Signature_Help;

   -------------------------
   -- Do_Workspace_Symbol --
   -------------------------

   function Do_Workspace_Symbol
    (Stream  : access Ada.Streams.Root_Stream_Type'Class;
     Handler : not null LSP.Message_Handlers.Request_Handler_Access)
      return LSP.Messages.ResponseMessage'Class
   is
      Params   : LSP.Messages.WorkspaceSymbolParams;
   begin
      LSP.Messages.WorkspaceSymbolParams'Read (Stream, Params);

      return Response : LSP.Messages.Symbol_Response do

         Handler.Workspace_Symbol_Request
           (Response => Response,
            Value    => Params);
      end return;
   end Do_Workspace_Symbol;

   -------------------------
   -- Ignore_Notification --
   -------------------------

   procedure Ignore_Notification
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null
        LSP.Notification_Handlers.Notification_Handler_Access) is
   begin
      null;
   end Ignore_Notification;

end LSP.Servers.Handlers;

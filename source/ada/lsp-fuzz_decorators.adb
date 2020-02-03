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

with Ada.Containers.Hashed_Maps;
with LSP.Types; use LSP.Types;

package body LSP.Fuzz_Decorators is

   package Document_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => LSP.Messages.DocumentUri,
      Element_Type    => LSP.Types.LSP_String,
      Hash            => LSP.Types.Hash,
      Equivalent_Keys => LSP.Types."=");

   Open_Docs : Document_Maps.Map;
   --  Container for documents indexed by URI
   --  Global variables are acceptable in this package.

   ---------------------------------
   -- On_Initialized_Notification --
   ---------------------------------

   overriding procedure On_Initialized_Notification
     (Self : access Fuzz_Notification_Decorator)
   is
   begin
      Self.Handler.On_Initialized_Notification;
   end On_Initialized_Notification;

   --------------------------
   -- On_Exit_Notification --
   --------------------------

   overriding procedure On_Exit_Notification
     (Self : access Fuzz_Notification_Decorator)
   is
   begin
      Self.Handler.On_Exit_Notification;
   end On_Exit_Notification;

   --------------------------------------------
   -- On_DidChangeConfiguration_Notification --
   --------------------------------------------

   overriding procedure On_DidChangeConfiguration_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DidChangeConfigurationParams)
   is
   begin
      Self.Handler.On_DidChangeConfiguration_Notification (Value);
   end On_DidChangeConfiguration_Notification;

   -----------------------------------------------
   -- On_DidChangeWorkspaceFolders_Notification --
   -----------------------------------------------

   overriding procedure On_DidChangeWorkspaceFolders_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DidChangeWorkspaceFoldersParams)
   is
   begin
      Self.Handler.On_DidChangeWorkspaceFolders_Notification (Value);
   end On_DidChangeWorkspaceFolders_Notification;

   ----------------------------
   -- On_Cancel_Notification --
   ----------------------------

   overriding procedure On_Cancel_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.CancelParams)
   is
   begin
      Self.Handler.On_Cancel_Notification (Value);
   end On_Cancel_Notification;

   -----------------------------------------
   -- On_DidOpenTextDocument_Notification --
   -----------------------------------------

   overriding procedure On_DidOpenTextDocument_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DidOpenTextDocumentParams)
   is
   begin
      Open_Docs.Insert (Value.textDocument.uri, Value.textDocument.text);
      --  This will raise Constraint_Error if the doc is already open

      Self.Handler.On_DidOpenTextDocument_Notification (Value);
   end On_DidOpenTextDocument_Notification;

   -------------------------------------------
   -- On_DidChangeTextDocument_Notification --
   -------------------------------------------

   overriding procedure On_DidChangeTextDocument_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DidChangeTextDocumentParams)
   is
   begin
      Self.Handler.On_DidChangeTextDocument_Notification (Value);
   end On_DidChangeTextDocument_Notification;

   -----------------------------------------
   -- On_DidSaveTextDocument_Notification --
   -----------------------------------------

   overriding procedure On_DidSaveTextDocument_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DidSaveTextDocumentParams)
   is
   begin
      if not Open_Docs.Contains (Value.textDocument.uri) then
         raise Program_Error with
           "got 'didSaveTextDocument' but document not open";
      end if;

      Self.Handler.On_DidSaveTextDocument_Notification (Value);
   end On_DidSaveTextDocument_Notification;

   ------------------------------------------
   -- On_DidCloseTextDocument_Notification --
   ------------------------------------------

   overriding procedure On_DidCloseTextDocument_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DidCloseTextDocumentParams)
   is
   begin
      Open_Docs.Delete (Value.textDocument.uri);
      --  This will raise Constraint_Error if the doc is not open

      Self.Handler.On_DidCloseTextDocument_Notification (Value);
   end On_DidCloseTextDocument_Notification;

end LSP.Fuzz_Decorators;

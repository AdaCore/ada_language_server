------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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

with Ada.Characters.Wide_Latin_1; use Ada.Characters.Wide_Latin_1;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Wide_Unbounded;  use Ada.Strings.Wide_Unbounded;
with Ada.Strings.UTF_Encoding.Conversions;

with VSS.String_Vectors;
with VSS.Strings.Conversions;
with VSS.Unicode;

with LSP.Types;                   use LSP.Types;

package body LSP.Fuzz_Decorators is

   package Document_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => LSP.Messages.DocumentUri,
      Element_Type    => VSS.Strings.Virtual_String,
      Hash            => LSP.Types.Hash,
      Equivalent_Keys => LSP.Types.Equal,
      "="             => VSS.Strings."=");

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

   -------------------------------------------
   -- On_DidChangeWatchedFiles_Notification --
   -------------------------------------------

   overriding procedure On_DidChangeWatchedFiles_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DidChangeWatchedFilesParams)
   is
   begin
      Self.Handler.On_DidChangeWatchedFiles_Notification (Value);
   end On_DidChangeWatchedFiles_Notification;

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

   ------------------------------------
   -- On_DidCreateFiles_Notification --
   ------------------------------------

   overriding procedure On_DidCreateFiles_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.CreateFilesParams) is
   begin
      Self.Handler.On_DidCreateFiles_Notification (Value);
   end On_DidCreateFiles_Notification;

   ------------------------------------
   -- On_DidRenameFiles_Notification --
   ------------------------------------

   overriding procedure On_DidRenameFiles_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.RenameFilesParams) is
   begin
      Self.Handler.On_DidRenameFiles_Notification (Value);
   end On_DidRenameFiles_Notification;

   ------------------------------------
   -- On_DidDeleteFiles_Notification --
   ------------------------------------

   overriding procedure On_DidDeleteFiles_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DeleteFilesParams) is
   begin
      Self.Handler.On_DidDeleteFiles_Notification (Value);
   end On_DidDeleteFiles_Notification;

   -----------------------------------------
   -- On_DidOpenTextDocument_Notification --
   -----------------------------------------

   overriding procedure On_DidOpenTextDocument_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DidOpenTextDocumentParams) is
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
      use type VSS.Strings.Virtual_String;
      use type VSS.Unicode.UTF16_Code_Unit_Count;

      function To_Unbounded_UTF_16_String
        (Item : VSS.Strings.Virtual_String) return Unbounded_Wide_String;

      function To_Virtual_String
        (Item : Unbounded_Wide_String) return VSS.Strings.Virtual_String;

      --------------------------------
      -- To_Unbounded_UTF_16_String --
      --------------------------------

      function To_Unbounded_UTF_16_String
        (Item : VSS.Strings.Virtual_String) return Unbounded_Wide_String is
           (To_Unbounded_Wide_String
              (Ada.Strings.UTF_Encoding.Conversions.Convert
                 (VSS.Strings.Conversions.To_UTF_8_String (Item))));

      -----------------------
      -- To_Virtual_String --
      -----------------------

      function To_Virtual_String
        (Item : Unbounded_Wide_String) return VSS.Strings.Virtual_String is
          (VSS.Strings.Conversions.To_Virtual_String
             (Ada.Strings.UTF_Encoding.Conversions.Convert
                (To_Wide_String (Item))));

      Doc_Content : Unbounded_Wide_String;

   begin
      Doc_Content :=
        To_Unbounded_UTF_16_String (Open_Docs (Value.textDocument.uri));

      for Change of Value.contentChanges loop
         if Change.span.Is_Set then
            --  Basic implementation of applying a text change. This is slow
            --  but the goal is to compare results with the "smarter"
            --  actual implementation.
            declare
               Line               : Integer := -1;
               Start_Ind, End_Ind : UTF_16_Index;
            begin
               for Ind in 0 .. UTF_16_Index (Length (Doc_Content)) loop
                  if Ind = 0
                    or else Element (Doc_Content, Natural (Ind)) = LF
                  then
                     Line := Line + 1;
                     if Line = Integer (Change.span.Value.first.line) then
                        Start_Ind := Ind + Change.span.Value.first.character;
                     end if;
                     if Line = Integer (Change.span.Value.last.line) then
                        End_Ind := Ind + Change.span.Value.last.character;
                        exit;
                     end if;
                  end if;
               end loop;
               Doc_Content := Unbounded_Slice
                 (Doc_Content, 1, Natural (Start_Ind))
                 & To_Unbounded_UTF_16_String (Change.text)
                 & Unbounded_Slice
                 (Doc_Content, Natural (End_Ind + 1), Length (Doc_Content));
            end;
         else
            Doc_Content := To_Unbounded_UTF_16_String (Change.text);
         end if;
      end loop;

      Open_Docs.Replace
        (Value.textDocument.uri, To_Virtual_String (Doc_Content));

      --  Let the real handler update the document
      Self.Handler.On_DidChangeTextDocument_Notification (Value);

      --  Compare the results of the basic implementation and the real one
      if Self.Doc_Provider.Get_Open_Document (Value.textDocument.uri).Text
        /= To_Virtual_String (Doc_Content)
      then
         declare
            Vector : VSS.String_Vectors.Virtual_String_Vector;
         begin
            Vector.Append
              (Self.Doc_Provider.Get_Open_Document (Value.textDocument.uri)
                 .Text);

            Vector.Append (" /= ");
            Vector.Append (To_Virtual_String (Doc_Content));

            Self.Trace.Trace
              (VSS.Strings.Conversions.To_UTF_8_String
                 (Vector.Join_Lines (VSS.Strings.LF)));

            raise Program_Error with "document content inconsistency";
         end;
      end if;
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

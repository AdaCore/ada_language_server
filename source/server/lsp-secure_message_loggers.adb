------------------------------------------------------------------------------
--                         Language Client Protocol                         --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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

with Ada.Containers;

with VSS.Strings;

package body LSP.Secure_Message_Loggers is

   ----------------------------
   -- On_Client_Notification --
   ----------------------------

   overriding procedure On_Client_Notification
     (Self  : in out Client_Logger;
      Value : LSP.Client_Notifications.Client_Notification'Class) is
   begin
      Value.Visit_Client_Receiver (Self.Notification);
   end On_Client_Notification;

   -----------------------
   -- On_Client_Request --
   -----------------------

   overriding procedure On_Client_Request
     (Self  : in out Client_Logger;
      Value : LSP.Client_Requests.Client_Request'Class) is
   begin
      Value.Visit_Client_Receiver (Self.Request);
   end On_Client_Request;

   ------------------------
   -- On_Client_Response --
   ------------------------

   overriding procedure On_Client_Response
     (Self  : in out Client_Logger;
      Value : LSP.Client_Responses.Client_Response'Class) is
   begin
      Value.Visit_Client_Receiver (Self.Response);
   end On_Client_Response;

   ----------------------------
   -- On_Server_Notification --
   ----------------------------

   overriding procedure On_Server_Notification
     (Self  : in out Server_Logger;
      Value : LSP.Server_Notifications.Server_Notification'Class) is
   begin
      Value.Visit_Server_Receiver (Self.Notification);
   end On_Server_Notification;

   -----------------------
   -- On_Server_Request --
   -----------------------

   overriding procedure On_Server_Request
     (Self  : in out Server_Logger;
      Value : LSP.Server_Requests.Server_Request'Class) is
   begin
      Value.Visit_Server_Receiver (Self.Request);
   end On_Server_Request;

   ------------------------
   -- On_Server_Response --
   ------------------------

   overriding procedure On_Server_Response
     (Self  : in out Server_Logger;
      Value : LSP.Server_Responses.Server_Response'Class) is
   begin
      Value.Visit_Server_Receiver (Self.Response);
   end On_Server_Response;

   ----------------------------
   -- On_Completion_Response --
   ----------------------------

   overriding procedure On_Completion_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Completion_Result)
   is
      Ok : Boolean := True;
   begin
      --  Hide response, because it could be lengthly and expose names
      Self.Output.Put ("'textDocument/completion'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);

      Self.Output.Put
        (VSS.Strings.To_Virtual_String
           (case Value.Kind is
               when LSP.Structures.Variant_1 =>
                 "CompletionItem_Vector len="
                   & Value.Variant_1.Length'Wide_Wide_Image,
               when LSP.Structures.Variant_2 =>
                 "CompletionList len="
                   & Value.Variant_2.items.Length'Wide_Wide_Image
                   & " isIncomplete="
                   & Value.Variant_2.isIncomplete'Wide_Wide_Image,
               when LSP.Structures.Variant_3 =>
                 "Null_Record"),
         Ok);

      Self.Output.New_Line (Ok);
   end On_Completion_Response;

   --------------------------------
   -- On_DocumentSymbol_Response --
   --------------------------------

   overriding procedure On_DocumentSymbol_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentSymbol_Result)
   is
      Ok : Boolean := True;
   begin
      --  Hide response, because it could be lengthly and expose names
      Self.Output.Put ("'textDocument/documentSymbol'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);

      Self.Output.Put
        (VSS.Strings.To_Virtual_String
           (case Value.Kind is
               when LSP.Structures.Variant_1 =>
                 "SymbolInformation_Vector len="
                   & Value.Variant_1.Length'Wide_Wide_Image,
               when LSP.Structures.Variant_2 =>
                 "DocumentSymbol_Vector len="
                   & Value.Variant_2.Length'Wide_Wide_Image,
               when LSP.Structures.Variant_3 =>
                 "Null_Record"),
         Ok);

      Self.Output.New_Line (Ok);
   end On_DocumentSymbol_Response;

   ------------------------------
   -- On_FoldingRange_Response --
   ------------------------------

   overriding procedure On_FoldingRange_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.FoldingRange_Vector_Or_Null)
   is
      Ok : Boolean := True;
   begin
      --  Hide response, because it could be lengthly
      Self.Output.Put ("'textDocument/foldingRange'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : len=", Ok);

      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value.Length'Wide_Wide_Image), Ok);

      Self.Output.New_Line (Ok);
   end On_FoldingRange_Response;

   ----------------------------
   -- On_References_Response --
   ----------------------------

   overriding procedure On_References_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Location_Vector_Or_Null)
   is
      Ok : Boolean := True;
   begin
      --  Hide response, because it could be lengthly and expose names
      Self.Output.Put ("'textDocument/references'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : len=", Ok);

      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value.Length'Wide_Wide_Image), Ok);

      Self.Output.New_Line (Ok);
   end On_References_Response;

   -----------------------------
   -- On_Tokens_Full_Response --
   -----------------------------

   overriding procedure On_Tokens_Full_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokens_Or_Null)
   is
      Ok : Boolean := True;
   begin
      --  Hide response, because it could be lengthly
      Self.Output.Put ("'textDocument/semanticTokens/full'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);

      Self.Output.Put
        (VSS.Strings.To_Virtual_String
           (case Value.Is_Null is
               when True  => "null",
               when False => "len="
                 & Value.Value.data.Length'Wide_Wide_Image), Ok);

      Self.Output.New_Line (Ok);
   end On_Tokens_Full_Response;

   ------------------------
   -- On_Symbol_Response --
   ------------------------

   overriding procedure On_Symbol_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Symbol_Result)
   is
      Ok : Boolean := True;
   begin
      --  Hide response, because it could be lengthly and expose names
      Self.Output.Put ("'workspace/symbol'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);

      Self.Output.Put
        (VSS.Strings.To_Virtual_String
           (case Value.Kind is
               when LSP.Structures.Variant_1 =>
                 "SymbolInformation_Vector len="
                   & Value.Variant_1.Length'Wide_Wide_Image,
               when LSP.Structures.Variant_2 =>
                 "WorkspaceSymbol_Vector len="
                   & Value.Variant_2.Length'Wide_Wide_Image,
               when LSP.Structures.Variant_3 =>
                 "Null_Record"),
         Ok);

      Self.Output.New_Line (Ok);
   end On_Symbol_Response;

   ----------------------------
   -- On_CodeAction_Response --
   ----------------------------

   overriding procedure On_CodeAction_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Command_Or_CodeAction_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/codeAction'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value.Length'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_CodeAction_Response;

   -------------------------------
   -- On_DidChange_Notification --
   -------------------------------

   overriding procedure On_DidChange_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.DidChangeTextDocumentParams)
   is
      use type Ada.Containers.Count_Type;
      use type VSS.Strings.Character_Count;

      Ok : Boolean := True;

      Content : constant LSP.Structures.TextDocumentContentChangeEvent :=
        (if Value.contentChanges.Length = 1
         then Value.contentChanges.First_Element
         else (text => VSS.Strings.Empty_Virtual_String,
               others => <>));
   begin
      --  Hide notification, because it could expose code
      Self.Output.Put ("'textDocument/didChange'", Ok);
      Self.Output.Put (" Params : textDocument=", Ok);

      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value.textDocument'Wide_Wide_Image),
         Ok);

      Self.Output.Put (" contentChanges=", Ok);

      --  If contentChanges has a single item with full text, hide it
      if Value.contentChanges.Length = 1
        and then not Content.a_range.Is_Set
        and then not Content.rangeLength.Is_Set
      then
         Self.Output.Put ("<some text>", Ok);
      elsif (for some Change of Value.contentChanges =>
              Change.text.Character_Length > 80)
        or else Value.contentChanges.Last_Index > 20
      then
         --  Don't dump very long changes to avoid stack overflow
         Self.Output.Put ("<some big change>", Ok);
      else
         Self.Output.Put
           (VSS.Strings.To_Virtual_String
              (Value.contentChanges'Wide_Wide_Image),
            Ok);
      end if;

      Self.Output.New_Line (Ok);
   end On_DidChange_Notification;

   -----------------------------
   -- On_DidOpen_Notification --
   -----------------------------

   overriding procedure On_DidOpen_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.DidOpenTextDocumentParams)
   is
      Ok : Boolean := True;
      Item : LSP.Structures.TextDocumentItem := Value.textDocument;
   begin
      --  Hide notification, because it could expose code
      Self.Output.Put ("'textDocument/didOpen'", Ok);
      Self.Output.Put (" Params : ", Ok);

      Item.text := "<some text>";

      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Item'Wide_Wide_Image), Ok);

      Self.Output.New_Line (Ok);
   end On_DidOpen_Notification;

end LSP.Secure_Message_Loggers;

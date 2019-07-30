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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;

with GNATCOLL.JSON;

package body LSP.Messages is

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
      return LSP.Types.LSP_String renames
       LSP.Types.To_LSP_String;

   procedure Read_If_String
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out LSP.Types.LSP_String);

   procedure Read_MessageType
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out MessageType);

   procedure Read_Boolean
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out Boolean);

   procedure Write_Boolean
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : Boolean);

   procedure Read_Optional_Boolean
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out LSP.Types.Optional_Boolean);

   procedure Read_Optional_Number
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out LSP.Types.Optional_Number);

   procedure Read_Number
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out LSP.Types.LSP_Number);

   procedure Write_MessageType
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : MessageType);

   procedure Write_Optional_Boolean
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : LSP.Types.Optional_Boolean);

   procedure Write_Optional_Number
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : LSP.Types.Optional_Number);

   procedure Write_Optional_String
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : LSP.Types.Optional_String);

   procedure Read_String_Vector
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out LSP.Types.LSP_String_Vector);

   procedure Write_String_Vector
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : LSP.Types.LSP_String_Vector);

   Error_Map : constant array (ErrorCodes) of Long_Integer
     :=
     (ParseError           => -32700,
      InvalidRequest       => -32600,
      MethodNotFound       => -32601,
      InvalidParams        => -32602,
      InternalError        => -32603,
      serverErrorStart     => -32099,
      serverErrorEnd       => -32000,
      ServerNotInitialized => -32002,
      UnknownErrorCode     => -32001,
      RequestCancelled     => -32800);

   AlsReferenceKind_Map : constant array
     (AlsReferenceKind) of Standard.String (1 .. 1) :=
     (Write            => "w",
      Static_Call      => "c",
      Dispatching_Call => "d");

   -------------------------------
   -- Read_AlsReferenceKind_Set --
   -------------------------------

   procedure Read_AlsReferenceKind_Set
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out AlsReferenceKind_Set)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Value  : constant GNATCOLL.JSON.JSON_Value := JS.Read;
      Vector : GNATCOLL.JSON.JSON_Array;
   begin
      V := Empty_Set;

      if Value.Kind in GNATCOLL.JSON.JSON_Array_Type then
         Vector := Value.Get;

         for J in 1 .. GNATCOLL.JSON.Length (Vector) loop
            if Value.Kind in GNATCOLL.JSON.JSON_String_Type then
               declare
                  Text : constant Standard.String :=
                    GNATCOLL.JSON.Get (Vector, J).Get;
               begin
                  for J in AlsReferenceKind_Map'Range loop
                     if Text = AlsReferenceKind_Map (J) then
                        V (J) := True;
                     end if;
                  end loop;
               end;
            end if;
         end loop;
      end if;
   end Read_AlsReferenceKind_Set;

   --------------------------------
   -- Write_AlsReferenceKind_Set --
   --------------------------------

   procedure Write_AlsReferenceKind_Set
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : AlsReferenceKind_Set)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V /= Empty_Set then
         JS.Start_Array;

         for J in V'Range loop
            if V (J) then
               JS.Write (GNATCOLL.JSON.Create (AlsReferenceKind_Map (J)));
            end if;
         end loop;

         JS.End_Array;
      end if;
   end Write_AlsReferenceKind_Set;

   -----------------------------------
   -- Read_ApplyWorkspaceEditParams --
   -----------------------------------

   procedure Read_ApplyWorkspaceEditParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ApplyWorkspaceEditParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("edit");
      WorkspaceEdit'Read (S, V.edit);
      JS.End_Object;
   end Read_ApplyWorkspaceEditParams;

   -----------------------------------
   -- Read_ApplyWorkspaceEditResult --
   -----------------------------------

   procedure Read_ApplyWorkspaceEditResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ApplyWorkspaceEditResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Boolean (JS, +"applied", V.applied);
      JS.End_Object;
   end Read_ApplyWorkspaceEditResult;

   ------------------
   -- Read_Boolean --
   ------------------

   procedure Read_Boolean
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out Boolean)
   is
      Value : GNATCOLL.JSON.JSON_Value;
   begin
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
      Value := Stream.Read;

      if Value.Kind in GNATCOLL.JSON.JSON_Null_Type then
         Item := False;  --  No such property
      elsif Value.Kind in GNATCOLL.JSON.JSON_Boolean_Type then
         Item := Value.Get;  --  Property of a boolean type
      else
         Item := True;  --  Property of non-boolean type, protocol extension
         --  could provide an object instead of boolean.
      end if;
   end Read_Boolean;

   -----------------------------
   -- Read_ClientCapabilities --
   -----------------------------

   procedure Read_ClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workspace");
      WorkspaceClientCapabilities'Read (S, V.workspace);
      JS.Key ("textDocument");
      TextDocumentClientCapabilities'Read (S, V.textDocument);
      JS.End_Object;
   end Read_ClientCapabilities;

   ----------------------------
   -- Read_CodeActionContext --
   ----------------------------

   procedure Read_CodeActionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("diagnostics");
      Diagnostic_Vector'Read (S, V.diagnostics);
      JS.End_Object;
   end Read_CodeActionContext;

   ---------------------------
   -- Read_CodeActionParams --
   ---------------------------

   procedure Read_CodeActionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("range");
      Span'Read (S, V.span);
      JS.Key ("context");
      CodeActionContext'Read (S, V.context);
      JS.End_Object;
   end Read_CodeActionParams;

   --------------------------
   -- Read_CodeLensOptions --
   --------------------------

   procedure Read_CodeLensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeLensOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean (JS, +"resolveProvider", V.resolveProvider);
      JS.End_Object;
   end Read_CodeLensOptions;

   ------------------
   -- Read_Command --
   ------------------

   procedure Read_Command
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Command)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_String (JS, +"title", V.title);
      Read_String (JS, +"command", V.command);
      JS.Key ("arguments");
      V.arguments := JS.Read;
      JS.End_Object;
   end Read_Command;

   -------------------------
   -- Read_Command_Vector --
   -------------------------

   procedure Read_Command_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Command_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V.Clear;
      JS.Start_Array;

      while not JS.End_Of_Array loop
         declare
            Item : Command;
         begin
            Command'Read (S, Item);
            V.Append (Item);
         end;
      end loop;

      JS.End_Array;
   end Read_Command_Vector;

   ---------------------
   -- Read_completion --
   ---------------------

   procedure Read_completion
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out completion)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean
        (JS, +"dynamicRegistration", V.dynamicRegistration);
      Read_Optional_Boolean (JS, +"snippetSupport", V.snippetSupport);
      JS.End_Object;
   end Read_completion;

   --------------------------------
   -- Read_CompletionItem_Vector --
   --------------------------------

   procedure Read_CompletionItem_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItem_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V.Clear;
      JS.Start_Array;

      while not JS.End_Of_Array loop
         declare
            Item : CompletionItem;
         begin
            CompletionItem'Read (S, Item);
            V.Append (Item);
         end;
      end loop;

      JS.End_Array;
   end Read_CompletionItem_Vector;

   -------------------------
   -- Read_CompletionList --
   -------------------------

   procedure Read_CompletionList
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionList)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("isIncomplete");
      V.isIncomplete := JS.Read.Get;
      JS.Key ("items");
      CompletionItem_Vector'Read (S, V.items);
      JS.End_Object;
   end Read_CompletionList;

   -------------------------
   -- Read_CompletionItem --
   -------------------------

   procedure Read_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_String (JS, +"label", V.label);
      JS.Key ("kind");
      Optional_CompletionItemKind'Read (S, V.kind);
      Read_Optional_String (JS, +"detail", V.detail);
      Read_Optional_String (JS, +"documentation", V.documentation);
      Read_Optional_String (JS, +"sortText", V.sortText);
      Read_Optional_String (JS, +"filterText", V.filterText);
      Read_Optional_String (JS, +"insertText", V.insertText);
      JS.Key ("insertTextFormat");
      Optional_InsertTextFormat'Read (S, V.insertTextFormat);
      JS.Key ("textEdit");
      Optional_TextEdit'Read (S, V.textEdit);
      JS.Key ("additionalTextEdits");
      TextEdit_Vector'Read (S, V.additionalTextEdits);
      Read_String_Vector (JS, +"commitCharacters", V.commitCharacters);
      JS.Key ("command");
      Optional_Command'Read (S, V.command);
      JS.End_Object;
   end Read_CompletionItem;

   -----------------------------
   -- Read_CompletionItemKind --
   -----------------------------

   procedure Read_CompletionItemKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItemKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := CompletionItemKind'Val (JS.Read.Get - 1);
   end Read_CompletionItemKind;

   ----------------------------
   -- Read_CompletionOptions --
   ----------------------------

   procedure Read_CompletionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean (JS, +"resolveProvider", V.resolveProvider);
      Read_String_Vector (JS, +"triggerCharacters", V.triggerCharacters);
      JS.End_Object;
   end Read_CompletionOptions;

   ---------------------
   -- Read_Diagnostic --
   ---------------------

   procedure Read_Diagnostic
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Diagnostic)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      Span'Read (S, V.span);
      JS.Key ("severity");
      Optional_DiagnosticSeverity'Read (S, V.severity);
      LSP.Types.Read_Number_Or_String (JS, +"code", V.code);
      Read_Optional_String (JS, +"source", V.source);
      Read_String (JS, +"message", V.message);
      JS.End_Object;
   end Read_Diagnostic;

   ----------------------------
   -- Read_Diagnostic_Vector --
   ----------------------------

   procedure Read_Diagnostic_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Diagnostic_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V.Clear;
      JS.Start_Array;
      while not JS.End_Of_Array loop
         declare
            Item : Diagnostic;
         begin
            Diagnostic'Read (S, Item);
            V.Append (Item);
         end;
      end loop;
      JS.End_Array;
   end Read_Diagnostic_Vector;

   -----------------------------
   -- Read_DiagnosticSeverity --
   -----------------------------

   procedure Read_DiagnosticSeverity
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DiagnosticSeverity)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := DiagnosticSeverity'Val (JS.Read.Get - 1);
   end Read_DiagnosticSeverity;

   ---------------------------------------
   -- Read_DidChangeConfigurationParams --
   ---------------------------------------

   procedure Read_DidChangeConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeConfigurationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("settings");
      V.settings := JS.Read;
      JS.End_Object;
   end Read_DidChangeConfigurationParams;

   --------------------------------------
   -- Read_DidChangeTextDocumentParams --
   --------------------------------------

   procedure Read_DidChangeTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      VersionedTextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("contentChanges");
      TextDocumentContentChangeEvent_Vector'Read (S, V.contentChanges);
      JS.End_Object;
   end Read_DidChangeTextDocumentParams;

   -------------------------------------
   -- Read_DidCloseTextDocumentParams --
   -------------------------------------

   procedure Read_DidCloseTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidCloseTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.End_Object;
   end Read_DidCloseTextDocumentParams;

   ------------------------------------
   -- Read_DidOpenTextDocumentParams --
   ------------------------------------

   procedure Read_DidOpenTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidOpenTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentItem'Read (S, V.textDocument);
      JS.End_Object;
   end Read_DidOpenTextDocumentParams;

   ------------------------------------
   -- Read_DidSaveTextDocumentParams --
   ------------------------------------

   procedure Read_DidSaveTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidSaveTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      Read_Optional_String (JS, +"text", V.text);
      JS.End_Object;
   end Read_DidSaveTextDocumentParams;

   --------------------------
   -- Read_documentChanges --
   --------------------------

   procedure Read_documentChanges
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out documentChanges)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean
        (JS, +"documentChanges", Optional_Boolean (V));
      JS.End_Object;
   end Read_documentChanges;

   ----------------------------
   -- Read_DocumentHighlight --
   ----------------------------

   procedure Read_DocumentHighlight
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentHighlight)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      Span'Read (S, V.span);
      JS.Key ("kind");
      V.kind := DocumentHighlightKind'Val (JS.Read.Get - 1);
      JS.End_Object;
   end Read_DocumentHighlight;

   -----------------------------------
   -- Read_DocumentHighlight_Vector --
   -----------------------------------

   procedure Read_DocumentHighlight_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentHighlight_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V.Clear;
      JS.Start_Array;
      while not JS.End_Of_Array loop
         declare
            Item : DocumentHighlight;
         begin
            DocumentHighlight'Read (S, Item);
            V.Append (Item);
         end;
      end loop;
      JS.End_Array;
   end Read_DocumentHighlight_Vector;

   --------------------------------
   -- Read_DocumentHighlightKind --
   --------------------------------

   procedure Read_DocumentHighlightKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentHighlightKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Value : constant GNATCOLL.JSON.JSON_Value := JS.Read;

      Map : constant array (1 .. 3) of DocumentHighlightKind :=
        (1 => Text, 2 => Read, 3 => Write);
   begin
      V := Map (Value.Get);
   end Read_DocumentHighlightKind;

   ------------------------------
   -- Read_DocumentLinkOptions --
   ------------------------------

   procedure Read_DocumentLinkOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentLinkOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean (JS, +"resolveProvider", V.resolveProvider);
      JS.End_Object;
   end Read_DocumentLinkOptions;

   ------------------------------------------
   -- Read_DocumentOnTypeFormattingOptions --
   ------------------------------------------

   procedure Read_DocumentOnTypeFormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentOnTypeFormattingOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_String (JS, +"firstTriggerCharacter", V.firstTriggerCharacter);
      Read_String_Vector
        (JS, +"moreTriggerCharacter", V.moreTriggerCharacter);
      JS.End_Object;
   end Read_DocumentOnTypeFormattingOptions;

   -------------------------------
   -- Read_DocumentSymbolParams --
   -------------------------------

   procedure Read_DocumentSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentSymbolParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.End_Object;
   end Read_DocumentSymbolParams;

   ------------------------------
   -- Read_dynamicRegistration --
   ------------------------------

   procedure Read_dynamicRegistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out dynamicRegistration)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean
        (JS, +"dynamicRegistration", Optional_Boolean (V));
      JS.End_Object;
   end Read_dynamicRegistration;

   --------------------------------
   -- Read_ExecuteCommandOptions --
   --------------------------------

   procedure Read_ExecuteCommandOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ExecuteCommandOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_String_Vector (JS, +"commands", V.commands);
      JS.End_Object;
   end Read_ExecuteCommandOptions;

   -------------------------------
   -- Read_ExecuteCommandParams --
   -------------------------------

   procedure Read_ExecuteCommandParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ExecuteCommandParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_String (JS, +"command", V.command);
      JS.Key ("arguments");
      V.arguments := JS.Read;
      JS.End_Object;
   end Read_ExecuteCommandParams;

   ----------------
   -- Read_Hover --
   ----------------

   procedure Read_Hover
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Hover)
   is
      use type GNATCOLL.JSON.JSON_Value_Type;
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("contents");

      if JS.Read.Kind = GNATCOLL.JSON.JSON_Array_Type then
         MarkedString_Vector'Read (S, V.contents);
      else
         declare
            Item : MarkedString;
         begin
            MarkedString'Read (S, Item);
            V.contents.Clear;
            V.contents.Append (Item);
         end;
      end if;

      JS.Key ("range");
      Optional_Span'Read (S, V.Span);
      JS.End_Object;
   end Read_Hover;

   --------------------
   -- Read_If_String --
   --------------------

   procedure Read_If_String
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out LSP.Types.LSP_String)
   is
      Value : GNATCOLL.JSON.JSON_Value;
   begin
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
      Value := Stream.Read;

      if Value.Kind in GNATCOLL.JSON.JSON_Null_Type then
         Item := Empty_LSP_String;
      else
         --  Item := League.IRIs.From_Universal_String (Stream.Read.To_String);
         Item := To_LSP_String (Unbounded_String'(Stream.Read.Get));
      end if;
   end Read_If_String;

   ---------------------------
   -- Read_InitializeParams --
   ---------------------------

   procedure Read_InitializeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializeParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Trace : LSP.Types.Optional_String;
   begin
      JS.Start_Object;
      Read_Optional_Number (JS, +"processId", V.processId);
      JS.Key ("rootPath");
      Read_If_String (JS, +"rootPath", V.rootPath);
      Read_If_String (JS, +"rootUri", V.rootUri);
      JS.Key ("capabilities");
      LSP.Messages.ClientCapabilities'Read (S, V.capabilities);
      Read_Optional_String (JS, +"trace", Trace);

      if not Trace.Is_Set then
         V.trace := LSP.Types.Unspecified;
      elsif Trace.Value = +"off" then
         V.trace := LSP.Types.Off;
      elsif Trace.Value = +"messages" then
         V.trace := LSP.Types.Messages;
      elsif Trace.Value = +"verbose" then
         V.trace := LSP.Types.Verbose;
      end if;

      JS.End_Object;
   end Read_InitializeParams;

   ---------------------------
   -- Read_InitializeResult --
   ---------------------------

   procedure Read_InitializeResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializeResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("capabilities");
      ServerCapabilities'Read (S, V.capabilities);
      JS.End_Object;
   end Read_InitializeResult;

   ----------------------------
   -- Read_InitializedParams --
   ----------------------------

   not overriding procedure Read_InitializedParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializedParams)
   is
      pragma Unreferenced (V);

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.End_Object;
   end Read_InitializedParams;

   ---------------------------
   -- Read_InsertTextFormat --
   ---------------------------

   procedure Read_InsertTextFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InsertTextFormat)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := InsertTextFormat'Val (JS.Read.Get - 1);
   end Read_InsertTextFormat;

   -------------------
   -- Read_Location --
   -------------------

   procedure Read_Location
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Location)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      DocumentUri'Read (S, V.uri);
      JS.Key ("range");
      Span'Read (S, V.span);
      JS.Key ("alsKind");
      AlsReferenceKind_Set'Read (S, V.alsKind);
      JS.End_Object;
   end Read_Location;

   --------------------------
   -- Read_Location_Vector --
   --------------------------

   procedure Read_Location_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Location_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V.Clear;
      JS.Start_Array;

      while not JS.End_Of_Array loop
         declare
            Item : Location;
         begin
            Location'Read (S, Item);
            V.Append (Item);
         end;
      end loop;

      JS.End_Array;
   end Read_Location_Vector;

   ---------------------------
   -- Read_LogMessageParams --
   ---------------------------

   procedure Read_LogMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LogMessageParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_MessageType (JS, +"type", V.the_type);
      Read_String (JS, +"message", V.message);
      JS.End_Object;
   end Read_LogMessageParams;

   -----------------------
   -- Read_MarkedString --
   -----------------------

   procedure Read_MarkedString
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MarkedString)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Value : constant GNATCOLL.JSON.JSON_Value := JS.Read;
   begin
      case Value.Kind is
         when GNATCOLL.JSON.JSON_String_Type =>
            V := (Is_String => True,
                  Value => To_LSP_String (Unbounded_String'(Value.Get)));
         when GNATCOLL.JSON.JSON_Object_Type =>
            --  We can't use Start_Object/End_Object here because JS.Read
            --  call has already skipped the array item.
            V := (Is_String => False,
                  language => To_LSP_String
                    (Unbounded_String'(Value.Get ("language"))),
                  value    => To_LSP_String
                    (Unbounded_String'(Value.Get ("value"))));
         when others =>
            --  Unexpected JSON type
            V := (Is_String => True, Value => Empty_LSP_String);
      end case;
   end Read_MarkedString;

   ------------------------------
   -- Read_MarkedString_Vector --
   ------------------------------

   procedure Read_MarkedString_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MarkedString_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V.Clear;
      JS.Start_Array;
      while not JS.End_Of_Array loop
         declare
            Item : MarkedString;
         begin
            MarkedString'Read (S, Item);
            V.Append (Item);
         end;
      end loop;
      JS.End_Array;
   end Read_MarkedString_Vector;

   procedure Read_MessageType
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out MessageType)
   is
      Value : LSP.Types.LSP_Number;
   begin
      Read_Number (Stream, Key, Value);
      Item := MessageType'Val (Value - 1);
   end Read_MessageType;

   ------------------------------
   -- Read_NotificationMessage --
   ------------------------------

   procedure Read_NotificationMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out NotificationMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Notification_Prefix (S, V);
      JS.End_Object;
   end Read_NotificationMessage;

   ------------------------------
   -- Read_Notification_Prefix --
   ------------------------------

   procedure Read_Notification_Prefix
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.NotificationMessage'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      Read_String (JS, +"jsonrpc", V.jsonrpc);
      Read_String (JS, +"method", V.method);
   end Read_Notification_Prefix;

   -----------------
   -- Read_Number --
   -----------------

   procedure Read_Number
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out LSP.Types.LSP_Number) is
   begin
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
      Item := LSP.Types.LSP_Number (Integer'(Stream.Read.Get));
   end Read_Number;

   ---------------------------
   -- Read_Optional_Boolean --
   ---------------------------

   procedure Read_Optional_Boolean
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out LSP.Types.Optional_Boolean)
   is
      Value : GNATCOLL.JSON.JSON_Value;
   begin
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
      Value := Stream.Read;

      if Value.Kind in GNATCOLL.JSON.JSON_Null_Type then
         Item := (Is_Set => False);
      else
         Item := (Is_Set => True, Value => Value.Get);
      end if;
   end Read_Optional_Boolean;

   --------------------------
   -- Read_Optional_Number --
   --------------------------

   procedure Read_Optional_Number
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out LSP.Types.Optional_Number)
   is
      Value : GNATCOLL.JSON.JSON_Value;
   begin
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
      Value := Stream.Read;

      if Value.Kind in GNATCOLL.JSON.JSON_Null_Type then
         Item := (Is_Set => False);
      else
         Item := (Is_Set => True, Value => Integer'(Value.Get));
      end if;
   end Read_Optional_Number;

   -------------------------------------------
   -- Read_Optional_TextDocumentSyncOptions --
   -------------------------------------------

   procedure Read_Optional_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Optional_TextDocumentSyncOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Value : constant GNATCOLL.JSON.JSON_Value := JS.Read;
   begin
      if Value.Kind in GNATCOLL.JSON.JSON_Null_Type then
         V := (False, False);
      elsif Value.Kind in GNATCOLL.JSON.JSON_Object_Type then
         V := (True, False, others => <>);
         TextDocumentSyncOptions'Read (S, V.Options);
      else
         V := (True, True, others => <>);
         TextDocumentSyncKind'Read (S, V.Value);
      end if;
   end Read_Optional_TextDocumentSyncOptions;

   -------------------------------
   -- Read_ParameterInformation --
   -------------------------------

   procedure Read_ParameterInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ParameterInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_String (JS, +"label", V.label);
      Read_Optional_String (JS, +"documentation", V.documentation);
      JS.End_Object;
   end Read_ParameterInformation;

   --------------------------------------
   -- Read_ParameterInformation_Vector --
   --------------------------------------

   procedure Read_ParameterInformation_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ParameterInformation_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V.Clear;
      JS.Start_Array;
      while not JS.End_Of_Array loop
         declare
            Item : ParameterInformation;
         begin
            ParameterInformation'Read (S, Item);
            V.Append (Item);
         end;
      end loop;
      JS.End_Array;
   end Read_ParameterInformation_Vector;

   -------------------
   -- Read_Position --
   -------------------

   procedure Read_Position
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Position)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Number (JS, +"line", LSP_Number (V.line));
      Read_Number (JS, +"character", LSP_Number (V.character));
      JS.End_Object;
   end Read_Position;

   -----------------------------------
   -- Read_PublishDiagnosticsParams --
   -----------------------------------

   procedure Read_PublishDiagnosticsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out PublishDiagnosticsParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      DocumentUri'Read (S, V.uri);
      JS.Key ("diagnostics");
      Diagnostic_Vector'Read (S, V.diagnostics);
      JS.End_Object;
   end Read_PublishDiagnosticsParams;

   ---------------------------
   -- Read_ReferenceContext --
   ---------------------------

   procedure Read_ReferenceContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ReferenceContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("includeDeclaration");
      V.includeDeclaration := JS.Read.Get;
      JS.End_Object;
   end Read_ReferenceContext;

   --------------------------
   -- Read_ReferenceParams --
   --------------------------

   procedure Read_ReferenceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ReferenceParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("position");
      Position'Read (S, V.position);
      JS.Key ("context");
      ReferenceContext'Read (S, V.context);
      JS.End_Object;
   end Read_ReferenceParams;

   -------------------------
   -- Read_RequestMessage --
   -------------------------

   procedure Read_RequestMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RequestMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      LSP.Types.Read_String (JS, +"jsonrpc", V.jsonrpc);
      LSP.Types.Read_String (JS, +"method", V.method);
      Read_Number_Or_String (JS, +"id", V.id);
      JS.End_Object;
   end Read_RequestMessage;

   --------------------------
   -- Read_Response_Prefix --
   --------------------------

   procedure Read_Response_Prefix
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ResponseMessage'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      Read_String (JS, +"jsonrpc", V.jsonrpc);
      Read_Number_Or_String (JS, +"id", V.id);
      JS.Key ("error");
      Optional_ResponseError'Read (S, V.error);
   end Read_Response_Prefix;

   --------------------------
   -- Read_ResponseMessage --
   --------------------------

   procedure Read_ResponseMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ResponseMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Response_Prefix (S, V);
      JS.End_Object;
   end Read_ResponseMessage;

   -----------------------
   -- Read_RenameParams --
   -----------------------

   procedure Read_RenameParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RenameParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("position");
      Position'Read (S, V.position);
      Read_String (JS, +"newName", V.newName);
      JS.End_Object;
   end Read_RenameParams;

   ------------------------
   -- Read_ResponseError --
   ------------------------

   procedure Read_ResponseError
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ResponseError)
   is
      Code : Long_Integer;
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("code");
      Code := JS.Read.Get;

      for J in Error_Map'Range loop
         if Error_Map (J) = Code then
            V.code := J;
            exit;
         end if;
      end loop;

      Read_String (JS, +"message", V.message);
      JS.Key ("data");
      V.data := JS.Read;

      JS.End_Object;
   end Read_ResponseError;

   -----------------------------
   -- Read_ServerCapabilities --
   -----------------------------

   procedure Read_ServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ServerCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocumentSync");
      Optional_TextDocumentSyncOptions'Read (S, V.textDocumentSync);
      Read_Optional_Boolean (JS, +"hoverProvider", V.hoverProvider);
      JS.Key ("completionProvider");
      Optional_CompletionOptions'Read (S, V.completionProvider);
      JS.Key ("signatureHelpProvider");
      Optional_SignatureHelpOptions'Read (S, V.signatureHelpProvider);
      Read_Optional_Boolean
        (JS, +"definitionProvider", V.definitionProvider);
      Read_Optional_Boolean
        (JS, +"typeDefinitionProvider", V.typeDefinitionProvider);
      Read_Optional_Boolean
        (JS, +"referencesProvider", V.referencesProvider);
      Read_Optional_Boolean
        (JS, +"documentHighlightProvider", V.documentHighlightProvider);
      Read_Optional_Boolean
        (JS, +"documentSymbolProvider", V.documentSymbolProvider);
      Read_Optional_Boolean
        (JS, +"workspaceSymbolProvider", V.workspaceSymbolProvider);
      Read_Optional_Boolean
        (JS, +"codeActionProvider", V.codeActionProvider);
      Read_Optional_Boolean
        (JS, +"documentFormattingProvider", V.documentFormattingProvider);
      Read_Optional_Boolean
        (JS,
         +"documentRangeFormattingProvider",
         V.documentRangeFormattingProvider);
      JS.Key ("documentOnTypeFormattingProvider");
      Optional_DocumentOnTypeFormattingOptions'Read
        (S, V.documentOnTypeFormattingProvider);
      Read_Optional_Boolean (JS, +"renameProvider", V.renameProvider);
      JS.Key ("documentLinkProvider");
      DocumentLinkOptions'Read (S, V.documentLinkProvider);
      JS.Key ("executeCommandProvider");
      ExecuteCommandOptions'Read (S, V.executeCommandProvider);

      Read_Optional_Boolean (JS, +"alsCalledByProvider",
                             V.alsCalledByProvider);

      JS.End_Object;
   end Read_ServerCapabilities;

   -------------------------------
   -- Read_SignatureInformation --
   -------------------------------

   procedure Read_SignatureInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_String (JS, +"label", V.label);
      Read_Optional_String (JS, +"documentation", V.documentation);
      JS.Key ("parameters");
      ParameterInformation_Vector'Read (S, V.parameters);
      JS.End_Object;
   end Read_SignatureInformation;

   --------------------------------------
   -- Read_SignatureInformation_Vector --
   --------------------------------------

   procedure Read_SignatureInformation_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureInformation_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V.Clear;
      JS.Start_Array;

      while not JS.End_Of_Array loop
         declare
            Item : SignatureInformation;
         begin
            SignatureInformation'Read (S, Item);
            V.Append (Item);
         end;
      end loop;

      JS.End_Array;
   end Read_SignatureInformation_Vector;

   ------------------------
   -- Read_SignatureHelp --
   ------------------------

   procedure Read_SignatureHelp
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelp)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("signatures");
      JS.Start_Array;

      while not JS.End_Of_Array loop
         declare
            Item : SignatureInformation;
         begin
            SignatureInformation'Read (S, Item);
            V.signatures.Append (Item);
         end;
      end loop;

      JS.End_Array;
      Read_Optional_Number (JS, +"activeSignature", V.activeSignature);
      Read_Optional_Number (JS, +"activeParameter", V.activeParameter);
      JS.End_Object;
   end Read_SignatureHelp;

   -------------------------------
   -- Read_SignatureHelpOptions --
   -------------------------------

   procedure Read_SignatureHelpOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_String_Vector (JS, +"triggerCharacters", V.triggerCharacters);
      JS.End_Object;
   end Read_SignatureHelpOptions;

   ----------------------------
   -- Read_ShowMessageParams --
   ----------------------------

   procedure Read_ShowMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowMessageParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_MessageType (JS, +"type", V.the_type);
      Read_String (JS, +"message", V.message);
      JS.End_Object;
   end Read_ShowMessageParams;

   -----------------------------------
   -- Read_ShowMessageRequestParams --
   -----------------------------------

   procedure Read_ShowMessageRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowMessageRequestParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_MessageType (JS, +"type", V.the_type);
      Read_String (JS, +"message", V.message);
      Read_String_Vector (JS, +"actions", V.actions);
      JS.End_Object;
   end Read_ShowMessageRequestParams;

   ---------------
   -- Read_Span --
   ---------------

   procedure Read_Span
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Span)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("start");
      Position'Read (S, V.first);
      JS.Key ("end");
      Position'Read (S, V.last);
      JS.End_Object;
   end Read_Span;

   ------------------------
   -- Read_String_Vector --
   ------------------------

   procedure Read_String_Vector
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out LSP.Types.LSP_String_Vector) is
   begin
      Item.Clear;
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
      Stream.Start_Array;

      while not Stream.End_Of_Array loop
         Item.Append (To_LSP_String (Unbounded_String'(Stream.Read.Get)));
      end loop;

      Stream.End_Array;
   end Read_String_Vector;

   ----------------------------
   -- Read_SymbolInformation --
   ----------------------------

   procedure Read_SymbolInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SymbolInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_String (JS, +"name", V.name);
      JS.Key ("kind");
      V.kind := SymbolKind'Val (JS.Read.Get - 1);
      JS.Key ("location");
      Location'Read (S, V.location);
      JS.Key ("edits");
      Read_Optional_String (JS, +"containerName", V.containerName);
      JS.End_Object;
   end Read_SymbolInformation;

   -----------------------------------
   -- Read_SymbolInformation_Vector --
   -----------------------------------

   procedure Read_SymbolInformation_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SymbolInformation_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V.Clear;
      JS.Start_Array;

      while not JS.End_Of_Array loop
         declare
            Item : SymbolInformation;
         begin
            SymbolInformation'Read (S, Item);
            V.Append (Item);
         end;
      end loop;

      JS.End_Array;
   end Read_SymbolInformation_Vector;

   --------------------------
   -- Read_synchronization --
   --------------------------

   procedure Read_synchronization
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out synchronization)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean
        (JS, +"dynamicRegistration", V.dynamicRegistration);
      Read_Optional_Boolean (JS, +"willSave", V.willSave);
      Read_Optional_Boolean (JS, +"willSaveWaitUntil", V.willSaveWaitUntil);
      Read_Optional_Boolean (JS, +"didSave", V.didSave);
      JS.End_Object;
   end Read_synchronization;

   -----------------------------------------
   -- Read_TextDocumentClientCapabilities --
   -----------------------------------------

   procedure Read_TextDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("synchronization");
      synchronization'Read (S, V.synchronization);
      JS.Key ("completion");
      completion'Read (S, V.completion);
      JS.Key ("hover");
      dynamicRegistration'Read (S, V.hover);
      JS.Key ("signatureHelp");
      dynamicRegistration'Read (S, V.signatureHelp);
      JS.Key ("references");
      dynamicRegistration'Read (S, V.references);
      JS.Key ("documentHighlight");
      dynamicRegistration'Read (S, V.documentHighlight);
      JS.Key ("documentSymbol");
      dynamicRegistration'Read (S, V.documentSymbol);
      JS.Key ("formatting");
      dynamicRegistration'Read (S, V.formatting);
      JS.Key ("rangeFormatting");
      dynamicRegistration'Read (S, V.rangeFormatting);
      JS.Key ("onTypeFormatting");
      dynamicRegistration'Read (S, V.onTypeFormatting);
      JS.Key ("definition");
      dynamicRegistration'Read (S, V.definition);
      JS.Key ("typeDefinition");
      dynamicRegistration'Read (S, V.typeDefinition);
      JS.Key ("codeAction");
      dynamicRegistration'Read (S, V.codeAction);
      JS.Key ("codeLens");
      dynamicRegistration'Read (S, V.codeLens);
      JS.Key ("documentLink");
      dynamicRegistration'Read (S, V.documentLink);
      JS.Key ("rename");
      dynamicRegistration'Read (S, V.rename);
      JS.End_Object;
   end Read_TextDocumentClientCapabilities;

   -----------------------------------------
   -- Read_TextDocumentContentChangeEvent --
   -----------------------------------------

   procedure Read_TextDocumentContentChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentContentChangeEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      Optional_Span'Read (S, V.span);
      Read_Optional_Number (JS, +"rangeLength", V.rangeLength);
      Read_String (JS, +"text", V.text);
      JS.End_Object;
   end Read_TextDocumentContentChangeEvent;

   ------------------------------------------------
   -- Read_TextDocumentContentChangeEvent_Vector --
   ------------------------------------------------

   procedure Read_TextDocumentContentChangeEvent_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentContentChangeEvent_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V.Clear;
      JS.Start_Array;
      while not JS.End_Of_Array loop
         declare
            Item : TextDocumentContentChangeEvent;
         begin
            TextDocumentContentChangeEvent'Read (S, Item);
            V.Append (Item);
         end;
      end loop;
      JS.End_Array;
   end Read_TextDocumentContentChangeEvent_Vector;

   ---------------------------
   -- Read_TextDocumentEdit --
   ---------------------------

   procedure Read_TextDocumentEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      VersionedTextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("edits");
      TextEdit_Vector'Read (S, V.edits);
      JS.End_Object;
   end Read_TextDocumentEdit;

   ----------------------------------
   -- Read_TextDocumentEdit_Vector --
   ----------------------------------

   procedure Read_TextDocumentEdit_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentEdit_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V.Clear;
      JS.Start_Array;
      while not JS.End_Of_Array loop
         declare
            Item : TextDocumentEdit;
         begin
            TextDocumentEdit'Read (S, Item);
            V.Append (Item);
         end;
      end loop;
      JS.End_Array;
   end Read_TextDocumentEdit_Vector;

   ---------------------------------
   -- Read_TextDocumentIdentifier --
   ---------------------------------

   procedure Read_TextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentIdentifier)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      DocumentUri'Read (S, V.uri);
      JS.End_Object;
   end Read_TextDocumentIdentifier;

   ---------------------------
   -- Read_TextDocumentItem --
   ---------------------------

   procedure Read_TextDocumentItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_If_String (JS, +"uri", V.uri);
      Read_String (JS, +"languageId", V.languageId);
      Read_Number (JS, +"version", LSP.Types.LSP_Number (V.version));
      Read_String (JS, +"text", V.text);
      JS.End_Object;
   end Read_TextDocumentItem;

   -------------------------------------
   -- Read_TextDocumentPositionParams --
   -------------------------------------

   procedure Read_TextDocumentPositionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentPositionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("position");
      Position'Read (S, V.position);
      JS.End_Object;
   end Read_TextDocumentPositionParams;

   -------------------------------
   -- Read_TextDocumentSyncKind --
   -------------------------------

   procedure Read_TextDocumentSyncKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSyncKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Value : constant GNATCOLL.JSON.JSON_Value := JS.Read;

      Map : constant array (0 .. 2) of TextDocumentSyncKind :=
        (0 => None, 1 => Full, 2 => Incremental);
   begin
      V := Map (Value.Get);
   end Read_TextDocumentSyncKind;

   ----------------------------------
   -- Read_TextDocumentSyncOptions --
   ----------------------------------

   procedure Read_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSyncOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean (JS, +"openClose", V.openClose);
      JS.Key ("change");
      Optional_TextDocumentSyncKind'Read (S, V.change);
      Read_Optional_Boolean (JS, +"willSave", V.willSave);
      Read_Optional_Boolean (JS, +"willSaveWaitUntil", V.willSaveWaitUntil);
      Read_Optional_Boolean (JS, +"save", V.save);
      JS.End_Object;
   end Read_TextDocumentSyncOptions;

   -------------------
   -- Read_TextEdit --
   -------------------

   procedure Read_TextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      Span'Read (S, V.span);
      Read_String (JS, +"newText", V.newText);
      JS.End_Object;
   end Read_TextEdit;

   --------------------------
   -- Read_TextEdit_Vector --
   --------------------------

   procedure Read_TextEdit_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextEdit_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V.Clear;
      JS.Start_Array;
      while not JS.End_Of_Array loop
         declare
            Item : TextEdit;
         begin
            TextEdit'Read (S, Item);
            V.Append (Item);
         end;
      end loop;
      JS.End_Array;
   end Read_TextEdit_Vector;

   ------------------------------------------
   -- Read_VersionedTextDocumentIdentifier --
   ------------------------------------------

   procedure Read_VersionedTextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out VersionedTextDocumentIdentifier)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      DocumentUri'Read (S, V.uri);
      Read_Number (JS, +"version", LSP_Number (V.version));
      JS.End_Object;
   end Read_VersionedTextDocumentIdentifier;

   --------------------------------------
   -- Read_WorkspaceClientCapabilities --
   --------------------------------------

   procedure Read_WorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean (JS, +"applyEdit", V.applyEdit);
      JS.Key ("workspaceEdit");
      documentChanges'Read (S, V.workspaceEdit);
      JS.Key ("didChangeConfiguration");
      dynamicRegistration'Read (S, V.didChangeConfiguration);
      JS.Key ("didChangeWatchedFiles");
      dynamicRegistration'Read (S, V.didChangeWatchedFiles);
      JS.Key ("symbol");
      dynamicRegistration'Read (S, V.symbol);
      JS.Key ("executeCommand");
      dynamicRegistration'Read (S, V.executeCommand);
      JS.End_Object;
   end Read_WorkspaceClientCapabilities;

   ------------------------
   -- Read_WorkspaceEdit --
   ------------------------

   procedure Read_WorkspaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceEdit)
   is
      procedure Each
        (Name  : GNATCOLL.JSON.UTF8_String;
         Value : GNATCOLL.JSON.JSON_Value);

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      ----------
      -- Each --
      ----------

      procedure Each
        (Name  : GNATCOLL.JSON.UTF8_String;
         Value : GNATCOLL.JSON.JSON_Value)
      is
         pragma Unreferenced (Value);
         Key    : constant LSP.Types.LSP_String := +Name;
         Vector : TextEdit_Vector;
      begin
         JS.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
         JS.Start_Array;
         while not JS.End_Of_Array loop
            declare
               Item : TextEdit;
            begin
               TextEdit'Read (S, Item);
               Vector.Append (Item);
            end;
         end loop;
         JS.End_Array;

         V.changes.Insert (Key, Vector);
      end Each;

      Value : GNATCOLL.JSON.JSON_Value;
   begin
      JS.Start_Object;
      JS.Key ("changes");
      Value := JS.Read;

      if Value.Kind in GNATCOLL.JSON.JSON_Object_Type then
         JS.Key ("changes");
         JS.Start_Object;
         Value.Map_JSON_Object (Each'Access);
         JS.End_Object;
      else
         JS.Key ("documentChanges");
         TextDocumentEdit_Vector'Write (S, V.documentChanges);
      end if;

      JS.End_Object;
   end Read_WorkspaceEdit;

   --------------------------------
   -- Read_WorkspaceSymbolParams --
   --------------------------------

   procedure Read_WorkspaceSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceSymbolParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_String (JS, +"query", V.query);
      JS.End_Object;
   end Read_WorkspaceSymbolParams;

   -----------------------------
   -- Write_ShowMessageParams --
   -----------------------------

   procedure Write_ShowMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowMessageParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_MessageType (JS, +"type", V.the_type);
      Write_String (JS, +"message", V.message);
      JS.End_Object;
   end Write_ShowMessageParams;

   ------------------------------------
   -- Write_ShowMessageRequestParams --
   ------------------------------------

   procedure Write_ShowMessageRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowMessageRequestParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_MessageType (JS, +"type", V.the_type);
      Write_String (JS, +"message", V.message);
      Write_String_Vector (JS, +"actions", V.actions);
      JS.End_Object;
   end Write_ShowMessageRequestParams;

   ------------------------------------
   -- Write_ApplyWorkspaceEditParams --
   ------------------------------------

   procedure Write_ApplyWorkspaceEditParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ApplyWorkspaceEditParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("edit");
      WorkspaceEdit'Write (S, V.edit);
      JS.End_Object;
   end Write_ApplyWorkspaceEditParams;

   ------------------------------------
   -- Write_ApplyWorkspaceEditResult --
   ------------------------------------

   procedure Write_ApplyWorkspaceEditResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ApplyWorkspaceEditResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Boolean (JS, +"applied", V.applied);
      JS.End_Object;
   end Write_ApplyWorkspaceEditResult;

   ------------------------------
   -- Write_ClientCapabilities --
   ------------------------------

   procedure Write_ClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workspace");
      WorkspaceClientCapabilities'Write (S, V.workspace);
      JS.Key ("textDocument");
      TextDocumentClientCapabilities'Write (S, V.textDocument);
      JS.End_Object;
   end Write_ClientCapabilities;

   -----------------------------
   -- Write_CodeActionContext --
   -----------------------------

   procedure Write_CodeActionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("diagnostics");
      Diagnostic_Vector'Write (S, V.diagnostics);
      JS.End_Object;
   end Write_CodeActionContext;

   ----------------------------
   -- Write_CodeActionParams --
   ----------------------------

   procedure Write_CodeActionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("range");
      Span'Write (S, V.span);
      JS.Key ("context");
      CodeActionContext'Write (S, V.context);
      JS.End_Object;
   end Write_CodeActionParams;

   ---------------------------
   -- Write_CodeLensOptions --
   ---------------------------

   procedure Write_CodeLensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeLensOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Optional_Boolean (JS, +"resolveProvider", V.resolveProvider);
      JS.End_Object;
   end Write_CodeLensOptions;

   -------------------
   -- Write_Command --
   -------------------

   procedure Write_Command
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Command)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if Is_Empty (V.command) then
         return;
      end if;

      JS.Start_Object;
      Write_String (JS, +"title", V.title);
      Write_String (JS, +"command", V.command);
--      if not Is_Empty (V.arguments) then  FIXME!!!
         JS.Key ("arguments");
         JS.Write (V.arguments);
--      end if;
      JS.End_Object;
   end Write_Command;

   --------------------------
   -- Write_Command_Vector --
   --------------------------

   procedure Write_Command_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Command_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V.Is_Empty then
         JS.Write (GNATCOLL.JSON.Create (GNATCOLL.JSON.Empty_Array));
         return;
      end if;

      JS.Start_Array;
      for Item of V loop
         Command'Write (S, Item);
      end loop;
      JS.End_Array;
   end Write_Command_Vector;

   ----------------------
   -- Write_completion --
   ----------------------

   procedure Write_completion
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : completion)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Optional_Boolean
        (JS, +"dynamicRegistration", V.dynamicRegistration);
      Write_Optional_Boolean (JS, +"snippetSupport", V.snippetSupport);
      JS.End_Object;
   end Write_completion;

   --------------------------
   -- Write_CompletionItem --
   --------------------------

   procedure Write_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_String (JS, +"label", V.label);
      JS.Key ("kind");
      Optional_CompletionItemKind'Write (S, V.kind);
      Write_Optional_String (JS, +"detail", V.detail);
      Write_Optional_String (JS, +"documentation", V.documentation);
      Write_Optional_String (JS, +"sortText", V.sortText);
      Write_Optional_String (JS, +"filterText", V.filterText);
      Write_Optional_String (JS, +"insertText", V.insertText);
      JS.Key ("insertTextFormat");
      Optional_InsertTextFormat'Write (S, V.insertTextFormat);
      JS.Key ("textEdit");
      Optional_TextEdit'Write (S, V.textEdit);
      JS.Key ("additionalTextEdits");
      TextEdit_Vector'Write (S, V.additionalTextEdits);

      if not V.commitCharacters.Is_Empty then
         Write_String_Vector (JS, +"commitCharacters", V.commitCharacters);
      end if;

      JS.Key ("command");
      Optional_Command'Write (S, V.command);
      JS.End_Object;
   end Write_CompletionItem;

   ---------------------------------
   -- Write_CompletionItem_Vector --
   ---------------------------------

   procedure Write_CompletionItem_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItem_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V.Is_Empty then
         JS.Write (GNATCOLL.JSON.Create (GNATCOLL.JSON.Empty_Array));
         return;
      end if;

      JS.Start_Array;
      for Item of V loop
         CompletionItem'Write (S, Item);
      end loop;
      JS.End_Array;
   end Write_CompletionItem_Vector;

   ------------------------------
   -- Write_CompletionItemKind --
   ------------------------------

   procedure Write_CompletionItemKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItemKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write
        (GNATCOLL.JSON.Create
           (Integer'(CompletionItemKind'Pos (V)) + 1));
   end Write_CompletionItemKind;

   --------------------------
   -- Write_CompletionList --
   --------------------------

   procedure Write_CompletionList
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionList)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Optional_Boolean (JS, +"isIncomplete", (True, V.isIncomplete));
      JS.Key ("items");
      CompletionItem_Vector'Write (S, V.items);
      JS.End_Object;
   end Write_CompletionList;

   -----------------------------
   -- Write_CompletionOptions --
   -----------------------------

   procedure Write_CompletionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Optional_Boolean (JS, +"resolveProvider", V.resolveProvider);
      Write_String_Vector (JS, +"triggerCharacters", V.triggerCharacters);
      JS.End_Object;
   end Write_CompletionOptions;

   ----------------------
   -- Write_Diagnostic --
   ----------------------

   procedure Write_Diagnostic
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Diagnostic)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      Span'Write (S, V.span);
      JS.Key ("severity");
      Optional_DiagnosticSeverity'Write (S, V.severity);

      if V.code.Is_Number then
         Write_Number (JS, +"code", V.code.Number);
      elsif not Is_Empty (V.code.String) then
         Write_String (JS, +"code", V.code.String);
      end if;
      Write_Optional_String (JS, +"source", V.source);
      Write_String (JS, +"message", V.message);
      JS.End_Object;
   end Write_Diagnostic;

   -----------------------------
   -- Write_Diagnostic_Vector --
   -----------------------------

   procedure Write_Diagnostic_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Diagnostic_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for Item of V loop
         Diagnostic'Write (S, Item);
      end loop;
      JS.End_Array;
   end Write_Diagnostic_Vector;

   ------------------------------
   -- Write_DiagnosticSeverity --
   ------------------------------

   procedure Write_DiagnosticSeverity
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DiagnosticSeverity)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write
        (GNATCOLL.JSON.Create
           (Integer'(DiagnosticSeverity'Pos (V)) + 1));
   end Write_DiagnosticSeverity;

   ----------------------------------------
   -- Write_DidChangeConfigurationParams --
   ----------------------------------------

   procedure Write_DidChangeConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeConfigurationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("settings");
      JS.Write (V.settings);
      JS.End_Object;
   end Write_DidChangeConfigurationParams;

   ---------------------------------------
   -- Write_DidChangeTextDocumentParams --
   ---------------------------------------

   procedure Write_DidChangeTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      VersionedTextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("contentChanges");
      TextDocumentContentChangeEvent_Vector'Write (S, V.contentChanges);
      JS.End_Object;
   end Write_DidChangeTextDocumentParams;

   --------------------------------------
   -- Write_DidCloseTextDocumentParams --
   --------------------------------------

   procedure Write_DidCloseTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidCloseTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.End_Object;
   end Write_DidCloseTextDocumentParams;

   -------------------------------------
   -- Write_DidOpenTextDocumentParams --
   -------------------------------------

   procedure Write_DidOpenTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidOpenTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentItem'Write (S, V.textDocument);
      JS.End_Object;
   end Write_DidOpenTextDocumentParams;

   -------------------------------------
   -- Write_DidSaveTextDocumentParams --
   -------------------------------------

   procedure Write_DidSaveTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidSaveTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      Write_Optional_String (JS, +"text", V.text);
      JS.End_Object;
   end Write_DidSaveTextDocumentParams;

   ---------------------------
   -- Write_documentChanges --
   ---------------------------

   procedure Write_documentChanges
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : documentChanges)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Optional_Boolean
        (JS, +"documentChanges", Optional_Boolean (V));
      JS.End_Object;
   end Write_documentChanges;

   -----------------------------
   -- Write_DocumentHighlight --
   -----------------------------

   procedure Write_DocumentHighlight
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentHighlight)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      Span'Write (S, V.span);
      JS.Key ("kind");
      JS.Write
        (GNATCOLL.JSON.Create
           (Integer'(DocumentHighlightKind'Pos (V.kind)) + 1));

      JS.End_Object;
   end Write_DocumentHighlight;

   ------------------------------------
   -- Write_DocumentHighlight_Vector --
   ------------------------------------

   procedure Write_DocumentHighlight_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentHighlight_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V.Is_Empty then
         JS.Write (GNATCOLL.JSON.Create (GNATCOLL.JSON.Empty_Array));
         return;
      end if;

      JS.Start_Array;
      for Item of V loop
         DocumentHighlight'Write (S, Item);
      end loop;
      JS.End_Array;
   end Write_DocumentHighlight_Vector;

   ---------------------------------
   -- Write_DocumentHighlightKind --
   ---------------------------------

   procedure Write_DocumentHighlightKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentHighlightKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Map : constant array (DocumentHighlightKind) of Integer :=
        (Text => 1, Read => 2, Write => 3);
   begin
      JS.Write (GNATCOLL.JSON.Create (Map (V)));
   end Write_DocumentHighlightKind;

   -------------------------------
   -- Write_DocumentLinkOptions --
   -------------------------------

   procedure Write_DocumentLinkOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentLinkOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Optional_Boolean (JS, +"resolveProvider", V.resolveProvider);
      JS.End_Object;
   end Write_DocumentLinkOptions;

   -------------------------------------------
   -- Write_DocumentOnTypeFormattingOptions --
   -------------------------------------------

   procedure Write_DocumentOnTypeFormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentOnTypeFormattingOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_String (JS, +"firstTriggerCharacter", V.firstTriggerCharacter);
      Write_String_Vector
        (JS, +"moreTriggerCharacter", V.moreTriggerCharacter);
      JS.End_Object;
   end Write_DocumentOnTypeFormattingOptions;

   --------------------------------
   -- Write_DocumentSymbolParams --
   --------------------------------

   procedure Write_DocumentSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentSymbolParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.End_Object;
   end Write_DocumentSymbolParams;

   -------------------------------
   -- Write_dynamicRegistration --
   -------------------------------

   procedure Write_dynamicRegistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : dynamicRegistration)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Optional_Boolean
        (JS, +"dynamicRegistration", Optional_Boolean (V));
      JS.End_Object;
   end Write_dynamicRegistration;

   ---------------------------------
   -- Write_ExecuteCommandOptions --
   ---------------------------------

   procedure Write_ExecuteCommandOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ExecuteCommandOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_String_Vector (JS, +"commands", V.commands);
      JS.End_Object;
   end Write_ExecuteCommandOptions;

   --------------------------------
   -- Write_ExecuteCommandParams --
   --------------------------------

   procedure Write_ExecuteCommandParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ExecuteCommandParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_String (JS, +"command", V.command);
      JS.Key ("arguments");
      JS.Write (V.arguments);
      JS.End_Object;
   end Write_ExecuteCommandParams;

   -----------------
   -- Write_Hover --
   -----------------

   procedure Write_Hover
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Hover)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("contents");

      if V.contents.Last_Index = 1 then
         MarkedString'Write (S, V.contents.First_Element);
      else
         MarkedString_Vector'Write (S, V.contents);
      end if;

      JS.Key ("range");
      Optional_Span'Write (S, V.Span);
      JS.End_Object;
   end Write_Hover;

   ----------------------------
   -- Write_InitializeParams --
   ----------------------------

   procedure Write_InitializeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InitializeParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Trace : LSP.Types.Optional_String;
   begin
      JS.Start_Object;
      Write_Optional_Number (JS, +"processId", V.processId);

      if not LSP.Types.Is_Empty (V.rootPath) then
         Write_String (JS, +"rootPath", V.rootPath);
      end if;

      Write_String (JS, +"rootUri", V.rootUri);
      JS.Key ("capabilities");
      LSP.Messages.ClientCapabilities'Write (S, V.capabilities);

      case V.trace is
         when LSP.Types.Unspecified =>
            null;
         when LSP.Types.Off =>
            Trace := (True, +"off");
         when LSP.Types.Messages =>
            Trace := (True, +"messages");
         when LSP.Types.Verbose =>
            Trace := (True, +"verbose");
      end case;

      if V.trace /= LSP.Types.Unspecified then
         Write_Optional_String (JS, +"trace", Trace);
      end if;

      JS.End_Object;
   end Write_InitializeParams;

   ----------------------------
   -- Write_InitializeResult --
   ----------------------------

   procedure Write_InitializeResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InitializeResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("capabilities");
      ServerCapabilities'Write (S, V.capabilities);
      JS.End_Object;
   end Write_InitializeResult;

   -----------------------------
   -- Write_InitializedParams --
   -----------------------------

   not overriding procedure Write_InitializedParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InitializedParams)
   is
      pragma Unreferenced (V);

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.End_Object;
   end Write_InitializedParams;

   ----------------------------
   -- Write_InsertTextFormat --
   ----------------------------

   procedure Write_InsertTextFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InsertTextFormat)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write
        (GNATCOLL.JSON.Create
           (Integer'(InsertTextFormat'Pos (V)) + 1));
   end Write_InsertTextFormat;

   --------------------
   -- Write_Location --
   --------------------

   procedure Write_Location
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Location)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      DocumentUri'Write (S, V.uri);
      JS.Key ("range");
      Span'Write (S, V.span);
      JS.Key ("alsKind");
      AlsReferenceKind_Set'Write (S, V.alsKind);
      JS.End_Object;
   end Write_Location;

   ---------------------------
   -- Write_Location_Vector --
   ---------------------------

   procedure Write_Location_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Location_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V.Is_Empty then
         JS.Write (GNATCOLL.JSON.Create (GNATCOLL.JSON.Empty_Array));
         return;
      end if;

      JS.Start_Array;
      for Item of V loop
         Location'Write (S, Item);
      end loop;
      JS.End_Array;
   end Write_Location_Vector;

   ----------------------------
   -- Write_LogMessageParams --
   ----------------------------

   procedure Write_LogMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LogMessageParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_MessageType (JS, +"type", V.the_type);
      Write_String (JS, +"message", V.message);
      JS.End_Object;
   end Write_LogMessageParams;

   ------------------------
   -- Write_MarkedString --
   ------------------------

   procedure Write_MarkedString
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MarkedString)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V.Is_String then
         JS.Write (GNATCOLL.JSON.Create (To_UTF_8_String (V.value)));
      else
         JS.Start_Object;
         Write_String (JS, +"language", V.language);
         Write_String (JS, +"value", V.value);
         JS.End_Object;
      end if;
   end Write_MarkedString;

   -------------------------------
   -- Write_MarkedString_Vector --
   -------------------------------

   procedure Write_MarkedString_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MarkedString_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V.Is_Empty then
         JS.Write (GNATCOLL.JSON.Create (GNATCOLL.JSON.Empty_Array));
         return;
      end if;

      JS.Start_Array;
      for Item of V loop
         MarkedString'Write (S, Item);
      end loop;
      JS.End_Array;
   end Write_MarkedString_Vector;

   -----------------------
   -- Write_MessageType --
   -----------------------

   procedure Write_MessageType
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : MessageType) is
   begin
      Write_Number (Stream, Key, MessageType'Pos (Item) + 1);
   end Write_MessageType;

   -------------------------------
   -- Write_NotificationMessage --
   -------------------------------

   procedure Write_NotificationMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : NotificationMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Notification_Prefix (S, V);
      JS.End_Object;
   end Write_NotificationMessage;

   -------------------------------
   -- Write_Notification_Prefix --
   -------------------------------

   procedure Write_Notification_Prefix
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.NotificationMessage'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      Write_String (JS, +"jsonrpc", V.jsonrpc);
      Write_String (JS, +"method", V.method);
   end Write_Notification_Prefix;

   ------------------
   -- Write_Number --
   ------------------

   procedure Write_Number
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : LSP.Types.LSP_Number) is
   begin
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
      Stream.Write (GNATCOLL.JSON.Create (Item));
   end Write_Number;

   -------------------
   -- Write_Boolean --
   -------------------

   procedure Write_Boolean
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : Boolean) is
   begin
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
      Stream.Write (GNATCOLL.JSON.Create (Item));
   end Write_Boolean;

   ----------------------------
   -- Write_Optional_Boolean --
   ----------------------------

   procedure Write_Optional_Boolean
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : LSP.Types.Optional_Boolean) is
   begin
      if Item.Is_Set then
         Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
         Stream.Write (GNATCOLL.JSON.Create (Item.Value));
      end if;
   end Write_Optional_Boolean;

   ---------------------------
   -- Write_Optional_Number --
   ---------------------------

   procedure Write_Optional_Number
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : LSP.Types.Optional_Number) is
   begin
      if Item.Is_Set then
         Write_Number (Stream, Key, Item.Value);
      end if;
   end Write_Optional_Number;

   ---------------------------
   -- Write_Optional_String --
   ---------------------------

   procedure Write_Optional_String
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : LSP.Types.Optional_String) is
   begin
      if Item.Is_Set then
         Write_String (Stream, Key, Item.Value);
      end if;
   end Write_Optional_String;

   --------------------------------------------
   -- Write_Optional_TextDocumentSyncOptions --
   --------------------------------------------

   procedure Write_Optional_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Optional_TextDocumentSyncOptions) is
   begin
      if not V.Is_Set then
         return;
      elsif V.Is_Number then
         TextDocumentSyncKind'Write (S, V.Value);
      else
         TextDocumentSyncOptions'Write (S, V.Options);
      end if;
   end Write_Optional_TextDocumentSyncOptions;

   --------------------------------
   -- Write_ParameterInformation --
   --------------------------------

   procedure Write_ParameterInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ParameterInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_String (JS, +"label", V.label);
      Write_Optional_String (JS, +"documentation", V.documentation);
      JS.End_Object;
   end Write_ParameterInformation;

   ---------------------------------------
   -- Write_ParameterInformation_Vector --
   ---------------------------------------

   procedure Write_ParameterInformation_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ParameterInformation_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V.Is_Empty then
         JS.Write (GNATCOLL.JSON.Create (GNATCOLL.JSON.Empty_Array));
         return;
      end if;

      JS.Start_Array;
      for Item of V loop
         ParameterInformation'Write (S, Item);
      end loop;
      JS.End_Array;
   end Write_ParameterInformation_Vector;

   --------------------
   -- Write_Position --
   --------------------

   procedure Write_Position
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Position)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Number (JS, +"line", LSP_Number (V.line));
      Write_Number (JS, +"character", LSP_Number (V.character));
      JS.End_Object;
   end Write_Position;

   ------------------------------------
   -- Write_PublishDiagnosticsParams --
   ------------------------------------

   procedure Write_PublishDiagnosticsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : PublishDiagnosticsParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      DocumentUri'Write (S, V.uri);
      JS.Key ("diagnostics");

      if V.diagnostics.Is_Empty then
         JS.Write (GNATCOLL.JSON.Create (GNATCOLL.JSON.Empty_Array));
      else
         Diagnostic_Vector'Write (S, V.diagnostics);
      end if;

      JS.End_Object;
   end Write_PublishDiagnosticsParams;

   ----------------------------
   -- Write_ReferenceContext --
   ----------------------------

   procedure Write_ReferenceContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ReferenceContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("includeDeclaration");
      JS.Write (GNATCOLL.JSON.Create (V.includeDeclaration));
      JS.End_Object;
   end Write_ReferenceContext;

   ---------------------------
   -- Write_ReferenceParams --
   ---------------------------

   procedure Write_ReferenceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ReferenceParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      Position'Write (S, V.position);
      JS.Key ("context");
      ReferenceContext'Write (S, V.context);
      JS.End_Object;
   end Write_ReferenceParams;

   --------------------
   -- Write_Response --
   --------------------

   procedure Write_Response_Prefix
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ResponseMessage'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      Write_String (JS, +"jsonrpc", V.jsonrpc);

      if V.id.Is_Number then
         Write_Number (JS, +"id", V.id.Number);
      elsif not Is_Empty (V.id.String) then
         Write_String (JS, +"id", V.id.String);
      end if;

      JS.Key ("error");
      Optional_ResponseError'Write (S, V.error);
   end Write_Response_Prefix;

   --------------------------
   -- Write_RequestMessage --
   --------------------------

   procedure Write_RequestMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RequestMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_String (JS, +"jsonrpc", V.jsonrpc);
      Write_String (JS, +"method", V.method);

      if V.id.Is_Number then
         Write_Number (JS, +"id", V.id.Number);
      elsif not Is_Empty (V.id.String) then
         Write_String (JS, +"id", V.id.String);
      end if;

      JS.End_Object;
   end Write_RequestMessage;

   ------------------------
   -- Write_RenameParams --
   ------------------------

   procedure Write_RenameParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      Position'Write (S, V.position);
      Write_String (JS, +"newName", V.newName);
      JS.End_Object;
   end Write_RenameParams;

   -------------------------
   -- Write_ResponseError --
   -------------------------

   procedure Write_ResponseError
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ResponseError)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("code");
      JS.Write (GNATCOLL.JSON.Create (Error_Map (V.code)));
      Write_String (JS, +"message", V.message);

      if not V.data.Is_Empty and then not V.data.Is_Empty then
         JS.Key ("data");
         JS.Write (V.data);
      end if;

      JS.End_Object;
   end Write_ResponseError;

   ---------------------------
   -- Write_ResponseMessage --
   ---------------------------

   procedure Write_ResponseMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ResponseMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Response_Prefix (S, V);

      if not V.Is_Error then
         JS.Key ("result");
         JS.Write (GNATCOLL.JSON.JSON_Null);
      end if;

      JS.End_Object;
   end Write_ResponseMessage;

   ------------------------------
   -- Write_ServerCapabilities --
   ------------------------------

   procedure Write_ServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ServerCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocumentSync");
      Optional_TextDocumentSyncOptions'Write (S, V.textDocumentSync);
      Write_Optional_Boolean (JS, +"hoverProvider", V.hoverProvider);
      JS.Key ("completionProvider");
      Optional_CompletionOptions'Write (S, V.completionProvider);
      JS.Key ("signatureHelpProvider");
      Optional_SignatureHelpOptions'Write (S, V.signatureHelpProvider);
      Write_Optional_Boolean
        (JS, +"definitionProvider", V.definitionProvider);
      Write_Optional_Boolean
        (JS, +"typeDefinitionProvider", V.typeDefinitionProvider);
      Write_Optional_Boolean
        (JS, +"referencesProvider", V.referencesProvider);
      Write_Optional_Boolean
        (JS, +"documentHighlightProvider", V.documentHighlightProvider);
      Write_Optional_Boolean
        (JS, +"documentSymbolProvider", V.documentSymbolProvider);
      Write_Optional_Boolean
        (JS, +"workspaceSymbolProvider", V.workspaceSymbolProvider);
      Write_Optional_Boolean (JS, +"codeActionProvider", V.codeActionProvider);
      Write_Optional_Boolean
        (JS, +"documentFormattingProvider", V.documentFormattingProvider);
      Write_Optional_Boolean
        (JS,
         +"documentRangeFormattingProvider",
         V.documentRangeFormattingProvider);
      JS.Key ("documentOnTypeFormattingProvider");
      Optional_DocumentOnTypeFormattingOptions'Write
        (S, V.documentOnTypeFormattingProvider);
      Write_Optional_Boolean (JS, +"renameProvider", V.renameProvider);
      JS.Key ("documentLinkProvider");
      DocumentLinkOptions'Write (S, V.documentLinkProvider);
      JS.Key ("executeCommandProvider");
      ExecuteCommandOptions'Write (S, V.executeCommandProvider);

      Write_Optional_Boolean
        (JS, +"alsCalledByProvider", V.alsCalledByProvider);

      JS.End_Object;
   end Write_ServerCapabilities;

   -------------------------
   -- Write_SignatureHelp --
   -------------------------

   procedure Write_SignatureHelp
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelp)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("signatures");
      if V.signatures.Is_Empty then
         JS.Write (GNATCOLL.JSON.Create (GNATCOLL.JSON.Empty_Array));
      else
         JS.Start_Array;
         for Item of V.signatures loop
            SignatureInformation'Write (S, Item);
         end loop;
         JS.End_Array;
      end if;

      Write_Optional_Number (JS, +"activeSignature", V.activeSignature);
      Write_Optional_Number (JS, +"activeParameter", V.activeParameter);
      JS.End_Object;
   end Write_SignatureHelp;

   --------------------------------
   -- Write_SignatureHelpOptions --
   --------------------------------

   procedure Write_SignatureHelpOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_String_Vector (JS, +"triggerCharacters", V.triggerCharacters);
      JS.End_Object;
   end Write_SignatureHelpOptions;

   --------------------------------
   -- Write_SignatureInformation --
   --------------------------------

   procedure Write_SignatureInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_String (JS, +"label", V.label);
      Write_Optional_String (JS, +"documentation", V.documentation);
      JS.Key ("parameters");
      ParameterInformation_Vector'Write (S, V.parameters);
      JS.End_Object;
   end Write_SignatureInformation;

   ---------------------------------------
   -- Write_SignatureInformation_Vector --
   ---------------------------------------

   procedure Write_SignatureInformation_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureInformation_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V.Is_Empty then
         JS.Write (GNATCOLL.JSON.Create (GNATCOLL.JSON.Empty_Array));
      else
         JS.Start_Array;
         for Item of V loop
            SignatureInformation'Write (S, Item);
         end loop;
         JS.End_Array;
      end if;
   end Write_SignatureInformation_Vector;

   ----------------
   -- Write_Span --
   ----------------

   procedure Write_Span
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Span)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("start");
      Position'Write (S, V.first);
      JS.Key ("end");
      Position'Write (S, V.last);
      JS.End_Object;
   end Write_Span;

   ------------------
   -- Write_String --
   ------------------

   procedure Write_String
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : LSP.Types.LSP_String) is
   begin
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
      Stream.Write (GNATCOLL.JSON.Create (To_UTF_8_Unbounded_String (Item)));
   end Write_String;

   -------------------------
   -- Write_String_Vector --
   -------------------------

   procedure Write_String_Vector
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : LSP.Types.LSP_String_Vector) is
   begin
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
      Stream.Start_Array;

      for J in 1 .. Item.Last_Index loop
         Stream.Write
           (GNATCOLL.JSON.Create (To_UTF_8_String (Item.Element (J))));
      end loop;

      Stream.End_Array;
   end Write_String_Vector;

   -----------------------------
   -- Write_SymbolInformation --
   -----------------------------

   procedure Write_SymbolInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SymbolInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_String (JS, +"name", V.name);
      JS.Key ("kind");
      JS.Write
        (GNATCOLL.JSON.Create
           (Integer'(SymbolKind'Pos (V.kind)) + 1));

      JS.Key ("location");
      Location'Write (S, V.location);
      JS.Key ("edits");
      Write_Optional_String (JS, +"containerName", V.containerName);
      JS.End_Object;
   end Write_SymbolInformation;

   ------------------------------------
   -- Write_SymbolInformation_Vector --
   ------------------------------------

   procedure Write_SymbolInformation_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SymbolInformation_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V.Is_Empty then
         JS.Write (GNATCOLL.JSON.Create (GNATCOLL.JSON.Empty_Array));
      else
         JS.Start_Array;
         for Item of V loop
            SymbolInformation'Write (S, Item);
         end loop;
         JS.End_Array;
      end if;
   end Write_SymbolInformation_Vector;

   ---------------------------
   -- Write_synchronization --
   ---------------------------

   procedure Write_synchronization
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : synchronization)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Optional_Boolean
        (JS, +"dynamicRegistration", V.dynamicRegistration);
      Write_Optional_Boolean (JS, +"willSave", V.willSave);
      Write_Optional_Boolean (JS, +"willSaveWaitUntil", V.willSaveWaitUntil);
      Write_Optional_Boolean (JS, +"didSave", V.didSave);
      JS.End_Object;
   end Write_synchronization;

   ------------------------------------------
   -- Write_TextDocumentClientCapabilities --
   ------------------------------------------

   procedure Write_TextDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("synchronization");
      synchronization'Write (S, V.synchronization);
      JS.Key ("completion");
      completion'Write (S, V.completion);
      JS.Key ("hover");
      dynamicRegistration'Write (S, V.hover);
      JS.Key ("signatureHelp");
      dynamicRegistration'Write (S, V.signatureHelp);
      JS.Key ("references");
      dynamicRegistration'Write (S, V.references);
      JS.Key ("documentHighlight");
      dynamicRegistration'Write (S, V.documentHighlight);
      JS.Key ("documentSymbol");
      dynamicRegistration'Write (S, V.documentSymbol);
      JS.Key ("formatting");
      dynamicRegistration'Write (S, V.formatting);
      JS.Key ("rangeFormatting");
      dynamicRegistration'Write (S, V.rangeFormatting);
      JS.Key ("onTypeFormatting");
      dynamicRegistration'Write (S, V.onTypeFormatting);
      JS.Key ("definition");
      dynamicRegistration'Write (S, V.definition);
      JS.Key ("typeDefinition");
      dynamicRegistration'Write (S, V.typeDefinition);
      JS.Key ("codeAction");
      dynamicRegistration'Write (S, V.codeAction);
      JS.Key ("codeLens");
      dynamicRegistration'Write (S, V.codeLens);
      JS.Key ("documentLink");
      dynamicRegistration'Write (S, V.documentLink);
      JS.Key ("rename");
      dynamicRegistration'Write (S, V.rename);
      JS.End_Object;
   end Write_TextDocumentClientCapabilities;

   ------------------------------------------
   -- Write_TextDocumentContentChangeEvent --
   ------------------------------------------

   procedure Write_TextDocumentContentChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentContentChangeEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      Optional_Span'Write (S, V.span);
      Write_Optional_Number (JS, +"rangeLength", V.rangeLength);
      Write_String (JS, +"text", V.text);
      JS.End_Object;
   end Write_TextDocumentContentChangeEvent;

   -------------------------------------------------
   -- Write_TextDocumentContentChangeEvent_Vector --
   -------------------------------------------------

   procedure Write_TextDocumentContentChangeEvent_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentContentChangeEvent_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;

      for Item of V loop
         TextDocumentContentChangeEvent'Write (S, Item);
      end loop;

      JS.End_Array;
   end Write_TextDocumentContentChangeEvent_Vector;

   ----------------------------
   -- Write_TextDocumentEdit --
   ----------------------------

   procedure Write_TextDocumentEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      VersionedTextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("edits");
      TextEdit_Vector'Write (S, V.edits);
      JS.End_Object;
   end Write_TextDocumentEdit;

   -----------------------------------
   -- Write_TextDocumentEdit_Vector --
   -----------------------------------

   procedure Write_TextDocumentEdit_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentEdit_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V.Is_Empty then
         JS.Write (GNATCOLL.JSON.Create (GNATCOLL.JSON.Empty_Array));
         return;
      end if;

      JS.Start_Array;
      for Item of V loop
         TextDocumentEdit'Write (S, Item);
      end loop;
      JS.End_Array;
   end Write_TextDocumentEdit_Vector;

   ----------------------------------
   -- Write_TextDocumentIdentifier --
   ----------------------------------

   procedure Write_TextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentIdentifier)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      DocumentUri'Write (S, V.uri);
      JS.End_Object;
   end Write_TextDocumentIdentifier;

   ----------------------------
   -- Write_TextDocumentItem --
   ----------------------------

   procedure Write_TextDocumentItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_String (JS, +"uri", V.uri);
      Write_String (JS, +"languageId", V.languageId);
      Write_Number (JS, +"version", LSP.Types.LSP_Number (V.version));
      Write_String (JS, +"text", V.text);
      JS.End_Object;
   end Write_TextDocumentItem;

   --------------------------------------
   -- Write_TextDocumentPositionParams --
   --------------------------------------

   procedure Write_TextDocumentPositionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentPositionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      Position'Write (S, V.position);
      JS.End_Object;
   end Write_TextDocumentPositionParams;

   --------------------------------
   -- Write_TextDocumentSyncKind --
   --------------------------------

   procedure Write_TextDocumentSyncKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSyncKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Map : constant array (TextDocumentSyncKind) of Integer :=
        (None => 0, Full => 1, Incremental => 2);
   begin
      JS.Write (GNATCOLL.JSON.Create (Map (V)));
   end Write_TextDocumentSyncKind;

   -----------------------------------
   -- Write_TextDocumentSyncOptions --
   -----------------------------------

   procedure Write_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSyncOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Optional_Boolean (JS, +"openClose", V.openClose);
      JS.Key ("change");
      Optional_TextDocumentSyncKind'Write (S, V.change);
      Write_Optional_Boolean (JS, +"willSave", V.willSave);
      Write_Optional_Boolean (JS, +"willSaveWaitUntil", V.willSaveWaitUntil);
      Write_Optional_Boolean (JS, +"save", V.save);
      JS.End_Object;
   end Write_TextDocumentSyncOptions;

   --------------------
   -- Write_TextEdit --
   --------------------

   procedure Write_TextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      Span'Write (S, V.span);
      Write_String (JS, +"newText", V.newText);
      JS.End_Object;
   end Write_TextEdit;

   ---------------------------
   -- Write_TextEdit_Vector --
   ---------------------------

   procedure Write_TextEdit_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextEdit_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for Item of V loop
         TextEdit'Write (S, Item);
      end loop;
      JS.End_Array;
   end Write_TextEdit_Vector;

   -------------------------------------------
   -- Write_VersionedTextDocumentIdentifier --
   -------------------------------------------

   procedure Write_VersionedTextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : VersionedTextDocumentIdentifier)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      DocumentUri'Write (S, V.uri);
      Write_Number (JS, +"version", LSP_Number (V.version));
      JS.End_Object;
   end Write_VersionedTextDocumentIdentifier;

   ---------------------------------------
   -- Write_WorkspaceClientCapabilities --
   ---------------------------------------

   procedure Write_WorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Optional_Boolean (JS, +"applyEdit", V.applyEdit);
      JS.Key ("workspaceEdit");
      documentChanges'Write (S, V.workspaceEdit);
      JS.Key ("didChangeConfiguration");
      dynamicRegistration'Write (S, V.didChangeConfiguration);
      JS.Key ("didChangeWatchedFiles");
      dynamicRegistration'Write (S, V.didChangeWatchedFiles);
      JS.Key ("symbol");
      dynamicRegistration'Write (S, V.symbol);
      JS.Key ("executeCommand");
      dynamicRegistration'Write (S, V.executeCommand);
      JS.End_Object;
   end Write_WorkspaceClientCapabilities;

   -------------------------
   -- Write_WorkspaceEdit --
   -------------------------

   procedure Write_WorkspaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      if V.documentChanges.Is_Empty then
         JS.Key ("changes");

         if V.changes.Is_Empty then
            --  Special case for an empty result: return 'changes:{}'
            --  Without it JSON_Stream optimizes result to nothing.
            JS.Write (GNATCOLL.JSON.Create_Object);
         else
            JS.Start_Object;
            for Cursor in V.changes.Iterate loop
               JS.Key
                 (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String
                    (TextDocumentEdit_Maps.Key (Cursor)));
               JS.Start_Array;
               for Edit of V.changes (Cursor) loop
                  TextEdit'Write (S, Edit);
               end loop;
               JS.End_Array;
            end loop;
            JS.End_Object;
         end if;
      else
         JS.Key ("documentChanges");
         TextDocumentEdit_Vector'Write (S, V.documentChanges);
      end if;
      JS.End_Object;
   end Write_WorkspaceEdit;

   ---------------------------------
   -- Write_WorkspaceSymbolParams --
   ---------------------------------

   procedure Write_WorkspaceSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceSymbolParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_String (JS, +"query", V.query);
      JS.End_Object;
   end Write_WorkspaceSymbolParams;

   ----------------------------------------
   -- Read_ALS_Subprogram_And_References --
   ----------------------------------------

   procedure Read_ALS_Subprogram_And_References
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_Subprogram_And_References)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("location");
      Location'Read (S, V.loc);
      Read_String (JS, +"name", V.name);
      JS.Key ("refs");
      Location_Vector'Read (S, V.refs);
      JS.End_Object;
   end Read_ALS_Subprogram_And_References;

   -----------------------------------------
   -- Write_ALS_Subprogram_And_References --
   -----------------------------------------

   procedure Write_ALS_Subprogram_And_References
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_Subprogram_And_References)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("location");
      Location'Write (S, V.loc);
      Write_String (JS, +"name", V.name);
      JS.Key ("refs");
      Location_Vector'Write (S, V.refs);
      JS.End_Object;
   end Write_ALS_Subprogram_And_References;

   -----------------------------------------------
   -- Read_ALS_Subprogram_And_References_Vector --
   -----------------------------------------------

   procedure Read_ALS_Subprogram_And_References_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_Subprogram_And_References_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V.Clear;
      JS.Start_Array;

      while not JS.End_Of_Array loop
         declare
            Item : ALS_Subprogram_And_References;
         begin
            ALS_Subprogram_And_References'Read (S, Item);
            V.Append (Item);
         end;
      end loop;

      JS.End_Array;
   end Read_ALS_Subprogram_And_References_Vector;

   ------------------------------------------------
   -- Write_ALS_Subprogram_And_References_Vector --
   ------------------------------------------------

   procedure Write_ALS_Subprogram_And_References_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_Subprogram_And_References_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V.Is_Empty then
         JS.Write (GNATCOLL.JSON.Create (GNATCOLL.JSON.Empty_Array));
         return;
      end if;

      JS.Start_Array;
      for Item of V loop
         ALS_Subprogram_And_References'Write (S, Item);
      end loop;
      JS.End_Array;
   end Write_ALS_Subprogram_And_References_Vector;

end LSP.Messages;

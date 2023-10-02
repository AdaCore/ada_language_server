------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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

package body LSP.Ada_Empty_Handlers is

   -------------------------------
   -- On_AlsCheckSyntax_Request --
   -------------------------------

   overriding procedure On_AlsCheckSyntax_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.AlsCheckSyntaxParams)
   is
      Empty : LSP.Structures.AlsCheckSyntaxResult;
   begin
      Self.Sender.On_AlsCheckSyntax_Response (Id, Empty);
   end On_AlsCheckSyntax_Request;

   ------------------------------
   -- On_IncomingCalls_Request --
   ------------------------------

   overriding procedure On_IncomingCalls_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.CallHierarchyIncomingCallsParams)
   is
      Empty : LSP.Structures.CallHierarchyIncomingCall_Vector_Or_Null;
   begin
      Self.Sender.On_IncomingCalls_Response (Id, Empty);
   end On_IncomingCalls_Request;

   ------------------------------
   -- On_OutgoingCalls_Request --
   ------------------------------

   overriding procedure On_OutgoingCalls_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.CallHierarchyOutgoingCallsParams)
   is
      Empty : LSP.Structures.CallHierarchyOutgoingCall_Vector_Or_Null;
   begin
      Self.Sender.On_OutgoingCalls_Response (Id, Empty);
   end On_OutgoingCalls_Request;

   ------------------------------------
   -- On_Code_Action_Resolve_Request --
   ------------------------------------

   overriding procedure On_Code_Action_Resolve_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.CodeAction)
   is
      Empty : LSP.Structures.CodeAction;
   begin
      Self.Sender.On_Code_Action_Resolve_Response (Id, Empty);
   end On_Code_Action_Resolve_Request;

   ----------------------------------
   -- On_Code_Lens_Resolve_Request --
   ----------------------------------

   overriding procedure On_Code_Lens_Resolve_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.CodeLens)
   is
      Empty : LSP.Structures.CodeLens;
   begin
      Self.Sender.On_Code_Lens_Resolve_Response (Id, Empty);
   end On_Code_Lens_Resolve_Request;

   -----------------------------------
   -- On_Completion_Resolve_Request --
   -----------------------------------

   overriding procedure On_Completion_Resolve_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.CompletionItem)
   is
      Empty : LSP.Structures.CompletionItem;
   begin
      Self.Sender.On_Completion_Resolve_Response (Id, Empty);
   end On_Completion_Resolve_Request;

   -----------------------------
   -- On_Link_Resolve_Request --
   -----------------------------

   overriding procedure On_Link_Resolve_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.DocumentLink)
   is
      Empty : LSP.Structures.DocumentLink;
   begin
      Self.Sender.On_Link_Resolve_Response (Id, Empty);
   end On_Link_Resolve_Request;

   ---------------------------
   -- On_Initialize_Request --
   ---------------------------

   overriding procedure On_Initialize_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.InitializeParams)
   is
      Empty : LSP.Structures.InitializeResult;
   begin
      Self.Sender.On_Initialize_Response (Id, Empty);
   end On_Initialize_Request;

   ------------------------------
   -- On_Inlay_Resolve_Request --
   ------------------------------

   overriding procedure On_Inlay_Resolve_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.InlayHint)
   is
      Empty : LSP.Structures.InlayHint;
   begin
      Self.Sender.On_Inlay_Resolve_Response (Id, Empty);
   end On_Inlay_Resolve_Request;

   -------------------------
   -- On_Shutdown_Request --
   -------------------------

   overriding procedure On_Shutdown_Request
     (Self : in out Empty_Message_Handler;
      Id   :        LSP.Structures.Integer_Or_Virtual_String)
   is
      Empty : LSP.Structures.Null_Record;
   begin
      Self.Sender.On_Shutdown_Response (Id, Empty);
   end On_Shutdown_Request;

   ---------------------------
   -- On_CodeAction_Request --
   ---------------------------

   overriding procedure On_CodeAction_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.CodeActionParams)
   is
      Empty : LSP.Structures.Command_Or_CodeAction_Vector_Or_Null;
   begin
      Self.Sender.On_CodeAction_Response (Id, Empty);
   end On_CodeAction_Request;

   -------------------------
   -- On_CodeLens_Request --
   -------------------------

   overriding procedure On_CodeLens_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.CodeLensParams)
   is
      Empty : LSP.Structures.CodeLens_Vector_Or_Null;
   begin
      Self.Sender.On_CodeLens_Response (Id, Empty);
   end On_CodeLens_Request;

   ----------------------------------
   -- On_ColorPresentation_Request --
   ----------------------------------

   overriding procedure On_ColorPresentation_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.ColorPresentationParams)
   is
      Empty : LSP.Structures.ColorPresentation_Vector;
   begin
      Self.Sender.On_ColorPresentation_Response (Id, Empty);
   end On_ColorPresentation_Request;

   ---------------------------
   -- On_Completion_Request --
   ---------------------------

   overriding procedure On_Completion_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.CompletionParams)
   is
      Empty : LSP.Structures.Completion_Result;
   begin
      Self.Sender.On_Completion_Response (Id, Empty);
   end On_Completion_Request;

   ----------------------------
   -- On_Declaration_Request --
   ----------------------------

   overriding procedure On_Declaration_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.DeclarationParams)
   is
      Empty : LSP.Structures.Declaration_Result;
   begin
      Self.Sender.On_Declaration_Response (Id, Empty);
   end On_Declaration_Request;

   ---------------------------
   -- On_Definition_Request --
   ---------------------------

   overriding procedure On_Definition_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.DefinitionParams)
   is
      Empty : LSP.Structures.Definition_Result;
   begin
      Self.Sender.On_Definition_Response (Id, Empty);
   end On_Definition_Request;

   ---------------------------
   -- On_Diagnostic_Request --
   ---------------------------

   overriding procedure On_Diagnostic_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.DocumentDiagnosticParams)
   is
      Empty : LSP.Structures.DocumentDiagnosticReport;
   begin
      Self.Sender.On_Diagnostic_Response (Id, Empty);
   end On_Diagnostic_Request;

   ------------------------------
   -- On_DocumentColor_Request --
   ------------------------------

   overriding procedure On_DocumentColor_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.DocumentColorParams)
   is
      Empty : LSP.Structures.ColorInformation_Vector;
   begin
      Self.Sender.On_DocumentColor_Response (Id, Empty);
   end On_DocumentColor_Request;

   ----------------------------------
   -- On_DocumentHighlight_Request --
   ----------------------------------

   overriding procedure On_DocumentHighlight_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.DocumentHighlightParams)
   is
      Empty : LSP.Structures.DocumentHighlight_Vector_Or_Null;
   begin
      Self.Sender.On_DocumentHighlight_Response (Id, Empty);
   end On_DocumentHighlight_Request;

   -----------------------------
   -- On_DocumentLink_Request --
   -----------------------------

   overriding procedure On_DocumentLink_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.DocumentLinkParams)
   is
      Empty : LSP.Structures.DocumentLink_Vector_Or_Null;
   begin
      Self.Sender.On_DocumentLink_Response (Id, Empty);
   end On_DocumentLink_Request;

   -------------------------------
   -- On_DocumentSymbol_Request --
   -------------------------------

   overriding procedure On_DocumentSymbol_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.DocumentSymbolParams)
   is
      Empty : LSP.Structures.DocumentSymbol_Result;
   begin
      Self.Sender.On_DocumentSymbol_Response (Id, Empty);
   end On_DocumentSymbol_Request;

   -----------------------------
   -- On_FoldingRange_Request --
   -----------------------------

   overriding procedure On_FoldingRange_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.FoldingRangeParams)
   is
      Empty : LSP.Structures.FoldingRange_Vector_Or_Null;
   begin
      Self.Sender.On_FoldingRange_Response (Id, Empty);
   end On_FoldingRange_Request;

   ---------------------------
   -- On_Formatting_Request --
   ---------------------------

   overriding procedure On_Formatting_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.DocumentFormattingParams)
   is
      Empty : LSP.Structures.TextEdit_Vector_Or_Null;
   begin
      Self.Sender.On_Formatting_Response (Id, Empty);
   end On_Formatting_Request;

   ----------------------
   -- On_Hover_Request --
   ----------------------

   overriding procedure On_Hover_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.HoverParams)
   is
      Empty : LSP.Structures.Hover_Or_Null;
   begin
      Self.Sender.On_Hover_Response (Id, Empty);
   end On_Hover_Request;

   -------------------------------
   -- On_Implementation_Request --
   -------------------------------

   overriding procedure On_Implementation_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.ImplementationParams)
   is
      Empty : LSP.Structures.Definition_Result;
   begin
      Self.Sender.On_Implementation_Response (Id, Empty);
   end On_Implementation_Request;

   --------------------------
   -- On_InlayHint_Request --
   --------------------------

   overriding procedure On_InlayHint_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.InlayHintParams)
   is
      Empty : LSP.Structures.InlayHint_Vector_Or_Null;
   begin
      Self.Sender.On_InlayHint_Response (Id, Empty);
   end On_InlayHint_Request;

   ----------------------------
   -- On_InlineValue_Request --
   ----------------------------

   overriding procedure On_InlineValue_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.InlineValueParams)
   is
      Empty : LSP.Structures.InlineValue_Vector_Or_Null;
   begin
      Self.Sender.On_InlineValue_Response (Id, Empty);
   end On_InlineValue_Request;

   -----------------------------------
   -- On_LinkedEditingRange_Request --
   -----------------------------------

   overriding procedure On_LinkedEditingRange_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.LinkedEditingRangeParams)
   is
      Empty : LSP.Structures.LinkedEditingRanges_Or_Null;
   begin
      Self.Sender.On_LinkedEditingRange_Response (Id, Empty);
   end On_LinkedEditingRange_Request;

   ------------------------
   -- On_Moniker_Request --
   ------------------------

   overriding procedure On_Moniker_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.MonikerParams)
   is
      Empty : LSP.Structures.Moniker_Vector_Or_Null;
   begin
      Self.Sender.On_Moniker_Response (Id, Empty);
   end On_Moniker_Request;

   ---------------------------------
   -- On_OnTypeFormatting_Request --
   ---------------------------------

   overriding procedure On_OnTypeFormatting_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.DocumentOnTypeFormattingParams)
   is
      Empty : LSP.Structures.TextEdit_Vector_Or_Null;
   begin
      Self.Sender.On_OnTypeFormatting_Response (Id, Empty);
   end On_OnTypeFormatting_Request;

   -------------------------------------
   -- On_PrepareCallHierarchy_Request --
   -------------------------------------

   overriding procedure On_PrepareCallHierarchy_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.CallHierarchyPrepareParams)
   is
      Empty : LSP.Structures.CallHierarchyItem_Vector_Or_Null;
   begin
      Self.Sender.On_PrepareCallHierarchy_Response (Id, Empty);
   end On_PrepareCallHierarchy_Request;

   ------------------------------
   -- On_PrepareRename_Request --
   ------------------------------

   overriding procedure On_PrepareRename_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.PrepareRenameParams)
   is
      Empty : LSP.Structures.PrepareRenameResult_Or_Null;
   begin
      Self.Sender.On_PrepareRename_Response (Id, Empty);
   end On_PrepareRename_Request;

   -------------------------------------
   -- On_PrepareTypeHierarchy_Request --
   -------------------------------------

   overriding procedure On_PrepareTypeHierarchy_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.TypeHierarchyPrepareParams)
   is
      Empty : LSP.Structures.TypeHierarchyItem_Vector_Or_Null;
   begin
      Self.Sender.On_PrepareTypeHierarchy_Response (Id, Empty);
   end On_PrepareTypeHierarchy_Request;

   --------------------------------
   -- On_RangeFormatting_Request --
   --------------------------------

   overriding procedure On_RangeFormatting_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.DocumentRangeFormattingParams)
   is
      Empty : LSP.Structures.TextEdit_Vector_Or_Null;
   begin
      Self.Sender.On_RangeFormatting_Response (Id, Empty);
   end On_RangeFormatting_Request;

   ---------------------------
   -- On_References_Request --
   ---------------------------

   overriding procedure On_References_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.ReferenceParams)
   is
      Empty : LSP.Structures.Location_Vector_Or_Null;
   begin
      Self.Sender.On_References_Response (Id, Empty);
   end On_References_Request;

   -----------------------
   -- On_Rename_Request --
   -----------------------

   overriding procedure On_Rename_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.RenameParams)
   is
      Empty : LSP.Structures.WorkspaceEdit_Or_Null;
   begin
      Self.Sender.On_Rename_Response (Id, Empty);
   end On_Rename_Request;

   -------------------------------
   -- On_SelectionRange_Request --
   -------------------------------

   overriding procedure On_SelectionRange_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.SelectionRangeParams)
   is
      Empty : LSP.Structures.SelectionRange_Vector_Or_Null;
   begin
      Self.Sender.On_SelectionRange_Response (Id, Empty);
   end On_SelectionRange_Request;

   ----------------------------
   -- On_Tokens_Full_Request --
   ----------------------------

   overriding procedure On_Tokens_Full_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.SemanticTokensParams)
   is
      Empty : LSP.Structures.SemanticTokens_Or_Null;
   begin
      Self.Sender.On_Tokens_Full_Response (Id, Empty);
   end On_Tokens_Full_Request;

   -----------------------------
   -- On_Tokens_Delta_Request --
   -----------------------------

   overriding procedure On_Tokens_Delta_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.SemanticTokensDeltaParams)
   is
      Empty : LSP.Structures.Tokens_Delta_Result;
   begin
      Self.Sender.On_Tokens_Delta_Response (Id, Empty);
   end On_Tokens_Delta_Request;

   -----------------------------
   -- On_Tokens_Range_Request --
   -----------------------------

   overriding procedure On_Tokens_Range_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.SemanticTokensRangeParams)
   is
      Empty : LSP.Structures.SemanticTokens_Or_Null;
   begin
      Self.Sender.On_Tokens_Range_Response (Id, Empty);
   end On_Tokens_Range_Request;

   ------------------------------
   -- On_SignatureHelp_Request --
   ------------------------------

   overriding procedure On_SignatureHelp_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.SignatureHelpParams)
   is
      Empty : LSP.Structures.SignatureHelp_Or_Null;
   begin
      Self.Sender.On_SignatureHelp_Response (Id, Empty);
   end On_SignatureHelp_Request;

   -------------------------------
   -- On_TypeDefinition_Request --
   -------------------------------

   overriding procedure On_TypeDefinition_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.TypeDefinitionParams)
   is
      Empty : LSP.Structures.Definition_Result;
   begin
      Self.Sender.On_TypeDefinition_Response (Id, Empty);
   end On_TypeDefinition_Request;

   ----------------------------------
   -- On_WillSaveWaitUntil_Request --
   ----------------------------------

   overriding procedure On_WillSaveWaitUntil_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.WillSaveTextDocumentParams)
   is
      Empty : LSP.Structures.TextEdit_Vector_Or_Null;
   begin
      Self.Sender.On_WillSaveWaitUntil_Response (Id, Empty);
   end On_WillSaveWaitUntil_Request;

   -------------------------
   -- On_Subtypes_Request --
   -------------------------

   overriding procedure On_Subtypes_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.TypeHierarchySubtypesParams)
   is
      Empty : LSP.Structures.TypeHierarchyItem_Vector_Or_Null;
   begin
      Self.Sender.On_Subtypes_Response (Id, Empty);
   end On_Subtypes_Request;

   ---------------------------
   -- On_Supertypes_Request --
   ---------------------------

   overriding procedure On_Supertypes_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.TypeHierarchySupertypesParams)
   is
      Empty : LSP.Structures.TypeHierarchyItem_Vector_Or_Null;
   begin
      Self.Sender.On_Supertypes_Response (Id, Empty);
   end On_Supertypes_Request;

   -------------------------------------
   -- On_Workspace_Diagnostic_Request --
   -------------------------------------

   overriding procedure On_Workspace_Diagnostic_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.WorkspaceDiagnosticParams)
   is
      Empty : LSP.Structures.WorkspaceDiagnosticReport;
   begin
      Self.Sender.On_Workspace_Diagnostic_Response (Id, Empty);
   end On_Workspace_Diagnostic_Request;

   -------------------------------
   -- On_ExecuteCommand_Request --
   -------------------------------

   overriding procedure On_ExecuteCommand_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.ExecuteCommandParams)
   is
      Empty : LSP.Structures.LSPAny_Or_Null;
   begin
      Self.Sender.On_ExecuteCommand_Response (Id, Empty);
   end On_ExecuteCommand_Request;

   -----------------------
   -- On_Symbol_Request --
   -----------------------

   overriding procedure On_Symbol_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.WorkspaceSymbolParams)
   is
      Empty : LSP.Structures.Symbol_Result;
   begin
      Self.Sender.On_Symbol_Response (Id, Empty);
   end On_Symbol_Request;

   --------------------------------
   -- On_WillCreateFiles_Request --
   --------------------------------

   overriding procedure On_WillCreateFiles_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.CreateFilesParams)
   is
      Empty : LSP.Structures.WorkspaceEdit_Or_Null;
   begin
      Self.Sender.On_WillCreateFiles_Response (Id, Empty);
   end On_WillCreateFiles_Request;

   --------------------------------
   -- On_WillDeleteFiles_Request --
   --------------------------------

   overriding procedure On_WillDeleteFiles_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.DeleteFilesParams)
   is
      Empty : LSP.Structures.WorkspaceEdit_Or_Null;
   begin
      Self.Sender.On_WillDeleteFiles_Response (Id, Empty);
   end On_WillDeleteFiles_Request;

   --------------------------------
   -- On_WillRenameFiles_Request --
   --------------------------------

   overriding procedure On_WillRenameFiles_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.RenameFilesParams)
   is
      Empty : LSP.Structures.WorkspaceEdit_Or_Null;
   begin
      Self.Sender.On_WillRenameFiles_Response (Id, Empty);
   end On_WillRenameFiles_Request;

   -------------------------------
   -- On_Symbol_Resolve_Request --
   -------------------------------

   overriding procedure On_Symbol_Resolve_Request
     (Self  : in out Empty_Message_Handler;
      Id    :        LSP.Structures.Integer_Or_Virtual_String;
      Value :        LSP.Structures.WorkspaceSymbol)
   is
      Empty : LSP.Structures.WorkspaceSymbol;
   begin
      Self.Sender.On_Symbol_Resolve_Response (Id, Empty);
   end On_Symbol_Resolve_Request;

end LSP.Ada_Empty_Handlers;

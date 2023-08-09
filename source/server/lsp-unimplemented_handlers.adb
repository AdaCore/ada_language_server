------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
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

package body LSP.Unimplemented_Handlers is

   ------------------------------------
   -- On_Code_Action_Resolve_Request --
   ------------------------------------

   overriding procedure On_Code_Action_Resolve_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeAction)
   is
   begin
      Self.Implemented := False;

   end On_Code_Action_Resolve_Request;

   ----------------------------------
   -- On_Code_Lens_Resolve_Request --
   ----------------------------------

   overriding procedure On_Code_Lens_Resolve_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLens)
   is
   begin
      Self.Implemented := False;

   end On_Code_Lens_Resolve_Request;

   ---------------------------
   -- On_CodeAction_Request --
   ---------------------------

   overriding procedure On_CodeAction_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeActionParams)
   is
   begin
      Self.Implemented := False;

   end On_CodeAction_Request;

   -------------------------
   -- On_CodeLens_Request --
   -------------------------

   overriding procedure On_CodeLens_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLensParams)
   is
   begin
      Self.Implemented := False;

   end On_CodeLens_Request;

   ----------------------------------
   -- On_ColorPresentation_Request --
   ----------------------------------

   overriding procedure On_ColorPresentation_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ColorPresentationParams)
   is
   begin
      Self.Implemented := False;

   end On_ColorPresentation_Request;

   ---------------------------
   -- On_Completion_Request --
   ---------------------------

   overriding procedure On_Completion_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionParams)
   is
   begin
      Self.Implemented := False;

   end On_Completion_Request;

   -----------------------------------
   -- On_Completion_Resolve_Request --
   -----------------------------------

   overriding procedure On_Completion_Resolve_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionItem)
   is
   begin
      Self.Implemented := False;

   end On_Completion_Resolve_Request;

   ----------------------------
   -- On_Declaration_Request --
   ----------------------------

   overriding procedure On_Declaration_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DeclarationParams)
   is
   begin
      Self.Implemented := False;

   end On_Declaration_Request;

   ---------------------------
   -- On_Definition_Request --
   ---------------------------

   overriding procedure On_Definition_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DefinitionParams)
   is
   begin
      Self.Implemented := False;

   end On_Definition_Request;

   ---------------------------
   -- On_Diagnostic_Request --
   ---------------------------

   overriding procedure On_Diagnostic_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentDiagnosticParams)
   is
   begin
      Self.Implemented := False;

   end On_Diagnostic_Request;

   ------------------------------
   -- On_DocumentColor_Request --
   ------------------------------

   overriding procedure On_DocumentColor_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentColorParams)
   is
   begin
      Self.Implemented := False;

   end On_DocumentColor_Request;

   ----------------------------------
   -- On_DocumentHighlight_Request --
   ----------------------------------

   overriding procedure On_DocumentHighlight_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentHighlightParams)
   is
   begin
      Self.Implemented := False;

   end On_DocumentHighlight_Request;

   -----------------------------
   -- On_DocumentLink_Request --
   -----------------------------

   overriding procedure On_DocumentLink_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLinkParams)
   is
   begin
      Self.Implemented := False;

   end On_DocumentLink_Request;

   -------------------------------
   -- On_DocumentSymbol_Request --
   -------------------------------

   overriding procedure On_DocumentSymbol_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentSymbolParams)
   is
   begin
      Self.Implemented := False;

   end On_DocumentSymbol_Request;

   -------------------------------
   -- On_ExecuteCommand_Request --
   -------------------------------

   overriding procedure On_ExecuteCommand_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ExecuteCommandParams)
   is
   begin
      Self.Implemented := False;

   end On_ExecuteCommand_Request;

   -----------------------------
   -- On_FoldingRange_Request --
   -----------------------------

   overriding procedure On_FoldingRange_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.FoldingRangeParams)
   is
   begin
      Self.Implemented := False;

   end On_FoldingRange_Request;

   ---------------------------
   -- On_Formatting_Request --
   ---------------------------

   overriding procedure On_Formatting_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentFormattingParams)
   is
   begin
      Self.Implemented := False;

   end On_Formatting_Request;

   ---------------------
   -- On_Full_Request --
   ---------------------

   overriding procedure On_Full_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensParams)
   is
   begin
      Self.Implemented := False;

   end On_Full_Request;

   ----------------------
   -- On_Hover_Request --
   ----------------------

   overriding procedure On_Hover_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.HoverParams)
   is
   begin
      Self.Implemented := False;

   end On_Hover_Request;

   -------------------------------
   -- On_Implementation_Request --
   -------------------------------

   overriding procedure On_Implementation_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ImplementationParams)
   is
   begin
      Self.Implemented := False;

   end On_Implementation_Request;

   ------------------------------
   -- On_IncomingCalls_Request --
   ------------------------------

   overriding procedure On_IncomingCalls_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyIncomingCallsParams)
   is
   begin
      Self.Implemented := False;

   end On_IncomingCalls_Request;

   ---------------------------
   -- On_Initialize_Request --
   ---------------------------

   overriding procedure On_Initialize_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InitializeParams)
   is
   begin
      Self.Implemented := False;

   end On_Initialize_Request;

   ------------------------------
   -- On_Inlay_Resolve_Request --
   ------------------------------

   overriding procedure On_Inlay_Resolve_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHint)
   is
   begin
      Self.Implemented := False;

   end On_Inlay_Resolve_Request;

   --------------------------
   -- On_InlayHint_Request --
   --------------------------

   overriding procedure On_InlayHint_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHintParams)
   is
   begin
      Self.Implemented := False;

   end On_InlayHint_Request;

   ----------------------------
   -- On_InlineValue_Request --
   ----------------------------

   overriding procedure On_InlineValue_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlineValueParams)
   is
   begin
      Self.Implemented := False;

   end On_InlineValue_Request;

   -----------------------------
   -- On_Link_Resolve_Request --
   -----------------------------

   overriding procedure On_Link_Resolve_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLink)
   is
   begin
      Self.Implemented := False;

   end On_Link_Resolve_Request;

   -----------------------------------
   -- On_LinkedEditingRange_Request --
   -----------------------------------

   overriding procedure On_LinkedEditingRange_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.LinkedEditingRangeParams)
   is
   begin
      Self.Implemented := False;

   end On_LinkedEditingRange_Request;

   ------------------------
   -- On_Moniker_Request --
   ------------------------

   overriding procedure On_Moniker_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.MonikerParams)
   is
   begin
      Self.Implemented := False;

   end On_Moniker_Request;

   ---------------------------------
   -- On_OnTypeFormatting_Request --
   ---------------------------------

   overriding procedure On_OnTypeFormatting_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentOnTypeFormattingParams)
   is
   begin
      Self.Implemented := False;

   end On_OnTypeFormatting_Request;

   ------------------------------
   -- On_OutgoingCalls_Request --
   ------------------------------

   overriding procedure On_OutgoingCalls_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyOutgoingCallsParams)
   is
   begin
      Self.Implemented := False;

   end On_OutgoingCalls_Request;

   -------------------------------------
   -- On_PrepareCallHierarchy_Request --
   -------------------------------------

   overriding procedure On_PrepareCallHierarchy_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyPrepareParams)
   is
   begin
      Self.Implemented := False;

   end On_PrepareCallHierarchy_Request;

   ------------------------------
   -- On_PrepareRename_Request --
   ------------------------------

   overriding procedure On_PrepareRename_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.PrepareRenameParams)
   is
   begin
      Self.Implemented := False;

   end On_PrepareRename_Request;

   -------------------------------------
   -- On_PrepareTypeHierarchy_Request --
   -------------------------------------

   overriding procedure On_PrepareTypeHierarchy_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyPrepareParams)
   is
   begin
      Self.Implemented := False;

   end On_PrepareTypeHierarchy_Request;

   --------------------------------
   -- On_RangeFormatting_Request --
   --------------------------------

   overriding procedure On_RangeFormatting_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentRangeFormattingParams)
   is
   begin
      Self.Implemented := False;

   end On_RangeFormatting_Request;

   ---------------------------
   -- On_References_Request --
   ---------------------------

   overriding procedure On_References_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ReferenceParams)
   is
   begin
      Self.Implemented := False;

   end On_References_Request;

   -----------------------
   -- On_Rename_Request --
   -----------------------

   overriding procedure On_Rename_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.RenameParams)
   is
   begin
      Self.Implemented := False;

   end On_Rename_Request;

   -------------------------------
   -- On_SelectionRange_Request --
   -------------------------------

   overriding procedure On_SelectionRange_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SelectionRangeParams)
   is
   begin
      Self.Implemented := False;

   end On_SelectionRange_Request;

   -------------------------
   -- On_Shutdown_Request --
   -------------------------

   overriding procedure On_Shutdown_Request
     (Self : in out Unimplemented_Handler;
      Id   :        LSP.Structures.Integer_Or_Virtual_String)
   is
   begin
      Self.Implemented := False;

   end On_Shutdown_Request;

   ------------------------------
   -- On_SignatureHelp_Request --
   ------------------------------

   overriding procedure On_SignatureHelp_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SignatureHelpParams)
   is
   begin
      Self.Implemented := False;

   end On_SignatureHelp_Request;

   -------------------------
   -- On_Subtypes_Request --
   -------------------------

   overriding procedure On_Subtypes_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchySubtypesParams)
   is
   begin
      Self.Implemented := False;

   end On_Subtypes_Request;

   ---------------------------
   -- On_Supertypes_Request --
   ---------------------------

   overriding procedure On_Supertypes_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchySupertypesParams)
   is
   begin
      Self.Implemented := False;

   end On_Supertypes_Request;

   -----------------------
   -- On_Symbol_Request --
   -----------------------

   overriding procedure On_Symbol_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceSymbolParams)
   is
   begin
      Self.Implemented := False;

   end On_Symbol_Request;

   -------------------------------
   -- On_Symbol_Resolve_Request --
   -------------------------------

   overriding procedure On_Symbol_Resolve_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceSymbol)
   is
   begin
      Self.Implemented := False;

   end On_Symbol_Resolve_Request;

   -----------------------------
   -- On_Tokens_Delta_Request --
   -----------------------------

   overriding procedure On_Tokens_Delta_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensDeltaParams)
   is
   begin
      Self.Implemented := False;

   end On_Tokens_Delta_Request;

   -----------------------------
   -- On_Tokens_Range_Request --
   -----------------------------

   overriding procedure On_Tokens_Range_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensRangeParams)
   is
   begin
      Self.Implemented := False;

   end On_Tokens_Range_Request;

   -------------------------------
   -- On_TypeDefinition_Request --
   -------------------------------

   overriding procedure On_TypeDefinition_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeDefinitionParams)
   is
   begin
      Self.Implemented := False;

   end On_TypeDefinition_Request;

   --------------------------------
   -- On_WillCreateFiles_Request --
   --------------------------------

   overriding procedure On_WillCreateFiles_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CreateFilesParams)
   is
   begin
      Self.Implemented := False;

   end On_WillCreateFiles_Request;

   --------------------------------
   -- On_WillDeleteFiles_Request --
   --------------------------------

   overriding procedure On_WillDeleteFiles_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DeleteFilesParams)
   is
   begin
      Self.Implemented := False;

   end On_WillDeleteFiles_Request;

   --------------------------------
   -- On_WillRenameFiles_Request --
   --------------------------------

   overriding procedure On_WillRenameFiles_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.RenameFilesParams)
   is
   begin
      Self.Implemented := False;

   end On_WillRenameFiles_Request;

   ----------------------------------
   -- On_WillSaveWaitUntil_Request --
   ----------------------------------

   overriding procedure On_WillSaveWaitUntil_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WillSaveTextDocumentParams)
   is
   begin
      Self.Implemented := False;

   end On_WillSaveWaitUntil_Request;

   -------------------------------------
   -- On_Workspace_Diagnostic_Request --
   -------------------------------------

   overriding procedure On_Workspace_Diagnostic_Request
     (Self  : in out Unimplemented_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceDiagnosticParams)
   is
   begin
      Self.Implemented := False;

   end On_Workspace_Diagnostic_Request;

end LSP.Unimplemented_Handlers;

--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with Minimal_Perfect_Hash;
with LSP.Inputs;
with LSP.Input_Tools;
with LSP.Structures;

with LSP.Server_Requests.IncomingCalls;
with LSP.Server_Requests.OutgoingCalls;
with LSP.Server_Requests.Code_Action_Resolve;
with LSP.Server_Requests.Code_Lens_Resolve;
with LSP.Server_Requests.Completion_Resolve;
with LSP.Server_Requests.Link_Resolve;
with LSP.Server_Requests.Initialize;
with LSP.Server_Requests.Inlay_Resolve;
with LSP.Server_Requests.Shutdown;
with LSP.Server_Requests.CodeAction;
with LSP.Server_Requests.CodeLens;
with LSP.Server_Requests.ColorPresentation;
with LSP.Server_Requests.Completion;
with LSP.Server_Requests.Declaration;
with LSP.Server_Requests.Definition;
with LSP.Server_Requests.Diagnostic;
with LSP.Server_Requests.DocumentColor;
with LSP.Server_Requests.DocumentHighlight;
with LSP.Server_Requests.DocumentLink;
with LSP.Server_Requests.DocumentSymbol;
with LSP.Server_Requests.FoldingRange;
with LSP.Server_Requests.Formatting;
with LSP.Server_Requests.Hover;
with LSP.Server_Requests.Implementation;
with LSP.Server_Requests.InlayHint;
with LSP.Server_Requests.InlineValue;
with LSP.Server_Requests.LinkedEditingRange;
with LSP.Server_Requests.Moniker;
with LSP.Server_Requests.OnTypeFormatting;
with LSP.Server_Requests.PrepareCallHierarchy;
with LSP.Server_Requests.PrepareRename;
with LSP.Server_Requests.PrepareTypeHierarchy;
with LSP.Server_Requests.RangeFormatting;
with LSP.Server_Requests.References;
with LSP.Server_Requests.Rename;
with LSP.Server_Requests.SelectionRange;
with LSP.Server_Requests.Tokens_Full;
with LSP.Server_Requests.Tokens_Delta;
with LSP.Server_Requests.Tokens_Range;
with LSP.Server_Requests.SignatureHelp;
with LSP.Server_Requests.TypeDefinition;
with LSP.Server_Requests.WillSaveWaitUntil;
with LSP.Server_Requests.Subtypes;
with LSP.Server_Requests.Supertypes;
with LSP.Server_Requests.Workspace_Diagnostic;
with LSP.Server_Requests.ExecuteCommand;
with LSP.Server_Requests.Symbol;
with LSP.Server_Requests.WillCreateFiles;
with LSP.Server_Requests.WillDeleteFiles;
with LSP.Server_Requests.WillRenameFiles;
with LSP.Server_Requests.Symbol_Resolve;

package body LSP.Server_Request_Readers is

   package Method_Map is new Minimal_Perfect_Hash
     (["callHierarchy/incomingCalls",
      "callHierarchy/outgoingCalls",
      "codeAction/resolve",
      "codeLens/resolve",
      "completionItem/resolve",
      "documentLink/resolve",
      "initialize",
      "inlayHint/resolve",
      "shutdown",
      "textDocument/codeAction",
      "textDocument/codeLens",
      "textDocument/colorPresentation",
      "textDocument/completion",
      "textDocument/declaration",
      "textDocument/definition",
      "textDocument/diagnostic",
      "textDocument/documentColor",
      "textDocument/documentHighlight",
      "textDocument/documentLink",
      "textDocument/documentSymbol",
      "textDocument/foldingRange",
      "textDocument/formatting",
      "textDocument/hover",
      "textDocument/implementation",
      "textDocument/inlayHint",
      "textDocument/inlineValue",
      "textDocument/linkedEditingRange",
      "textDocument/moniker",
      "textDocument/onTypeFormatting",
      "textDocument/prepareCallHierarchy",
      "textDocument/prepareRename",
      "textDocument/prepareTypeHierarchy",
      "textDocument/rangeFormatting",
      "textDocument/references",
      "textDocument/rename",
      "textDocument/selectionRange",
      "textDocument/semanticTokens/full",
      "textDocument/semanticTokens/full/delta",
      "textDocument/semanticTokens/range",
      "textDocument/signatureHelp",
      "textDocument/typeDefinition",
      "textDocument/willSaveWaitUntil",
      "typeHierarchy/subtypes",
      "typeHierarchy/supertypes",
      "workspace/diagnostic",
      "workspace/executeCommand",
      "workspace/symbol",
      "workspace/willCreateFiles",
      "workspace/willDeleteFiles",
      "workspace/willRenameFiles",
      "workspaceSymbol/resolve"]);

   procedure Initialize is
   begin
      Method_Map.Initialize;
   end Initialize;

   procedure Read_IncomingCalls is new LSP.Input_Tools.Read_Request
     (LSP.Structures.CallHierarchyIncomingCallsParams,
      "callHierarchy/incomingCalls",
      LSP.Inputs.Read_CallHierarchyIncomingCallsParams);

   procedure Read_OutgoingCalls is new LSP.Input_Tools.Read_Request
     (LSP.Structures.CallHierarchyOutgoingCallsParams,
      "callHierarchy/outgoingCalls",
      LSP.Inputs.Read_CallHierarchyOutgoingCallsParams);

   procedure Read_Code_Action_Resolve is new LSP.Input_Tools.Read_Request
     (LSP.Structures.CodeAction, "codeAction/resolve",
      LSP.Inputs.Read_CodeAction);

   procedure Read_Code_Lens_Resolve is new LSP.Input_Tools.Read_Request
     (LSP.Structures.CodeLens, "codeLens/resolve", LSP.Inputs.Read_CodeLens);

   procedure Read_Completion_Resolve is new LSP.Input_Tools.Read_Request
     (LSP.Structures.CompletionItem, "completionItem/resolve",
      LSP.Inputs.Read_CompletionItem);

   procedure Read_Link_Resolve is new LSP.Input_Tools.Read_Request
     (LSP.Structures.DocumentLink, "documentLink/resolve",
      LSP.Inputs.Read_DocumentLink);

   procedure Read_Initialize is new LSP.Input_Tools.Read_Request
     (LSP.Structures.InitializeParams, "initialize",
      LSP.Inputs.Read_InitializeParams);

   procedure Read_Inlay_Resolve is new LSP.Input_Tools.Read_Request
     (LSP.Structures.InlayHint, "inlayHint/resolve",
      LSP.Inputs.Read_InlayHint);

   procedure Read_Shutdown
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Id      : in out LSP.Structures.Integer_Or_Virtual_String);

   procedure Read_Shutdown
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Id      : in out LSP.Structures.Integer_Or_Virtual_String) is
   begin
      LSP.Input_Tools.Read_Null_Request (Handler, "shutdown", Id);
   end Read_Shutdown;

   procedure Read_CodeAction is new LSP.Input_Tools.Read_Request
     (LSP.Structures.CodeActionParams, "textDocument/codeAction",
      LSP.Inputs.Read_CodeActionParams);

   procedure Read_CodeLens is new LSP.Input_Tools.Read_Request
     (LSP.Structures.CodeLensParams, "textDocument/codeLens",
      LSP.Inputs.Read_CodeLensParams);

   procedure Read_ColorPresentation is new LSP.Input_Tools.Read_Request
     (LSP.Structures.ColorPresentationParams, "textDocument/colorPresentation",
      LSP.Inputs.Read_ColorPresentationParams);

   procedure Read_Completion is new LSP.Input_Tools.Read_Request
     (LSP.Structures.CompletionParams, "textDocument/completion",
      LSP.Inputs.Read_CompletionParams);

   procedure Read_Declaration is new LSP.Input_Tools.Read_Request
     (LSP.Structures.DeclarationParams, "textDocument/declaration",
      LSP.Inputs.Read_DeclarationParams);

   procedure Read_Definition is new LSP.Input_Tools.Read_Request
     (LSP.Structures.DefinitionParams, "textDocument/definition",
      LSP.Inputs.Read_DefinitionParams);

   procedure Read_Diagnostic is new LSP.Input_Tools.Read_Request
     (LSP.Structures.DocumentDiagnosticParams, "textDocument/diagnostic",
      LSP.Inputs.Read_DocumentDiagnosticParams);

   procedure Read_DocumentColor is new LSP.Input_Tools.Read_Request
     (LSP.Structures.DocumentColorParams, "textDocument/documentColor",
      LSP.Inputs.Read_DocumentColorParams);

   procedure Read_DocumentHighlight is new LSP.Input_Tools.Read_Request
     (LSP.Structures.DocumentHighlightParams, "textDocument/documentHighlight",
      LSP.Inputs.Read_DocumentHighlightParams);

   procedure Read_DocumentLink is new LSP.Input_Tools.Read_Request
     (LSP.Structures.DocumentLinkParams, "textDocument/documentLink",
      LSP.Inputs.Read_DocumentLinkParams);

   procedure Read_DocumentSymbol is new LSP.Input_Tools.Read_Request
     (LSP.Structures.DocumentSymbolParams, "textDocument/documentSymbol",
      LSP.Inputs.Read_DocumentSymbolParams);

   procedure Read_FoldingRange is new LSP.Input_Tools.Read_Request
     (LSP.Structures.FoldingRangeParams, "textDocument/foldingRange",
      LSP.Inputs.Read_FoldingRangeParams);

   procedure Read_Formatting is new LSP.Input_Tools.Read_Request
     (LSP.Structures.DocumentFormattingParams, "textDocument/formatting",
      LSP.Inputs.Read_DocumentFormattingParams);

   procedure Read_Hover is new LSP.Input_Tools.Read_Request
     (LSP.Structures.HoverParams, "textDocument/hover",
      LSP.Inputs.Read_HoverParams);

   procedure Read_Implementation is new LSP.Input_Tools.Read_Request
     (LSP.Structures.ImplementationParams, "textDocument/implementation",
      LSP.Inputs.Read_ImplementationParams);

   procedure Read_InlayHint is new LSP.Input_Tools.Read_Request
     (LSP.Structures.InlayHintParams, "textDocument/inlayHint",
      LSP.Inputs.Read_InlayHintParams);

   procedure Read_InlineValue is new LSP.Input_Tools.Read_Request
     (LSP.Structures.InlineValueParams, "textDocument/inlineValue",
      LSP.Inputs.Read_InlineValueParams);

   procedure Read_LinkedEditingRange is new LSP.Input_Tools.Read_Request
     (LSP.Structures.LinkedEditingRangeParams,
      "textDocument/linkedEditingRange",
      LSP.Inputs.Read_LinkedEditingRangeParams);

   procedure Read_Moniker is new LSP.Input_Tools.Read_Request
     (LSP.Structures.MonikerParams, "textDocument/moniker",
      LSP.Inputs.Read_MonikerParams);

   procedure Read_OnTypeFormatting is new LSP.Input_Tools.Read_Request
     (LSP.Structures.DocumentOnTypeFormattingParams,
      "textDocument/onTypeFormatting",
      LSP.Inputs.Read_DocumentOnTypeFormattingParams);

   procedure Read_PrepareCallHierarchy is new LSP.Input_Tools.Read_Request
     (LSP.Structures.CallHierarchyPrepareParams,
      "textDocument/prepareCallHierarchy",
      LSP.Inputs.Read_CallHierarchyPrepareParams);

   procedure Read_PrepareRename is new LSP.Input_Tools.Read_Request
     (LSP.Structures.PrepareRenameParams, "textDocument/prepareRename",
      LSP.Inputs.Read_PrepareRenameParams);

   procedure Read_PrepareTypeHierarchy is new LSP.Input_Tools.Read_Request
     (LSP.Structures.TypeHierarchyPrepareParams,
      "textDocument/prepareTypeHierarchy",
      LSP.Inputs.Read_TypeHierarchyPrepareParams);

   procedure Read_RangeFormatting is new LSP.Input_Tools.Read_Request
     (LSP.Structures.DocumentRangeFormattingParams,
      "textDocument/rangeFormatting",
      LSP.Inputs.Read_DocumentRangeFormattingParams);

   procedure Read_References is new LSP.Input_Tools.Read_Request
     (LSP.Structures.ReferenceParams, "textDocument/references",
      LSP.Inputs.Read_ReferenceParams);

   procedure Read_Rename is new LSP.Input_Tools.Read_Request
     (LSP.Structures.RenameParams, "textDocument/rename",
      LSP.Inputs.Read_RenameParams);

   procedure Read_SelectionRange is new LSP.Input_Tools.Read_Request
     (LSP.Structures.SelectionRangeParams, "textDocument/selectionRange",
      LSP.Inputs.Read_SelectionRangeParams);

   procedure Read_Tokens_Full is new LSP.Input_Tools.Read_Request
     (LSP.Structures.SemanticTokensParams, "textDocument/semanticTokens/full",
      LSP.Inputs.Read_SemanticTokensParams);

   procedure Read_Tokens_Delta is new LSP.Input_Tools.Read_Request
     (LSP.Structures.SemanticTokensDeltaParams,
      "textDocument/semanticTokens/full/delta",
      LSP.Inputs.Read_SemanticTokensDeltaParams);

   procedure Read_Tokens_Range is new LSP.Input_Tools.Read_Request
     (LSP.Structures.SemanticTokensRangeParams,
      "textDocument/semanticTokens/range",
      LSP.Inputs.Read_SemanticTokensRangeParams);

   procedure Read_SignatureHelp is new LSP.Input_Tools.Read_Request
     (LSP.Structures.SignatureHelpParams, "textDocument/signatureHelp",
      LSP.Inputs.Read_SignatureHelpParams);

   procedure Read_TypeDefinition is new LSP.Input_Tools.Read_Request
     (LSP.Structures.TypeDefinitionParams, "textDocument/typeDefinition",
      LSP.Inputs.Read_TypeDefinitionParams);

   procedure Read_WillSaveWaitUntil is new LSP.Input_Tools.Read_Request
     (LSP.Structures.WillSaveTextDocumentParams,
      "textDocument/willSaveWaitUntil",
      LSP.Inputs.Read_WillSaveTextDocumentParams);

   procedure Read_Subtypes is new LSP.Input_Tools.Read_Request
     (LSP.Structures.TypeHierarchySubtypesParams, "typeHierarchy/subtypes",
      LSP.Inputs.Read_TypeHierarchySubtypesParams);

   procedure Read_Supertypes is new LSP.Input_Tools.Read_Request
     (LSP.Structures.TypeHierarchySupertypesParams, "typeHierarchy/supertypes",
      LSP.Inputs.Read_TypeHierarchySupertypesParams);

   procedure Read_Workspace_Diagnostic is new LSP.Input_Tools.Read_Request
     (LSP.Structures.WorkspaceDiagnosticParams, "workspace/diagnostic",
      LSP.Inputs.Read_WorkspaceDiagnosticParams);

   procedure Read_ExecuteCommand is new LSP.Input_Tools.Read_Request
     (LSP.Structures.ExecuteCommandParams, "workspace/executeCommand",
      LSP.Inputs.Read_ExecuteCommandParams);

   procedure Read_Symbol is new LSP.Input_Tools.Read_Request
     (LSP.Structures.WorkspaceSymbolParams, "workspace/symbol",
      LSP.Inputs.Read_WorkspaceSymbolParams);

   procedure Read_WillCreateFiles is new LSP.Input_Tools.Read_Request
     (LSP.Structures.CreateFilesParams, "workspace/willCreateFiles",
      LSP.Inputs.Read_CreateFilesParams);

   procedure Read_WillDeleteFiles is new LSP.Input_Tools.Read_Request
     (LSP.Structures.DeleteFilesParams, "workspace/willDeleteFiles",
      LSP.Inputs.Read_DeleteFilesParams);

   procedure Read_WillRenameFiles is new LSP.Input_Tools.Read_Request
     (LSP.Structures.RenameFilesParams, "workspace/willRenameFiles",
      LSP.Inputs.Read_RenameFilesParams);

   procedure Read_Symbol_Resolve is new LSP.Input_Tools.Read_Request
     (LSP.Structures.WorkspaceSymbol, "workspaceSymbol/resolve",
      LSP.Inputs.Read_WorkspaceSymbol);

   function Read_Request
     (Input  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Method : VSS.Strings.Virtual_String)
      return LSP.Server_Requests.Server_Request'Class is
      Index : constant Natural := Method_Map.Get_Index (Method);
   begin
      case Index is
         when 1 =>  --  callHierarchy/incomingCalls
            return Result : LSP.Server_Requests.IncomingCalls.Request do
               Read_IncomingCalls (Input, Result.Id, Result.Params);
            end return;

         when 2 =>  --  callHierarchy/outgoingCalls
            return Result : LSP.Server_Requests.OutgoingCalls.Request do
               Read_OutgoingCalls (Input, Result.Id, Result.Params);
            end return;

         when 3 =>  --  codeAction/resolve
            return Result : LSP.Server_Requests.Code_Action_Resolve.Request do
               Read_Code_Action_Resolve (Input, Result.Id, Result.Params);
            end return;

         when 4 =>  --  codeLens/resolve
            return Result : LSP.Server_Requests.Code_Lens_Resolve.Request do
               Read_Code_Lens_Resolve (Input, Result.Id, Result.Params);
            end return;

         when 5 =>  --  completionItem/resolve
            return Result : LSP.Server_Requests.Completion_Resolve.Request do
               Read_Completion_Resolve (Input, Result.Id, Result.Params);
            end return;

         when 6 =>  --  documentLink/resolve
            return Result : LSP.Server_Requests.Link_Resolve.Request do
               Read_Link_Resolve (Input, Result.Id, Result.Params);
            end return;

         when 7 =>  --  initialize
            return Result : LSP.Server_Requests.Initialize.Request do
               Read_Initialize (Input, Result.Id, Result.Params);
            end return;

         when 8 =>  --  inlayHint/resolve
            return Result : LSP.Server_Requests.Inlay_Resolve.Request do
               Read_Inlay_Resolve (Input, Result.Id, Result.Params);
            end return;

         when 9 =>  --  shutdown
            return Result : LSP.Server_Requests.Shutdown.Request do
               Read_Shutdown (Input, Result.Id);
            end return;

         when 10 =>  --  textDocument/codeAction
            return Result : LSP.Server_Requests.CodeAction.Request do
               Read_CodeAction (Input, Result.Id, Result.Params);
            end return;

         when 11 =>  --  textDocument/codeLens
            return Result : LSP.Server_Requests.CodeLens.Request do
               Read_CodeLens (Input, Result.Id, Result.Params);
            end return;

         when 12 =>  --  textDocument/colorPresentation
            return Result : LSP.Server_Requests.ColorPresentation.Request do
               Read_ColorPresentation (Input, Result.Id, Result.Params);
            end return;

         when 13 =>  --  textDocument/completion
            return Result : LSP.Server_Requests.Completion.Request do
               Read_Completion (Input, Result.Id, Result.Params);
            end return;

         when 14 =>  --  textDocument/declaration
            return Result : LSP.Server_Requests.Declaration.Request do
               Read_Declaration (Input, Result.Id, Result.Params);
            end return;

         when 15 =>  --  textDocument/definition
            return Result : LSP.Server_Requests.Definition.Request do
               Read_Definition (Input, Result.Id, Result.Params);
            end return;

         when 16 =>  --  textDocument/diagnostic
            return Result : LSP.Server_Requests.Diagnostic.Request do
               Read_Diagnostic (Input, Result.Id, Result.Params);
            end return;

         when 17 =>  --  textDocument/documentColor
            return Result : LSP.Server_Requests.DocumentColor.Request do
               Read_DocumentColor (Input, Result.Id, Result.Params);
            end return;

         when 18 =>  --  textDocument/documentHighlight
            return Result : LSP.Server_Requests.DocumentHighlight.Request do
               Read_DocumentHighlight (Input, Result.Id, Result.Params);
            end return;

         when 19 =>  --  textDocument/documentLink
            return Result : LSP.Server_Requests.DocumentLink.Request do
               Read_DocumentLink (Input, Result.Id, Result.Params);
            end return;

         when 20 =>  --  textDocument/documentSymbol
            return Result : LSP.Server_Requests.DocumentSymbol.Request do
               Read_DocumentSymbol (Input, Result.Id, Result.Params);
            end return;

         when 21 =>  --  textDocument/foldingRange
            return Result : LSP.Server_Requests.FoldingRange.Request do
               Read_FoldingRange (Input, Result.Id, Result.Params);
            end return;

         when 22 =>  --  textDocument/formatting
            return Result : LSP.Server_Requests.Formatting.Request do
               Read_Formatting (Input, Result.Id, Result.Params);
            end return;

         when 23 =>  --  textDocument/hover
            return Result : LSP.Server_Requests.Hover.Request do
               Read_Hover (Input, Result.Id, Result.Params);
            end return;

         when 24 =>  --  textDocument/implementation
            return Result : LSP.Server_Requests.Implementation.Request do
               Read_Implementation (Input, Result.Id, Result.Params);
            end return;

         when 25 =>  --  textDocument/inlayHint
            return Result : LSP.Server_Requests.InlayHint.Request do
               Read_InlayHint (Input, Result.Id, Result.Params);
            end return;

         when 26 =>  --  textDocument/inlineValue
            return Result : LSP.Server_Requests.InlineValue.Request do
               Read_InlineValue (Input, Result.Id, Result.Params);
            end return;

         when 27 =>  --  textDocument/linkedEditingRange
            return Result : LSP.Server_Requests.LinkedEditingRange.Request do
               Read_LinkedEditingRange (Input, Result.Id, Result.Params);
            end return;

         when 28 =>  --  textDocument/moniker
            return Result : LSP.Server_Requests.Moniker.Request do
               Read_Moniker (Input, Result.Id, Result.Params);
            end return;

         when 29 =>  --  textDocument/onTypeFormatting
            return Result : LSP.Server_Requests.OnTypeFormatting.Request do
               Read_OnTypeFormatting (Input, Result.Id, Result.Params);
            end return;

         when 30 =>  --  textDocument/prepareCallHierarchy
            return Result : LSP.Server_Requests.PrepareCallHierarchy.Request do
               Read_PrepareCallHierarchy (Input, Result.Id, Result.Params);
            end return;

         when 31 =>  --  textDocument/prepareRename
            return Result : LSP.Server_Requests.PrepareRename.Request do
               Read_PrepareRename (Input, Result.Id, Result.Params);
            end return;

         when 32 =>  --  textDocument/prepareTypeHierarchy
            return Result : LSP.Server_Requests.PrepareTypeHierarchy.Request do
               Read_PrepareTypeHierarchy (Input, Result.Id, Result.Params);
            end return;

         when 33 =>  --  textDocument/rangeFormatting
            return Result : LSP.Server_Requests.RangeFormatting.Request do
               Read_RangeFormatting (Input, Result.Id, Result.Params);
            end return;

         when 34 =>  --  textDocument/references
            return Result : LSP.Server_Requests.References.Request do
               Read_References (Input, Result.Id, Result.Params);
            end return;

         when 35 =>  --  textDocument/rename
            return Result : LSP.Server_Requests.Rename.Request do
               Read_Rename (Input, Result.Id, Result.Params);
            end return;

         when 36 =>  --  textDocument/selectionRange
            return Result : LSP.Server_Requests.SelectionRange.Request do
               Read_SelectionRange (Input, Result.Id, Result.Params);
            end return;

         when 37 =>  --  textDocument/semanticTokens/full
            return Result : LSP.Server_Requests.Tokens_Full.Request do
               Read_Tokens_Full (Input, Result.Id, Result.Params);
            end return;

         when 38 =>  --  textDocument/semanticTokens/full/delta
            return Result : LSP.Server_Requests.Tokens_Delta.Request do
               Read_Tokens_Delta (Input, Result.Id, Result.Params);
            end return;

         when 39 =>  --  textDocument/semanticTokens/range
            return Result : LSP.Server_Requests.Tokens_Range.Request do
               Read_Tokens_Range (Input, Result.Id, Result.Params);
            end return;

         when 40 =>  --  textDocument/signatureHelp
            return Result : LSP.Server_Requests.SignatureHelp.Request do
               Read_SignatureHelp (Input, Result.Id, Result.Params);
            end return;

         when 41 =>  --  textDocument/typeDefinition
            return Result : LSP.Server_Requests.TypeDefinition.Request do
               Read_TypeDefinition (Input, Result.Id, Result.Params);
            end return;

         when 42 =>  --  textDocument/willSaveWaitUntil
            return Result : LSP.Server_Requests.WillSaveWaitUntil.Request do
               Read_WillSaveWaitUntil (Input, Result.Id, Result.Params);
            end return;

         when 43 =>  --  typeHierarchy/subtypes
            return Result : LSP.Server_Requests.Subtypes.Request do
               Read_Subtypes (Input, Result.Id, Result.Params);
            end return;

         when 44 =>  --  typeHierarchy/supertypes
            return Result : LSP.Server_Requests.Supertypes.Request do
               Read_Supertypes (Input, Result.Id, Result.Params);
            end return;

         when 45 =>  --  workspace/diagnostic
            return Result : LSP.Server_Requests.Workspace_Diagnostic.Request do
               Read_Workspace_Diagnostic (Input, Result.Id, Result.Params);
            end return;

         when 46 =>  --  workspace/executeCommand
            return Result : LSP.Server_Requests.ExecuteCommand.Request do
               Read_ExecuteCommand (Input, Result.Id, Result.Params);
            end return;

         when 47 =>  --  workspace/symbol
            return Result : LSP.Server_Requests.Symbol.Request do
               Read_Symbol (Input, Result.Id, Result.Params);
            end return;

         when 48 =>  --  workspace/willCreateFiles
            return Result : LSP.Server_Requests.WillCreateFiles.Request do
               Read_WillCreateFiles (Input, Result.Id, Result.Params);
            end return;

         when 49 =>  --  workspace/willDeleteFiles
            return Result : LSP.Server_Requests.WillDeleteFiles.Request do
               Read_WillDeleteFiles (Input, Result.Id, Result.Params);
            end return;

         when 50 =>  --  workspace/willRenameFiles
            return Result : LSP.Server_Requests.WillRenameFiles.Request do
               Read_WillRenameFiles (Input, Result.Id, Result.Params);
            end return;

         when 51 =>  --  workspaceSymbol/resolve
            return Result : LSP.Server_Requests.Symbol_Resolve.Request do
               Read_Symbol_Resolve (Input, Result.Id, Result.Params);
            end return;

         when others =>
            return raise Program_Error with "Unknown method";
      end case;
   end Read_Request;
end LSP.Server_Request_Readers;

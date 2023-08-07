--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

with Minimal_Perfect_Hash;
with LSP.Inputs;
with LSP.Input_Tools;
with LSP.Structures;

with LSP.Client_Responses.IncomingCalls;
with LSP.Client_Responses.OutgoingCalls;
with LSP.Client_Responses.Code_Action_Resolve;
with LSP.Client_Responses.Code_Lens_Resolve;
with LSP.Client_Responses.Completion_Resolve;
with LSP.Client_Responses.Link_Resolve;
with LSP.Client_Responses.Initialize;
with LSP.Client_Responses.Inlay_Resolve;
with LSP.Client_Responses.Shutdown;
with LSP.Client_Responses.CodeAction;
with LSP.Client_Responses.CodeLens;
with LSP.Client_Responses.ColorPresentation;
with LSP.Client_Responses.Completion;
with LSP.Client_Responses.Declaration;
with LSP.Client_Responses.Definition;
with LSP.Client_Responses.Diagnostic;
with LSP.Client_Responses.DocumentColor;
with LSP.Client_Responses.DocumentHighlight;
with LSP.Client_Responses.DocumentLink;
with LSP.Client_Responses.DocumentSymbol;
with LSP.Client_Responses.FoldingRange;
with LSP.Client_Responses.Formatting;
with LSP.Client_Responses.Hover;
with LSP.Client_Responses.Implementation;
with LSP.Client_Responses.InlayHint;
with LSP.Client_Responses.InlineValue;
with LSP.Client_Responses.LinkedEditingRange;
with LSP.Client_Responses.Moniker;
with LSP.Client_Responses.OnTypeFormatting;
with LSP.Client_Responses.PrepareCallHierarchy;
with LSP.Client_Responses.PrepareRename;
with LSP.Client_Responses.PrepareTypeHierarchy;
with LSP.Client_Responses.RangeFormatting;
with LSP.Client_Responses.References;
with LSP.Client_Responses.Rename;
with LSP.Client_Responses.SelectionRange;
with LSP.Client_Responses.Full;
with LSP.Client_Responses.Tokens_Delta;
with LSP.Client_Responses.Tokens_Range;
with LSP.Client_Responses.SignatureHelp;
with LSP.Client_Responses.TypeDefinition;
with LSP.Client_Responses.WillSaveWaitUntil;
with LSP.Client_Responses.Subtypes;
with LSP.Client_Responses.Supertypes;
with LSP.Client_Responses.Workspace_Diagnostic;
with LSP.Client_Responses.ExecuteCommand;
with LSP.Client_Responses.Symbol;
with LSP.Client_Responses.WillCreateFiles;
with LSP.Client_Responses.WillDeleteFiles;
with LSP.Client_Responses.WillRenameFiles;
with LSP.Client_Responses.Symbol_Resolve;

package body LSP.Client_Response_Readers is

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

   procedure Read_IncomingCalls is new LSP.Input_Tools.Read_Response
     (LSP.Structures.CallHierarchyIncomingCall_Vector_Or_Null,
      LSP.Inputs.Read_CallHierarchyIncomingCall_Vector_Or_Null);

   procedure Read_OutgoingCalls is new LSP.Input_Tools.Read_Response
     (LSP.Structures.CallHierarchyOutgoingCall_Vector_Or_Null,
      LSP.Inputs.Read_CallHierarchyOutgoingCall_Vector_Or_Null);

   procedure Read_Code_Action_Resolve is new LSP.Input_Tools.Read_Response
     (LSP.Structures.CodeAction, LSP.Inputs.Read_CodeAction);

   procedure Read_Code_Lens_Resolve is new LSP.Input_Tools.Read_Response
     (LSP.Structures.CodeLens, LSP.Inputs.Read_CodeLens);

   procedure Read_Completion_Resolve is new LSP.Input_Tools.Read_Response
     (LSP.Structures.CompletionItem, LSP.Inputs.Read_CompletionItem);

   procedure Read_Link_Resolve is new LSP.Input_Tools.Read_Response
     (LSP.Structures.DocumentLink, LSP.Inputs.Read_DocumentLink);

   procedure Read_Initialize is new LSP.Input_Tools.Read_Response
     (LSP.Structures.InitializeResult, LSP.Inputs.Read_InitializeResult);

   procedure Read_Inlay_Resolve is new LSP.Input_Tools.Read_Response
     (LSP.Structures.InlayHint, LSP.Inputs.Read_InlayHint);

   procedure Read_Shutdown is new LSP.Input_Tools.Read_Response
     (LSP.Structures.Null_Record, LSP.Inputs.Read_Null_Record);

   procedure Read_CodeAction is new LSP.Input_Tools.Read_Response
     (LSP.Structures.Command_Or_CodeAction_Vector_Or_Null,
      LSP.Inputs.Read_Command_Or_CodeAction_Vector_Or_Null);

   procedure Read_CodeLens is new LSP.Input_Tools.Read_Response
     (LSP.Structures.CodeLens_Vector_Or_Null,
      LSP.Inputs.Read_CodeLens_Vector_Or_Null);

   procedure Read_ColorPresentation is new LSP.Input_Tools.Read_Response
     (LSP.Structures.ColorPresentation_Vector,
      LSP.Inputs.Read_ColorPresentation_Vector);

   procedure Read_Completion is new LSP.Input_Tools.Read_Response
     (LSP.Structures.Completion_Result, LSP.Inputs.Read_Completion_Result);

   procedure Read_Declaration is new LSP.Input_Tools.Read_Response
     (LSP.Structures.Declaration_Result, LSP.Inputs.Read_Declaration_Result);

   procedure Read_Definition is new LSP.Input_Tools.Read_Response
     (LSP.Structures.Definition_Result, LSP.Inputs.Read_Definition_Result);

   procedure Read_Diagnostic is new LSP.Input_Tools.Read_Response
     (LSP.Structures.DocumentDiagnosticReport,
      LSP.Inputs.Read_DocumentDiagnosticReport);

   procedure Read_DocumentColor is new LSP.Input_Tools.Read_Response
     (LSP.Structures.ColorInformation_Vector,
      LSP.Inputs.Read_ColorInformation_Vector);

   procedure Read_DocumentHighlight is new LSP.Input_Tools.Read_Response
     (LSP.Structures.DocumentHighlight_Vector_Or_Null,
      LSP.Inputs.Read_DocumentHighlight_Vector_Or_Null);

   procedure Read_DocumentLink is new LSP.Input_Tools.Read_Response
     (LSP.Structures.DocumentLink_Vector_Or_Null,
      LSP.Inputs.Read_DocumentLink_Vector_Or_Null);

   procedure Read_DocumentSymbol is new LSP.Input_Tools.Read_Response
     (LSP.Structures.DocumentSymbol_Result,
      LSP.Inputs.Read_DocumentSymbol_Result);

   procedure Read_FoldingRange is new LSP.Input_Tools.Read_Response
     (LSP.Structures.FoldingRange_Vector_Or_Null,
      LSP.Inputs.Read_FoldingRange_Vector_Or_Null);

   procedure Read_Formatting is new LSP.Input_Tools.Read_Response
     (LSP.Structures.TextEdit_Vector_Or_Null,
      LSP.Inputs.Read_TextEdit_Vector_Or_Null);

   procedure Read_Hover is new LSP.Input_Tools.Read_Response
     (LSP.Structures.Hover_Or_Null, LSP.Inputs.Read_Hover_Or_Null);

   procedure Read_Implementation is new LSP.Input_Tools.Read_Response
     (LSP.Structures.Definition_Result, LSP.Inputs.Read_Definition_Result);

   procedure Read_InlayHint is new LSP.Input_Tools.Read_Response
     (LSP.Structures.InlayHint_Vector_Or_Null,
      LSP.Inputs.Read_InlayHint_Vector_Or_Null);

   procedure Read_InlineValue is new LSP.Input_Tools.Read_Response
     (LSP.Structures.InlineValue_Vector_Or_Null,
      LSP.Inputs.Read_InlineValue_Vector_Or_Null);

   procedure Read_LinkedEditingRange is new LSP.Input_Tools.Read_Response
     (LSP.Structures.LinkedEditingRanges_Or_Null,
      LSP.Inputs.Read_LinkedEditingRanges_Or_Null);

   procedure Read_Moniker is new LSP.Input_Tools.Read_Response
     (LSP.Structures.Moniker_Vector_Or_Null,
      LSP.Inputs.Read_Moniker_Vector_Or_Null);

   procedure Read_OnTypeFormatting is new LSP.Input_Tools.Read_Response
     (LSP.Structures.TextEdit_Vector_Or_Null,
      LSP.Inputs.Read_TextEdit_Vector_Or_Null);

   procedure Read_PrepareCallHierarchy is new LSP.Input_Tools.Read_Response
     (LSP.Structures.CallHierarchyItem_Vector_Or_Null,
      LSP.Inputs.Read_CallHierarchyItem_Vector_Or_Null);

   procedure Read_PrepareRename is new LSP.Input_Tools.Read_Response
     (LSP.Structures.PrepareRenameResult_Or_Null,
      LSP.Inputs.Read_PrepareRenameResult_Or_Null);

   procedure Read_PrepareTypeHierarchy is new LSP.Input_Tools.Read_Response
     (LSP.Structures.TypeHierarchyItem_Vector_Or_Null,
      LSP.Inputs.Read_TypeHierarchyItem_Vector_Or_Null);

   procedure Read_RangeFormatting is new LSP.Input_Tools.Read_Response
     (LSP.Structures.TextEdit_Vector_Or_Null,
      LSP.Inputs.Read_TextEdit_Vector_Or_Null);

   procedure Read_References is new LSP.Input_Tools.Read_Response
     (LSP.Structures.Location_Vector_Or_Null,
      LSP.Inputs.Read_Location_Vector_Or_Null);

   procedure Read_Rename is new LSP.Input_Tools.Read_Response
     (LSP.Structures.WorkspaceEdit_Or_Null,
      LSP.Inputs.Read_WorkspaceEdit_Or_Null);

   procedure Read_SelectionRange is new LSP.Input_Tools.Read_Response
     (LSP.Structures.SelectionRange_Vector_Or_Null,
      LSP.Inputs.Read_SelectionRange_Vector_Or_Null);

   procedure Read_Full is new LSP.Input_Tools.Read_Response
     (LSP.Structures.SemanticTokens_Or_Null,
      LSP.Inputs.Read_SemanticTokens_Or_Null);

   procedure Read_Tokens_Delta is new LSP.Input_Tools.Read_Response
     (LSP.Structures.Tokens_Delta_Result, LSP.Inputs.Read_Tokens_Delta_Result);

   procedure Read_Tokens_Range is new LSP.Input_Tools.Read_Response
     (LSP.Structures.SemanticTokens_Or_Null,
      LSP.Inputs.Read_SemanticTokens_Or_Null);

   procedure Read_SignatureHelp is new LSP.Input_Tools.Read_Response
     (LSP.Structures.SignatureHelp_Or_Null,
      LSP.Inputs.Read_SignatureHelp_Or_Null);

   procedure Read_TypeDefinition is new LSP.Input_Tools.Read_Response
     (LSP.Structures.Definition_Result, LSP.Inputs.Read_Definition_Result);

   procedure Read_WillSaveWaitUntil is new LSP.Input_Tools.Read_Response
     (LSP.Structures.TextEdit_Vector_Or_Null,
      LSP.Inputs.Read_TextEdit_Vector_Or_Null);

   procedure Read_Subtypes is new LSP.Input_Tools.Read_Response
     (LSP.Structures.TypeHierarchyItem_Vector_Or_Null,
      LSP.Inputs.Read_TypeHierarchyItem_Vector_Or_Null);

   procedure Read_Supertypes is new LSP.Input_Tools.Read_Response
     (LSP.Structures.TypeHierarchyItem_Vector_Or_Null,
      LSP.Inputs.Read_TypeHierarchyItem_Vector_Or_Null);

   procedure Read_Workspace_Diagnostic is new LSP.Input_Tools.Read_Response
     (LSP.Structures.WorkspaceDiagnosticReport,
      LSP.Inputs.Read_WorkspaceDiagnosticReport);

   procedure Read_ExecuteCommand is new LSP.Input_Tools.Read_Response
     (LSP.Structures.LSPAny_Or_Null, LSP.Inputs.Read_LSPAny_Or_Null);

   procedure Read_Symbol is new LSP.Input_Tools.Read_Response
     (LSP.Structures.Symbol_Result, LSP.Inputs.Read_Symbol_Result);

   procedure Read_WillCreateFiles is new LSP.Input_Tools.Read_Response
     (LSP.Structures.WorkspaceEdit_Or_Null,
      LSP.Inputs.Read_WorkspaceEdit_Or_Null);

   procedure Read_WillDeleteFiles is new LSP.Input_Tools.Read_Response
     (LSP.Structures.WorkspaceEdit_Or_Null,
      LSP.Inputs.Read_WorkspaceEdit_Or_Null);

   procedure Read_WillRenameFiles is new LSP.Input_Tools.Read_Response
     (LSP.Structures.WorkspaceEdit_Or_Null,
      LSP.Inputs.Read_WorkspaceEdit_Or_Null);

   procedure Read_Symbol_Resolve is new LSP.Input_Tools.Read_Response
     (LSP.Structures.WorkspaceSymbol, LSP.Inputs.Read_WorkspaceSymbol);

   function Read_Response
     (Input  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Method : VSS.Strings.Virtual_String)
      return LSP.Client_Responses.Client_Response'Class is
      Index : constant Natural := Method_Map.Get_Index (Method);
   begin
      case Index is
         when 1 =>  --  callHierarchy/incomingCalls
            return Result : LSP.Client_Responses.IncomingCalls.Response do
               Read_IncomingCalls (Input, Result.Id, Result.Result);
            end return;

         when 2 =>  --  callHierarchy/outgoingCalls
            return Result : LSP.Client_Responses.OutgoingCalls.Response do
               Read_OutgoingCalls (Input, Result.Id, Result.Result);
            end return;

         when 3 =>  --  codeAction/resolve
            return
              Result : LSP.Client_Responses.Code_Action_Resolve.Response do
               Read_Code_Action_Resolve (Input, Result.Id, Result.Result);
            end return;

         when 4 =>  --  codeLens/resolve
            return Result : LSP.Client_Responses.Code_Lens_Resolve.Response do
               Read_Code_Lens_Resolve (Input, Result.Id, Result.Result);
            end return;

         when 5 =>  --  completionItem/resolve
            return Result : LSP.Client_Responses.Completion_Resolve.Response do
               Read_Completion_Resolve (Input, Result.Id, Result.Result);
            end return;

         when 6 =>  --  documentLink/resolve
            return Result : LSP.Client_Responses.Link_Resolve.Response do
               Read_Link_Resolve (Input, Result.Id, Result.Result);
            end return;

         when 7 =>  --  initialize
            return Result : LSP.Client_Responses.Initialize.Response do
               Read_Initialize (Input, Result.Id, Result.Result);
            end return;

         when 8 =>  --  inlayHint/resolve
            return Result : LSP.Client_Responses.Inlay_Resolve.Response do
               Read_Inlay_Resolve (Input, Result.Id, Result.Result);
            end return;

         when 9 =>  --  shutdown
            return Result : LSP.Client_Responses.Shutdown.Response do
               Read_Shutdown (Input, Result.Id, Result.Result);
            end return;

         when 10 =>  --  textDocument/codeAction
            return Result : LSP.Client_Responses.CodeAction.Response do
               Read_CodeAction (Input, Result.Id, Result.Result);
            end return;

         when 11 =>  --  textDocument/codeLens
            return Result : LSP.Client_Responses.CodeLens.Response do
               Read_CodeLens (Input, Result.Id, Result.Result);
            end return;

         when 12 =>  --  textDocument/colorPresentation
            return Result : LSP.Client_Responses.ColorPresentation.Response do
               Read_ColorPresentation (Input, Result.Id, Result.Result);
            end return;

         when 13 =>  --  textDocument/completion
            return Result : LSP.Client_Responses.Completion.Response do
               Read_Completion (Input, Result.Id, Result.Result);
            end return;

         when 14 =>  --  textDocument/declaration
            return Result : LSP.Client_Responses.Declaration.Response do
               Read_Declaration (Input, Result.Id, Result.Result);
            end return;

         when 15 =>  --  textDocument/definition
            return Result : LSP.Client_Responses.Definition.Response do
               Read_Definition (Input, Result.Id, Result.Result);
            end return;

         when 16 =>  --  textDocument/diagnostic
            return Result : LSP.Client_Responses.Diagnostic.Response do
               Read_Diagnostic (Input, Result.Id, Result.Result);
            end return;

         when 17 =>  --  textDocument/documentColor
            return Result : LSP.Client_Responses.DocumentColor.Response do
               Read_DocumentColor (Input, Result.Id, Result.Result);
            end return;

         when 18 =>  --  textDocument/documentHighlight
            return Result : LSP.Client_Responses.DocumentHighlight.Response do
               Read_DocumentHighlight (Input, Result.Id, Result.Result);
            end return;

         when 19 =>  --  textDocument/documentLink
            return Result : LSP.Client_Responses.DocumentLink.Response do
               Read_DocumentLink (Input, Result.Id, Result.Result);
            end return;

         when 20 =>  --  textDocument/documentSymbol
            return Result : LSP.Client_Responses.DocumentSymbol.Response do
               Read_DocumentSymbol (Input, Result.Id, Result.Result);
            end return;

         when 21 =>  --  textDocument/foldingRange
            return Result : LSP.Client_Responses.FoldingRange.Response do
               Read_FoldingRange (Input, Result.Id, Result.Result);
            end return;

         when 22 =>  --  textDocument/formatting
            return Result : LSP.Client_Responses.Formatting.Response do
               Read_Formatting (Input, Result.Id, Result.Result);
            end return;

         when 23 =>  --  textDocument/hover
            return Result : LSP.Client_Responses.Hover.Response do
               Read_Hover (Input, Result.Id, Result.Result);
            end return;

         when 24 =>  --  textDocument/implementation
            return Result : LSP.Client_Responses.Implementation.Response do
               Read_Implementation (Input, Result.Id, Result.Result);
            end return;

         when 25 =>  --  textDocument/inlayHint
            return Result : LSP.Client_Responses.InlayHint.Response do
               Read_InlayHint (Input, Result.Id, Result.Result);
            end return;

         when 26 =>  --  textDocument/inlineValue
            return Result : LSP.Client_Responses.InlineValue.Response do
               Read_InlineValue (Input, Result.Id, Result.Result);
            end return;

         when 27 =>  --  textDocument/linkedEditingRange
            return Result : LSP.Client_Responses.LinkedEditingRange.Response do
               Read_LinkedEditingRange (Input, Result.Id, Result.Result);
            end return;

         when 28 =>  --  textDocument/moniker
            return Result : LSP.Client_Responses.Moniker.Response do
               Read_Moniker (Input, Result.Id, Result.Result);
            end return;

         when 29 =>  --  textDocument/onTypeFormatting
            return Result : LSP.Client_Responses.OnTypeFormatting.Response do
               Read_OnTypeFormatting (Input, Result.Id, Result.Result);
            end return;

         when 30 =>  --  textDocument/prepareCallHierarchy
            return
              Result : LSP.Client_Responses.PrepareCallHierarchy.Response do
               Read_PrepareCallHierarchy (Input, Result.Id, Result.Result);
            end return;

         when 31 =>  --  textDocument/prepareRename
            return Result : LSP.Client_Responses.PrepareRename.Response do
               Read_PrepareRename (Input, Result.Id, Result.Result);
            end return;

         when 32 =>  --  textDocument/prepareTypeHierarchy
            return
              Result : LSP.Client_Responses.PrepareTypeHierarchy.Response do
               Read_PrepareTypeHierarchy (Input, Result.Id, Result.Result);
            end return;

         when 33 =>  --  textDocument/rangeFormatting
            return Result : LSP.Client_Responses.RangeFormatting.Response do
               Read_RangeFormatting (Input, Result.Id, Result.Result);
            end return;

         when 34 =>  --  textDocument/references
            return Result : LSP.Client_Responses.References.Response do
               Read_References (Input, Result.Id, Result.Result);
            end return;

         when 35 =>  --  textDocument/rename
            return Result : LSP.Client_Responses.Rename.Response do
               Read_Rename (Input, Result.Id, Result.Result);
            end return;

         when 36 =>  --  textDocument/selectionRange
            return Result : LSP.Client_Responses.SelectionRange.Response do
               Read_SelectionRange (Input, Result.Id, Result.Result);
            end return;

         when 37 =>  --  textDocument/semanticTokens/full
            return Result : LSP.Client_Responses.Full.Response do
               Read_Full (Input, Result.Id, Result.Result);
            end return;

         when 38 =>  --  textDocument/semanticTokens/full/delta
            return Result : LSP.Client_Responses.Tokens_Delta.Response do
               Read_Tokens_Delta (Input, Result.Id, Result.Result);
            end return;

         when 39 =>  --  textDocument/semanticTokens/range
            return Result : LSP.Client_Responses.Tokens_Range.Response do
               Read_Tokens_Range (Input, Result.Id, Result.Result);
            end return;

         when 40 =>  --  textDocument/signatureHelp
            return Result : LSP.Client_Responses.SignatureHelp.Response do
               Read_SignatureHelp (Input, Result.Id, Result.Result);
            end return;

         when 41 =>  --  textDocument/typeDefinition
            return Result : LSP.Client_Responses.TypeDefinition.Response do
               Read_TypeDefinition (Input, Result.Id, Result.Result);
            end return;

         when 42 =>  --  textDocument/willSaveWaitUntil
            return Result : LSP.Client_Responses.WillSaveWaitUntil.Response do
               Read_WillSaveWaitUntil (Input, Result.Id, Result.Result);
            end return;

         when 43 =>  --  typeHierarchy/subtypes
            return Result : LSP.Client_Responses.Subtypes.Response do
               Read_Subtypes (Input, Result.Id, Result.Result);
            end return;

         when 44 =>  --  typeHierarchy/supertypes
            return Result : LSP.Client_Responses.Supertypes.Response do
               Read_Supertypes (Input, Result.Id, Result.Result);
            end return;

         when 45 =>  --  workspace/diagnostic
            return
              Result : LSP.Client_Responses.Workspace_Diagnostic.Response do
               Read_Workspace_Diagnostic (Input, Result.Id, Result.Result);
            end return;

         when 46 =>  --  workspace/executeCommand
            return Result : LSP.Client_Responses.ExecuteCommand.Response do
               Read_ExecuteCommand (Input, Result.Id, Result.Result);
            end return;

         when 47 =>  --  workspace/symbol
            return Result : LSP.Client_Responses.Symbol.Response do
               Read_Symbol (Input, Result.Id, Result.Result);
            end return;

         when 48 =>  --  workspace/willCreateFiles
            return Result : LSP.Client_Responses.WillCreateFiles.Response do
               Read_WillCreateFiles (Input, Result.Id, Result.Result);
            end return;

         when 49 =>  --  workspace/willDeleteFiles
            return Result : LSP.Client_Responses.WillDeleteFiles.Response do
               Read_WillDeleteFiles (Input, Result.Id, Result.Result);
            end return;

         when 50 =>  --  workspace/willRenameFiles
            return Result : LSP.Client_Responses.WillRenameFiles.Response do
               Read_WillRenameFiles (Input, Result.Id, Result.Result);
            end return;

         when 51 =>  --  workspaceSymbol/resolve
            return Result : LSP.Client_Responses.Symbol_Resolve.Response do
               Read_Symbol_Resolve (Input, Result.Id, Result.Result);
            end return;

         when others =>
            return raise Program_Error with "Unknown method";
      end case;
   end Read_Response;
end LSP.Client_Response_Readers;

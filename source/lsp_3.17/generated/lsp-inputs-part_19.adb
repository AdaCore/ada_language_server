--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

pragma Ada_2022;
pragma Warnings (Off, "is not referenced");
with Interfaces;
with LSP.Input_Tools;
with VSS.Strings;
with VSS.JSON.Pull_Readers.Buffered;
with Minimal_Perfect_Hash;

package body LSP.Inputs.Part_19 is

   procedure Read_TextDocumentContentChangeEvent_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentContentChangeEvent_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   :
           LSP.Structures.TextDocumentContentChangeEvent_Vector renames Value;
         Value : LSP.Structures.TextDocumentContentChangeEvent;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_TextDocumentContentChangeEvent (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_TextDocumentContentChangeEvent_Vector;

   procedure Read_TextDocumentSaveReason
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.TextDocumentSaveReason) is
   begin
      Value :=
        LSP.Enumerations.TextDocumentSaveReason'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_TextDocumentSaveReason;

   procedure Read_TextDocumentIdentifier_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentIdentifier_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.TextDocumentIdentifier_Vector renames Value;
         Value : LSP.Structures.TextDocumentIdentifier;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_TextDocumentIdentifier (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_TextDocumentIdentifier_Vector;

   procedure Read_CompletionItem_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionItem_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.CompletionItem_Vector renames Value;
         Value : LSP.Structures.CompletionItem;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_CompletionItem (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_CompletionItem_Vector;

   package InsertReplaceEdit_Scope is
      package InsertReplaceEdit_Map is new Minimal_Perfect_Hash
        (["newText",
         "insert",
         "replace"]);

   end InsertReplaceEdit_Scope;

   procedure Read_InsertReplaceEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InsertReplaceEdit) is
      use InsertReplaceEdit_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InsertReplaceEdit_Map.Get_Index (Key) is
               when 1 =>  --  newText
                  Value.newText.Clear;
                  Value.newText.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  insert
                  Read_A_Range (Handler, Value.insert);
               when 3 =>  --  replace
                  Read_A_Range (Handler, Value.replace);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InsertReplaceEdit;

   procedure Read_DocumentHighlightKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.DocumentHighlightKind) is
   begin
      Value :=
        LSP.Enumerations.DocumentHighlightKind'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_DocumentHighlightKind;

   package TextDocumentPositionParams_Scope is
      package TextDocumentPositionParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position"]);

   end TextDocumentPositionParams_Scope;

   procedure Read_TextDocumentPositionParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentPositionParams) is
      use TextDocumentPositionParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextDocumentPositionParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentPositionParams;

   package TextDocumentItem_Scope is
      package TextDocumentItem_Map is new Minimal_Perfect_Hash
        (["uri",
         "languageId",
         "version",
         "text"]);

   end TextDocumentItem_Scope;

   procedure Read_TextDocumentItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentItem) is
      use TextDocumentItem_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextDocumentItem_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 2 =>  --  languageId
                  Value.languageId.Clear;
                  Value.languageId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  version
                  Value.version :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 4 =>  --  text
                  Value.text.Clear;
                  Value.text.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentItem;

   package ColorPresentationParams_Scope is
      package ColorPresentationParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument",
         "color",
         "range"]);

   end ColorPresentationParams_Scope;

   procedure Read_ColorPresentationParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ColorPresentationParams) is
      use ColorPresentationParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ColorPresentationParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 3 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 4 =>  --  color
                  Read_Color (Handler, Value.color);
               when 5 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ColorPresentationParams;

   package InlayHintClientCapabilities_Scope is
      package InlayHintClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "resolveSupport"]);

   end InlayHintClientCapabilities_Scope;

   procedure Read_InlayHintClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintClientCapabilities) is
      use InlayHintClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlayHintClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  resolveSupport
                  Value.resolveSupport :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_resolveSupport_OfWorkspaceSymbolClientCapabilities
                    (Handler, Value.resolveSupport.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlayHintClientCapabilities;

   procedure Read_NotebookCellKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.NotebookCellKind) is
   begin
      Value :=
        LSP.Enumerations.NotebookCellKind'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_NotebookCellKind;

   procedure Read_LinkedEditingRanges_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LinkedEditingRanges_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value :=
           (Is_Null => False,
            Value   => <>);
         Read_LinkedEditingRanges (Handler, Value.Value);
      end if;
   end Read_LinkedEditingRanges_Or_Null;

   package DocumentFormattingClientCapabilities_Scope is
      package DocumentFormattingClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end DocumentFormattingClientCapabilities_Scope;

   procedure Read_DocumentFormattingClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentFormattingClientCapabilities) is
      use DocumentFormattingClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentFormattingClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentFormattingClientCapabilities;

   package FormattingOptions_Scope is
      package FormattingOptions_Map is new Minimal_Perfect_Hash
        (["tabSize",
         "insertSpaces",
         "trimTrailingWhitespace",
         "insertFinalNewline",
         "trimFinalNewlines"]);

   end FormattingOptions_Scope;

   procedure Read_FormattingOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FormattingOptions) is
      use FormattingOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FormattingOptions_Map.Get_Index (Key) is
               when 1 =>  --  tabSize
                  Value.tabSize :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 2 =>  --  insertSpaces
                  Value.insertSpaces := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  trimTrailingWhitespace
                  Value.trimTrailingWhitespace       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.trimTrailingWhitespace.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  insertFinalNewline
                  Value.insertFinalNewline       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.insertFinalNewline.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 5 =>  --  trimFinalNewlines
                  Value.trimFinalNewlines       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.trimFinalNewlines.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FormattingOptions;

   procedure Read_CallHierarchyIncomingCall_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyIncomingCall_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.CallHierarchyIncomingCall_Vector renames Value;
         Value : LSP.Structures.CallHierarchyIncomingCall;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_CallHierarchyIncomingCall (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_CallHierarchyIncomingCall_Vector;

   procedure Read_TextEdit_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextEdit_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.TextEdit_Vector renames Value;
         Value : LSP.Structures.TextEdit;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_TextEdit (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_TextEdit_Vector;

end LSP.Inputs.Part_19;

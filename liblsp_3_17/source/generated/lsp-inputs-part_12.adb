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

package body LSP.Inputs.Part_12 is

   package DocumentOnTypeFormattingClientCapabilities_Scope is
      package DocumentOnTypeFormattingClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end DocumentOnTypeFormattingClientCapabilities_Scope;

   procedure Read_DocumentOnTypeFormattingClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.DocumentOnTypeFormattingClientCapabilities) is
      use DocumentOnTypeFormattingClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentOnTypeFormattingClientCapabilities_Map.Get_Index
              (Key) is
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
   end Read_DocumentOnTypeFormattingClientCapabilities;

   procedure Read_SymbolKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SymbolKind) is
   begin
      Value :=
        LSP.Enumerations.SymbolKind'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_SymbolKind;

   package SemanticTokensParams_Scope is
      package SemanticTokensParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument"]);

   end SemanticTokensParams_Scope;

   procedure Read_SemanticTokensParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensParams) is
      use SemanticTokensParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensParams_Map.Get_Index (Key) is
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
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensParams;

   package DidChangeConfigurationRegistrationOptions_Scope is
      package DidChangeConfigurationRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["section"]);

   end DidChangeConfigurationRegistrationOptions_Scope;

   procedure Read_DidChangeConfigurationRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.DidChangeConfigurationRegistrationOptions) is
      use DidChangeConfigurationRegistrationOptions_Scope;
      procedure Read_Virtual_String_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Virtual_String_Vector);

      procedure Read_Virtual_String_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Virtual_String_Vector) is
      begin
         declare
            Set   : LSP.Structures.Virtual_String_Vector renames Value;
            Value : LSP.Structures.Virtual_String;
         begin
            Set.Clear;
            if Handler.Is_Start_Array then
               Handler.Read_Next;
               while not Handler.Is_End_Array loop
                  Value.Clear;
                  Value.Append (Handler.String_Value);
                  Handler.Read_Next;
                  Set.Append (Value);
               end loop;
               Handler.Read_Next;

            else
               Value.Clear;
               Value.Append (Handler.String_Value);
               Handler.Read_Next;
               Set.Append (Value);
            end if;
         end;

      end Read_Virtual_String_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidChangeConfigurationRegistrationOptions_Map.Get_Index
              (Key) is
               when 1 =>  --  section
                  Value.section :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Virtual_String_Vector (Handler, Value.section.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidChangeConfigurationRegistrationOptions;

   procedure Read_GlobPattern
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.GlobPattern) is
   begin
      if Handler.Is_String_Value then
         Value :=
           (Is_Pattern => True,
            others     => <>);
      else
         Value :=
           (Is_Pattern => False,
            others     => <>);
      end if;

      case Value.Is_Pattern is
         when True =>
            Read_Pattern (Handler, Value.Pattern);
         when False =>
            Read_RelativePattern (Handler, Value.RelativePattern);
      end case;
   end Read_GlobPattern;

   package DocumentHighlightRegistrationOptions_Scope is
      package DocumentHighlightRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress"]);

   end DocumentHighlightRegistrationOptions_Scope;

   procedure Read_DocumentHighlightRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlightRegistrationOptions) is
      use DocumentHighlightRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentHighlightRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentHighlightRegistrationOptions;

   package CallHierarchyOutgoingCallsParams_Scope is
      package CallHierarchyOutgoingCallsParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "item"]);

   end CallHierarchyOutgoingCallsParams_Scope;

   procedure Read_CallHierarchyOutgoingCallsParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyOutgoingCallsParams) is
      use CallHierarchyOutgoingCallsParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CallHierarchyOutgoingCallsParams_Map.Get_Index (Key) is
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
               when 3 =>  --  item
                  Read_CallHierarchyItem (Handler, Value.item);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CallHierarchyOutgoingCallsParams;

   package FoldingRangeOptions_Scope is
      package FoldingRangeOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end FoldingRangeOptions_Scope;

   procedure Read_FoldingRangeOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRangeOptions) is
      use FoldingRangeOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FoldingRangeOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FoldingRangeOptions;

   package TextDocumentEdit_Scope is
      package TextDocumentEdit_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "edits"]);

      package TextEdit_Or_AnnotatedTextEdit_Scope is
         package TextEdit_Or_AnnotatedTextEdit_Map is new Minimal_Perfect_Hash
           (["annotationId"]);

      end TextEdit_Or_AnnotatedTextEdit_Scope;

   end TextDocumentEdit_Scope;

   procedure Read_TextDocumentEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentEdit) is
      use TextDocumentEdit_Scope;
      procedure Read_TextEdit_Or_AnnotatedTextEdit
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.TextEdit_Or_AnnotatedTextEdit);

      procedure Read_TextEdit_Or_AnnotatedTextEdit_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.TextEdit_Or_AnnotatedTextEdit_Vector);

      procedure Read_TextEdit_Or_AnnotatedTextEdit
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.TextEdit_Or_AnnotatedTextEdit) is
         use TextEdit_Or_AnnotatedTextEdit_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Is_TextEdit => True,
                  others      => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       TextEdit_Or_AnnotatedTextEdit_Map.Get_Index (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  annotationId
                           Value :=
                             (Is_TextEdit => False,
                              others      => <>);
                           exit;
                        when others =>
                           Handler.Skip_Current_Value;
                     end case;
                  end;
               end loop;
            else
               raise Program_Error;  --  Unexpected JSON value
            end if;

            Handler.Reset;
            Handler.Unmark;

            case Value.Is_TextEdit is
               when True =>
                  Read_TextEdit (Handler, Value.TextEdit);
               when False =>
                  Read_AnnotatedTextEdit (Handler, Value.AnnotatedTextEdit);
            end case;
         end;
      end Read_TextEdit_Or_AnnotatedTextEdit;

      procedure Read_TextEdit_Or_AnnotatedTextEdit_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.TextEdit_Or_AnnotatedTextEdit_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   :
              LSP.Structures.TextEdit_Or_AnnotatedTextEdit_Vector renames
              Value;
            Value : LSP.Structures.TextEdit_Or_AnnotatedTextEdit;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_TextEdit_Or_AnnotatedTextEdit (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_TextEdit_Or_AnnotatedTextEdit_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextDocumentEdit_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_OptionalVersionedTextDocumentIdentifier
                    (Handler, Value.textDocument);
               when 2 =>  --  edits
                  Read_TextEdit_Or_AnnotatedTextEdit_Vector
                    (Handler, Value.edits);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentEdit;

end LSP.Inputs.Part_12;

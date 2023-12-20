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

package body LSP.Inputs.Part_29 is

   procedure Read_DocumentLink_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLink_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.DocumentLink_Vector renames Value;
         Value : LSP.Structures.DocumentLink;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_DocumentLink (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_DocumentLink_Vector;

   procedure Read_Range_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Range_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.Range_Vector renames Value;
         Value : LSP.Structures.A_Range;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_A_Range (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_Range_Vector;

   package LogTraceParams_Scope is
      package LogTraceParams_Map is new Minimal_Perfect_Hash
        (["message",
         "verbose"]);

   end LogTraceParams_Scope;

   procedure Read_LogTraceParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LogTraceParams) is
      use LogTraceParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case LogTraceParams_Map.Get_Index (Key) is
               when 1 =>  --  message
                  Value.message.Clear;
                  Value.message.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  verbose
                  Value.verbose.Clear;
                  Value.verbose.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_LogTraceParams;

   procedure Read_Moniker_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Moniker_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_Moniker_Vector (Handler, Value);
      end if;
   end Read_Moniker_Vector_Or_Null;

   package InlayHintRegistrationOptions_Scope is
      package InlayHintRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "resolveProvider",
         "documentSelector",
         "id"]);

   end InlayHintRegistrationOptions_Scope;

   procedure Read_InlayHintRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintRegistrationOptions) is
      use InlayHintRegistrationOptions_Scope;
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
            case InlayHintRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  resolveProvider
                  Value.resolveProvider       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.resolveProvider.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 4 =>  --  id
                  Value.id.Clear;
                  Value.id.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlayHintRegistrationOptions;

   package DefinitionParams_Scope is
      package DefinitionParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken",
         "partialResultToken",
         "alsDisplayMethodAncestryOnNavigation"]);

   end DefinitionParams_Scope;

   procedure Read_DefinitionParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionParams) is
      use DefinitionParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DefinitionParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when 3 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 4 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 5 =>  --  alsDisplayMethodAncestryOnNavigation
                  Value.alsDisplayMethodAncestryOnNavigation :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_AlsDisplayMethodAncestryOnNavigationPolicy
                    (Handler,
                     Value.alsDisplayMethodAncestryOnNavigation.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DefinitionParams;

   package InlayHintLabelPart_Scope is
      package InlayHintLabelPart_Map is new Minimal_Perfect_Hash
        (["value",
         "tooltip",
         "location",
         "command"]);

   end InlayHintLabelPart_Scope;

   procedure Read_InlayHintLabelPart
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintLabelPart) is
      use InlayHintLabelPart_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlayHintLabelPart_Map.Get_Index (Key) is
               when 1 =>  --  value
                  Value.value.Clear;
                  Value.value.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  tooltip
                  Value.tooltip :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Virtual_String_Or_MarkupContent
                    (Handler, Value.tooltip.Value);
               when 3 =>  --  location
                  Value.location :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Location (Handler, Value.location.Value);
               when 4 =>  --  command
                  Value.command :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Command (Handler, Value.command.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlayHintLabelPart;

   package DocumentFormattingRegistrationOptions_Scope is
      package DocumentFormattingRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress"]);

   end DocumentFormattingRegistrationOptions_Scope;

   procedure Read_DocumentFormattingRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentFormattingRegistrationOptions) is
      use DocumentFormattingRegistrationOptions_Scope;
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
            case DocumentFormattingRegistrationOptions_Map.Get_Index (Key) is
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
   end Read_DocumentFormattingRegistrationOptions;

   package RenameFilesParams_Scope is
      package RenameFilesParams_Map is new Minimal_Perfect_Hash (["files"]);

   end RenameFilesParams_Scope;

   procedure Read_RenameFilesParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameFilesParams) is
      use RenameFilesParams_Scope;
      procedure Read_FileRename_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileRename_Vector);

      procedure Read_FileRename_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileRename_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.FileRename_Vector renames Value;
            Value : LSP.Structures.FileRename;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_FileRename (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_FileRename_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RenameFilesParams_Map.Get_Index (Key) is
               when 1 =>  --  files
                  Read_FileRename_Vector (Handler, Value.files);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RenameFilesParams;

   procedure Read_SelectionRange_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRange_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.SelectionRange_Vector renames Value;
         Value : LSP.Structures.SelectionRange;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_SelectionRange (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_SelectionRange_Vector;

   package CodeLensOptions_Scope is
      package CodeLensOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "resolveProvider"]);

   end CodeLensOptions_Scope;

   procedure Read_CodeLensOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLensOptions) is
      use CodeLensOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeLensOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  resolveProvider
                  Value.resolveProvider       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.resolveProvider.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CodeLensOptions;

   package NotebookDocumentClientCapabilities_Scope is
      package NotebookDocumentClientCapabilities_Map is new Minimal_Perfect_Hash
        (["synchronization"]);

   end NotebookDocumentClientCapabilities_Scope;

   procedure Read_NotebookDocumentClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentClientCapabilities) is
      use NotebookDocumentClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case NotebookDocumentClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  synchronization
                  Read_NotebookDocumentSyncClientCapabilities
                    (Handler, Value.synchronization);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_NotebookDocumentClientCapabilities;

   package CodeLens_Scope is
      package CodeLens_Map is new Minimal_Perfect_Hash
        (["range",
         "command",
         "data"]);

   end CodeLens_Scope;

   procedure Read_CodeLens
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLens) is
      use CodeLens_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeLens_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  command
                  Value.command :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Command (Handler, Value.command.Value);
               when 3 =>  --  data
                  Read_LSPAny (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CodeLens;

   procedure Read_PrepareSupportDefaultBehavior
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.PrepareSupportDefaultBehavior) is
   begin
      Value :=
        LSP.Enumerations.PrepareSupportDefaultBehavior'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_PrepareSupportDefaultBehavior;

   package TraceValues_Map is new Minimal_Perfect_Hash
     (["off",
      "messages",
      "verbose"]);

   procedure Read_TraceValues
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.TraceValues) is
   begin
      Value :=
        LSP.Enumerations.TraceValues'Val
          (TraceValues_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_TraceValues;

   procedure Read_Command_Or_CodeAction_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Command_Or_CodeAction_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_Command_Or_CodeAction_Vector (Handler, Value);
      end if;
   end Read_Command_Or_CodeAction_Vector_Or_Null;

end LSP.Inputs.Part_29;

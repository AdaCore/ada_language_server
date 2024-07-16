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

package body LSP.Inputs.Part_31 is

   procedure Read_Virtual_String
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Virtual_String) is
   begin
      Value.Clear;
      Value.Append (Handler.String_Value);
      Handler.Read_Next;
   end Read_Virtual_String;

   package CompletionContext_Scope is
      package CompletionContext_Map is new Minimal_Perfect_Hash
        (["triggerKind",
         "triggerCharacter"]);

   end CompletionContext_Scope;

   procedure Read_CompletionContext
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionContext) is
      use CompletionContext_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CompletionContext_Map.Get_Index (Key) is
               when 1 =>  --  triggerKind
                  Read_CompletionTriggerKind (Handler, Value.triggerKind);
               when 2 =>  --  triggerCharacter
                  Value.triggerCharacter.Clear;
                  Value.triggerCharacter.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CompletionContext;

   package FileOperationPattern_Scope is
      package FileOperationPattern_Map is new Minimal_Perfect_Hash
        (["glob",
         "matches",
         "options"]);

   end FileOperationPattern_Scope;

   procedure Read_FileOperationPattern
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationPattern) is
      use FileOperationPattern_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileOperationPattern_Map.Get_Index (Key) is
               when 1 =>  --  glob
                  Value.glob.Clear;
                  Value.glob.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  matches
                  Value.matches :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FileOperationPatternKind (Handler, Value.matches.Value);
               when 3 =>  --  options
                  Value.options :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FileOperationPatternOptions
                    (Handler, Value.options.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FileOperationPattern;

   package FileOperationPatternOptions_Scope is
      package FileOperationPatternOptions_Map is new Minimal_Perfect_Hash
        (["ignoreCase"]);

   end FileOperationPatternOptions_Scope;

   procedure Read_FileOperationPatternOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationPatternOptions) is
      use FileOperationPatternOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileOperationPatternOptions_Map.Get_Index (Key) is
               when 1 =>  --  ignoreCase
                  Value.ignoreCase       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.ignoreCase.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FileOperationPatternOptions;

   package ColorPresentation_Scope is
      package ColorPresentation_Map is new Minimal_Perfect_Hash
        (["label",
         "textEdit",
         "additionalTextEdits"]);

   end ColorPresentation_Scope;

   procedure Read_ColorPresentation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ColorPresentation) is
      use ColorPresentation_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ColorPresentation_Map.Get_Index (Key) is
               when 1 =>  --  label
                  Value.label.Clear;
                  Value.label.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  textEdit
                  Value.textEdit :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_TextEdit (Handler, Value.textEdit.Value);
               when 3 =>  --  additionalTextEdits
                  Read_TextEdit_Vector (Handler, Value.additionalTextEdits);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ColorPresentation;

end LSP.Inputs.Part_31;

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

package body LSP.Inputs.Part_25 is

   package Color_Scope is
      package Color_Map is new Minimal_Perfect_Hash
        (["red",
         "green",
         "blue",
         "alpha"]);

   end Color_Scope;

   procedure Read_Color
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Color) is
      use Color_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case Color_Map.Get_Index (Key) is
               when 1 =>  --  red
                  Value.red := Float (Handler.Number_Value.Float_Value);
                  Handler.Read_Next;
               when 2 =>  --  green
                  Value.green := Float (Handler.Number_Value.Float_Value);
                  Handler.Read_Next;
               when 3 =>  --  blue
                  Value.blue := Float (Handler.Number_Value.Float_Value);
                  Handler.Read_Next;
               when 4 =>  --  alpha
                  Value.alpha := Float (Handler.Number_Value.Float_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Color;

   package InlineValueContext_Scope is
      package InlineValueContext_Map is new Minimal_Perfect_Hash
        (["frameId",
         "stoppedLocation"]);

   end InlineValueContext_Scope;

   procedure Read_InlineValueContext
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueContext) is
      use InlineValueContext_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlineValueContext_Map.Get_Index (Key) is
               when 1 =>  --  frameId
                  Value.frameId :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 2 =>  --  stoppedLocation
                  Read_A_Range (Handler, Value.stoppedLocation);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlineValueContext;

   procedure Read_Virtual_String_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Virtual_String_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.Virtual_String_Vector renames Value;
         Value : LSP.Structures.Virtual_String;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Value.Clear;
            Value.Append (Handler.String_Value);
            Handler.Read_Next;
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_Virtual_String_Vector;

   package DiagnosticWorkspaceClientCapabilities_Scope is
      package DiagnosticWorkspaceClientCapabilities_Map is new Minimal_Perfect_Hash
        (["refreshSupport"]);

   end DiagnosticWorkspaceClientCapabilities_Scope;

   procedure Read_DiagnosticWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticWorkspaceClientCapabilities) is
      use DiagnosticWorkspaceClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DiagnosticWorkspaceClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  refreshSupport
                  Value.refreshSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.refreshSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DiagnosticWorkspaceClientCapabilities;

   procedure Read_Location_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Location_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_Location_Vector (Handler, Value);
      end if;
   end Read_Location_Vector_Or_Null;

   package AlsReferenceKind_Map is new Minimal_Perfect_Hash
     (["reference",
      "access",
      "write",
      "call",
      "dispatching call",
      "parent",
      "child",
      "overriding"]);

   procedure Read_AlsReferenceKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.AlsReferenceKind) is
   begin
      Value :=
        LSP.Enumerations.AlsReferenceKind'Val
          (AlsReferenceKind_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_AlsReferenceKind;

   package CreateFilesParams_Scope is
      package CreateFilesParams_Map is new Minimal_Perfect_Hash (["files"]);

   end CreateFilesParams_Scope;

   procedure Read_CreateFilesParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CreateFilesParams) is
      use CreateFilesParams_Scope;
      procedure Read_FileCreate_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileCreate_Vector);

      procedure Read_FileCreate_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileCreate_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.FileCreate_Vector renames Value;
            Value : LSP.Structures.FileCreate;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_FileCreate (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_FileCreate_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CreateFilesParams_Map.Get_Index (Key) is
               when 1 =>  --  files
                  Read_FileCreate_Vector (Handler, Value.files);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CreateFilesParams;

   procedure Read_ColorPresentation_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ColorPresentation_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.ColorPresentation_Vector renames Value;
         Value : LSP.Structures.ColorPresentation;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_ColorPresentation (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_ColorPresentation_Vector;

   package DocumentOnTypeFormattingParams_Scope is
      package DocumentOnTypeFormattingParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "ch",
         "options"]);

   end DocumentOnTypeFormattingParams_Scope;

   procedure Read_DocumentOnTypeFormattingParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentOnTypeFormattingParams) is
      use DocumentOnTypeFormattingParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentOnTypeFormattingParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when 3 =>  --  ch
                  Value.ch.Clear;
                  Value.ch.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 4 =>  --  options
                  Read_FormattingOptions (Handler, Value.options);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentOnTypeFormattingParams;

   package AlsCheckSyntaxParams_Scope is
      package AlsCheckSyntaxParams_Map is new Minimal_Perfect_Hash
        (["input",
         "rules"]);

   end AlsCheckSyntaxParams_Scope;

   procedure Read_AlsCheckSyntaxParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.AlsCheckSyntaxParams) is
      use AlsCheckSyntaxParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case AlsCheckSyntaxParams_Map.Get_Index (Key) is
               when 1 =>  --  input
                  Value.input.Clear;
                  Value.input.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  rules
                  Read_Virtual_String_Vector (Handler, Value.rules);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_AlsCheckSyntaxParams;

   package SemanticTokensClientCapabilities_Scope is
      package SemanticTokensClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "requests",
         "tokenTypes",
         "tokenModifiers",
         "formats",
         "overlappingTokenSupport",
         "multilineTokenSupport",
         "serverCancelSupport",
         "augmentsSyntaxTokens"]);

      package requests_OfSemanticTokensClientCapabilities_Scope is
         package requests_OfSemanticTokensClientCapabilities_Map is new Minimal_Perfect_Hash
           (["range",
            "full"]);

      end requests_OfSemanticTokensClientCapabilities_Scope;

   end SemanticTokensClientCapabilities_Scope;

   procedure Read_SemanticTokensClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensClientCapabilities) is
      use SemanticTokensClientCapabilities_Scope;
      procedure Read_requests_OfSemanticTokensClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .requests_OfSemanticTokensClientCapabilities);

      procedure Read_TokenFormat_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.TokenFormat_Set);

      procedure Read_requests_OfSemanticTokensClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .requests_OfSemanticTokensClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use requests_OfSemanticTokensClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case requests_OfSemanticTokensClientCapabilities_Map.Get_Index
                 (Key) is
                  when 1 =>  --  range
                     Value.a_range :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_Boolean_Or_Any (Handler, Value.a_range.Value);
                  when 2 =>  --  full
                     Value.full :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_Boolean_Or_Something (Handler, Value.full.Value);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_requests_OfSemanticTokensClientCapabilities;

      procedure Read_TokenFormat_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.TokenFormat_Set) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.TokenFormat_Set renames Value;
            Value : LSP.Enumerations.TokenFormat;
         begin
            Set := (others => False);
            while not Handler.Is_End_Array loop
               Read_TokenFormat (Handler, Value);
               Set (Value) := True;
            end loop;
         end;

         Handler.Read_Next;
      end Read_TokenFormat_Set;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  requests
                  Read_requests_OfSemanticTokensClientCapabilities
                    (Handler, Value.requests);
               when 3 =>  --  tokenTypes
                  Read_Virtual_String_Vector (Handler, Value.tokenTypes);
               when 4 =>  --  tokenModifiers
                  Read_Virtual_String_Vector (Handler, Value.tokenModifiers);
               when 5 =>  --  formats
                  Read_TokenFormat_Set (Handler, Value.formats);
               when 6 =>  --  overlappingTokenSupport
                  Value.overlappingTokenSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.overlappingTokenSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 7 =>  --  multilineTokenSupport
                  Value.multilineTokenSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.multilineTokenSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 8 =>  --  serverCancelSupport
                  Value.serverCancelSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.serverCancelSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 9 =>  --  augmentsSyntaxTokens
                  Value.augmentsSyntaxTokens       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.augmentsSyntaxTokens.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensClientCapabilities;

   package RegistrationParams_Scope is
      package RegistrationParams_Map is new Minimal_Perfect_Hash
        (["registrations"]);

   end RegistrationParams_Scope;

   procedure Read_RegistrationParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RegistrationParams) is
      use RegistrationParams_Scope;
      procedure Read_Registration_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Registration_Vector);

      procedure Read_Registration_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Registration_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.Registration_Vector renames Value;
            Value : LSP.Structures.Registration;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_Registration (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_Registration_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RegistrationParams_Map.Get_Index (Key) is
               when 1 =>  --  registrations
                  Read_Registration_Vector (Handler, Value.registrations);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RegistrationParams;

end LSP.Inputs.Part_25;

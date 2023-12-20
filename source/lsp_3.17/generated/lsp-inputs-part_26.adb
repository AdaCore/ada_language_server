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

package body LSP.Inputs.Part_26 is

   package RelativePattern_Scope is
      package RelativePattern_Map is new Minimal_Perfect_Hash
        (["baseUri",
         "pattern"]);

   end RelativePattern_Scope;

   procedure Read_RelativePattern
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RelativePattern) is
      use RelativePattern_Scope;
      procedure Read_WorkspaceFolder_Or_URI
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.WorkspaceFolder_Or_URI);

      procedure Read_WorkspaceFolder_Or_URI
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.WorkspaceFolder_Or_URI) is
      begin
         if Handler.Is_String_Value then
            Value :=
              (Is_WorkspaceFolder => False,
               others             => <>);
         else
            Value :=
              (Is_WorkspaceFolder => True,
               others             => <>);
         end if;

         case Value.Is_WorkspaceFolder is
            when True =>
               Read_WorkspaceFolder (Handler, Value.WorkspaceFolder);
            when False =>
               Read_URI (Handler, Value.URI);
         end case;
      end Read_WorkspaceFolder_Or_URI;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RelativePattern_Map.Get_Index (Key) is
               when 1 =>  --  baseUri
                  Read_WorkspaceFolder_Or_URI (Handler, Value.baseUri);
               when 2 =>  --  pattern
                  Read_Pattern (Handler, Value.pattern);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RelativePattern;

   package ConfigurationParams_Scope is
      package ConfigurationParams_Map is new Minimal_Perfect_Hash (["items"]);

   end ConfigurationParams_Scope;

   procedure Read_ConfigurationParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ConfigurationParams) is
      use ConfigurationParams_Scope;
      procedure Read_ConfigurationItem_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.ConfigurationItem_Vector);

      procedure Read_ConfigurationItem_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.ConfigurationItem_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.ConfigurationItem_Vector renames Value;
            Value : LSP.Structures.ConfigurationItem;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_ConfigurationItem (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_ConfigurationItem_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ConfigurationParams_Map.Get_Index (Key) is
               when 1 =>  --  items
                  Read_ConfigurationItem_Vector (Handler, Value.items);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ConfigurationParams;

   package DeclarationParams_Scope is
      package DeclarationParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken",
         "partialResultToken",
         "alsDisplayMethodAncestryOnNavigation"]);

   end DeclarationParams_Scope;

   procedure Read_DeclarationParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationParams) is
      use DeclarationParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DeclarationParams_Map.Get_Index (Key) is
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
   end Read_DeclarationParams;

   package TypeHierarchySubtypesParams_Scope is
      package TypeHierarchySubtypesParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "item"]);

   end TypeHierarchySubtypesParams_Scope;

   procedure Read_TypeHierarchySubtypesParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchySubtypesParams) is
      use TypeHierarchySubtypesParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TypeHierarchySubtypesParams_Map.Get_Index (Key) is
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
                  Read_TypeHierarchyItem (Handler, Value.item);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TypeHierarchySubtypesParams;

   package WorkspaceDiagnosticReport_Scope is
      package WorkspaceDiagnosticReport_Map is new Minimal_Perfect_Hash
        (["items"]);

   end WorkspaceDiagnosticReport_Scope;

   procedure Read_WorkspaceDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceDiagnosticReport) is
      use WorkspaceDiagnosticReport_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceDiagnosticReport_Map.Get_Index (Key) is
               when 1 =>  --  items
                  Read_WorkspaceDocumentDiagnosticReport_Vector
                    (Handler, Value.items);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceDiagnosticReport;

   package WorkspaceSymbolClientCapabilities_Scope is
      package WorkspaceSymbolClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "symbolKind",
         "tagSupport",
         "resolveSupport"]);

   end WorkspaceSymbolClientCapabilities_Scope;

   procedure Read_WorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbolClientCapabilities) is
      use WorkspaceSymbolClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceSymbolClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  symbolKind
                  Value.symbolKind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_symbolKind_OfWorkspaceSymbolClientCapabilities
                    (Handler, Value.symbolKind.Value);
               when 3 =>  --  tagSupport
                  Value.tagSupport :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_tagSupport_OfWorkspaceSymbolClientCapabilities
                    (Handler, Value.tagSupport.Value);
               when 4 =>  --  resolveSupport
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
   end Read_WorkspaceSymbolClientCapabilities;

   package FileOperationClientCapabilities_Scope is
      package FileOperationClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "didCreate",
         "willCreate",
         "didRename",
         "willRename",
         "didDelete",
         "willDelete"]);

   end FileOperationClientCapabilities_Scope;

   procedure Read_FileOperationClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationClientCapabilities) is
      use FileOperationClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileOperationClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  didCreate
                  Value.didCreate       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.didCreate.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  willCreate
                  Value.willCreate       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.willCreate.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  didRename
                  Value.didRename       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.didRename.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 5 =>  --  willRename
                  Value.willRename       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.willRename.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 6 =>  --  didDelete
                  Value.didDelete       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.didDelete.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 7 =>  --  willDelete
                  Value.willDelete       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.willDelete.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FileOperationClientCapabilities;

end LSP.Inputs.Part_26;

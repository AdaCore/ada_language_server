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

pragma Ada_2022;

with URIs;

with VSS.Characters.Latin;
with VSS.JSON.Streams;
with VSS.String_Vectors;
with VSS.Strings.Conversions;

with LSP.Ada_Configurations;
with LSP.Constants;
with LSP.Enumerations;
with LSP.Structures.Unwrap;

with LSP.Structures.LSPAny_Vectors;

package body LSP.Ada_Client_Capabilities is

   -----------------
   -- Code_Action --
   -----------------

   function Code_Action
     (Self : Client_Capability'Class) return Boolean
   is
      use LSP.Structures.Unwrap;

      Result : constant LSP.Structures.CodeActionClientCapabilities_Optional :=
        codeAction (Self.Value.capabilities.textDocument);
   begin
      return Result.Is_Set;
   end Code_Action;

   -------------------------------
   -- Code_ActionLiteralSupport --
   -------------------------------

   function Code_ActionLiteralSupport
     (Self : Client_Capability'Class) return Boolean
   is
      use LSP.Structures.Unwrap;

      Result : constant LSP.Structures.CodeActionClientCapabilities_Optional :=
        codeAction (Self.Value.capabilities.textDocument);
   begin
      return (if Result.Is_Set
              then Result.Value.codeActionLiteralSupport.Is_Set
              else False);
   end Code_ActionLiteralSupport;

   -------------------------------
   -- Completion_SnippetSupport --
   -------------------------------

   function Completion_SnippetSupport
     (Self : Client_Capability'Class) return Boolean
   is
      use LSP.Structures.Unwrap;

      Result : constant LSP.Structures.Boolean_Optional :=
        snippetSupport
          (completionItem
            (completion
               (Self.Value.capabilities.textDocument)));
   begin
      return (if Result.Is_Set then Result.Value else False);
   end Completion_SnippetSupport;

   -----------------------------------------------
   -- didChangeWatchedFiles_dynamicRegistration --
   -----------------------------------------------

   function didChangeWatchedFiles_dynamicRegistration
     (Self : Client_Capability'Class) return Boolean
   is
      use LSP.Structures.Unwrap;

      Result : constant LSP.Structures.Boolean_Optional :=
        dynamicRegistration
          (didChangeWatchedFiles
             (Self.Value.capabilities.workspace));
   begin
      return (if Result.Is_Set then Result.Value else False);
   end didChangeWatchedFiles_dynamicRegistration;

   ------------------------------
   -- fileOperations_didCreate --
   ------------------------------

   function fileOperations_didCreate
     (Self : Client_Capability'Class) return Boolean
   is
      use LSP.Structures.Unwrap;

      Result : constant LSP.Structures.Boolean_Optional :=
        didCreate
          (fileOperations
             (Self.Value.capabilities.workspace));
   begin
      return (if Result.Is_Set then Result.Value else False);
   end fileOperations_didCreate;

   ------------------------------
   -- fileOperations_didDelete --
   ------------------------------

   function fileOperations_didDelete
     (Self : Client_Capability'Class) return Boolean
   is
      use LSP.Structures.Unwrap;

      Result : constant LSP.Structures.Boolean_Optional :=
        didDelete
          (fileOperations
             (Self.Value.capabilities.workspace));
   begin
      return (if Result.Is_Set then Result.Value else False);
   end fileOperations_didDelete;

   ------------------------------
   -- fileOperations_didRename --
   ------------------------------

   function fileOperations_didRename
     (Self : Client_Capability'Class) return Boolean
   is
      use LSP.Structures.Unwrap;

      Result : constant LSP.Structures.Boolean_Optional :=
        didDelete
          (fileOperations
             (Self.Value.capabilities.workspace));
   begin
      return (if Result.Is_Set then Result.Value else False);
   end fileOperations_didRename;

   ----------------------------------------
   -- fileOperations_dynamicRegistration --
   ----------------------------------------

   function fileOperations_dynamicRegistration
     (Self : Client_Capability'Class) return Boolean
   is
      use LSP.Structures.Unwrap;

      Result : constant LSP.Structures.Boolean_Optional :=
        dynamicRegistration
          (fileOperations
             (Self.Value.capabilities.workspace));
   begin
      return (if Result.Is_Set then Result.Value else False);
   end fileOperations_dynamicRegistration;

   -------------------------
   -- Hierarchical_Symbol --
   -------------------------

   function Hierarchical_Symbol
     (Self : Client_Capability'Class) return Boolean
   is
      use LSP.Structures.Unwrap;

      Result : constant LSP.Structures.Boolean_Optional :=
        hierarchicalDocumentSymbolSupport
          (documentSymbol
             (Self.Value.capabilities.textDocument));
   begin
      return (if Result.Is_Set then Result.Value else False);
   end Hierarchical_Symbol;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : in out Client_Capability'Class;
      Value : LSP.Structures.InitializeParams) is
   begin
      Self.Value := Value;

      if Value.rootUri.Is_Null
        and then Value.rootPath.Is_Set
        and then not Value.rootPath.Value.Is_Null
      then
         --  URI isn't provided, rollback to deprecated rootPath
         Self.Root := Value.rootPath.Value.Value;

      elsif not Value.rootUri.Is_Null then
         Self.Root := VSS.Strings.Virtual_String (Value.rootUri.Value);
      end if;

      Self.Parse_Experimental;

   end Initialize;

   ------------------------
   -- Parse_Experimental --
   ------------------------

   procedure Parse_Experimental (Self : in out Client_Capability'Class) is
      use VSS.JSON.Streams;
      use VSS.Strings;
      use LSP.Structures.JSON_Event_Vectors;
      use LSP.Structures.LSPAny_Vectors;

      C : Cursor := Self.Value.capabilities.experimental.First;
   begin
      if not Has_Element (C) then
         return;
      end if;

      pragma Assert (Element (C).Kind = Start_Object);
      Next (C);

      while Has_Element (C)
        and then Element (C).Kind /= End_Object
      loop
         pragma Assert (Element (C).Kind = Key_Name);
         declare
            Key : constant Virtual_String := Element (C).Key_Name;
         begin
            Next (C);

            if Key = "advanced_refactorings" then
               pragma Assert (Element (C).Kind = Start_Array);
               Next (C);

               while Has_Element (C)
                 and then Element (C).Kind /= End_Array
               loop
                  pragma Assert (Element (C).Kind = String_Value);
                  Self.Advanced_Refactorings
                    (Advanced_Refactorings'Value
                       (VSS.Strings.Conversions.To_UTF_8_String
                            (Element (C).String_Value))) := True;
                  Next (C);
               end loop;
            else
               Skip_Value (C);
            end if;
         end;

         Next (C);
      end loop;
   end Parse_Experimental;

   -------------------------------
   -- Resource_Create_Supported --
   -------------------------------

   function Resource_Create_Supported
     (Self : Client_Capability'Class) return Boolean
   is
      use LSP.Structures.Unwrap;

      Result : constant LSP.Structures.ResourceOperationKind_Set :=
        resourceOperations (workspaceEdit (Self.Value.capabilities.workspace));
   begin
      return Result (LSP.Enumerations.Create);
   end Resource_Create_Supported;

   -------------------------------
   -- Resource_Delete_Supported --
   -------------------------------

   function Resource_Delete_Supported
     (Self : Client_Capability'Class) return Boolean
   is
      use LSP.Structures.Unwrap;

      Result : constant LSP.Structures.ResourceOperationKind_Set :=
        resourceOperations (workspaceEdit (Self.Value.capabilities.workspace));
   begin
      return Result (LSP.Enumerations.Delete);
   end Resource_Delete_Supported;

   -------------------------------
   -- Resource_Rename_Supported --
   -------------------------------

   function Resource_Rename_Supported
     (Self : Client_Capability'Class) return Boolean
   is
      use LSP.Structures.Unwrap;

      Result : constant LSP.Structures.ResourceOperationKind_Set :=
        resourceOperations (workspaceEdit (Self.Value.capabilities.workspace));
   begin
      return Result (LSP.Enumerations.Rename);
   end Resource_Rename_Supported;

   -----------------------
   -- Set_Root_If_Empty --
   -----------------------

   procedure Set_Root_If_Empty
     (Self  : in out Client_Capability'Class;
      Value : VSS.Strings.Virtual_String) is
   begin
      if Self.Root.Is_Empty then
         Self.Root := Value;
      end if;
   end Set_Root_If_Empty;

   ----------------------------------
   -- Supports_Related_Diagnostics --
   ----------------------------------

   function Supports_Related_Diagnostics
     (Self : Client_Capability'Class) return Boolean
   is
      use LSP.Structures.Unwrap;

      Result : constant LSP.Structures.Boolean_Optional :=
        relatedInformation
          (publishDiagnostics
             (Self.Value.capabilities.textDocument));
   begin
      return (if Result.Is_Set then Result.Value else False);
   end Supports_Related_Diagnostics;

   ----------------------------
   -- To_Server_Capabilities --
   ----------------------------

   function To_Server_Capabilities
     (Self : Client_Capability'Class;
      Incremental_Text_Changes : Boolean;
      Commands                 : LSP.Structures.Virtual_String_Vector;
      Token_Types              : LSP.Structures.Virtual_String_Vector;
      Token_Modifiers          : LSP.Structures.Virtual_String_Vector)
      return LSP.Structures.ServerCapabilities
   is
      use type VSS.Strings.Virtual_String;

      function Ada_Patterns
        return LSP.Structures.FileOperationFilter_Vectors.Vector is
           [(scheme  => "file",   --  shell we also support `untitled`???
             pattern =>
               (glob    => "**/*.ad{a,s,b,c}",
                matches => LSP.Constants.file,
                options => LSP.Constants.ignoreCase))];

      function Ada_Extensions (Enabled : Boolean)
        return LSP.Structures.FileOperationRegistrationOptions_Optional is
          (if Enabled then
             (Is_Set => True,
              Value  => (filters => (Ada_Patterns with null record)))
           else (Is_Set => False));

      function fileOperations
        return LSP.Structures.FileOperationOptions_Optional is
          (if Self.fileOperations_didCreate
             or else Self.fileOperations_didRename
             or else Self.fileOperations_didDelete
           then
             (Is_Set => True,
              Value  =>
                (didCreate => Ada_Extensions (Self.fileOperations_didCreate),
                 didRename => Ada_Extensions (Self.fileOperations_didRename),
                 didDelete => Ada_Extensions (Self.fileOperations_didDelete),
                 others    => <>))
           else
             (Is_Set => False));

      function Supported_Code_Action_Kinds
        return LSP.Structures.CodeActionKind_Vectors.Vector is
          [LSP.Enumerations.QuickFix,
           LSP.Enumerations.RefactorRewrite];

      function Full_codeActionProvider
        return LSP.Structures.Boolean_Or_CodeActionOptions_Optional is
          (Is_Set => True,
           Value  =>
             (Is_Boolean        => False,
              CodeActionOptions =>
                (workDoneProgress => LSP.Constants.False,
                 codeActionKinds  => (Supported_Code_Action_Kinds
                                        with null record),
                 resolveProvider  => LSP.Constants.False)));

   begin
      return Result : LSP.Structures.ServerCapabilities do
         Result.alsReferenceKinds := (others => True);

         Result.textDocumentSync :=
           (Is_Set => True,
            Value  =>
              (Is_TextDocumentSyncOptions => False,
               TextDocumentSyncKind       =>
                 (if Incremental_Text_Changes then LSP.Enumerations.Incremental
                  else LSP.Enumerations.Full)));

         Result.callHierarchyProvider           := LSP.Constants.True;
         Result.declarationProvider             := LSP.Constants.True;
         Result.definitionProvider              := LSP.Constants.True;
         Result.documentFormattingProvider      := LSP.Constants.True;
         Result.documentHighlightProvider       := LSP.Constants.True;
         Result.documentRangeFormattingProvider := LSP.Constants.True;
         Result.documentSymbolProvider          := LSP.Constants.True;
         Result.foldingRangeProvider            := LSP.Constants.True;
         Result.hoverProvider                   := LSP.Constants.True;
         Result.implementationProvider          := LSP.Constants.True;
         Result.referencesProvider              := LSP.Constants.True;
         Result.selectionRangeProvider          := LSP.Constants.True;
         Result.typeDefinitionProvider          := LSP.Constants.True;
         Result.typeHierarchyProvider           := LSP.Constants.True;
         Result.workspaceSymbolProvider         := LSP.Constants.True;

         Result.completionProvider :=
           (Is_Set => True,
            Value  => (triggerCharacters => [".", ",", "'", "("],
                       resolveProvider   => LSP.Constants.True,
                       others            => <>));

         Result.codeActionProvider :=
           (if Self.Code_ActionLiteralSupport then Full_codeActionProvider
            else LSP.Constants.True);

         Result.documentOnTypeFormattingProvider :=
           (if LSP.Ada_Configurations.On_Type_Formatting
            then (Is_Set => True,
                  Value  => LSP.Ada_Configurations.On_Type_Formatting_Settings)
            else (Is_Set => False));

         Result.executeCommandProvider :=
           (Is_Set => True,
            Value  =>
              (commands => Commands,
               others   => <>));

         Result.renameProvider :=
           (Is_Set => True,
            Value  =>
              (Is_Boolean    => False,
               RenameOptions =>
                 (prepareProvider => LSP.Constants.True,
                  others          => <>)));

         Result.semanticTokensProvider :=
           (Is_Set => True,
            Value  =>
              (Is_SemanticTokensOptions => True,
               SemanticTokensOptions    =>
                 (full    => LSP.Constants.True,
                  a_range => LSP.Constants.True,
                  legend  =>
                    (tokenTypes     => Token_Types,
                     tokenModifiers => Token_Modifiers),
                  others  => <>)));

         Result.signatureHelpProvider :=
           (Is_Set => True,
            Value  =>
              (triggerCharacters   => [",", "("],
               retriggerCharacters => [1 * VSS.Characters.Latin.Backspace],
               workDoneProgress    => <>));

         Result.workspace :=
           (Is_Set => True,
            Value  =>
              (fileOperations   => fileOperations,
               workspaceFolders => <>));

      end return;
   end To_Server_Capabilities;

   -----------------------
   -- Line_Folding_Only --
   -----------------------

   function Line_Folding_Only
     (Self : Client_Capability'Class) return Boolean
   is
      use LSP.Structures.Unwrap;

      Result : constant LSP.Structures.Boolean_Optional :=
        lineFoldingOnly (foldingRange (Self.Value.capabilities.textDocument));
   begin
      return (if Result.Is_Set then Result.Value else False);
   end Line_Folding_Only;

   ---------------------
   -- Token_Modifiers --
   ---------------------

   function Token_Modifiers (Self : Client_Capability'Class)
     return LSP.Structures.Virtual_String_Vector
   is
      use LSP.Structures.Unwrap;

   begin
      return tokenModifiers
        (semanticTokens (Self.Value.capabilities.textDocument));
   end Token_Modifiers;

   -----------------
   -- Token_Types --
   -----------------

   function Token_Types (Self : Client_Capability'Class)
     return LSP.Structures.Virtual_String_Vector
   is
      use LSP.Structures.Unwrap;

   begin
      return tokenTypes
        (semanticTokens (Self.Value.capabilities.textDocument));
   end Token_Types;

   -------------------------------
   -- Refactoring_Add_Parameter --
   -------------------------------

   function Refactoring_Add_Parameter
     (Self : Client_Capability'Class) return Boolean is
   begin
      return Self.Advanced_Refactorings (Add_Parameter);
   end Refactoring_Add_Parameter;

   -------------------------------------------------
   -- Refactoring_Change_Parameters_Default_Value --
   -------------------------------------------------

   function Refactoring_Change_Parameters_Default_Value
     (Self : Client_Capability'Class) return Boolean is
   begin
      return Self.Advanced_Refactorings (Change_Parameters_Default_Value);
   end Refactoring_Change_Parameters_Default_Value;

   ----------------------------------------
   -- Refactoring_Change_Parameters_Type --
   ----------------------------------------

   function Refactoring_Change_Parameters_Type
     (Self : Client_Capability'Class) return Boolean is
   begin
      return Self.Advanced_Refactorings (Change_Parameters_Type);
   end Refactoring_Change_Parameters_Type;

   ------------------------------
   -- Refactoring_Replace_Type --
   ------------------------------

   function Refactoring_Replace_Type
     (Self : Client_Capability'Class) return Boolean is
   begin
      return Self.Advanced_Refactorings (Replace_Type);
   end Refactoring_Replace_Type;

   --------------------
   -- Resolve_Lazily --
   --------------------

   function Resolve_Lazily (Self : Client_Capability'Class) return Boolean is
      use LSP.Structures.Unwrap;

      List : constant VSS.String_Vectors.Virtual_String_Vector :=
        properties
          (resolveSupport
             (completionItem
                (completion
                   (Self.Value.capabilities.textDocument))));

   begin
      return List.Contains ("detail") and then List.Contains ("documentation");
   end Resolve_Lazily;

   -------------------------
   -- Versioned_Documents --
   -------------------------

   function Versioned_Documents
     (Self : Client_Capability'Class) return Boolean
   is
      use LSP.Structures.Unwrap;

      Result : constant LSP.Structures.Boolean_Optional :=
        documentChanges (workspaceEdit (Self.Value.capabilities.workspace));
   begin
      return (if Result.Is_Set then Result.Value else False);
   end Versioned_Documents;

   --------------------
   -- Root_Directory --
   --------------------

   function Root_Directory (Client : Client_Capability'Class)
      return GNATCOLL.VFS.Virtual_File
   is
      Value : constant VSS.Strings.Virtual_String := Client.Root;
   begin
      if Value.Is_Empty then
         return GNATCOLL.VFS.No_File;
      else
         declare
            Root  : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (Value);
         begin
            return GNATCOLL.VFS.Create_From_UTF8
              (if Value.Starts_With ("file://")
               then URIs.Conversions.To_File (Root, True)
               else Root);
         end;
      end if;
   end Root_Directory;

end LSP.Ada_Client_Capabilities;

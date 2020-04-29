------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2020, AdaCore                     --
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

with Ada.Tags;  use Ada.Tags;
with Ada.Tags.Generic_Dispatching_Constructor;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;

with GNATCOLL.JSON;    use GNATCOLL.JSON;
with LSP.JSON_Streams;
with LSP.Message_IO;

package body LSP.Messages is

   procedure Read_If_String
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out LSP.Types.LSP_String);

   procedure Read_Number
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out LSP.Types.LSP_Number);

   procedure Write_Optional_Number
    (Stream     : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key        : LSP.Types.LSP_String;
     Item       : LSP.Types.Optional_Number;
     Write_Null : Boolean := False);
   --  If Item has a value write its value into Key. Otherwise if Write_Null,
   --  then write 'null' into Key. Otherwise do nothing.

   procedure Write_Optional_String
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : LSP.Types.Optional_String);

   procedure Read_String_Vector
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out LSP.Types.LSP_String_Vector);

   procedure Write_String_Vector
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : LSP.Types.LSP_String_Vector);

   Simple_Reference_Image           : aliased constant Standard.String :=
                                        "reference";
   Write_Reference_Image            : aliased constant Standard.String :=
                                        "write";
   Static_Call_Reference_Image      : aliased constant Standard.String :=
                                        "call";
   Dispatching_Call_Reference_Image : aliased constant Standard.String :=
                                        "dispatching call";
   Parent_Reference_Image           : aliased constant Standard.String :=
                                        "parent";
   Child_Reference_Image            : aliased constant Standard.String :=
                                        "child";

   type String_Constant_Access is access constant Standard.String;

   AlsReferenceKind_Map : constant array
     (AlsReferenceKind) of not null String_Constant_Access :=
     (Simple           => Simple_Reference_Image'Access,
      Write            => Write_Reference_Image'Access,
      Static_Call      => Static_Call_Reference_Image'Access,
      Dispatching_Call => Dispatching_Call_Reference_Image'Access,
      Parent           => Parent_Reference_Image'Access,
      Child            => Child_Reference_Image'Access);

   function Create_Command is new Ada.Tags.Generic_Dispatching_Constructor
     (T           => LSP.Commands.Command,
      Parameters  => LSP.JSON_Streams.JSON_Stream'Class,
      Constructor => LSP.Commands.Create);

   --------------------------------
   -- Get_WorkDoneProgressParams --
   --------------------------------

   procedure Get_WorkDoneProgressParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkDoneProgressParams'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Read (S, V.workDoneToken);
   end Get_WorkDoneProgressParams;

   --------------------------------
   -- Put_WorkDoneProgressParams --
   --------------------------------

   procedure Put_WorkDoneProgressParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkDoneProgressParams'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
   end Put_WorkDoneProgressParams;

   -----------------------------
   -- Get_PartialResultParams --
   -----------------------------

   procedure Get_PartialResultParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out PartialResultParams'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Read (S, V.partialResultToken);
   end Get_PartialResultParams;

   -----------------------------
   -- Put_PartialResultParams --
   -----------------------------

   procedure Put_PartialResultParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : PartialResultParams'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
   end Put_PartialResultParams;

   ---------------------------------
   -- Get_Progress_Partial_Params --
   ---------------------------------

   procedure Get_Progress_Partial_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Progress_Partial_Params'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Read (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Read (S, V.partialResultToken);
   end Get_Progress_Partial_Params;

   ---------------------------------
   -- Put_Progress_Partial_Params --
   ---------------------------------

   procedure Put_Progress_Partial_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Progress_Partial_Params'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
   end Put_Progress_Partial_Params;

   --------------------------------------
   -- Get_Text_Progress_Partial_Params --
   --------------------------------------

   procedure Get_Text_Progress_Partial_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Text_Progress_Partial_Params'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("position");
      Position'Read (S, V.position);
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Read (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Read (S, V.partialResultToken);
   end Get_Text_Progress_Partial_Params;

   --------------------------------------
   -- Put_Text_Progress_Partial_Params --
   --------------------------------------

   procedure Put_Text_Progress_Partial_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Text_Progress_Partial_Params'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      Position'Write (S, V.position);
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
   end Put_Text_Progress_Partial_Params;

   ------------------------------
   -- Get_Text_Progress_Params --
   ------------------------------

   procedure Get_Text_Progress_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Text_Progress_Params'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("position");
      Position'Read (S, V.position);
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Read (S, V.workDoneToken);
   end Get_Text_Progress_Params;

   ------------------------------
   -- Put_Text_Progress_Params --
   ------------------------------

   procedure Put_Text_Progress_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Text_Progress_Params'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      Position'Write (S, V.position);
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
   end Put_Text_Progress_Params;

   -------------------
   -- Method_To_Tag --
   -------------------

   function Method_To_Tag
     (Map    : Maps.Map;
      Method : LSP.Types.LSP_String) return Ada.Tags.Tag
   is
      Cursor : constant Maps.Cursor := Map.Find (Method);
   begin
      if Maps.Has_Element (Cursor) then
         return Maps.Element (Cursor);
      else
         return Ada.Tags.No_Tag;
      end if;
   end Method_To_Tag;

   ---------------------------
   -- Read_AlsReferenceKind --
   ---------------------------

   procedure Read_AlsReferenceKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out AlsReferenceKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String := JS.Read.Get;
   begin
      for J in AlsReferenceKind_Map'Range loop
         if Text = AlsReferenceKind_Map (J).all then
            V := J;
            return;
         end if;
      end loop;

      V := AlsReferenceKind'First;
   end Read_AlsReferenceKind;

   -------------------------------
   -- Read_AlsReferenceKind_Set --
   -------------------------------

   procedure Read_AlsReferenceKind_Set
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out AlsReferenceKind_Set)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Kind : AlsReferenceKind;
   begin
      if JS.Is_Server_Side then
         V := (Is_Server_Side => True, others => <>);
      else
         V := (Is_Server_Side => False, others => <>);
      end if;

      JS.Start_Array;

      while not JS.End_Of_Array loop
         if V.Is_Server_Side then
            AlsReferenceKind'Read (S, Kind);
            V.As_Flags (Kind) := True;
         else
            V.As_Strings.Append (+JS.Read.Get);
         end if;
      end loop;

      JS.End_Array;
   end Read_AlsReferenceKind_Set;

   procedure Write_AlsReferenceKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : AlsReferenceKind)
      renames LSP.Message_IO.Write_AlsReferenceKind;

   --------------------------------
   -- Write_AlsReferenceKind_Set --
   --------------------------------

   procedure Write_AlsReferenceKind_Set
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : AlsReferenceKind_Set)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V /= Empty_Set then
         if V.Is_Server_Side then
            JS.Start_Array;

            for J in V.As_Flags'Range loop
               if V.As_Flags (J) then
                  AlsReferenceKind'Write (S, J);
               end if;
            end loop;

            JS.End_Array;
         else
            LSP_String_Vector'Write (S, V.As_Strings);
         end if;

      end if;
   end Write_AlsReferenceKind_Set;

   -----------------------------------
   -- Read_ApplyWorkspaceEditParams --
   -----------------------------------

   procedure Read_ApplyWorkspaceEditParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ApplyWorkspaceEditParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("edit");
      WorkspaceEdit'Read (S, V.edit);
      JS.End_Object;
   end Read_ApplyWorkspaceEditParams;

   -----------------------------------
   -- Read_ApplyWorkspaceEditResult --
   -----------------------------------

   procedure Read_ApplyWorkspaceEditResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ApplyWorkspaceEditResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Boolean (JS, +"applied", V.applied);
      Read_Optional_String (JS, +"failureReason", V.failureReason);
      JS.End_Object;
   end Read_ApplyWorkspaceEditResult;

   -----------------------
   -- Read_CancelParams --
   -----------------------

   procedure Read_CancelParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CancelParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Number_Or_String (JS, +"id", V.id);
      JS.End_Object;
   end Read_CancelParams;

   -----------------------------
   -- Read_ClientCapabilities --
   -----------------------------

   procedure Read_ClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workspace");
      WorkspaceClientCapabilities'Read (S, V.workspace);
      JS.Key ("textDocument");
      TextDocumentClientCapabilities'Read (S, V.textDocument);
      JS.End_Object;
   end Read_ClientCapabilities;

   ---------------------
   -- Read_ClientInfo --
   ---------------------

   procedure Read_ProgramInfo
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ProgramInfo)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_String (JS, +"name", V.name);
      Read_Optional_String (JS, +"version", V.version);
      JS.End_Object;
   end Read_ProgramInfo;

   ---------------------
   -- Read_CodeAction --
   ---------------------

   procedure Read_CodeAction
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeAction)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Value : GNATCOLL.JSON.JSON_Value;
   begin
      JS.Start_Object;

      --  if the object is Command set just V.command property

      JS.Key ("command");
      Value := JS.Read;

      if Value.Kind in GNATCOLL.JSON.JSON_String_Type then
         V.command := (Is_Set => True, Value => <>);
         Read_String (JS, +"title", V.command.Value.title);
         Read_String (JS, +"command", V.command.Value.command);
         JS.Key ("arguments");
         Optional_Any_Vector'Read (S, V.command.Value.arguments);
      else
         Optional_Command'Read (S, V.command);
         Read_String (JS, +"title", V.title);
         JS.Key ("kind");
         Optional_CodeActionKind'Read (S, V.kind);
         JS.Key ("diagnostics");
         Optional_Diagnostic_Vector'Read (S, V.diagnostics);
         Read_Optional_Boolean (JS, +"isPreferred", V.isPreferred);
         JS.Key ("edit");
         Optional_WorkspaceEdit'Read (S, V.edit);
      end if;

      JS.End_Object;
   end Read_CodeAction;

   ----------------------------
   -- Read_CodeActionContext --
   ----------------------------

   procedure Read_CodeActionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("diagnostics");
      Diagnostic_Vector'Read (S, V.diagnostics);
      JS.Key ("only");
      Optional_CodeActionKindSet'Read (S, V.only);
      JS.End_Object;
   end Read_CodeActionContext;

   ----------------------------
   -- Read_CodeActionOptions --
   ----------------------------

   procedure Read_CodeActionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("codeActionKinds");
      Read_Optional_Boolean (JS, +"workDoneProgress", V.workDoneProgress);
      Optional_CodeActionKindSet'Read (S, V.codeActionKinds);
      JS.End_Object;
   end Read_CodeActionOptions;

   -------------------------
   -- Read_CodeActionKind --
   -------------------------

   procedure Read_CodeActionKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Text : constant Standard.String := JS.Read.Get;
   begin
      if Text = "quickfix" then
         V := QuickFix;
      elsif Text = "refactor" then
         V := Refactor;
      elsif Text = "refactor.extract" then
         V := RefactorExtract;
      elsif Text = "refactor.inline" then
         V := RefactorInline;
      elsif Text = "refactor.rewrite" then
         V := RefactorRewrite;
      elsif Text = "source" then
         V := Source;
      elsif Text = "source.organizeImports" then
         V := SourceOrganizeImports;
      else
         V := Empty;
      end if;
   end Read_CodeActionKind;

   -----------------------------------
   -- Read_codeActionKindCapability --
   -----------------------------------

   procedure Read_codeActionKindCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out codeActionKindCapability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      CodeActionKindSet'Read (S, V.valueSet);
      JS.End_Object;
   end Read_codeActionKindCapability;

   ----------------------------------------------
   -- Read_codeActionLiteralSupport_Capability --
   ----------------------------------------------

   procedure Read_codeActionLiteralSupport_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out codeActionLiteralSupport_Capability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("codeActionKind");
      codeActionKindCapability'Read (S, V.codeActionKind);

      JS.End_Object;
   end Read_codeActionLiteralSupport_Capability;

   ---------------------------
   -- Read_CodeActionParams --
   ---------------------------

   procedure Read_CodeActionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Get_Progress_Partial_Params (S, V);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("range");
      Span'Read (S, V.span);
      JS.Key ("context");
      CodeActionContext'Read (S, V.context);
      JS.End_Object;
   end Read_CodeActionParams;

   ---------------------------------------
   -- Read_CodeActionClientCapabilities --
   ---------------------------------------

   procedure Read_CodeActionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean
        (JS, +"dynamicRegistration", V.dynamicRegistration);
      JS.Key ("codeActionLiteralSupport");
      Optional_codeActionLiteralSupport_Capability'Read
        (S, V.codeActionLiteralSupport);
      Read_Optional_Boolean (JS, +"isPreferredSupport", V.isPreferredSupport);
      JS.End_Object;
   end Read_CodeActionClientCapabilities;

   --------------------------
   -- Read_CodeLensOptions --
   --------------------------

   procedure Read_CodeLensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeLensOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean (JS, +"workDoneProgress", V.workDoneProgress);
      Read_Optional_Boolean (JS, +"resolveProvider", V.resolveProvider);
      JS.End_Object;
   end Read_CodeLensOptions;

   ---------------------------
   -- Read_ColorInformation --
   ---------------------------

   procedure Read_ColorInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ColorInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      Span'Read (S, V.span);
      JS.Key ("color");
      RGBA_Color'Read (S, V.color);
      JS.End_Object;
   end Read_ColorInformation;

   ----------------------------
   -- Read_ColorPresentation --
   ----------------------------

   procedure Read_ColorPresentation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ColorPresentation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_String (JS, +"label", V.label);
      JS.Key ("textEdit");
      Optional_TextEdit'Read (S, V.textEdit);
      JS.Key ("additionalTextEdits");
      TextEdit_Vector'Read (S, V.additionalTextEdits);
      JS.End_Object;
   end Read_ColorPresentation;

   ----------------------------------
   -- Read_ColorPresentationParams --
   ----------------------------------

   procedure Read_ColorPresentationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ColorPresentationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Get_Progress_Partial_Params (S, V);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("color");
      RGBA_Color'Read (S, V.color);
      JS.Key ("range");
      Span'Read (S, V.span);
      JS.End_Object;
   end Read_ColorPresentationParams;

   ------------------
   -- Read_Command --
   ------------------

   procedure Read_Command
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Command)
   is
      Tag : Ada.Tags.Tag;
      JS  : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_String (JS, +"title", V.title);
      Read_String (JS, +"command", V.command);
      Tag := Ada.Tags.Internal_Tag (LSP.Types.To_UTF_8_String (V.command));
      JS.Key ("arguments");

      if Tag in Ada.Tags.No_Tag then
         Optional_Any_Vector'Read (S, V.arguments);
      else
         V :=
           (Is_Unknown => False,
            title      => V.title,
            Custom     => <>);

         JS.Start_Array;
         V.Custom.Set (Create_Command (Tag, JS'Access));
         JS.End_Array;
      end if;

      JS.End_Object;
   end Read_Command;

   ---------------------------------------
   -- Read_CompletionClientCapabilities --
   ---------------------------------------

   procedure Read_CompletionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean
        (JS, +"dynamicRegistration", V.dynamicRegistration);
      JS.Key ("completionItem");
      Optional_completionItemCapability'Read (S, V.completionItem);
      JS.Key ("completionItemKind");
      Optional_CompletionItemKindSetCapabilities'Read
        (S, V.completionItemKind);
      Read_Optional_Boolean (JS, +"contextSupport", V.contextSupport);
      JS.End_Object;
   end Read_CompletionClientCapabilities;

   -------------------------
   -- Read_CompletionList --
   -------------------------

   procedure Read_CompletionList
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionList)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("isIncomplete");
      V.isIncomplete := JS.Read.Get;
      JS.Key ("items");
      CompletionItem_Vector'Read (S, V.items);
      JS.End_Object;
   end Read_CompletionList;

   ----------------------------
   -- Read_CompletionContext --
   ----------------------------

   procedure Read_CompletionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("triggerKind");
      CompletionTriggerKind'Read (S, V.triggerKind);
      JS.Key ("triggerCharacter");
      Optional_String'Read (S, V.triggerCharacter);
      JS.End_Object;
   end Read_CompletionContext;

   -------------------------
   -- Read_CompletionItem --
   -------------------------

   procedure Read_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_String (JS, +"label", V.label);
      JS.Key ("kind");
      Optional_CompletionItemKind'Read (S, V.kind);
      JS.Key ("tags");
      Optional_CompletionItemTagSet'Read (S, V.tags);
      Read_Optional_String (JS, +"detail", V.detail);
      JS.Key ("documentation");
      Optional_String_Or_MarkupContent'Read (S, V.documentation);
      Read_Optional_Boolean (JS, +"deprecated", V.deprecated);
      Read_Optional_Boolean (JS, +"preselect", V.preselect);
      Read_Optional_String (JS, +"sortText", V.sortText);
      Read_Optional_String (JS, +"filterText", V.filterText);
      Read_Optional_String (JS, +"insertText", V.insertText);
      JS.Key ("insertTextFormat");
      Optional_InsertTextFormat'Read (S, V.insertTextFormat);
      JS.Key ("textEdit");
      Optional_TextEdit'Read (S, V.textEdit);
      JS.Key ("additionalTextEdits");
      TextEdit_Vector'Read (S, V.additionalTextEdits);
      JS.Key ("commitCharacters");
      Optional_LSP_String_Vector'Read (S, V.commitCharacters);
      JS.Key ("command");
      Optional_Command'Read (S, V.command);
      JS.End_Object;
   end Read_CompletionItem;

   -----------------------------------
   -- Read_completionItemCapability --
   -----------------------------------

   procedure Read_completionItemCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out completionItemCapability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean (JS, +"snippetSupport", V.snippetSupport);
      Read_Optional_Boolean
        (JS, +"commitCharactersSupport", V.commitCharactersSupport);

      JS.Key ("documentationFormat");
      MarkupKind_Vector'Read (S, V.documentationFormat);
      Read_Optional_Boolean (JS, +"deprecatedSupport", V.deprecatedSupport);
      Read_Optional_Boolean (JS, +"preselectSupport", V.preselectSupport);
      JS.Key ("tagSupport");
      Optional_CompletionItemTagSupport'Read (S, V.tagSupport);

      JS.End_Object;
   end Read_completionItemCapability;

   -----------------------------
   -- Read_CompletionItemKind --
   -----------------------------

   procedure Read_CompletionItemKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItemKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := CompletionItemKind'Val (JS.Read.Get - 1);
   end Read_CompletionItemKind;

   --------------------------------------------
   -- Read_CompletionItemKindSetCapabilities --
   --------------------------------------------

   procedure Read_CompletionItemKindSetCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItemKindSetCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      Optional_CompletionItemKindSet'Read (S, V.valueSet);
      JS.End_Object;
   end Read_CompletionItemKindSetCapabilities;

   ----------------------------
   -- Read_CompletionItemTag --
   ----------------------------

   procedure Read_CompletionItemTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItemTag)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := CompletionItemTag'Val (JS.Read.Get - 1);
   end Read_CompletionItemTag;

   -----------------------------------
   -- Read_CompletionItemTagSupport --
   -----------------------------------

   procedure Read_CompletionItemTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItemTagSupport)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      CompletionItemTagSet'Read (S, V.valueSet);
      JS.End_Object;
   end Read_CompletionItemTagSupport;

   ----------------------------
   -- Read_CompletionOptions --
   ----------------------------

   procedure Read_CompletionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean (JS, +"workDoneProgress", V.workDoneProgress);
      JS.Key ("triggerCharacters");
      Optional_LSP_String_Vector'Read (S, V.triggerCharacters);
      JS.Key ("allCommitCharacters");
      Optional_LSP_String_Vector'Read (S, V.allCommitCharacters);
      Read_Optional_Boolean (JS, +"resolveProvider", V.resolveProvider);
      JS.End_Object;
   end Read_CompletionOptions;

   ---------------------------
   -- Read_CompletionParams --
   ---------------------------

   procedure Read_CompletionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Get_Text_Progress_Partial_Params (S, V);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("position");
      Position'Read (S, V.position);
      JS.Key ("context");
      Optional_CompletionContext'Read (S, V.context);
      JS.End_Object;
   end Read_CompletionParams;

   --------------------------------
   -- Read_CompletionTriggerKind --
   --------------------------------

   procedure Read_CompletionTriggerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionTriggerKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := CompletionTriggerKind'Val (JS.Read.Get - 1);
   end Read_CompletionTriggerKind;

   ----------------------------
   -- Read_ConfigurationItem --
   ----------------------------

   procedure Read_ConfigurationItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ConfigurationItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_String (JS, +"scopeUri", V.scopeUri);
      Read_Optional_String (JS, +"section", V.section);
      JS.End_Object;
   end Read_ConfigurationItem;

   ------------------------------
   -- Read_ConfigurationParams --
   ------------------------------

   procedure Read_ConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ConfigurationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("items");
      ConfigurationItem_Vector'Read (S, V.items);
      JS.End_Object;
   end Read_ConfigurationParams;

   ---------------------
   -- Read_Diagnostic --
   ---------------------

   procedure Read_Diagnostic
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Diagnostic)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      Span'Read (S, V.span);
      JS.Key ("severity");
      Optional_DiagnosticSeverity'Read (S, V.severity);
      LSP.Types.Read_Number_Or_String (JS, +"code", V.code);
      Read_Optional_String (JS, +"source", V.source);
      Read_String (JS, +"message", V.message);
      JS.Key ("relatedInformation");
      DiagnosticRelatedInformation_Vector'Read (S, V.relatedInformation);
      JS.End_Object;
   end Read_Diagnostic;

   ---------------------------------------
   -- Read_DiagnosticRelatedInformation --
   ---------------------------------------

   procedure Read_DiagnosticRelatedInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DiagnosticRelatedInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("location");
      Location'Read (S, V.location);
      Read_String (JS, +"message", V.message);
      JS.End_Object;
   end Read_DiagnosticRelatedInformation;

   -----------------------------
   -- Read_DiagnosticSeverity --
   -----------------------------

   procedure Read_DiagnosticSeverity
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DiagnosticSeverity)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := DiagnosticSeverity'Val (JS.Read.Get - 1);
   end Read_DiagnosticSeverity;

   ------------------------
   -- Read_DiagnosticTag --
   ------------------------

   procedure Read_DiagnosticTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DiagnosticTag)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := DiagnosticTag'Val (JS.Read.Get - 1);
   end Read_DiagnosticTag;

   -------------------------------
   -- Read_DiagnosticTagSupport --
   -------------------------------

   procedure Read_DiagnosticTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DiagnosticTagSupport)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      DiagnosticTagSet'Read (S, V.valueSet);
      JS.End_Object;
   end Read_DiagnosticTagSupport;

   ---------------------------------------
   -- Read_DidChangeConfigurationParams --
   ---------------------------------------

   procedure Read_DidChangeConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeConfigurationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("settings");
      LSP.Types.LSP_Any'Read (S, V.settings);
      JS.End_Object;
   end Read_DidChangeConfigurationParams;

   --------------------------------------
   -- Read_DidChangeTextDocumentParams --
   --------------------------------------

   procedure Read_DidChangeTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      VersionedTextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("contentChanges");
      TextDocumentContentChangeEvent_Vector'Read (S, V.contentChanges);
      JS.End_Object;
   end Read_DidChangeTextDocumentParams;

   ---------------------------------------------------
   -- Read_DidChangeWatchedFilesRegistrationOptions --
   ---------------------------------------------------

   procedure Read_DidChangeWatchedFilesRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeWatchedFilesRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("watchers");
      FileSystemWatcher_Vector'Read (S, V.watchers);
      JS.End_Object;
   end Read_DidChangeWatchedFilesRegistrationOptions;

   ------------------------------------------
   -- Read_DidChangeWorkspaceFoldersParams --
   ------------------------------------------

   procedure Read_DidChangeWorkspaceFoldersParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeWorkspaceFoldersParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("event");
      WorkspaceFoldersChangeEvent'Read (S, V.event);
      JS.End_Object;
   end Read_DidChangeWorkspaceFoldersParams;

   -------------------------------------
   -- Read_DidCloseTextDocumentParams --
   -------------------------------------

   procedure Read_DidCloseTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidCloseTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.End_Object;
   end Read_DidCloseTextDocumentParams;

   ------------------------------------
   -- Read_DidOpenTextDocumentParams --
   ------------------------------------

   procedure Read_DidOpenTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidOpenTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentItem'Read (S, V.textDocument);
      JS.End_Object;
   end Read_DidOpenTextDocumentParams;

   ------------------------------------
   -- Read_DidSaveTextDocumentParams --
   ------------------------------------

   procedure Read_DidSaveTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidSaveTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      Read_Optional_String (JS, +"text", V.text);
      JS.End_Object;
   end Read_DidSaveTextDocumentParams;

   ----------------------------------------
   -- Read_DeclarationClientCapabilities --
   ----------------------------------------

   procedure Read_DeclarationClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DeclarationClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean
        (JS, +"dynamicRegistration", V.dynamicRegistration);
      Read_Optional_Boolean (JS, +"linkSupport", V.linkSupport);
      JS.End_Object;
   end Read_DeclarationClientCapabilities;

   ------------------------------------------
   -- Read_WorkspaceEditClientCapabilities --
   ------------------------------------------

   procedure Read_WorkspaceEditClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceEditClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean (JS, +"documentChanges", V.documentChanges);
      JS.Key ("resourceOperations");
      Optional_ResourceOperationKindSet'Read (S, V.resourceOperations);
      JS.Key ("failureHandling");
      Optional_FailureHandlingKind'Read (S, V.failureHandling);
      JS.End_Object;
   end Read_WorkspaceEditClientCapabilities;

   ----------------------------
   -- Read_DocumentHighlight --
   ----------------------------

   procedure Read_DocumentHighlight
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentHighlight)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      Span'Read (S, V.span);
      JS.Key ("kind");
      Optional_DocumentHighlightKind'Read (S, V.kind);
      JS.End_Object;
   end Read_DocumentHighlight;

   --------------------------------
   -- Read_DocumentHighlightKind --
   --------------------------------

   procedure Read_DocumentHighlightKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentHighlightKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := DocumentHighlightKind'Val (JS.Read.Get - 1);
   end Read_DocumentHighlightKind;

   -----------------------------------------
   -- Read_DocumentLinkClientCapabilities --
   -----------------------------------------

   procedure Read_DocumentLinkClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentLinkClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean
        (JS, +"dynamicRegistration", V.dynamicRegistration);
      Read_Optional_Boolean (JS, +"tooltipSupport", V.tooltipSupport);
      JS.End_Object;
   end Read_DocumentLinkClientCapabilities;

   ------------------------------
   -- Read_DocumentLinkOptions --
   ------------------------------

   procedure Read_DocumentLinkOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentLinkOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean (JS, +"workDoneProgress", V.workDoneProgress);
      Read_Optional_Boolean (JS, +"resolveProvider", V.resolveProvider);
      JS.End_Object;
   end Read_DocumentLinkOptions;

   ------------------------------------------
   -- Read_DocumentOnTypeFormattingOptions --
   ------------------------------------------

   procedure Read_DocumentOnTypeFormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentOnTypeFormattingOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean (JS, +"workDoneProgress", V.workDoneProgress);
      Read_String (JS, +"firstTriggerCharacter", V.firstTriggerCharacter);
      JS.Key ("moreTriggerCharacter");
      Optional_LSP_String_Vector'Read (S, V.moreTriggerCharacter);
      JS.End_Object;
   end Read_DocumentOnTypeFormattingOptions;

   -------------------------------
   -- Read_DocumentSymbolParams --
   -------------------------------

   procedure Read_DocumentSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentSymbolParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Get_Progress_Partial_Params (S, V);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.End_Object;
   end Read_DocumentSymbolParams;

   ------------------------------
   -- Read_DocumentSymbol_Tree --
   ------------------------------

   procedure Read_DocumentSymbol_Tree
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentSymbol_Tree)
   is
      procedure Read_Array (Parent : DocumentSymbol_Trees.Cursor);

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      ----------------
      -- Read_Array --
      ----------------

      procedure Read_Array (Parent : DocumentSymbol_Trees.Cursor) is
      begin
         JS.Start_Array;

         while not JS.End_Of_Array loop
            declare
               Item : DocumentSymbol;
               Next : DocumentSymbol_Trees.Cursor;
            begin
               JS.Start_Object;
               Read_String (JS, +"name", Item.name);
               Read_Optional_String (JS, +"detail", Item.detail);
               JS.Key ("kind");
               SymbolKind'Read (S, Item.kind);
               Read_Optional_Boolean (JS, +"deprecated", Item.deprecated);
               JS.Key ("range");
               Span'Read (S, Item.span);
               JS.Key ("selectionRange");
               Span'Read (S, Item.selectionRange);
               Read_Optional_Boolean
                 (JS, +"alsIsDeclaration", Item.alsIsDeclaration);
               Read_Optional_Boolean
                 (JS, +"alsIsAdaProcedure", Item.alsIsAdaProcedure);
               JS.Key ("alsVisibility");
               Optional_Als_Visibility'Read (S, Item.alsVisibility);
               JS.Key ("children");

               if JS.Read.Kind in GNATCOLL.JSON.JSON_Array_Type then
                  Item.children := True;

                  V.Insert_Child
                    (Parent   => Parent,
                     Before   => DocumentSymbol_Trees.No_Element,
                     New_Item => Item,
                     Position => Next);

                  Read_Array (Next);
               else
                  Item.children := False;
                  V.Append_Child (Parent, Item);
               end if;

               JS.End_Object;
            end;
         end loop;

         JS.End_Array;
      end Read_Array;
   begin
      V.Clear;
      Read_Array (V.Root);
   end Read_DocumentSymbol_Tree;

   ------------------------------
   -- Read_dynamicRegistration --
   ------------------------------

   procedure Read_dynamicRegistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out dynamicRegistration)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean
        (JS, +"dynamicRegistration", V.dynamicRegistration);
      JS.End_Object;
   end Read_dynamicRegistration;

   --------------------------------
   -- Read_ExecuteCommandOptions --
   --------------------------------

   procedure Read_ExecuteCommandOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ExecuteCommandOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean (JS, +"workDoneProgress", V.workDoneProgress);
      Read_String_Vector (JS, +"commands", V.commands);
      JS.End_Object;
   end Read_ExecuteCommandOptions;

   -------------------------------
   -- Read_ExecuteCommandParams --
   -------------------------------

   procedure Read_ExecuteCommandParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ExecuteCommandParams)
   is
      Tag : Ada.Tags.Tag;
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Get_WorkDoneProgressParams (S, V.Base);
      Read_String (JS, +"command", V.command);
      Tag := Ada.Tags.Internal_Tag (LSP.Types.To_UTF_8_String (V.command));
      JS.Key ("arguments");

      if Tag in Ada.Tags.No_Tag then
         Optional_Any_Vector'Read (S, V.arguments);
      else
         --  Overwrite discriminant with Is_Unknown => False
         V :=
           (Is_Unknown => False,
            Base       => V.Base,
            command    => V.command,
            Custom     => <>);

         JS.Start_Array;
         V.Custom.Set (Create_Command (Tag, JS'Access));
         JS.End_Array;
      end if;

      JS.End_Object;
   end Read_ExecuteCommandParams;

   ----------------------------
   -- Read_FileSystemWatcher --
   ----------------------------

   procedure Read_FileSystemWatcher
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileSystemWatcher)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_String (JS, +"globPattern", V.globPattern);
      JS.Key ("kind");
      WatchKind_Set'Read (S, V.kind);
      JS.End_Object;
   end Read_FileSystemWatcher;

   -----------------------
   -- Read_FoldingRange --
   -----------------------

   procedure Read_FoldingRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FoldingRange)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("startLine");
      Line_Number'Read (S, V.startLine);
      JS.Key ("startCharacter");
      Optional_Number'Read (S, V.startCharacter);
      JS.Key ("endLine");
      Line_Number'Read (S, V.endLine);
      JS.Key ("endCharacter");
      Optional_Number'Read (S, V.endCharacter);
      Read_Optional_String (JS, +"kind", V.kind);
      JS.End_Object;
   end Read_FoldingRange;

   -----------------------------
   -- Read_FoldingRangeParams --
   -----------------------------

   procedure Read_FoldingRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FoldingRangeParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Get_Progress_Partial_Params (S, V);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.End_Object;
   end Read_FoldingRangeParams;

   ------------------------------
   -- Read_DocumentColorParams --
   ------------------------------

   procedure Read_DocumentColorParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentColorParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Get_Progress_Partial_Params (S, V);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.End_Object;
   end Read_DocumentColorParams;

   ------------------------------
   -- Read_FailureHandlingKind --
   ------------------------------

   procedure Read_FailureHandlingKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FailureHandlingKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Value : constant GNATCOLL.JSON.UTF8_String := JS.Read.Get;
   begin
      if Value = "abort" then
         V := abortApplying;
      elsif Value = "transactional" then
         V := transactional;
      elsif Value = "undo" then
         V := undo;
      elsif Value = "textOnlyTransactional" then
         V := textOnlyTransactional;
      else
         V := abortApplying;
      end if;
   end Read_FailureHandlingKind;

   -----------------------------------------
   -- Read_FoldingRangeClientCapabilities --
   -----------------------------------------

   procedure Read_FoldingRangeClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FoldingRangeClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean
        (JS, +"dynamicRegistration", V.dynamicRegistration);
      JS.Key ("rangeLimit");
      Optional_Number'Read (S, V.rangeLimit);
      Read_Optional_Boolean (JS, +"lineFoldingOnly", V.lineFoldingOnly);
      JS.End_Object;
   end Read_FoldingRangeClientCapabilities;

   ----------------
   -- Read_Hover --
   ----------------

   procedure Read_Hover
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Hover)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("contents");
      MarkupContent_Or_MarkedString_Vector'Read (S, V.contents);
      JS.Key ("range");
      Optional_Span'Read (S, V.Span);
      JS.End_Object;
   end Read_Hover;

   --------------------
   -- Read_If_String --
   --------------------

   procedure Read_If_String
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out LSP.Types.LSP_String)
   is
      Value : GNATCOLL.JSON.JSON_Value;
   begin
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
      Value := Stream.Read;

      if Value.Kind in GNATCOLL.JSON.JSON_Null_Type then
         Item := Empty_LSP_String;
      else
         --  Item := League.IRIs.From_Universal_String (Stream.Read.To_String);
         Item := To_LSP_String (Unbounded_String'(Stream.Read.Get));
      end if;
   end Read_If_String;

   ---------------------------
   -- Read_InitializeParams --
   ---------------------------

   procedure Read_InitializeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializeParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Trace : LSP.Types.Optional_String;
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Read (S, V.workDoneToken);
      JS.Key ("processId");
      Optional_Number'Read (S, V.processId);
      JS.Key ("clientInfo");
      Optional_ProgramInfo'Read (S, V.clientInfo);
      JS.Key ("rootPath");
      Read_If_String (JS, +"rootPath", V.rootPath);
      Read_If_String (JS, +"rootUri", V.rootUri);
      JS.Key ("capabilities");
      LSP.Messages.ClientCapabilities'Read (S, V.capabilities);
      Read_Optional_String (JS, +"trace", Trace);
      JS.Key ("trace");
      Optional_Trace_Kind'Read (S, V.trace);
      JS.Key ("workspaceFolders");
      Optional_WorkspaceFolder_Vector'Read (S, V.workspaceFolders);
      JS.End_Object;
   end Read_InitializeParams;

   ---------------------------
   -- Read_InitializeResult --
   ---------------------------

   procedure Read_InitializeResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializeResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("capabilities");
      ServerCapabilities'Read (S, V.capabilities);
      JS.Key ("serverInfo");
      Optional_ProgramInfo'Read (S, V.serverInfo);
      JS.End_Object;
   end Read_InitializeResult;

   ----------------------------
   -- Read_InitializedParams --
   ----------------------------

   not overriding procedure Read_InitializedParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializedParams)
   is
      pragma Unreferenced (V);

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.End_Object;
   end Read_InitializedParams;

   ---------------------------
   -- Read_InsertTextFormat --
   ---------------------------

   procedure Read_InsertTextFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InsertTextFormat)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := InsertTextFormat'Val (JS.Read.Get - 1);
   end Read_InsertTextFormat;

   -------------------
   -- Read_Location --
   -------------------

   procedure Read_Location
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Location)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      DocumentUri'Read (S, V.uri);
      JS.Key ("range");
      Span'Read (S, V.span);

      JS.Key ("alsKind");
      AlsReferenceKind_Set'Read (S, V.alsKind);

      JS.End_Object;
   end Read_Location;

   -----------------------
   -- Read_LocationLink --
   -----------------------

   procedure Read_LocationLink
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LocationLink)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("originSelectionRange");
      Optional_Span'Read (S, V.originSelectionRange);
      Read_String (JS, +"targetUri", V.targetUri);
      JS.Key ("targetRange");
      Span'Read (S, V.targetRange);
      JS.Key ("targetSelectionRange");
      Span'Read (S, V.targetSelectionRange);
      JS.Key ("alsKind");
      AlsReferenceKind_Set'Read (S, V.alsKind);
      JS.End_Object;
   end Read_LocationLink;

   ----------------------------------
   -- Read_Location_Or_Link_Vector --
   ----------------------------------

   procedure Read_Location_Or_Link_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Location_Or_Link_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Look_Ahead : constant GNATCOLL.JSON.JSON_Value := JS.Read;
   begin
      if Look_Ahead.Kind not in GNATCOLL.JSON.JSON_Array_Type then
         V := (Kind => Empty_Vector_Kind);
         return;
      end if;

      declare
         Vector : constant GNATCOLL.JSON.JSON_Array := Look_Ahead.Get;
      begin
         if GNATCOLL.JSON.Length (Vector) = 0 then
            V := (Kind => Empty_Vector_Kind);
            return;
         elsif GNATCOLL.JSON.Get (Vector, 1).Has_Field ("uri") then
            V := (Kind => Location_Vector_Kind, Locations => <>);
            Location_Vector'Read (S, V.Locations);
         else
            V := (Kind => LocationLink_Vector_Kind, LocationLinks => <>);
            LocationLink_Vector'Read (S, V.LocationLinks);
         end if;
      end;
   end Read_Location_Or_Link_Vector;

   ---------------------------
   -- Read_LogMessageParams --
   ---------------------------

   procedure Read_LogMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LogMessageParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("type");
      MessageType'Read (S, V.the_type);
      Read_String (JS, +"message", V.message);
      JS.End_Object;
   end Read_LogMessageParams;

   -----------------------
   -- Read_MarkedString --
   -----------------------

   procedure Read_MarkedString
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MarkedString)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Value : constant GNATCOLL.JSON.JSON_Value := JS.Read;
   begin
      case Value.Kind is
         when GNATCOLL.JSON.JSON_String_Type =>
            V := (Is_String => True,
                  Value => To_LSP_String (Unbounded_String'(Value.Get)));
         when GNATCOLL.JSON.JSON_Object_Type =>
            --  We can't use Start_Object/End_Object here because JS.Read
            --  call has already skipped the array item.
            V := (Is_String => False,
                  language => To_LSP_String
                    (Unbounded_String'(Value.Get ("language"))),
                  value    => To_LSP_String
                    (Unbounded_String'(Value.Get ("value"))));
         when others =>
            --  Unexpected JSON type
            V := (Is_String => True, Value => Empty_LSP_String);
      end case;
   end Read_MarkedString;

   ------------------------
   -- Read_MarkupContent --
   ------------------------

   procedure Read_MarkupContent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MarkupContent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("kind");
      MarkupKind'Read (S, V.kind);
      Read_String (JS, +"value", V.value);
      JS.End_Object;
   end Read_MarkupContent;

   -----------------------------------------------
   -- Read_MarkupContent_Or_MarkedString_Vector --
   -----------------------------------------------

   procedure Read_MarkupContent_Or_MarkedString_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MarkupContent_Or_MarkedString_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Value : constant GNATCOLL.JSON.JSON_Value := JS.Read;
   begin
      if Value.Kind in GNATCOLL.JSON.JSON_String_Type then
         V := (Is_MarkupContent => False,
               Vector           => <>);
         V.Vector.Append
           (MarkedString'(Is_String => True,
                          value     => To_LSP_String
                                         (Unbounded_String'(Value.Get))));
      elsif Value.Kind in GNATCOLL.JSON.JSON_Array_Type then
         V := (Is_MarkupContent => False,
               Vector           => <>);
         MarkedString_Vector'Read (S, V.Vector);
      elsif Value.Kind not in GNATCOLL.JSON.JSON_Object_Type then
         null;  --  Nothing to do if this is not an object
      elsif Value.Has_Field ("kind") then
         V := (Is_MarkupContent => True,
               MarkupContent    => <>);
         MarkupContent'Read (S, V.MarkupContent);
      else
         declare
            Item : MarkedString;
         begin
            V := (Is_MarkupContent => False,
                  Vector           => <>);
            MarkedString'Read (S, Item);
            V.Vector.Append (Item);
         end;
      end if;
   end Read_MarkupContent_Or_MarkedString_Vector;

   ---------------------
   -- Read_MarkupKind --
   ---------------------

   procedure Read_MarkupKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MarkupKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Value : constant GNATCOLL.JSON.JSON_Value := JS.Read;
   begin
      if Value.Kind in GNATCOLL.JSON.JSON_String_Type then
         if Standard.String'(Value.Get) = "markdown" then
            V := markdown;
            return;
         end if;
      end if;

      V := plaintext;
   end Read_MarkupKind;

   ----------------------
   -- Read_MessageType --
   ----------------------

   procedure Read_MessageType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MessageType)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := MessageType'Val (JS.Read.Get - 1);
   end Read_MessageType;

   ------------------------------
   -- Read_NotificationMessage --
   ------------------------------

   procedure Read_NotificationMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out NotificationMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Notification_Prefix (S, V);
      JS.End_Object;
   end Read_NotificationMessage;

   ------------------------------
   -- Read_Notification_Prefix --
   ------------------------------

   procedure Read_Notification_Prefix
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.NotificationMessage'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      Read_String (JS, +"jsonrpc", V.jsonrpc);
      Read_String (JS, +"method", V.method);
   end Read_Notification_Prefix;

   -----------------
   -- Read_Number --
   -----------------

   procedure Read_Number
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out LSP.Types.LSP_Number) is
   begin
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
      Item := LSP.Types.LSP_Number (Integer'(Stream.Read.Get));
   end Read_Number;

   -------------------------------------------
   -- Read_Optional_TextDocumentSyncOptions --
   -------------------------------------------

   procedure Read_Optional_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Optional_TextDocumentSyncOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Value : constant GNATCOLL.JSON.JSON_Value := JS.Read;
   begin
      if Value.Kind in GNATCOLL.JSON.JSON_Null_Type then
         V := (False, False);
      elsif Value.Kind in GNATCOLL.JSON.JSON_Object_Type then
         V := (True, False, others => <>);
         TextDocumentSyncOptions'Read (S, V.Options);
      else
         V := (True, True, others => <>);
         TextDocumentSyncKind'Read (S, V.Value);
      end if;
   end Read_Optional_TextDocumentSyncOptions;

   -------------------------------
   -- Read_ParameterInformation --
   -------------------------------

   procedure Read_ParameterInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ParameterInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("label");
      Parameter_Label'Read (S, V.label);
      JS.Key ("documentation");
      Optional_String_Or_MarkupContent'Read (S, V.documentation);
      JS.End_Object;
   end Read_ParameterInformation;

   --------------------------
   -- Read_Parameter_Label --
   --------------------------

   procedure Read_Parameter_Label
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Parameter_Label)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Value : constant GNATCOLL.JSON.JSON_Value := JS.Read;
   begin
      if Value.Kind in GNATCOLL.JSON.JSON_String_Type then
         V := (Is_String => True,
               String    => To_LSP_String (Unbounded_String'(Value.Get)));
      else
         JS.Start_Array;
         V.From := UTF_16_Index (Integer'(JS.Read.Get));
         V.Till := UTF_16_Index (Integer'(JS.Read.Get));
         JS.End_Array;
      end if;
   end Read_Parameter_Label;

   ------------------------------------------
   -- Read_parameterInformation_Capability --
   ------------------------------------------

   procedure Read_parameterInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out parameterInformation_Capability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean (JS, +"labelOffsetSupport", V.labelOffsetSupport);
      JS.End_Object;
   end Read_parameterInformation_Capability;

   -------------------
   -- Read_Position --
   -------------------

   procedure Read_Position
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Position)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Number (JS, +"line", LSP_Number (V.line));
      Read_Number (JS, +"character", LSP_Number (V.character));
      JS.End_Object;
   end Read_Position;

   ---------------------------
   -- Read_Provider_Options --
   ---------------------------

   procedure Read_Provider_Options
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Provider_Options)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Look_Ahead : constant GNATCOLL.JSON.JSON_Value := JS.Read;
   begin
      if Look_Ahead.Kind = GNATCOLL.JSON.JSON_Boolean_Type then
         V := (Is_Boolean => True,
               Bool       => Look_Ahead.Get);
      elsif Look_Ahead.Kind = GNATCOLL.JSON.JSON_Object_Type
        and then Look_Ahead.Has_Field ("documentSelector")
      then
         V := (Is_Boolean => False,
               Options    => (Is_Set => True, Value => <>));

         Optional_TSW_RegistrationOptions'Read (S, V.Options);
      else
         V := (Is_Boolean => False,
               Options    => (Is_Set => False));
      end if;
   end Read_Provider_Options;

   -----------------------------------
   -- Read_PublishDiagnosticsParams --
   -----------------------------------

   procedure Read_PublishDiagnosticsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out PublishDiagnosticsParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      DocumentUri'Read (S, V.uri);
      JS.Key ("version");
      Optional_Number'Read (S, V.version);
      JS.Key ("diagnostics");
      Diagnostic_Vector'Read (S, V.diagnostics);
      JS.End_Object;
   end Read_PublishDiagnosticsParams;

   -----------------------------------------------
   -- Read_PublishDiagnosticsClientCapabilities --
   -----------------------------------------------

   procedure Read_PublishDiagnosticsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out PublishDiagnosticsClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean
        (JS, +"relatedInformation", V.relatedInformation);
      JS.Key ("tagSupport");
      Optional_DiagnosticTagSupport'Read (S, V.tagSupport);
      Read_Optional_Boolean (JS, +"versionSupport", V.versionSupport);
      JS.End_Object;
   end Read_PublishDiagnosticsClientCapabilities;

   ---------------------------
   -- Read_ReferenceContext --
   ---------------------------

   procedure Read_ReferenceContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ReferenceContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("includeDeclaration");
      V.includeDeclaration := JS.Read.Get;
      JS.End_Object;
   end Read_ReferenceContext;

   --------------------------
   -- Read_ReferenceParams --
   --------------------------

   procedure Read_ReferenceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ReferenceParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Get_Text_Progress_Partial_Params (S, V);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("position");
      Position'Read (S, V.position);
      JS.Key ("context");
      ReferenceContext'Read (S, V.context);
      JS.End_Object;
   end Read_ReferenceParams;

   -------------------------
   -- Read_RequestMessage --
   -------------------------

   procedure Read_RequestMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RequestMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      LSP.Types.Read_String (JS, +"jsonrpc", V.jsonrpc);
      LSP.Types.Read_String (JS, +"method", V.method);
      Read_Number_Or_String (JS, +"id", V.id);
      JS.End_Object;
   end Read_RequestMessage;

   --------------------------
   -- Read_Response_Prefix --
   --------------------------

   procedure Read_Response_Prefix
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ResponseMessage'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      Read_String (JS, +"jsonrpc", V.jsonrpc);
      Read_Number_Or_String (JS, +"id", V.id);
      JS.Key ("error");
      Optional_ResponseError'Read (S, V.error);
   end Read_Response_Prefix;

   ---------------------
   -- Read_RGBA_Color --
   ---------------------

   procedure Read_RGBA_Color
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RGBA_Color)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Number (JS, +"red", V.red);
      Read_Number (JS, +"green", V.green);
      Read_Number (JS, +"blue", V.blue);
      Read_Number (JS, +"alpha", V.alpha);
      JS.End_Object;
   end Read_RGBA_Color;

   --------------------------
   -- Read_ResponseMessage --
   --------------------------

   procedure Read_ResponseMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ResponseMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Response_Prefix (S, V);
      JS.End_Object;
   end Read_ResponseMessage;

   ------------------------
   -- Read_RenameOptions --
   ------------------------

   procedure Read_RenameOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RenameOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean (JS, +"workDoneProgress", V.workDoneProgress);
      Read_Optional_Boolean (JS, +"prepareProvider", V.prepareProvider);
      JS.End_Object;
   end Read_RenameOptions;

   -----------------------
   -- Read_RenameParams --
   -----------------------

   procedure Read_RenameParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RenameParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Get_Text_Progress_Params (S, V);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("position");
      Position'Read (S, V.position);
      Read_String (JS, +"newName", V.newName);
      JS.End_Object;
   end Read_RenameParams;

   -----------------------------------
   -- Read_RenameClientCapabilities --
   -----------------------------------

   procedure Read_RenameClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RenameClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean
        (JS, +"dynamicRegistration", V.dynamicRegistration);
      Read_Optional_Boolean (JS, +"prepareSupport", V.prepareSupport);
      JS.End_Object;
   end Read_RenameClientCapabilities;

   --------------------------------
   -- Read_ResourceOperationKind --
   --------------------------------

   procedure Read_ResourceOperationKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ResourceOperationKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Value : constant GNATCOLL.JSON.JSON_Value := JS.Read;
      Text  : constant GNATCOLL.JSON.UTF8_String := Value.Get;
   begin
      if Text = "create" then
         V := create;
      elsif Text = "rename" then
         V := rename;
      elsif Text = "delete" then
         V := delete;
      else
         V := create;
      end if;
   end Read_ResourceOperationKind;

   ----------------------
   -- Read_SaveOptions --
   ----------------------

   procedure Read_SaveOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SaveOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean (JS, +"includeText", V.includeText);
      JS.End_Object;
   end Read_SaveOptions;

   -------------------------
   -- Read_SelectionRange --
   -------------------------

   procedure Read_SelectionRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SelectionRange)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      Span'Read (S, V.span);
      JS.End_Object;
   end Read_SelectionRange;

   -------------------------------
   -- Read_SelectionRangeParams --
   -------------------------------

   procedure Read_SelectionRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SelectionRangeParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Get_Progress_Partial_Params (S, V);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("positions");
      Position_Vector'Read (S, V.positions);
      JS.End_Object;
   end Read_SelectionRangeParams;

   -----------------------------
   -- Read_ServerCapabilities --
   -----------------------------

   procedure Read_ServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ServerCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocumentSync");
      Optional_TextDocumentSyncOptions'Read (S, V.textDocumentSync);
      JS.Key ("completionProvider");
      Optional_CompletionOptions'Read (S, V.completionProvider);
      JS.Key ("hoverProvider");
      HoverOptions'Read (S, V.hoverProvider);
      JS.Key ("signatureHelpProvider");
      Optional_SignatureHelpOptions'Read (S, V.signatureHelpProvider);
      JS.Key ("declarationProvider");
      DeclarationOptions'Read (S, V.declarationProvider);
      JS.Key ("definitionProvider");
      DefinitionOptions'Read (S, V.definitionProvider);
      JS.Key ("typeDefinitionProvider");
      TypeDefinitionOptions'Read (S, V.typeDefinitionProvider);
      JS.Key ("implementationProvider");
      ImplementationOptions'Read (S, V.implementationProvider);
      JS.Key ("referencesProvider");
      ReferenceOptions'Read (S, V.referencesProvider);
      JS.Key ("documentHighlightProvider");
      DocumentHighlightOptions'Read (S, V.documentHighlightProvider);
      JS.Key ("documentSymbolProvider");
      DocumentSymbolOptions'Read (S, V.documentSymbolProvider);
      JS.Key ("codeActionProvider");
      Optional_CodeActionOptions'Read (S, V.codeActionProvider);
      JS.Key ("codeLensProvider");
      Optional_CodeLensOptions'Read (S, V.codeLensProvider);
      JS.Key ("documentLinkProvider");
      Optional_DocumentLinkOptions'Read (S, V.documentLinkProvider);
      JS.Key ("colorProvider");
      DocumentColorOptions'Read (S, V.colorProvider);
      JS.Key ("documentFormattingProvider");
      DocumentFormattingOptions'Read (S, V.documentFormattingProvider);
      JS.Key ("documentRangeFormattingProvider");
      DocumentRangeFormattingOptions'Read
        (S, V.documentRangeFormattingProvider);
      JS.Key ("documentOnTypeFormattingProvider");
      Optional_DocumentOnTypeFormattingOptions'Read
        (S, V.documentOnTypeFormattingProvider);
      JS.Key ("renameProvider");
      Optional_RenameOptions'Read (S, V.renameProvider);
      JS.Key ("foldingRangeProvider");
      FoldingRangeOptions'Read (S, V.foldingRangeProvider);
      JS.Key ("executeCommandProvider");
      Optional_ExecuteCommandOptions'Read (S, V.executeCommandProvider);
      JS.Key ("selectionRangeProvider");
      SelectionRangeOptions'Read (S, V.selectionRangeProvider);
      JS.Key ("workspaceSymbolProvider");
      WorkspaceSymbolOptions'Read (S, V.workspaceSymbolProvider);

      JS.Key ("workspace");
      Optional_workspace_Options'Read (S, V.workspace);

      Read_Optional_Boolean (JS, +"alsCalledByProvider",
                             V.alsCalledByProvider);

      JS.End_Object;
   end Read_ServerCapabilities;

   ------------------------------------------
   -- Read_SignatureHelpClientCapabilities --
   ------------------------------------------

   procedure Read_SignatureHelpClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean
        (JS, +"dynamicRegistration", V.dynamicRegistration);
      JS.Key ("signatureInformation");
      Optional_signatureInformation_Capability'Read
        (S, V.signatureInformation);
      Read_Optional_Boolean (JS, +"contextSupport", V.contextSupport);
      JS.End_Object;
   end Read_SignatureHelpClientCapabilities;

   -------------------------------
   -- Read_SignatureInformation --
   -------------------------------

   procedure Read_SignatureInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_String (JS, +"label", V.label);
      JS.Key ("documentation");
      Optional_String_Or_MarkupContent'Read (S, V.documentation);
      JS.Key ("parameters");
      ParameterInformation_Vector'Read (S, V.parameters);
      JS.End_Object;
   end Read_SignatureInformation;

   ------------------------------------------
   -- Read_signatureInformation_Capability --
   ------------------------------------------

   procedure Read_signatureInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out signatureInformation_Capability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("documentationFormat");
      Optional_MarkupKind_Vector'Read (S, V.documentationFormat);
      JS.Key ("parameterInformation");
      Optional_parameterInformation_Capability'Read
        (S, V.parameterInformation);
      JS.End_Object;
   end Read_signatureInformation_Capability;

   ------------------------
   -- Read_SignatureHelp --
   ------------------------

   procedure Read_SignatureHelp
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelp)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("signatures");
      JS.Start_Array;

      while not JS.End_Of_Array loop
         declare
            Item : SignatureInformation;
         begin
            SignatureInformation'Read (S, Item);
            V.signatures.Append (Item);
         end;
      end loop;

      JS.End_Array;
      JS.Key ("activeSignature");
      Optional_Number'Read (S, V.activeSignature);
      JS.Key ("activeParameter");
      Optional_Number'Read (S, V.activeParameter);
      JS.End_Object;
   end Read_SignatureHelp;

   -------------------------------
   -- Read_SignatureHelpOptions --
   -------------------------------

   procedure Read_SignatureHelpOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("triggerCharacters");
      Optional_LSP_String_Vector'Read (S, V.triggerCharacters);
      JS.Key ("retriggerCharacters");
      Optional_LSP_String_Vector'Read (S, V.retriggerCharacters);
      JS.End_Object;
   end Read_SignatureHelpOptions;

   ----------------------------
   -- Read_ShowMessageParams --
   ----------------------------

   procedure Read_ShowMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowMessageParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("type");
      MessageType'Read (S, V.the_type);
      Read_String (JS, +"message", V.message);
      JS.End_Object;
   end Read_ShowMessageParams;

   -----------------------------------
   -- Read_ShowMessageRequestParams --
   -----------------------------------

   procedure Read_ShowMessageRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowMessageRequestParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("type");
      MessageType'Read (S, V.the_type);
      Read_String (JS, +"message", V.message);
      Read_String_Vector (JS, +"actions", V.actions);
      JS.End_Object;
   end Read_ShowMessageRequestParams;

   ---------------
   -- Read_Span --
   ---------------

   procedure Read_Span
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Span)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("start");
      Position'Read (S, V.first);
      JS.Key ("end");
      Position'Read (S, V.last);
      JS.End_Object;
   end Read_Span;

   ------------------------------------
   -- Read_StaticRegistrationOptions --
   ------------------------------------

   procedure Read_TSW_RegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TSW_RegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean (JS, +"workDoneProgress", V.workDoneProgress);
      Read_Optional_String (JS, +"id", V.id);
      JS.Key ("documentSelector");
      DocumentSelector'Read (S, V.documentSelector);
      JS.End_Object;
   end Read_TSW_RegistrationOptions;

   ----------------------------------
   -- Read_String_Or_MarkupContent --
   ----------------------------------

   procedure Read_String_Or_MarkupContent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out String_Or_MarkupContent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Value : constant GNATCOLL.JSON.JSON_Value := JS.Read;
   begin
      if Value.Kind in GNATCOLL.JSON.JSON_String_Type then
         V := (Is_String => True,
               String    => To_LSP_String (Unbounded_String'(Value.Get)));
      else
         V := (Is_String => False, Content => <>);
         MarkupContent'Read (S, V.Content);
      end if;
   end Read_String_Or_MarkupContent;

   ------------------------
   -- Read_String_Vector --
   ------------------------

   procedure Read_String_Vector
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out LSP.Types.LSP_String_Vector) is
   begin
      Item.Clear;
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
      Stream.Start_Array;

      while not Stream.End_Of_Array loop
         Item.Append (To_LSP_String (Unbounded_String'(Stream.Read.Get)));
      end loop;

      Stream.End_Array;
   end Read_String_Vector;

   ----------------------------
   -- Read_SymbolInformation --
   ----------------------------

   procedure Read_SymbolInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SymbolInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_String (JS, +"name", V.name);
      JS.Key ("kind");
      SymbolKind'Read (S, V.kind);
      Read_Optional_Boolean (JS, +"alsIsAdaProcedure", V.alsIsAdaProcedure);
      Read_Optional_Boolean (JS, +"deprecated", V.deprecated);
      JS.Key ("location");
      Location'Read (S, V.location);
      Read_Optional_String (JS, +"containerName", V.containerName);
      JS.End_Object;
   end Read_SymbolInformation;

   ---------------------
   -- Read_SymbolKind --
   ---------------------

   procedure Read_SymbolKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SymbolKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := SymbolKind'Val (JS.Read.Get - 1);
   end Read_SymbolKind;

   ---------------------------------
   -- Read_symbolKindCapabilities --
   ---------------------------------

   procedure Read_symbolKindCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out symbolKindCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      Optional_SymbolKindSet'Read (S, V.valueSet);
      JS.End_Object;
   end Read_symbolKindCapabilities;

   -------------------------
   -- Read_Als_Visibility --
   -------------------------

   procedure Read_Als_Visibility
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Als_Visibility)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := Als_Visibility'Val (JS.Read.Get - 1);
   end Read_Als_Visibility;

   ---------------------------------
   -- Read_TextDocumentSaveReason --
   ---------------------------------

   procedure Read_TextDocumentSaveReason
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSaveReason)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := TextDocumentSaveReason'Val (JS.Read.Get - 1);
   end Read_TextDocumentSaveReason;

   -------------------------
   -- Read_FileChangeType --
   -------------------------

   procedure Read_FileChangeType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileChangeType)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := FileChangeType'Val (JS.Read.Get - 1);
   end Read_FileChangeType;

   ---------------------------------------------
   -- Read_TextDocumentSyncClientCapabilities --
   ---------------------------------------------

   procedure Read_TextDocumentSyncClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSyncClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean
        (JS, +"dynamicRegistration", V.dynamicRegistration);
      Read_Optional_Boolean (JS, +"willSave", V.willSave);
      Read_Optional_Boolean (JS, +"willSaveWaitUntil", V.willSaveWaitUntil);
      Read_Optional_Boolean (JS, +"didSave", V.didSave);
      JS.End_Object;
   end Read_TextDocumentSyncClientCapabilities;

   -----------------------------------------
   -- Read_TextDocumentClientCapabilities --
   -----------------------------------------

   procedure Read_TextDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("synchronization");
      TextDocumentSyncClientCapabilities'Read (S, V.synchronization);
      JS.Key ("completion");
      CompletionClientCapabilities'Read (S, V.completion);
      JS.Key ("hover");
      Optional_HoverClientCapabilities'Read (S, V.hover);
      JS.Key ("signatureHelp");
      Optional_SignatureHelpClientCapabilities'Read (S, V.signatureHelp);
      JS.Key ("references");
      dynamicRegistration'Read (S, V.references);
      JS.Key ("documentHighlight");
      dynamicRegistration'Read (S, V.documentHighlight);
      JS.Key ("documentSymbol");
      Optional_DocumentSymbolClientCapabilities'Read (S, V.documentSymbol);
      JS.Key ("formatting");
      dynamicRegistration'Read (S, V.formatting);
      JS.Key ("rangeFormatting");
      dynamicRegistration'Read (S, V.rangeFormatting);
      JS.Key ("onTypeFormatting");
      dynamicRegistration'Read (S, V.onTypeFormatting);
      JS.Key ("declaration");
      Optional_DeclarationClientCapabilities'Read (S, V.declaration);
      JS.Key ("definition");
      Optional_DefinitionClientCapabilities'Read (S, V.definition);
      JS.Key ("typeDefinition");
      Optional_TypeDefinitionClientCapabilities'Read (S, V.typeDefinition);
      JS.Key ("implementation");
      Optional_ImplementationClientCapabilities'Read (S, V.implementation);
      JS.Key ("codeAction");
      Optional_CodeActionClientCapabilities'Read (S, V.codeAction);
      JS.Key ("codeLens");
      dynamicRegistration'Read (S, V.codeLens);
      JS.Key ("documentLink");
      Optional_DocumentLinkClientCapabilities'Read (S, V.documentLink);
      JS.Key ("colorProvider");
      dynamicRegistration'Read (S, V.colorProvider);
      JS.Key ("rename");
      Optional_RenameClientCapabilities'Read (S, V.rename);
      JS.Key ("publishDiagnostics");
      Optional_PublishDiagnosticsClientCapabilities'Read
        (S, V.publishDiagnostics);
      JS.Key ("foldingRange");
      Optional_FoldingRangeClientCapabilities'Read (S, V.foldingRange);
      JS.Key ("selectionRange");
      SelectionRangeClientCapabilities'Read (S, V.selectionRange);
      JS.End_Object;
   end Read_TextDocumentClientCapabilities;

   -----------------------------------------
   -- Read_TextDocumentContentChangeEvent --
   -----------------------------------------

   procedure Read_TextDocumentContentChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentContentChangeEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      Optional_Span'Read (S, V.span);
      JS.Key ("rangeLength");
      Optional_Number'Read (S, V.rangeLength);
      Read_String (JS, +"text", V.text);
      JS.End_Object;
   end Read_TextDocumentContentChangeEvent;

   ---------------------------
   -- Read_TextDocumentEdit --
   ---------------------------

   procedure Read_TextDocumentEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      VersionedTextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("edits");
      TextEdit_Vector'Read (S, V.edits);
      JS.End_Object;
   end Read_TextDocumentEdit;

   --------------------------
   -- Read_Document_Change --
   --------------------------

   procedure Read_Document_Change
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Document_Change) is
   begin
      --  FIXME: rewrite reading procedure
      TextDocumentEdit'Read (S, V.Text_Document_Edit);
   end Read_Document_Change;

   ------------------------
   -- Read_Symbol_Vector --
   ------------------------

   procedure Read_Symbol_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Symbol_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Value : constant GNATCOLL.JSON.JSON_Value := JS.Read;
   begin
      if Value.Kind in GNATCOLL.JSON.JSON_Array_Type then
         declare
            Vector : constant GNATCOLL.JSON.JSON_Array := Value.Get;
            --  ??? This copies whole array and slow, try to avoid it.
         begin
            if GNATCOLL.JSON.Length (Vector) > 0 then
               if GNATCOLL.JSON.Get (Vector, 1).Has_Field ("range") then
                  V := (Is_Tree => True, Tree => <>);
                  DocumentSymbol_Tree'Read (S, V.Tree);
               else
                  V := (Is_Tree => False, Vector => <>);
                  SymbolInformation_Vector'Read (S, V.Vector);
               end if;
            else
               V := (Is_Tree => False, Vector => <>);
            end if;
         end;
      else
         V := (Is_Tree => False, Vector => <>);
      end if;
   end Read_Symbol_Vector;

   -------------------------------------------
   -- Read_DocumentSymbolClientCapabilities --
   -------------------------------------------

   procedure Read_DocumentSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentSymbolClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean
        (JS, +"dynamicRegistration", V.dynamicRegistration);

      JS.Key ("symbolKind");
      Optional_symbolKindCapabilities'Read (S, V.symbolKind);

      Read_Optional_Boolean
        (JS,
         +"hierarchicalDocumentSymbolSupport",
         V.hierarchicalDocumentSymbolSupport);
      JS.End_Object;
   end Read_DocumentSymbolClientCapabilities;

   ---------------------------------
   -- Read_TextDocumentIdentifier --
   ---------------------------------

   procedure Read_TextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentIdentifier)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      DocumentUri'Read (S, V.uri);
      JS.End_Object;
   end Read_TextDocumentIdentifier;

   ---------------------------
   -- Read_TextDocumentItem --
   ---------------------------

   procedure Read_TextDocumentItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_If_String (JS, +"uri", V.uri);
      Read_String (JS, +"languageId", V.languageId);
      Read_Number (JS, +"version", LSP.Types.LSP_Number (V.version));
      Read_String (JS, +"text", V.text);
      JS.End_Object;
   end Read_TextDocumentItem;

   -------------------------------------
   -- Read_TextDocumentPositionParams --
   -------------------------------------

   procedure Read_TextDocumentPositionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentPositionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("position");
      Position'Read (S, V.position);
      JS.End_Object;
   end Read_TextDocumentPositionParams;

   ------------------------------------
   -- Get_TextDocumentPositionParams --
   ------------------------------------

   procedure Get_TextDocumentPositionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentPositionParams'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("position");
      Position'Read (S, V.position);
   end Get_TextDocumentPositionParams;

   ------------------------------------
   -- Put_TextDocumentPositionParams --
   ------------------------------------

   procedure Put_TextDocumentPositionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentPositionParams'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      LSP.Messages.Position'Write (S, V.position);
   end Put_TextDocumentPositionParams;

   -------------------------------
   -- Read_TextDocumentSyncKind --
   -------------------------------

   procedure Read_TextDocumentSyncKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSyncKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Value : constant GNATCOLL.JSON.JSON_Value := JS.Read;

      Map : constant array (0 .. 2) of TextDocumentSyncKind :=
        (0 => None, 1 => Full, 2 => Incremental);
   begin
      V := Map (Value.Get);
   end Read_TextDocumentSyncKind;

   ----------------------------------
   -- Read_TextDocumentSyncOptions --
   ----------------------------------

   procedure Read_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSyncOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean (JS, +"openClose", V.openClose);
      JS.Key ("change");
      Optional_TextDocumentSyncKind'Read (S, V.change);
      Read_Optional_Boolean (JS, +"willSave", V.willSave);
      Read_Optional_Boolean (JS, +"willSaveWaitUntil", V.willSaveWaitUntil);
      JS.Key ("save");
      Optional_SaveOptions'Read (S, V.save);
      JS.End_Object;
   end Read_TextDocumentSyncOptions;

   -------------------
   -- Read_TextEdit --
   -------------------

   procedure Read_TextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      Span'Read (S, V.span);
      Read_String (JS, +"newText", V.newText);
      JS.End_Object;
   end Read_TextEdit;

   procedure Read_Trace_Kind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Trace_Kind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Value : constant GNATCOLL.JSON.JSON_Value := JS.Read;
      Text  : constant GNATCOLL.JSON.UTF8_String := Value.Get;
   begin
      if Text = "off" then
         V := off;
      elsif Text = "messages" then
         V := messages_trace;
      elsif Text = "verbose" then
         V := verbose;
      else
         V := off;
      end if;
   end Read_Trace_Kind;

   ------------------------------------------
   -- Read_VersionedTextDocumentIdentifier --
   ------------------------------------------

   procedure Read_VersionedTextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out VersionedTextDocumentIdentifier)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      DocumentUri'Read (S, V.uri);
      JS.Key ("version");
      Optional_Number'Read (S, V.version);
      JS.End_Object;
   end Read_VersionedTextDocumentIdentifier;

   --------------------------------------------
   -- Read_WorkspaceSymbolClientCapabilities --
   --------------------------------------------

   procedure Read_WorkspaceSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceSymbolClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean
        (JS, +"dynamicRegistration", V.dynamicRegistration);

      JS.Key ("symbolKind");
      Optional_symbolKindCapabilities'Read (S, V.symbolKind);

      JS.End_Object;
   end Read_WorkspaceSymbolClientCapabilities;

   ------------------------
   -- Read_WatchKind_Set --
   ------------------------

   procedure Read_WatchKind_Set
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WatchKind_Set)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Value  : constant GNATCOLL.JSON.JSON_Value := JS.Read;
      Result : Integer;
      Mask   : Integer := 4;
   begin
      if Value.Kind in GNATCOLL.JSON.JSON_Null_Type then
         Result := Value.Get;

         for J in reverse WatchKind loop
            if Result >= Mask then
               V (J) := True;
               Result := Result - Mask;
            end if;

            Mask := Mask / 2;
         end loop;
      else
         V := Default_WatchKind_Set;
      end if;
   end Read_WatchKind_Set;

   -----------------------------------
   -- Read_WindowClientCapabilities --
   -----------------------------------

   procedure Read_WindowClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WindowClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean (JS, +"workDoneProgress", V.workDoneProgress);
      JS.End_Object;
   end Read_WindowClientCapabilities;

   ---------------------------------------
   -- Read_WorkDoneProgressCreateParams --
   ---------------------------------------

   procedure Read_WorkDoneProgressCreateParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkDoneProgressCreateParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Number_Or_String (JS, +"token", V.token);
      JS.End_Object;
   end Read_WorkDoneProgressCreateParams;

   ----------------------------------
   -- Read_WorkDoneProgressOptions --
   ----------------------------------

   procedure Read_WorkDoneProgressOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkDoneProgressOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean (JS, +"workDoneProgress", V.workDoneProgress);
      JS.End_Object;
   end Read_WorkDoneProgressOptions;

   --------------------------------------
   -- Read_WorkspaceClientCapabilities --
   --------------------------------------

   procedure Read_WorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean (JS, +"applyEdit", V.applyEdit);
      JS.Key ("workspaceEdit");
      WorkspaceEditClientCapabilities'Read (S, V.workspaceEdit);
      JS.Key ("didChangeConfiguration");
      dynamicRegistration'Read (S, V.didChangeConfiguration);
      JS.Key ("didChangeWatchedFiles");
      dynamicRegistration'Read (S, V.didChangeWatchedFiles);
      JS.Key ("symbol");
      Optional_WorkspaceSymbolClientCapabilities'Read (S, V.symbol);
      JS.Key ("executeCommand");
      dynamicRegistration'Read (S, V.executeCommand);
      Read_Optional_Boolean (JS, +"workspaceFolders", V.workspaceFolders);
      Read_Optional_Boolean (JS, +"configuration", V.configuration);
      JS.End_Object;
   end Read_WorkspaceClientCapabilities;

   ------------------------
   -- Read_WorkspaceEdit --
   ------------------------

   procedure Read_WorkspaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceEdit)
   is
      procedure Each
        (Name  : GNATCOLL.JSON.UTF8_String;
         Value : GNATCOLL.JSON.JSON_Value);

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      ----------
      -- Each --
      ----------

      procedure Each
        (Name  : GNATCOLL.JSON.UTF8_String;
         Value : GNATCOLL.JSON.JSON_Value)
      is
         pragma Unreferenced (Value);
         Key    : constant LSP.Types.LSP_String := +Name;
         Vector : TextEdit_Vector;
      begin
         JS.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
         JS.Start_Array;
         while not JS.End_Of_Array loop
            declare
               Item : TextEdit;
            begin
               TextEdit'Read (S, Item);
               Vector.Append (Item);
            end;
         end loop;
         JS.End_Array;

         V.changes.Insert (Key, Vector);
      end Each;

      Value : GNATCOLL.JSON.JSON_Value;
   begin
      JS.Start_Object;
      JS.Key ("changes");
      Value := JS.Read;

      if Value.Kind in GNATCOLL.JSON.JSON_Object_Type then
         JS.Key ("changes");
         JS.Start_Object;
         Value.Map_JSON_Object (Each'Access);
         JS.End_Object;
      else
         JS.Key ("documentChanges");
         Document_Change_Vector'Read (S, V.documentChanges);
      end if;

      JS.End_Object;
   end Read_WorkspaceEdit;

   --------------------------
   -- Read_WorkspaceFolder --
   --------------------------

   procedure Read_WorkspaceFolder
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceFolder)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_String (JS, +"uri", V.uri);
      Read_String (JS, +"name", V.name);
      JS.End_Object;
   end Read_WorkspaceFolder;

   --------------------------------------
   -- Read_WorkspaceFoldersChangeEvent --
   --------------------------------------

   procedure Read_WorkspaceFoldersChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceFoldersChangeEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("added");
      WorkspaceFolder_Vector'Read (S, V.added);
      JS.Key ("removed");
      WorkspaceFolder_Vector'Read (S, V.removed);
      JS.End_Object;
   end Read_WorkspaceFoldersChangeEvent;

   ---------------------------
   -- Read_workspaceFolders --
   ---------------------------

   procedure Read_WorkspaceFoldersServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceFoldersServerCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean (JS, +"supported", V.supported);
      JS.Key ("changeNotifications");
      Optional_Boolean_Or_String'Read (S, V.changeNotifications);
      JS.End_Object;
   end Read_WorkspaceFoldersServerCapabilities;

   ----------------------------
   -- Read_workspace_Options --
   ----------------------------

   procedure Read_workspace_Options
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out workspace_Options)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workspaceFolders");
      Optional_WorkspaceFoldersServerCapabilities'Read (S, V.workspaceFolders);
      JS.End_Object;
   end Read_workspace_Options;

   --------------------------------
   -- Read_WorkspaceSymbolParams --
   --------------------------------

   procedure Read_WorkspaceSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceSymbolParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Get_Progress_Partial_Params (S, V);
      Read_String (JS, +"query", V.query);
      JS.End_Object;
   end Read_WorkspaceSymbolParams;

   -----------------------------
   -- Write_ShowMessageParams --
   -----------------------------

   procedure Write_ShowMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowMessageParams)
      renames LSP.Message_IO.Write_ShowMessageParams;

   ------------------------------------
   -- Write_ShowMessageRequestParams --
   ------------------------------------

   procedure Write_ShowMessageRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowMessageRequestParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("type");
      MessageType'Write (S, V.the_type);
      Write_String (JS, +"message", V.message);
      Write_String_Vector (JS, +"actions", V.actions);
      JS.End_Object;
   end Write_ShowMessageRequestParams;

   ------------------------------------
   -- Write_ApplyWorkspaceEditParams --
   ------------------------------------

   procedure Write_ApplyWorkspaceEditParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ApplyWorkspaceEditParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("edit");
      WorkspaceEdit'Write (S, V.edit);
      JS.End_Object;
   end Write_ApplyWorkspaceEditParams;

   ------------------------------------
   -- Write_ApplyWorkspaceEditResult --
   ------------------------------------

   procedure Write_ApplyWorkspaceEditResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ApplyWorkspaceEditResult)
      renames LSP.Message_IO.Write_ApplyWorkspaceEditResult;

   ------------------------
   -- Write_CancelParams --
   ------------------------

   procedure Write_CancelParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CancelParams)
      renames LSP.Message_IO.Write_CancelParams;

   ------------------------------
   -- Write_ClientCapabilities --
   ------------------------------

   procedure Write_ClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ClientCapabilities)
      renames LSP.Message_IO.Write_ClientCapabilities;

   ----------------------
   -- Write_ClientInfo --
   ----------------------

   procedure Write_ProgramInfo
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ProgramInfo)
      renames LSP.Message_IO.Write_ProgramInfo;

   ----------------------
   -- Write_CodeAction --
   ----------------------

   procedure Write_CodeAction
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeAction)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if LSP.Types.Is_Empty (V.title) and then V.command.Is_Set then
         Optional_Command'Write (S, V.command);
      else
         JS.Start_Object;
         Write_String (JS, +"title", V.title);
         JS.Key ("kind");
         Optional_CodeActionKind'Write (S, V.kind);
         JS.Key ("diagnostics");
         Optional_Diagnostic_Vector'Write (S, V.diagnostics);
         Write_Optional_Boolean (JS, +"isPreferred", V.isPreferred);
         JS.Key ("edit");
         Optional_WorkspaceEdit'Write (S, V.edit);
         JS.Key ("command");
         Optional_Command'Write (S, V.command);
         JS.End_Object;
      end if;
   end Write_CodeAction;

   -----------------------------
   -- Write_CodeActionContext --
   -----------------------------

   procedure Write_CodeActionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionContext)
      renames LSP.Message_IO.Write_CodeActionContext;

   -----------------------------
   -- Write_CodeActionOptions --
   -----------------------------

   procedure Write_CodeActionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Optional_Boolean (JS, +"workDoneProgress", V.workDoneProgress);
      JS.Key ("codeActionKinds");
      Optional_CodeActionKindSet'Write (S, V.codeActionKinds);
      JS.End_Object;
   end Write_CodeActionOptions;

   --------------------------
   -- Write_CodeActionKind --
   --------------------------

   procedure Write_CodeActionKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionKind)
      renames LSP.Message_IO.Write_CodeActionKind;

   procedure Write_codeActionKindCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : codeActionKindCapability)
      renames LSP.Message_IO.Write_codeActionKindCapability;

   -----------------------------------------------
   -- Write_codeActionLiteralSupport_Capability --
   -----------------------------------------------

   procedure Write_codeActionLiteralSupport_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : codeActionLiteralSupport_Capability)
      renames LSP.Message_IO.Write_codeActionLiteralSupport_Capability;

   ----------------------------
   -- Write_CodeActionParams --
   ----------------------------

   procedure Write_CodeActionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Put_Progress_Partial_Params (S, V);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("range");
      Span'Write (S, V.span);
      JS.Key ("context");
      CodeActionContext'Write (S, V.context);
      JS.End_Object;
   end Write_CodeActionParams;

   ----------------------------------------
   -- Write_CodeActionClientCapabilities --
   ----------------------------------------

   procedure Write_CodeActionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionClientCapabilities)
      renames LSP.Message_IO.Write_CodeActionClientCapabilities;

   ---------------------------
   -- Write_CodeLensOptions --
   ---------------------------

   procedure Write_CodeLensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeLensOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Optional_Boolean (JS, +"workDoneProgress", V.workDoneProgress);
      Write_Optional_Boolean (JS, +"resolveProvider", V.resolveProvider);
      JS.End_Object;
   end Write_CodeLensOptions;

   ----------------------------
   -- Write_ColorInformation --
   ----------------------------

   procedure Write_ColorInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ColorInformation)
      renames LSP.Message_IO.Write_ColorInformation;

   -----------------------------
   -- Write_ColorPresentation --
   -----------------------------

   procedure Write_ColorPresentation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ColorPresentation)
      renames LSP.Message_IO.Write_ColorPresentation;

   -----------------------------------
   -- Write_ColorPresentationParams --
   -----------------------------------

   procedure Write_ColorPresentationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ColorPresentationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Put_Progress_Partial_Params (S, V);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("color");
      RGBA_Color'Write (S, V.color);
      JS.Key ("range");
      Span'Write (S, V.span);
      JS.End_Object;
   end Write_ColorPresentationParams;

   -------------------
   -- Write_Command --
   -------------------

   procedure Write_Command
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Command)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Temp : LSP.Commands.Command_Access;
   begin
      if V.Is_Unknown and then Is_Empty (V.command) then
         return;
      end if;

      JS.Start_Object;
      Write_String (JS, +"title", V.title);

      if V.Is_Unknown then
         Write_String (JS, +"command", V.command);
         JS.Key ("arguments");
         Optional_Any_Vector'Write (S, V.arguments);
      else
         Write_String
           (JS,
            +"command",
            +Ada.Tags.External_Tag (V.Custom.Unchecked_Get'Tag));
         JS.Key ("arguments");
         JS.Start_Array;
         Temp := LSP.Commands.Command_Access (V.Custom.Unchecked_Get);
         --  This Temp variable prevents compiler from a crash.
         LSP.Commands.Command'Class'Write (S, Temp.all);
         JS.End_Array;
      end if;

      JS.End_Object;
   end Write_Command;

   ----------------------------------------
   -- Write_CompletionClientCapabilities --
   ----------------------------------------

   procedure Write_CompletionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionClientCapabilities)
      renames LSP.Message_IO.Write_CompletionClientCapabilities;

   -----------------------------
   -- Write_CompletionContext --
   -----------------------------

   procedure Write_CompletionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionContext)
      renames LSP.Message_IO.Write_CompletionContext;

   --------------------------
   -- Write_CompletionItem --
   --------------------------

   procedure Write_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItem)
      renames LSP.Message_IO.Write_CompletionItem;

   procedure Write_CompletionTriggerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionTriggerKind)
      renames LSP.Message_IO.Write_CompletionTriggerKind;

   ------------------------------------
   -- Write_completionItemCapability --
   ------------------------------------

   procedure Write_completionItemCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : completionItemCapability)
      renames LSP.Message_IO.Write_completionItemCapability;

   ------------------------------
   -- Write_CompletionItemKind --
   ------------------------------

   procedure Write_CompletionItemKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItemKind)
      renames LSP.Message_IO.Write_CompletionItemKind;

   procedure Write_CompletionItemKindSetCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItemKindSetCapabilities)
      renames LSP.Message_IO.Write_CompletionItemKindSetCapabilities;

   -----------------------------
   -- Write_CompletionItemTag --
   -----------------------------

   procedure Write_CompletionItemTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItemTag)
      renames LSP.Message_IO.Write_CompletionItemTag;

   ------------------------------------
   -- Write_CompletionItemTagSupport --
   ------------------------------------

   procedure Write_CompletionItemTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItemTagSupport)
     renames LSP.Message_IO.Write_CompletionItemTagSupport;

   --------------------------
   -- Write_CompletionList --
   --------------------------

   procedure Write_CompletionList
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionList)
      renames LSP.Message_IO.Write_CompletionList;

   -----------------------------
   -- Write_CompletionOptions --
   -----------------------------

   procedure Write_CompletionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Optional_Boolean (JS, +"workDoneProgress", V.workDoneProgress);
      Write_Optional_Boolean (JS, +"resolveProvider", V.resolveProvider);
      JS.Key ("triggerCharacters");
      Optional_LSP_String_Vector'Write (S, V.triggerCharacters);
      JS.Key ("allCommitCharacters");
      Optional_LSP_String_Vector'Write (S, V.allCommitCharacters);
      JS.End_Object;
   end Write_CompletionOptions;

   ----------------------------
   -- Write_CompletionParams --
   ----------------------------

   procedure Write_CompletionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Put_Text_Progress_Partial_Params (S, V);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      Position'Write (S, V.position);
      JS.Key ("context");
      Optional_CompletionContext'Write (S, V.context);
      JS.End_Object;
   end Write_CompletionParams;

   -----------------------------
   -- Write_ConfigurationItem --
   -----------------------------

   procedure Write_ConfigurationItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ConfigurationItem)
      renames LSP.Message_IO.Write_ConfigurationItem;

   -------------------------------
   -- Write_ConfigurationParams --
   -------------------------------

   procedure Write_ConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ConfigurationParams)
      renames LSP.Message_IO.Write_ConfigurationParams;

   ----------------------
   -- Write_Diagnostic --
   ----------------------

   procedure Write_Diagnostic
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Diagnostic)
      renames LSP.Message_IO.Write_Diagnostic;

   ----------------------------------------
   -- Write_DiagnosticRelatedInformation --
   ----------------------------------------

   procedure Write_DiagnosticRelatedInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DiagnosticRelatedInformation)
      renames LSP.Message_IO.Write_DiagnosticRelatedInformation;

   ------------------------------
   -- Write_DiagnosticSeverity --
   ------------------------------

   procedure Write_DiagnosticSeverity
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DiagnosticSeverity)
      renames LSP.Message_IO.Write_DiagnosticSeverity;

   -------------------------
   -- Write_DiagnosticTag --
   -------------------------

   procedure Write_DiagnosticTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DiagnosticTag)
      renames LSP.Message_IO.Write_DiagnosticTag;

   --------------------------------
   -- Write_DiagnosticTagSupport --
   --------------------------------

   procedure Write_DiagnosticTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DiagnosticTagSupport)
      renames LSP.Message_IO.Write_DiagnosticTagSupport;

   ----------------------------------------
   -- Write_DidChangeConfigurationParams --
   ----------------------------------------

   procedure Write_DidChangeConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeConfigurationParams)
      renames LSP.Message_IO.Write_DidChangeConfigurationParams;

   ---------------------------------------
   -- Write_DidChangeTextDocumentParams --
   ---------------------------------------

   procedure Write_DidChangeTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeTextDocumentParams)
      renames LSP.Message_IO.Write_DidChangeTextDocumentParams;

   ----------------------------------------------------
   -- Write_DidChangeWatchedFilesRegistrationOptions --
   ----------------------------------------------------

   procedure Write_DidChangeWatchedFilesRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeWatchedFilesRegistrationOptions)
      renames LSP.Message_IO.Write_DidChangeWatchedFilesRegistrationOptions;

   -------------------------------------------
   -- Write_DidChangeWorkspaceFoldersParams --
   -------------------------------------------

   procedure Write_DidChangeWorkspaceFoldersParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeWorkspaceFoldersParams)
      renames LSP.Message_IO.Write_DidChangeWorkspaceFoldersParams;

   --------------------------------------
   -- Write_DidCloseTextDocumentParams --
   --------------------------------------

   procedure Write_DidCloseTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidCloseTextDocumentParams)
      renames LSP.Message_IO.Write_DidCloseTextDocumentParams;

   -------------------------------------
   -- Write_DidOpenTextDocumentParams --
   -------------------------------------

   procedure Write_DidOpenTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidOpenTextDocumentParams)
      renames LSP.Message_IO.Write_DidOpenTextDocumentParams;

   -------------------------------------
   -- Write_DidSaveTextDocumentParams --
   -------------------------------------

   procedure Write_DidSaveTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidSaveTextDocumentParams)
      renames LSP.Message_IO.Write_DidSaveTextDocumentParams;

   -----------------------------------------
   -- Write_DeclarationClientCapabilities --
   -----------------------------------------

   procedure Write_DeclarationClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DeclarationClientCapabilities)
      renames LSP.Message_IO.Write_DeclarationClientCapabilities;

   -------------------------------------------
   -- Write_WorkspaceEditClientCapabilities --
   -------------------------------------------

   procedure Write_WorkspaceEditClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceEditClientCapabilities)
      renames LSP.Message_IO.Write_WorkspaceEditClientCapabilities;

   -----------------------------
   -- Write_DocumentHighlight --
   -----------------------------

   procedure Write_DocumentHighlight
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentHighlight)
      renames LSP.Message_IO.Write_DocumentHighlight;

   ---------------------------------
   -- Write_DocumentHighlightKind --
   ---------------------------------

   procedure Write_DocumentHighlightKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentHighlightKind)
      renames LSP.Message_IO.Write_DocumentHighlightKind;

   ------------------------------------------
   -- Write_DocumentLinkClientCapabilities --
   ------------------------------------------

   procedure Write_DocumentLinkClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentLinkClientCapabilities)
      renames LSP.Message_IO.Write_DocumentLinkClientCapabilities;

   -------------------------------
   -- Write_DocumentLinkOptions --
   -------------------------------

   procedure Write_DocumentLinkOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentLinkOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Optional_Boolean (JS, +"workDoneProgress", V.workDoneProgress);
      Write_Optional_Boolean (JS, +"resolveProvider", V.resolveProvider);
      JS.End_Object;
   end Write_DocumentLinkOptions;

   -------------------------------------------
   -- Write_DocumentOnTypeFormattingOptions --
   -------------------------------------------

   procedure Write_DocumentOnTypeFormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentOnTypeFormattingOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Optional_Boolean (JS, +"workDoneProgress", V.workDoneProgress);
      Write_String (JS, +"firstTriggerCharacter", V.firstTriggerCharacter);
      JS.Key ("moreTriggerCharacter");
      Optional_LSP_String_Vector'Write (S, V.moreTriggerCharacter);
      JS.End_Object;
   end Write_DocumentOnTypeFormattingOptions;

   --------------------------------
   -- Write_DocumentSymbolParams --
   --------------------------------

   procedure Write_DocumentSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentSymbolParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Put_Progress_Partial_Params (S, V);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.End_Object;
   end Write_DocumentSymbolParams;

   -------------------------------
   -- Write_DocumentSymbol_Tree --
   -------------------------------

   procedure Write_DocumentSymbol_Tree
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentSymbol_Tree)
   is
      procedure Write_Array (Parent : DocumentSymbol_Trees.Cursor);

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      -----------------
      -- Write_Array --
      -----------------

      procedure Write_Array (Parent : DocumentSymbol_Trees.Cursor) is
      begin
         JS.Start_Array;

         for J in V.Iterate_Children (Parent) loop
            declare
               Item : DocumentSymbol renames DocumentSymbol_Trees.Element (J);
            begin
               JS.Start_Object;
               Write_String (JS, +"name", Item.name);
               Write_Optional_String (JS, +"detail", Item.detail);
               JS.Key ("kind");
               SymbolKind'Write (S, Item.kind);
               Write_Optional_Boolean (JS, +"deprecated", Item.deprecated);
               JS.Key ("range");
               Span'Write (S, Item.span);
               JS.Key ("selectionRange");
               Span'Write (S, Item.selectionRange);
               Write_Optional_Boolean
                 (JS, +"alsIsDeclaration", Item.alsIsDeclaration);
               Write_Optional_Boolean
                   (JS, +"alsIsAdaProcedure", Item.alsIsAdaProcedure);
               JS.Key ("alsVisibility");
               Optional_Als_Visibility'Write (S, Item.alsVisibility);

               if Item.children then
                  JS.Key ("children");
                  Write_Array (J);
               end if;

               JS.End_Object;
            end;
         end loop;

         JS.End_Array;
      end Write_Array;
   begin
      Write_Array (V.Root);
   end Write_DocumentSymbol_Tree;

   -------------------------------
   -- Write_dynamicRegistration --
   -------------------------------

   procedure Write_dynamicRegistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : dynamicRegistration)
      renames LSP.Message_IO.Write_dynamicRegistration;

   procedure Write_FileChangeType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileChangeType)
      renames LSP.Message_IO.Write_FileChangeType;

   -----------------------------
   -- Write_FileSystemWatcher --
   -----------------------------

   procedure Write_FileSystemWatcher
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileSystemWatcher)
      renames LSP.Message_IO.Write_FileSystemWatcher;

   ------------------------
   -- Write_FoldingRange --
   ------------------------

   procedure Write_FoldingRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FoldingRange)
      renames LSP.Message_IO.Write_FoldingRange;

   ------------------------------
   -- Write_FoldingRangeParams --
   ------------------------------

   procedure Write_FoldingRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FoldingRangeParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Put_Progress_Partial_Params (S, V);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.End_Object;
   end Write_FoldingRangeParams;

   -------------------------------
   -- Write_DocumentColorParams --
   -------------------------------

   procedure Write_DocumentColorParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentColorParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Put_Progress_Partial_Params (S, V);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.End_Object;
   end Write_DocumentColorParams;

   ---------------------------------
   -- Write_ExecuteCommandOptions --
   ---------------------------------

   procedure Write_ExecuteCommandOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ExecuteCommandOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Optional_Boolean (JS, +"workDoneProgress", V.workDoneProgress);
      Write_String_Vector (JS, +"commands", V.commands);
      JS.End_Object;
   end Write_ExecuteCommandOptions;

   --------------------------------
   -- Write_ExecuteCommandParams --
   --------------------------------

   procedure Write_ExecuteCommandParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ExecuteCommandParams)
   is
      Temp : LSP.Commands.Command_Access;
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Put_WorkDoneProgressParams (S, V.Base);
      JS.Key ("arguments");

      if V.Is_Unknown then
         Optional_Any_Vector'Write (S, V.arguments);
         Write_String (JS, +"command", V.command);
      else
         JS.Start_Array;
         Temp := LSP.Commands.Command_Access (V.Custom.Unchecked_Get);
         --  This Temp variable prevents compiler from a crash.
         LSP.Commands.Command'Class'Write (S, Temp.all);
         JS.End_Array;
         Write_String (JS, +"command", V.command);
      end if;

      JS.End_Object;
   end Write_ExecuteCommandParams;

   -------------------------------
   -- Write_FailureHandlingKind --
   -------------------------------

   procedure Write_FailureHandlingKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FailureHandlingKind)
      renames LSP.Message_IO.Write_FailureHandlingKind;

   ------------------------------------------
   -- Write_FoldingRangeClientCapabilities --
   ------------------------------------------

   procedure Write_FoldingRangeClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FoldingRangeClientCapabilities)
      renames LSP.Message_IO.Write_FoldingRangeClientCapabilities;

   -----------------
   -- Write_Hover --
   -----------------

   procedure Write_Hover
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Hover)
      renames LSP.Message_IO.Write_Hover;

   -----------------------------------
   -- Write_HoverClientCapabilities --
   -----------------------------------

   procedure Write_HoverClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : HoverClientCapabilities)
      renames LSP.Message_IO.Write_HoverClientCapabilities;

   ----------------------------------
   -- Read_HoverClientCapabilities --
   ----------------------------------

   procedure Read_HoverClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out HoverClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Optional_Boolean
        (JS, +"dynamicRegistration", V.dynamicRegistration);
      JS.Key ("contentFormat");
      Optional_MarkupKind_Vector'Read (S, V.contentFormat);
      JS.End_Object;
   end Read_HoverClientCapabilities;

   ----------------------------
   -- Write_InitializeParams --
   ----------------------------

   procedure Write_InitializeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InitializeParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      Write_Optional_Number (JS, +"processId", V.processId);
      JS.Key ("clientInfo");
      Optional_ProgramInfo'Write (S, V.clientInfo);

      if not LSP.Types.Is_Empty (V.rootPath) then
         Write_String (JS, +"rootPath", V.rootPath);
      end if;

      Write_String (JS, +"rootUri", V.rootUri);
      JS.Key ("capabilities");
      LSP.Messages.ClientCapabilities'Write (S, V.capabilities);
      JS.Key ("trace");
      Optional_Trace_Kind'Write (S, V.trace);
      JS.Key ("workspaceFolders");
      Optional_WorkspaceFolder_Vector'Write (S, V.workspaceFolders);
      JS.End_Object;
   end Write_InitializeParams;

   ----------------------------
   -- Write_InitializeResult --
   ----------------------------

   procedure Write_InitializeResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InitializeResult)
      renames LSP.Message_IO.Write_InitializeResult;

   -----------------------------
   -- Write_InitializedParams --
   -----------------------------

   not overriding procedure Write_InitializedParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InitializedParams)
      renames LSP.Message_IO.Write_InitializedParams;

   ----------------------------
   -- Write_InsertTextFormat --
   ----------------------------

   procedure Write_InsertTextFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InsertTextFormat)
      renames LSP.Message_IO.Write_InsertTextFormat;

   --------------------
   -- Write_Location --
   --------------------

   procedure Write_Location
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Location)
      renames LSP.Message_IO.Write_Location;

   ------------------------
   -- Write_LocationLink --
   ------------------------

   procedure Write_LocationLink
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LocationLink)
      renames LSP.Message_IO.Write_LocationLink;

   -----------------------------------
   -- Write_Location_Or_Link_Vector --
   -----------------------------------

   procedure Write_Location_Or_Link_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Location_Or_Link_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      case V.Kind is
         when Empty_Vector_Kind =>
            JS.Write (GNATCOLL.JSON.JSON_Null);
         when Location_Vector_Kind =>
            Location_Vector'Write (S, V.Locations);
         when LocationLink_Vector_Kind =>
            LocationLink_Vector'Write (S, V.LocationLinks);
      end case;
   end Write_Location_Or_Link_Vector;

   ----------------------------
   -- Write_LogMessageParams --
   ----------------------------

   procedure Write_LogMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LogMessageParams)
      renames LSP.Message_IO.Write_LogMessageParams;

   ------------------------
   -- Write_MarkedString --
   ------------------------

   procedure Write_MarkedString
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MarkedString)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V.Is_String then
         JS.Write (GNATCOLL.JSON.Create (To_UTF_8_String (V.value)));
      else
         JS.Start_Object;
         Write_String (JS, +"language", V.language);
         Write_String (JS, +"value", V.value);
         JS.End_Object;
      end if;
   end Write_MarkedString;

   -------------------------
   -- Write_MarkupContent --
   -------------------------

   procedure Write_MarkupContent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MarkupContent)
      renames LSP.Message_IO.Write_MarkupContent;

   ------------------------------------------------
   -- Write_MarkupContent_Or_MarkedString_Vector --
   ------------------------------------------------

   procedure Write_MarkupContent_Or_MarkedString_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MarkupContent_Or_MarkedString_Vector) is
   begin
      if V.Is_MarkupContent then
         MarkupContent'Write (S, V.MarkupContent);
      elsif V.Vector.Last_Index = 1 then
         MarkedString'Write (S, V.Vector.First_Element);
      else
         MarkedString_Vector'Write (S, V.Vector);
      end if;
   end Write_MarkupContent_Or_MarkedString_Vector;

   ----------------------
   -- Write_MarkupKind --
   ----------------------

   procedure Write_MarkupKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MarkupKind)
      renames LSP.Message_IO.Write_MarkupKind;

   -----------------------
   -- Write_MessageType --
   -----------------------

   procedure Write_MessageType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MessageType)
      renames LSP.Message_IO.Write_MessageType;

   -------------------------------
   -- Write_NotificationMessage --
   -------------------------------

   procedure Write_NotificationMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : NotificationMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Notification_Prefix (S, V);
      JS.End_Object;
   end Write_NotificationMessage;

   -------------------------------
   -- Write_Notification_Prefix --
   -------------------------------

   procedure Write_Notification_Prefix
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.NotificationMessage'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      Write_String (JS, +"jsonrpc", V.jsonrpc);
      Write_String (JS, +"method", V.method);
   end Write_Notification_Prefix;

   ---------------------------
   -- Write_Optional_Number --
   ---------------------------

   procedure Write_Optional_Number
    (Stream     : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key        : LSP.Types.LSP_String;
     Item       : LSP.Types.Optional_Number;
     Write_Null : Boolean := False) is
   begin
      if Item.Is_Set then
         Write_Number (Stream, Key, Item.Value);
      elsif Write_Null then
         Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
         Stream.Write (GNATCOLL.JSON.Create);
      end if;
   end Write_Optional_Number;

   ---------------------------
   -- Write_Optional_String --
   ---------------------------

   procedure Write_Optional_String
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : LSP.Types.Optional_String) is
   begin
      if Item.Is_Set then
         Write_String (Stream, Key, Item.Value);
      end if;
   end Write_Optional_String;

   --------------------------------------------
   -- Write_Optional_TextDocumentSyncOptions --
   --------------------------------------------

   procedure Write_Optional_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Optional_TextDocumentSyncOptions) is
   begin
      if not V.Is_Set then
         return;
      elsif V.Is_Number then
         TextDocumentSyncKind'Write (S, V.Value);
      else
         TextDocumentSyncOptions'Write (S, V.Options);
      end if;
   end Write_Optional_TextDocumentSyncOptions;

   --------------------------------
   -- Write_ParameterInformation --
   --------------------------------

   procedure Write_ParameterInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ParameterInformation)
      renames LSP.Message_IO.Write_ParameterInformation;

   ---------------------------
   -- Write_Parameter_Label --
   ---------------------------

   procedure Write_Parameter_Label
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Parameter_Label)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V.Is_String then
         JS.Write
           (GNATCOLL.JSON.Create (To_UTF_8_Unbounded_String (V.String)));
      else
         JS.Start_Array;
         JS.Write (GNATCOLL.JSON.Create (Integer (V.From)));
         JS.Write (GNATCOLL.JSON.Create (Integer (V.Till)));
         JS.End_Array;
      end if;
   end Write_Parameter_Label;

   -------------------------------------------
   -- Write_parameterInformation_Capability --
   -------------------------------------------

   procedure Write_parameterInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : parameterInformation_Capability)
      renames LSP.Message_IO.Write_parameterInformation_Capability;

   --------------------
   -- Write_Position --
   --------------------

   procedure Write_Position
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Position)
      renames LSP.Message_IO.Write_Position;

   ----------------------------
   -- Write_Provider_Options --
   ----------------------------

   procedure Write_Provider_Options
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Provider_Options)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V.Is_Boolean then
         JS.Write (GNATCOLL.JSON.Create (V.Bool));
      elsif V.Options.Is_Set then
         Optional_TSW_RegistrationOptions'Write (S, V.Options);
      else
         JS.Write (GNATCOLL.JSON.Create_Object);  --  Write {}
      end if;
   end Write_Provider_Options;

   ------------------------------------
   -- Write_PublishDiagnosticsParams --
   ------------------------------------

   procedure Write_PublishDiagnosticsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : PublishDiagnosticsParams)
      renames LSP.Message_IO.Write_PublishDiagnosticsParams;

   ------------------------------------------------
   -- Write_PublishDiagnosticsClientCapabilities --
   ------------------------------------------------

   procedure Write_PublishDiagnosticsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : PublishDiagnosticsClientCapabilities)
      renames LSP.Message_IO.Write_PublishDiagnosticsClientCapabilities;

   ----------------------
   -- Write_RGBA_Color --
   ----------------------

   procedure Write_RGBA_Color
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RGBA_Color)
      renames LSP.Message_IO.Write_RGBA_Color;

   ----------------------------
   -- Write_ReferenceContext --
   ----------------------------

   procedure Write_ReferenceContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ReferenceContext)
      renames LSP.Message_IO.Write_ReferenceContext;

   ---------------------------
   -- Write_ReferenceParams --
   ---------------------------

   procedure Write_ReferenceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ReferenceParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Put_Text_Progress_Partial_Params (S, V);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      Position'Write (S, V.position);
      JS.Key ("context");
      ReferenceContext'Write (S, V.context);
      JS.End_Object;
   end Write_ReferenceParams;

   --------------------
   -- Write_Response --
   --------------------

   procedure Write_Response_Prefix
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ResponseMessage'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      Write_String (JS, +"jsonrpc", V.jsonrpc);
      Write_Number_Or_String (JS, +"id", V.id);
      JS.Key ("error");
      Optional_ResponseError'Write (S, V.error);
   end Write_Response_Prefix;

   --------------------------
   -- Write_RequestMessage --
   --------------------------

   procedure Write_RequestMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RequestMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_String (JS, +"jsonrpc", V.jsonrpc);
      Write_String (JS, +"method", V.method);
      Write_Number_Or_String (JS, +"id", V.id);
      JS.End_Object;
   end Write_RequestMessage;

   -------------------------
   -- Write_RenameOptions --
   -------------------------

   procedure Write_RenameOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Optional_Boolean (JS, +"workDoneProgress", V.workDoneProgress);
      Write_Optional_Boolean (JS, +"prepareProvider", V.prepareProvider);
      JS.End_Object;
   end Write_RenameOptions;

   ------------------------
   -- Write_RenameParams --
   ------------------------

   procedure Write_RenameParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Put_Text_Progress_Params (S, V);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      Position'Write (S, V.position);
      Write_String (JS, +"newName", V.newName);
      JS.End_Object;
   end Write_RenameParams;

   ------------------------------------
   -- Write_RenameClientCapabilities --
   ------------------------------------

   procedure Write_RenameClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameClientCapabilities)
      renames LSP.Message_IO.Write_RenameClientCapabilities;

   ---------------------------------
   -- Write_ResourceOperationKind --
   ---------------------------------

   procedure Write_ResourceOperationKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ResourceOperationKind)
      renames LSP.Message_IO.Write_ResourceOperationKind;

   ---------------------------
   -- Write_ResponseMessage --
   ---------------------------

   procedure Write_ResponseMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ResponseMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Response_Prefix (S, V);

      if not V.Is_Error then
         JS.Key ("result");
         JS.Write (GNATCOLL.JSON.JSON_Null);
      end if;

      JS.End_Object;
   end Write_ResponseMessage;

   -----------------------
   -- Write_SaveOptions --
   -----------------------

   procedure Write_SaveOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SaveOptions)
      renames LSP.Message_IO.Write_SaveOptions;

   --------------------------
   -- Write_SelectionRange --
   --------------------------

   procedure Write_SelectionRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SelectionRange)
      renames LSP.Message_IO.Write_SelectionRange;

   --------------------------------
   -- Write_SelectionRangeParams --
   --------------------------------

   procedure Write_SelectionRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SelectionRangeParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Put_Progress_Partial_Params (S, V);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("positions");
      Position_Vector'Write (S, V.positions);
      JS.End_Object;
   end Write_SelectionRangeParams;

   ------------------------------
   -- Write_ServerCapabilities --
   ------------------------------

   procedure Write_ServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ServerCapabilities)
      renames LSP.Message_IO.Write_ServerCapabilities;

   -------------------------
   -- Write_SignatureHelp --
   -------------------------

   procedure Write_SignatureHelp
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelp)
      renames LSP.Message_IO.Write_SignatureHelp;

   --------------------------------
   -- Write_SignatureHelpOptions --
   --------------------------------

   procedure Write_SignatureHelpOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("triggerCharacters");
      Optional_LSP_String_Vector'Write (S, V.triggerCharacters);
      JS.Key ("retriggerCharacters");
      Optional_LSP_String_Vector'Write (S, V.retriggerCharacters);
      JS.End_Object;
   end Write_SignatureHelpOptions;

   -------------------------------------------
   -- Write_SignatureHelpClientCapabilities --
   -------------------------------------------

   procedure Write_SignatureHelpClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpClientCapabilities)
      renames LSP.Message_IO.Write_SignatureHelpClientCapabilities;

   --------------------------------
   -- Write_SignatureInformation --
   --------------------------------

   procedure Write_SignatureInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureInformation)
      renames LSP.Message_IO.Write_SignatureInformation;

   -------------------------------------------
   -- Write_signatureInformation_Capability --
   -------------------------------------------

   procedure Write_signatureInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : signatureInformation_Capability)
      renames LSP.Message_IO.Write_signatureInformation_Capability;

   ----------------
   -- Write_Span --
   ----------------

   procedure Write_Span
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Span)
      renames LSP.Message_IO.Write_Span;

   -------------------------------------
   -- Write_StaticRegistrationOptions --
   -------------------------------------

   procedure Write_TSW_RegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TSW_RegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Optional_Boolean (JS, +"workDoneProgress", V.workDoneProgress);
      Write_Optional_String (JS, +"id", V.id);
      JS.Key ("documentSelector");
      DocumentSelector'Write (S, V.documentSelector);
      JS.End_Object;
   end Write_TSW_RegistrationOptions;

   -----------------------------------
   -- Write_String_Or_MarkupContent --
   -----------------------------------

   procedure Write_String_Or_MarkupContent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : String_Or_MarkupContent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      case V.Is_String is
         when True =>
            JS.Write
              (GNATCOLL.JSON.Create (To_UTF_8_Unbounded_String (V.String)));
         when False =>
            MarkupContent'Write (S, V.Content);
      end case;
   end Write_String_Or_MarkupContent;

   -------------------------
   -- Write_String_Vector --
   -------------------------

   procedure Write_String_Vector
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : LSP.Types.LSP_String_Vector) is
   begin
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
      Stream.Start_Array;

      for J in 1 .. Item.Last_Index loop
         Stream.Write
           (GNATCOLL.JSON.Create (To_UTF_8_String (Item.Element (J))));
      end loop;

      Stream.End_Array;
   end Write_String_Vector;

   -----------------------------
   -- Write_SymbolInformation --
   -----------------------------

   procedure Write_SymbolInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SymbolInformation)
      renames LSP.Message_IO.Write_SymbolInformation;

   ----------------------
   -- Write_SymbolKind --
   ----------------------

   procedure Write_SymbolKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SymbolKind)
      renames LSP.Message_IO.Write_SymbolKind;

   procedure Write_symbolKindCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : symbolKindCapabilities)
      renames LSP.Message_IO.Write_symbolKindCapabilities;

   --------------------------
   -- Write_Als_Visibility --
   --------------------------

   procedure Write_Als_Visibility
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Als_Visibility)
      renames LSP.Message_IO.Write_Als_Visibility;

   -------------------------
   -- Write_Symbol_Vector --
   -------------------------

   procedure Write_Symbol_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Symbol_Vector) is
   begin
      if V.Is_Tree then
         DocumentSymbol_Tree'Write (S, V.Tree);
      else
         SymbolInformation_Vector'Write (S, V.Vector);
      end if;
   end Write_Symbol_Vector;

   procedure Write_TextDocumentSaveReason
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSaveReason)
      renames LSP.Message_IO.Write_TextDocumentSaveReason;

   ----------------------------------------------
   -- Write_TextDocumentSyncClientCapabilities --
   ----------------------------------------------

   procedure Write_TextDocumentSyncClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSyncClientCapabilities)
      renames LSP.Message_IO.Write_TextDocumentSyncClientCapabilities;

   ------------------------------------------
   -- Write_TextDocumentClientCapabilities --
   ------------------------------------------

   procedure Write_TextDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentClientCapabilities)
      renames LSP.Message_IO.Write_TextDocumentClientCapabilities;

   ------------------------------------------
   -- Write_TextDocumentContentChangeEvent --
   ------------------------------------------

   procedure Write_TextDocumentContentChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentContentChangeEvent)
      renames LSP.Message_IO.Write_TextDocumentContentChangeEvent;

   ----------------------------
   -- Write_TextDocumentEdit --
   ----------------------------

   procedure Write_TextDocumentEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentEdit) renames LSP.Message_IO.Write_TextDocumentEdit;

   ---------------------------
   -- Write_Document_Change --
   ---------------------------

   procedure Write_Document_Change
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Document_Change) is
   begin
      case V.Kind is
         when Text_Document_Edit =>
            TextDocumentEdit'Write (S, V.Text_Document_Edit);
         when Create_File =>
            CreateFile'Write (S, V.Create_File);
         when Rename_File =>
            RenameFile'Write (S, V.Rename_File);
         when Delete_File =>
            DeleteFile'Write (S, V.Delete_File);
      end case;
   end Write_Document_Change;

   --------------------------------------------
   -- Write_DocumentSymbolClientCapabilities --
   --------------------------------------------

   procedure Write_DocumentSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentSymbolClientCapabilities)
      renames LSP.Message_IO.Write_DocumentSymbolClientCapabilities;

   ----------------------------------
   -- Write_TextDocumentIdentifier --
   ----------------------------------

   procedure Write_TextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentIdentifier)
      renames LSP.Message_IO.Write_TextDocumentIdentifier;

   ----------------------------
   -- Write_TextDocumentItem --
   ----------------------------

   procedure Write_TextDocumentItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentItem)
      renames LSP.Message_IO.Write_TextDocumentItem;

   --------------------------------------
   -- Write_TextDocumentPositionParams --
   --------------------------------------

   procedure Write_TextDocumentPositionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentPositionParams)
      renames LSP.Message_IO.Write_TextDocumentPositionParams;

   --------------------------------
   -- Write_TextDocumentSyncKind --
   --------------------------------

   procedure Write_TextDocumentSyncKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSyncKind)
      renames LSP.Message_IO.Write_TextDocumentSyncKind;

   -----------------------------------
   -- Write_TextDocumentSyncOptions --
   -----------------------------------

   procedure Write_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSyncOptions)
      renames LSP.Message_IO.Write_TextDocumentSyncOptions;

   --------------------
   -- Write_TextEdit --
   --------------------

   procedure Write_TextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextEdit)
      renames LSP.Message_IO.Write_TextEdit;

   procedure Write_Trace_Kind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Trace_Kind)
      renames LSP.Message_IO.Write_Trace_Kind;

   -------------------------------------------
   -- Write_VersionedTextDocumentIdentifier --
   -------------------------------------------

   procedure Write_VersionedTextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : VersionedTextDocumentIdentifier)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      DocumentUri'Write (S, V.uri);
      Write_Optional_Number (JS, +"version", V.version, Write_Null => True);
      JS.End_Object;
   end Write_VersionedTextDocumentIdentifier;

   -------------------------
   -- Write_WatchKind_Set --
   -------------------------

   procedure Write_WatchKind_Set
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WatchKind_Set)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Result : Integer := 0;
      Mask   : Integer := 1;
   begin
      if V /= Default_WatchKind_Set then
         for J in WatchKind loop
            if V (J) then
               Result := Result + Mask;
            end if;

            Mask := Mask * 2;
         end loop;

         JS.Write (GNATCOLL.JSON.Create (Result));
      end if;
   end Write_WatchKind_Set;

   ---------------------------------------------
   -- Write_WorkspaceSymbolClientCapabilities --
   ---------------------------------------------

   procedure Write_WorkspaceSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceSymbolClientCapabilities)
      renames LSP.Message_IO.Write_WorkspaceSymbolClientCapabilities;

   ---------------------------------------
   -- Write_WorkspaceClientCapabilities --
   ---------------------------------------

   procedure Write_WorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceClientCapabilities)
     renames LSP.Message_IO.Write_WorkspaceClientCapabilities;

   -------------------------
   -- Write_WorkspaceEdit --
   -------------------------

   procedure Write_WorkspaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      if V.documentChanges.Is_Empty then
         JS.Key ("changes");

         JS.Start_Object;
         for Cursor in V.changes.Iterate loop
            JS.Key
              (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String
                 (TextDocumentEdit_Maps.Key (Cursor)));
            JS.Start_Array;
            for Edit of V.changes (Cursor) loop
               TextEdit'Write (S, Edit);
            end loop;
            JS.End_Array;
         end loop;
         JS.End_Object;
      else
         JS.Key ("documentChanges");
         Document_Change_Vector'Write (S, V.documentChanges);
      end if;
      JS.End_Object;
   end Write_WorkspaceEdit;

   ---------------------------
   -- Write_WorkspaceFolder --
   ---------------------------

   procedure Write_WorkspaceFolder
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceFolder)
      renames LSP.Message_IO.Write_WorkspaceFolder;

   ---------------------------------------
   -- Write_WorkspaceFoldersChangeEvent --
   ---------------------------------------

   procedure Write_WorkspaceFoldersChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceFoldersChangeEvent)
      renames LSP.Message_IO.Write_WorkspaceFoldersChangeEvent;

   ----------------------------
   -- Write_workspaceFolders --
   ----------------------------

   procedure Write_WorkspaceFoldersServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceFoldersServerCapabilities)
      renames LSP.Message_IO.Write_WorkspaceFoldersServerCapabilities;

   -----------------------------
   -- Write_workspace_Options --
   -----------------------------

   procedure Write_workspace_Options
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : workspace_Options)
      renames LSP.Message_IO.Write_workspace_Options;

   ---------------------------------
   -- Write_WorkspaceSymbolParams --
   ---------------------------------

   procedure Write_WorkspaceSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceSymbolParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Put_Progress_Partial_Params (S, V);
      Write_String (JS, +"query", V.query);
      JS.End_Object;
   end Write_WorkspaceSymbolParams;

   ----------------------------------------
   -- Read_ALS_Subprogram_And_References --
   ----------------------------------------

   procedure Read_ALS_Subprogram_And_References
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_Subprogram_And_References)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("location");
      Location'Read (S, V.loc);
      Read_String (JS, +"name", V.name);
      JS.Key ("refs");
      Location_Vector'Read (S, V.refs);
      JS.End_Object;
   end Read_ALS_Subprogram_And_References;

   -------------------------
   -- Read_ALSDebugParams --
   -------------------------

   procedure Read_ALSDebugParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALSDebugParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Number (JS, +"inputQueueLength", V.inputQueueLength);
      JS.End_Object;
   end Read_ALSDebugParams;

   -----------------------------------------
   -- Write_ALS_Subprogram_And_References --
   -----------------------------------------

   procedure Write_ALS_Subprogram_And_References
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_Subprogram_And_References)
      renames LSP.Message_IO.Write_ALS_Subprogram_And_References;

   --------------------------
   -- Write_ALSDebugParams --
   --------------------------

   procedure Write_ALSDebugParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALSDebugParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Number (JS, +"inputQueueLength", V.inputQueueLength);
      JS.End_Object;
   end Write_ALSDebugParams;

   ------------------------------------
   -- Write_WindowClientCapabilities --
   ------------------------------------

   procedure Write_WindowClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WindowClientCapabilities)
      renames LSP.Message_IO.Write_WindowClientCapabilities;

   ---------------------------------
   -- Write_WorkDoneProgressBegin --
   ---------------------------------

   procedure Write_WorkDoneProgressBegin
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkDoneProgressBegin)
      renames LSP.Message_IO.Write_WorkDoneProgressBegin;

   ----------------------------------------
   -- Write_WorkDoneProgressCreateParams --
   ----------------------------------------

   procedure Write_WorkDoneProgressCreateParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkDoneProgressCreateParams)
      renames LSP.Message_IO.Write_WorkDoneProgressCreateParams;

   -----------------------------------
   -- Write_WorkDoneProgressOptions --
   -----------------------------------

   procedure Write_WorkDoneProgressOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkDoneProgressOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      --  In case we don't have workDoneProgress property let's write a boolean
      --  value instead of an empty object to be compatible with older protocol
      --  readers.

      if not V.workDoneProgress.Is_Set then
         JS.Write (GNATCOLL.JSON.Create (True));
      else
         JS.Start_Object;
         Write_Optional_Boolean (JS, +"workDoneProgress", V.workDoneProgress);
         JS.End_Object;
      end if;
   end Write_WorkDoneProgressOptions;

   ----------------------------------
   -- Write_WorkDoneProgressReport --
   ----------------------------------

   procedure Write_WorkDoneProgressReport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkDoneProgressReport)
      renames LSP.Message_IO.Write_WorkDoneProgressReport;

   -------------------------------
   -- Write_WorkDoneProgressEnd --
   -------------------------------

   procedure Write_WorkDoneProgressEnd
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkDoneProgressEnd)
      renames LSP.Message_IO.Write_WorkDoneProgressEnd;

   --------------------------
   -- Read_Progress_Params --
   --------------------------

   procedure Read_Progress_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Progress_Params)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      kind  : LSP_String;
      token : LSP_Number_Or_String;
   begin
      JS.Start_Object;
      Read_Number_Or_String (JS, +"token", token);
      JS.Key ("value");
      JS.Start_Object;
      Read_String (JS, +"kind", kind);
      if kind = +"begin" then
         declare
            P : Progress_Params (Progress_Begin);
         begin
            P.Begin_Param.token := token;
            Read_String (JS, +"title", P.Begin_Param.value.title);
            Read_Optional_Boolean (JS, +"cancellable",
                                   P.Begin_Param.value.cancellable);
            Read_Optional_String (JS, +"message",
                                  P.Begin_Param.value.message);
            JS.Key ("percentage");
            Optional_Number'Read (S, P.Begin_Param.value.percentage);
            JS.End_Object;
            JS.End_Object;
            V := P;
            return;
         end;
      elsif kind = +"report" then
         declare
            P : Progress_Params (Progress_Report);
         begin
            P.Report_Param.token := token;
            Read_Optional_Boolean (JS, +"cancellable",
                                   P.Report_Param.value.cancellable);
            Read_Optional_String (JS, +"message",
                                  P.Report_Param.value.message);
            JS.Key ("percentage");
            Optional_Number'Read (S, P.Report_Param.value.percentage);
            JS.End_Object;
            JS.End_Object;
            V := P;
            return;
         end;
      elsif kind = +"end" then
         declare
            P : Progress_Params (Progress_End);
         begin
            P.End_Param.token := token;
            Read_Optional_String (JS, +"message",
                                  P.End_Param.value.message);
            JS.End_Object;
            JS.End_Object;
            V := P;
            return;
         end;
      else
         --  Not implemented
         raise Program_Error;
      end if;
   end Read_Progress_Params;

   ---------------------------
   -- Write_Progress_Params --
   ---------------------------

   procedure Write_Progress_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Progress_Params)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      case V.Kind is
         when Progress_Begin =>
            Write_Number_Or_String (JS, +"token", V.Begin_Param.token);
            JS.Key ("value");
            JS.Start_Object;
            Write_String (JS, +"kind", +"begin");
            Write_String (JS, +"title", V.Begin_Param.value.title);
            Write_Optional_Boolean (JS, +"cancellable",
                                    V.Begin_Param.value.cancellable);
            Write_Optional_String (JS, +"message",
                                   V.Begin_Param.value.message);
            Write_Optional_Number (JS, +"percentage",
                                   V.Begin_Param.value.percentage);
            JS.End_Object;
         when Progress_Report =>
            Write_Number_Or_String (JS, +"token", V.Report_Param.token);
            JS.Key ("value");
            JS.Start_Object;
            Write_String (JS, +"kind", +"report");
            Write_Optional_Boolean (JS, +"cancellable",
                                    V.Report_Param.value.cancellable);
            Write_Optional_String (JS, +"message",
                                   V.Report_Param.value.message);
            Write_Optional_Number (JS, +"percentage",
                                   V.Report_Param.value.percentage);
            JS.End_Object;
         when Progress_End =>
            Write_Number_Or_String (JS, +"token", V.End_Param.token);
            JS.Key ("value");
            JS.Start_Object;
            Write_String (JS, +"kind", +"end");
            Write_Optional_String (JS, +"message", V.End_Param.value.message);
            JS.End_Object;
      end case;

      JS.End_Object;
   end Write_Progress_Params;

   ----------------------------
   -- Read_FormattingOptions --
   ----------------------------

   procedure Read_FormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FormattingOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Number (JS, +"tabSize", V.tabSize);

      JS.Key ("insertSpaces");
      V.insertSpaces := JS.Read.Get;

      Read_Optional_Boolean
        (JS, +"trimTrailingWhitespace", V.trimTrailingWhitespace);
      Read_Optional_Boolean (JS, +"insertFinalNewline", V.insertFinalNewline);
      Read_Optional_Boolean (JS, +"trimFinalNewlines", V.trimFinalNewlines);
      JS.End_Object;
   end Read_FormattingOptions;

   -----------------------------
   -- Write_FormattingOptions --
   -----------------------------

   procedure Write_FormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FormattingOptions)
      renames LSP.Message_IO.Write_FormattingOptions;

   ----------------------------------------
   -- Read_DocumentRangeFormattingParams --
   ----------------------------------------

   procedure Read_DocumentRangeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentRangeFormattingParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Get_WorkDoneProgressParams (S, V);

      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);

      JS.Key ("range");
      Span'Read (S, V.span);

      JS.Key ("options");
      FormattingOptions'Read (S, V.options);

      JS.End_Object;
   end Read_DocumentRangeFormattingParams;

   -----------------------------------------
   -- Write_DocumentRangeFormattingParams --
   -----------------------------------------

   procedure Write_DocumentRangeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentRangeFormattingParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Put_WorkDoneProgressParams (S, V);

      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);

      JS.Key ("range");
      Span'Write (S, V.span);

      JS.Key ("options");
      FormattingOptions'Write (S, V.options);

      JS.End_Object;
   end Write_DocumentRangeFormattingParams;

   -----------------------------------
   -- Read_DocumentFormattingParams --
   -----------------------------------

   procedure Read_DocumentFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentFormattingParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Get_WorkDoneProgressParams (S, V);

      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);

      JS.Key ("options");
      FormattingOptions'Read (S, V.options);

      JS.End_Object;
   end Read_DocumentFormattingParams;

   ------------------------------------
   -- Write_DocumentFormattingParams --
   ------------------------------------

   procedure Write_DocumentFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentFormattingParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Put_WorkDoneProgressParams (S, V);

      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);

      JS.Key ("options");
      FormattingOptions'Write (S, V.options);

      JS.End_Object;
   end Write_DocumentFormattingParams;

   -----------------------------------------
   -- Read_DocumentOnTypeFormattingParams --
   -----------------------------------------

   procedure Read_DocumentOnTypeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentOnTypeFormattingParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Get_TextDocumentPositionParams (S, V);

      Read_String (JS, +"ch", V.ch);

      JS.Key ("options");
      FormattingOptions'Read (S, V.options);

      JS.End_Object;
   end Read_DocumentOnTypeFormattingParams;

   ------------------------------------------
   -- Write_DocumentOnTypeFormattingParams --
   ------------------------------------------

   procedure Write_DocumentOnTypeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentOnTypeFormattingParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Put_TextDocumentPositionParams (S, V);

      Write_String (JS, +"ch", V.ch);

      JS.Key ("options");
      FormattingOptions'Write (S, V.options);

      JS.End_Object;
   end Write_DocumentOnTypeFormattingParams;

end LSP.Messages;

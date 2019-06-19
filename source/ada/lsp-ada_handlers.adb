------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with Ada.Strings.UTF_Encoding;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Strings.Unbounded;
with Ada.Directories;

with GNAT.Strings;
with GNATCOLL.JSON;
with GNATCOLL.Utils;             use GNATCOLL.Utils;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GNATCOLL.Traces;

with LSP.Messages.Notifications; use LSP.Messages.Notifications;
with LSP.Types; use LSP.Types;

with LSP.Ada_Documents;
with LSP.Lal_Utils;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Doc_Utils;

package body LSP.Ada_Handlers is

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
                 return LSP.Types.LSP_String renames
     LSP.Types.To_LSP_String;

   function Get_Node_Location
     (Self : access Message_Handler;
      Node : Libadalang.Analysis.Ada_Node) return LSP.Messages.Location;
   --  Return the location of the given node

   -----------------------
   -- Get_Node_Location --
   -----------------------

   function Get_Node_Location
     (Self : access Message_Handler;
      Node : Libadalang.Analysis.Ada_Node) return LSP.Messages.Location
   is
      use Libadalang.Analysis;
      use Libadalang.Common;

      Start_Sloc_Range                                     :
      constant Langkit_Support.Slocs.Source_Location_Range :=
         Sloc_Range (Data (Node.Token_Start));
      End_Sloc_Range                                       :
      constant Langkit_Support.Slocs.Source_Location_Range :=
         Sloc_Range (Data (Node.Token_End));

      First_Position : constant LSP.Messages.Position :=
                         (Line_Number (Start_Sloc_Range.Start_Line) - 1,
                          UTF_16_Index (Start_Sloc_Range.Start_Column) - 1);
      Last_Position  : constant LSP.Messages.Position :=
                         (Line_Number (End_Sloc_Range.End_Line) - 1,
                          UTF_16_Index (End_Sloc_Range.End_Column) - 1);

      Location : constant LSP.Messages.Location :=
                   (uri  => Self.Context.File_To_URI (+Node.Unit.Get_Filename),
                    span => LSP.Messages.Span'(First_Position, Last_Position));
   begin
      return Location;
   end Get_Node_Location;

   -----------------------
   -- Exit_Notification --
   -----------------------

   overriding procedure On_Exit_Notification (Self : access Message_Handler) is
   begin
      Self.Server.Stop;
   end On_Exit_Notification;

   ------------------------
   -- Initialize_Request --
   ------------------------

   overriding function On_Initialize_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.InitializeParams)
      return LSP.Messages.Initialize_Response
   is
      Response : LSP.Messages.Initialize_Response (Is_Error => False);
      Root     : LSP.Types.LSP_String;
   begin
      Response.result.capabilities.definitionProvider := True;
      Response.result.capabilities.typeDefinitionProvider := True;
      Response.result.capabilities.referencesProvider := True;
      Response.result.capabilities.documentSymbolProvider := True;
      Response.result.capabilities.textDocumentSync :=
        (Is_Set => True, Is_Number => True, Value => LSP.Messages.Full);
      Response.result.capabilities.completionProvider :=
        (True,
         (resolveProvider => (True, False),
          triggerCharacters => Empty_Vector & To_LSP_String (".")));
      Response.result.capabilities.hoverProvider := True;

      Response.result.capabilities.alsCalledByProvider := True;

      if not LSP.Types.Is_Empty (Value.rootUri) then
         Root := Self.Context.URI_To_File (Value.rootUri);
      else
         --  URI isn't provided, rollback to deprecated rootPath
         Root := Value.rootPath;
      end if;

      --  Some clients - notably VS Code as of version 33, when opening a file
      --  rather than a workspace - don't provide a root at all. In that case
      --  use the current directory as root.

      if LSP.Types.Is_Empty (Root) then
         Root := To_LSP_String (".");
      end if;

      --  Log the context root
      Server_Trace.Trace ("Context root: " & To_UTF_8_String (Root));

      Self.Context.Initialize (Root);

      return Response;
   end On_Initialize_Request;

   -------------------------
   -- On_Shutdown_Request --
   -------------------------

   overriding function On_Shutdown_Request
     (Self  : access Message_Handler)
      return LSP.Messages.ResponseMessage
   is
      pragma Unreferenced (Self);
   begin
      return Response : LSP.Messages.ResponseMessage (Is_Error => False);
   end On_Shutdown_Request;

   ---------------------------
   -- On_CodeAction_Request --
   ---------------------------

   overriding function On_CodeAction_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.CodeActionParams)
      return LSP.Messages.CodeAction_Response
   is
      pragma Unreferenced (Self, Value);
      Response : LSP.Messages.CodeAction_Response (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Messages.InternalError,
          message => To_LSP_String ("Not implemented"),
          data => <>));
      return Response;
   end On_CodeAction_Request;

   ----------------------------
   -- On_ShowMessage_Request --
   ----------------------------

   overriding function On_ShowMessage_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.ShowMessageRequestParams)
      return LSP.Messages.ResponseMessage
   is
      pragma Unreferenced (Self, Value);
      Response : LSP.Messages.ResponseMessage (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Messages.InternalError,
          message => To_LSP_String ("Not implemented"),
          data => <>));
      return Response;
   end On_ShowMessage_Request;

   -----------------------------------
   -- On_ApplyWorkspaceEdit_Request --
   -----------------------------------

   overriding function On_ApplyWorkspaceEdit_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.ApplyWorkspaceEditParams)
      return LSP.Messages.ResponseMessage
   is
      pragma Unreferenced (Self, Value);
      Response : LSP.Messages.ResponseMessage (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Messages.InternalError,
          message => To_LSP_String ("Not implemented"),
          data => <>));
      return Response;
   end On_ApplyWorkspaceEdit_Request;

   --------------------------------
   -- On_Execute_Command_Request --
   --------------------------------

   overriding function On_Execute_Command_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.ExecuteCommandParams)
      return LSP.Messages.ExecuteCommand_Response
   is
      pragma Unreferenced (Self, Value);
      Response : LSP.Messages.ExecuteCommand_Response (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Messages.InternalError,
          message => To_LSP_String ("Not implemented"),
          data => <>));
      return Response;
   end On_Execute_Command_Request;

   ---------------------------
   -- On_Definition_Request --
   ---------------------------

   overriding function On_Definition_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Location_Response
   is
      use Libadalang.Analysis;

      procedure Append_Location
        (Result     : in out LSP.Messages.Location_Vector;
         Definition : Defining_Name);
      --  Append given defining name location to the result

      function Find_Next_Part
        (Definition : Defining_Name) return Defining_Name;
      --  Find defining name of a completion if any

      function Find_First_Part
        (Definition : Defining_Name) return Defining_Name;
      --  Find defining name of a declaration for given completion

      ---------------------
      -- Append_Location --
      ---------------------

      procedure Append_Location
        (Result     : in out LSP.Messages.Location_Vector;
         Definition : Defining_Name)
      is
         Location : constant LSP.Messages.Location :=
                      Get_Node_Location
                        (Self => Self,
                         Node => As_Ada_Node (Definition));
      begin
         Result.Append (Location);
      end Append_Location;

      ---------------------
      -- Find_First_Part --
      ---------------------

      function Find_First_Part
        (Definition : Defining_Name) return Defining_Name
      is
         Next : Defining_Name := Definition;
         Prev : Defining_Name;
      begin
         --  Iterate over Next.P_Previous_Part names until no name found
         loop
            begin
               Prev := Next.P_Previous_Part;

               exit when Prev in No_Defining_Name | Next;

               Next := Prev;
            exception
               when Libadalang.Common.Property_Error =>
                  exit;
            end;
         end loop;

         if Next = Definition then
            return No_Defining_Name;
         else
            return Next;
         end if;
      end Find_First_Part;

      --------------------
      -- Find_Next_Part --
      --------------------

      function Find_Next_Part
        (Definition : Defining_Name) return Defining_Name
      is
         Next : Defining_Name;
      begin
         Next := Definition.P_Next_Part;

         if Next = Definition then
            return No_Defining_Name;
         else
            return Next;
         end if;
      exception
         when Libadalang.Common.Property_Error =>
            return No_Defining_Name;
      end Find_Next_Part;

      Document   : constant LSP.Ada_Documents.Document_Access :=
        Self.Context.Get_Document (Value.textDocument.uri);

      Name_Node : constant Name := LSP.Lal_Utils.Get_Node_As_Name
        (Document.Get_Node_At (Value.position));

      Definition : Defining_Name;
      Other_Part : Defining_Name;
      Response   : LSP.Messages.Location_Response (Is_Error => False);

   begin

      if Name_Node = No_Name then
         return Response;
      end if;

      --  Check is we are on some defining name
      Definition := LSP.Lal_Utils.Get_Name_As_Defining (Name_Node);

      if Definition = No_Defining_Name then
         Definition := LSP.Lal_Utils.Resolve_Name (Name_Node);

         if Definition /= No_Defining_Name then
            Append_Location (Response.result, Definition);
         end if;
      else  --  If we are on a defining_name already
         Other_Part := Find_Next_Part (Definition);

         if Other_Part = No_Defining_Name then
            --  No next part is found. Check first defining name
            Other_Part := Find_First_Part (Definition);
         end if;

         if Other_Part /= No_Defining_Name then
            Append_Location (Response.result, Other_Part);
         end if;
      end if;

      return Response;
   end On_Definition_Request;

   --------------------------------
   -- On_Type_Definition_Request --
   --------------------------------

   overriding function On_Type_Definition_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Location_Response
   is
      use Libadalang.Analysis;
      use Libadalang.Common;

      Response  : LSP.Messages.Location_Response (Is_Error => False);
      Document  : constant LSP.Ada_Documents.Document_Access :=
                    Self.Context.Get_Document (Value.textDocument.uri);
      Name_Node : constant Name :=
                    LSP.Lal_Utils.Get_Node_As_Name
                      (Document.Get_Node_At (Value.position));
      Type_Decl : Base_Type_Decl;
      Location  : LSP.Messages.Location;
   begin
      if Name_Node = No_Name then
         return Response;
      end if;

      Type_Decl := Name_Node.P_Expression_Type;

      if Type_Decl = No_Basic_Decl then
         return Response;
      end if;

      --  TODO: for anonymous access type declarations we should go to the
      --  pointed type instead.
      --  A private API already exists in LAL for that: waiting for it to be
      --  public.

      if Type_Decl.Kind in Ada_Anonymous_Type_Decl then
         Location := Get_Node_Location
           (Self => Self,
            Node => As_Ada_Node (Type_Decl));
      else
         Location := Get_Node_Location
           (Self => Self,
            Node => As_Ada_Node (Type_Decl));
      end if;

      Response.result.Append (Location);

      return Response;
   end On_Type_Definition_Request;

   -------------------------------------------
   -- On_DidChangeTextDocument_Notification --
   -------------------------------------------

   overriding procedure On_DidChangeTextDocument_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidChangeTextDocumentParams)
   is
      Document : constant LSP.Ada_Documents.Document_Access :=
        Self.Context.Get_Document (Value.textDocument.uri);
      Diag     : LSP.Messages.PublishDiagnosticsParams;
   begin
      Document.Apply_Changes (Value.contentChanges);

      if Self.Context.Get_Diagnostics_Enabled then
         Document.Get_Errors (Diag.diagnostics);

         Diag.uri := Value.textDocument.uri;
         Self.Server.Publish_Diagnostics (Diag);
      end if;
   end On_DidChangeTextDocument_Notification;

   ------------------------------------------
   -- On_DidCloseTextDocument_Notification --
   ------------------------------------------

   overriding procedure On_DidCloseTextDocument_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidCloseTextDocumentParams)
   is
      Diag : LSP.Messages.PublishDiagnosticsParams;
   begin
      Self.Context.Unload_Document (Value.textDocument);

      --  Clean diagnostics up on closing document
      Diag.uri := Value.textDocument.uri;
      Self.Server.Publish_Diagnostics (Diag);
   end On_DidCloseTextDocument_Notification;

   -----------------------------------------
   -- On_DidOpenTextDocument_Notification --
   -----------------------------------------

   overriding procedure On_DidOpenTextDocument_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidOpenTextDocumentParams)
   is
      Errors   : LSP.Messages.ShowMessageParams;
      Diag     : LSP.Messages.PublishDiagnosticsParams;
      Document : LSP.Ada_Documents.Document_Access;
   begin
      GNATCOLL.Traces.Trace (Server_Trace, "In Text_Document_Did_Open");
      GNATCOLL.Traces.Trace
        (Server_Trace, "Uri : " & To_UTF_8_String (Value.textDocument.uri));

      --  Some clients don't properly call initialize, in which case we want to
      --  call it anyway at the first open file request.

      if not Self.Context.Is_Initialized then
         GNATCOLL.Traces.Trace
           (Server_Trace, "No project loaded, creating default one ...");

         declare
            Root : LSP.Types.LSP_String :=
              Self.Context.URI_To_File (Value.textDocument.uri);
         begin
            Root := To_LSP_String
              (Ada.Directories.Containing_Directory (To_UTF_8_String (Root)));

            GNATCOLL.Traces.Trace
              (Server_Trace, "Root : " & To_UTF_8_String (Root));

            Self.Context.Initialize (Root);

         end;
      end if;

      if not Self.Context.Has_Project then
         Self.Context.Load_Project
           (Empty_LSP_String, GNATCOLL.JSON.JSON_Null,

            --  We're loading a default project: set the default charset
            --  to latin-1, since this is the GNAT default.
            "iso-8859-1",

            Errors);

         if not LSP.Types.Is_Empty (Errors.message) then
            Self.Server.Show_Message (Errors);
         end if;
      end if;

      Document := Self.Context.Load_Document (Value.textDocument);

      if Self.Context.Get_Diagnostics_Enabled then
         Document.Get_Errors (Diag.diagnostics);

         Diag.uri := Value.textDocument.uri;
         Self.Server.Publish_Diagnostics (Diag);
      end if;
   end On_DidOpenTextDocument_Notification;

   --------------------------
   -- On_Highlight_Request --
   --------------------------

   overriding function On_Highlight_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Highlight_Response
   is
      pragma Unreferenced (Self, Value);
      Response : LSP.Messages.Highlight_Response (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Messages.InternalError,
          message => To_LSP_String ("Not implemented"),
          data => <>));
      return Response;
   end On_Highlight_Request;

   ----------------------
   -- On_Hover_Request --
   ----------------------

   overriding function On_Hover_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Hover_Response
   is
      use Libadalang.Analysis;
      use Libadalang.Common;

      Response           : LSP.Messages.Hover_Response (Is_Error => False);
      Document           : constant LSP.Ada_Documents.Document_Access :=
                             Self.Context.Get_Document
                               (Value.textDocument.uri);
      Name_Node          : constant Name :=
                             LSP.Lal_Utils.Get_Node_As_Name
                               (Document.Get_Node_At (Value.position));
      Defining_Name_Node : Defining_Name;
      Decl               : Basic_Decl;
      Subp_Spec_Node     : Base_Subp_Spec;
      Decl_Text          : LSP_String;
      Comments_Text      : LSP_String;

      procedure Create_Decl_Text_For_Basic_Decl;
      --  Create the hover text for for basic declarations

      procedure Create_Decl_Text_For_Subp_Spec;
      --  Create the hover text for for subprogram declarations

      --------------------------------------
      -- Create_Decl_Text_For_Basic_Decl --
      --------------------------------------

      procedure Create_Decl_Text_For_Basic_Decl is
      begin
         case Decl.Kind is
            when Ada_Package_Body =>

               --  This means that user is hovering on the package declaration
               --  itself: in this case, return a empty response since all the
               --  relevant information is already visible to the user.
               return;

            when Ada_Base_Package_Decl =>

               --  Return the first line of the package declaration and its
               --  generic parameters if any.
               declare
                  Text           : constant String :=
                                     Langkit_Support.Text.To_UTF8  (Decl.Text);
                  Generic_Params : LSP_String;
                  End_Idx        : Natural := Text'First;
               begin
                  Skip_To_String
                    (Str       => Text,
                     Index     => End_Idx,
                     Substring => " is");

                  if Decl.Parent /= No_Ada_Node
                    and then Decl.Parent.Kind in Ada_Generic_Decl
                  then
                     Generic_Params := To_LSP_String
                       (Langkit_Support.Text.To_UTF8
                          (As_Generic_Decl (Decl.Parent).F_Formal_Part.Text)
                        & ASCII.LF);
                  end if;

                  Decl_Text := Generic_Params
                    & To_LSP_String (Text (Text'First .. End_Idx));
                  return;
               end;

            when Ada_For_Loop_Var_Decl =>

               --  Return the first line of the enclosing for loop when
               --  hovering a for loop variable declaration.
               declare
                  Parent_Text : constant String := Langkit_Support.Text.To_UTF8
                    (Decl.P_Semantic_Parent.Text);
                  End_Idx     : Natural := Parent_Text'First;
               begin
                  Skip_To_String
                    (Str       => Parent_Text,
                     Index     => End_Idx,
                     Substring => "loop");

                  Decl_Text := To_LSP_String
                    (Parent_Text (Parent_Text'First .. End_Idx + 4));
                  return;
               end;

            when others =>
               declare
                  Text        : constant String := Langkit_Support.Text.To_UTF8
                    (Decl.Text);
                  Lines       : GNAT.Strings.String_List_Access := Split
                    (Text,
                     On               => ASCII.LF,
                     Omit_Empty_Lines => True);
                  Idx         : Integer;
               begin
                  --  Return an empty hover text if there is no text for this
                  --  delclaration (only for safety).
                  if Text = "" then
                     return;
                  end if;

                  --  If it's a single-line declaration, replace all the
                  --  series of whitespaces by only one blankspace. If it's
                  --  a multi-line declaration, remove only the unneeded
                  --  indentation whitespaces.

                  if Lines'Length = 1 then
                     declare
                        Res_Idx : Integer := Text'First;
                        Result  : String (Text'First .. Text'Last);
                     begin
                        Idx := Text'First;

                        while Idx <= Text'Last loop
                           Skip_Blanks (Text, Idx);

                           while Idx <= Text'Last
                             and then not Is_Whitespace (Text (Idx))
                           loop
                              Result (Res_Idx) := Text (Idx);
                              Idx := Idx + 1;
                              Res_Idx := Res_Idx + 1;
                           end loop;

                           if Res_Idx < Result'Last then
                              Result (Res_Idx) := ' ';
                              Res_Idx := Res_Idx + 1;
                           end if;
                        end loop;

                        if Res_Idx > Text'First then
                           Decl_Text := To_LSP_String
                             (Result (Text'First .. Res_Idx - 1));
                        end if;
                     end;
                  else
                     declare
                        Blanks_Count_Per_Line : array
                          (Lines'First + 1 .. Lines'Last) of Natural;
                        Indent_Blanks_Count   : Natural := Natural'Last;
                        Start_Idx             : Integer;
                     begin
                        Decl_Text := To_LSP_String (Lines (Lines'First).all);

                        --  Count the blankpaces per line and track how many
                        --  blankspaces we should remove on each line by
                        --  finding the common identation blankspaces.

                        for J in Lines'First + 1 .. Lines'Last loop
                           Idx := Lines (J)'First;
                           Skip_Blanks (Lines (J).all, Idx);

                           Blanks_Count_Per_Line (J) := Idx - Lines (J)'First;
                           Indent_Blanks_Count := Natural'Min
                             (Indent_Blanks_Count,
                              Blanks_Count_Per_Line (J));
                        end loop;

                        for J in Lines'First + 1 .. Lines'Last loop
                           Start_Idx := Lines (J)'First + Indent_Blanks_Count;
                           Decl_Text := Decl_Text & To_LSP_String
                             (ASCII.LF
                              & Lines (J).all (Start_Idx .. Lines (J)'Last));
                        end loop;
                     end;
                  end if;

                  GNAT.Strings.Free (Lines);
               end;
         end case;
      end Create_Decl_Text_For_Basic_Decl;

      -------------------------------------
      -- Create_Decl_Text_For_Subp_Spec --
      -------------------------------------

      procedure Create_Decl_Text_For_Subp_Spec is
         Text  : constant String := Langkit_Support.Text.To_UTF8
           (Subp_Spec_Node.Text);
         Lines : GNAT.Strings.String_List_Access := Split
           (Text,
            On               => ASCII.LF,
            Omit_Empty_Lines => True);
         Idx   : Integer;
      begin
         --  For single-line subprogram specifications, we display the
         --  associated text directly.
         --  For multi-line ones, remove the identation blankspaces to replace
         --  them by a fixed number of blankspaces.

         if Lines'Length = 1 then
            Decl_Text := To_LSP_String (Text);
         else
            Decl_Text := To_LSP_String (Lines (Lines'First).all);

            for J in Lines'First + 1 .. Lines'Last loop
               Idx := Lines (J)'First;
               Skip_Blanks (Lines (J).all, Idx);

               Decl_Text := Decl_Text
                 & To_LSP_String
                 (ASCII.LF
                  & (if Lines (J).all (Idx) = '(' then "  " else "   ")
                  & Lines (J).all (Idx .. Lines (J).all'Last));
            end loop;
         end if;

         GNAT.Strings.Free (Lines);
      end Create_Decl_Text_For_Subp_Spec;

   begin
      if Name_Node = No_Name then
         return Response;
      end if;

      --  Get the defining node of the node being hovered
      Defining_Name_Node := LSP.Lal_Utils.Resolve_Name
        (Name_Node => Name_Node);

      if Defining_Name_Node = No_Defining_Name then
         return Response;
      end if;

      --  Get the associated basic declaration
      Decl := Defining_Name_Node.P_Basic_Decl;

      if Decl = No_Basic_Decl then
         return Response;
      end if;

      --  If the basic declaration is an enum literal, display the whole
      --  enumeration type declaration instead.
      if Decl.Kind in Ada_Enum_Literal_Decl then
         Decl := As_Enum_Literal_Decl (Decl).P_Enum_Type.As_Basic_Decl;
         Create_Decl_Text_For_Basic_Decl;
      else

         --  Try to retrieve the subprogram spec node, if any: if it's a
         --  subprogram node that does not have any separate declaration we
         --  only want to display its specification, not the body.
         Subp_Spec_Node := Decl.P_Subp_Spec_Or_Null;

         if Subp_Spec_Node /= No_Base_Subp_Spec then
            Create_Decl_Text_For_Subp_Spec;
         else
            Create_Decl_Text_For_Basic_Decl;
         end if;
      end if;

      --  Append the associated basic declaration's text to the response.
      --  We set the language for highlighting.
      if Decl_Text /= Empty_LSP_String then
         Response.result.contents.Append
           (LSP.Messages.MarkedString'
              (Is_String => False,
               value     => Decl_Text,
               language  => To_LSP_String ("ada")));

         Comments_Text := To_LSP_String
           (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode
              (Libadalang.Doc_Utils.Get_Documentation
                   (Decl).Doc.To_String));

         --  Append the comments associated with the basic declaration
         --  if any.

         if Comments_Text /= Empty_LSP_String then
            Response.result.contents.Append
              (LSP.Messages.MarkedString'
                 (Is_String => True,
                  value     => Comments_Text));
         end if;
      end if;

      return Response;
   end On_Hover_Request;

   ---------------------------
   -- On_References_Request --
   ---------------------------

   overriding function On_References_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.ReferenceParams)
      return LSP.Messages.Location_Response
   is
      use Libadalang.Analysis;

      Document   : constant LSP.Ada_Documents.Document_Access :=
        Self.Context.Get_Document (Value.textDocument.uri);

      Name_Node : constant Name := LSP.Lal_Utils.Get_Node_As_Name
        (Document.Get_Node_At (Value.position));

      Definition : Defining_Name;
      Response   : LSP.Messages.Location_Response (Is_Error => False);

   begin
      if Name_Node = No_Name then
         return Response;
      end if;

      Definition := LSP.Lal_Utils.Resolve_Name (Name_Node);

      if Definition = No_Defining_Name then
         return Response;
      end if;

      declare
         Ada_Sources : constant File_Array_Access :=
           Self.Context.Get_Ada_Source_Files;

         References  : constant Ada_Node_Array :=
           LSP.Lal_Utils.Find_All_References
             (Definition         => Definition,
              Sources            => Ada_Sources,
              Charset            => Self.Context.Get_Charset,
              Include_Definition => Value.context.includeDeclaration);
      begin
         for Node of References loop
            declare
               Location : constant LSP.Messages.Location :=
                  Get_Node_Location
                     (Self => Self,
                      Node => As_Ada_Node (Node));
            begin
               Response.result.Append (Location);
            end;
         end loop;

         return Response;
      end;
   end On_References_Request;

   ------------------------------
   -- On_ALS_Called_By_Request --
   ------------------------------

   overriding function On_ALS_Called_By_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.ALS_Called_By_Response
   is
      use Libadalang.Analysis;

      Document   : constant LSP.Ada_Documents.Document_Access :=
        Self.Context.Get_Document (Value.textDocument.uri);

      Name_Node : constant Name := LSP.Lal_Utils.Get_Node_As_Name
        (Document.Get_Node_At (Value.position));

      Definition : Defining_Name;
      Response   : LSP.Messages.ALS_Called_By_Response (Is_Error => False);

   begin
      if Name_Node = No_Name then
         return Response;
      end if;

      Definition := LSP.Lal_Utils.Resolve_Name (Name_Node);

      if Definition = No_Defining_Name then
         return Response;
      end if;

      declare
         Ada_Sources : constant File_Array_Access :=
           Self.Context.Get_Ada_Source_Files;

         Called      : constant LSP.Lal_Utils.References_By_Subprogram.Map :=
           LSP.Lal_Utils.Is_Called_By
             (Name_Node,
              Sources            => Ada_Sources,
              Charset            => Self.Context.Get_Charset);

         use LSP.Lal_Utils.References_By_Subprogram;
         C : Cursor := Called.First;
      begin
         --  Iterate through all the results, converting them to protocol
         --  objects.
         while Has_Element (C) loop
            declare
               Node : constant Defining_Name := Key (C);
               Refs : constant LSP.Lal_Utils.References_List.List :=
                 Element (C);
               Subp_And_Refs : LSP.Messages.ALS_Subprogram_And_References;
            begin
               Subp_And_Refs.loc := Get_Node_Location (Self, Ada_Node (Node));
               Subp_And_Refs.name := To_LSP_String
                 (Langkit_Support.Text.To_UTF8 (Node.Text));
               for Ref of Refs loop
                  Subp_And_Refs.refs.Append
                    (Get_Node_Location (Self, Ada_Node (Ref)));
               end loop;
               Response.result.Append (Subp_And_Refs);
            end;
            Next (C);
         end loop;
      end;

      return Response;
   end On_ALS_Called_By_Request;

   -------------------------------
   -- On_Signature_Help_Request --
   -------------------------------

   overriding function On_Signature_Help_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.SignatureHelp_Response
   is
      pragma Unreferenced (Self, Value);
      Response : LSP.Messages.SignatureHelp_Response (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Messages.InternalError,
          message => To_LSP_String ("Not implemented"),
          data => <>));
      return Response;
   end On_Signature_Help_Request;

   ---------------------------------
   -- On_Document_Symbols_Request --
   ---------------------------------

   overriding function On_Document_Symbols_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.DocumentSymbolParams)
      return LSP.Messages.Symbol_Response
   is
      Document : constant LSP.Ada_Documents.Document_Access :=
        Self.Context.Get_Document (Value.textDocument.uri);
      Response : LSP.Messages.Symbol_Response (Is_Error => False);
   begin
      Document.Get_Symbols (Response.result);
      return Response;
   end On_Document_Symbols_Request;

   --------------------------------------------
   -- On_DidChangeConfiguration_Notification --
   --------------------------------------------

   overriding procedure On_DidChangeConfiguration_Notification
     (Self     : access Message_Handler;
      Value    : LSP.Messages.DidChangeConfigurationParams)
   is
      use type GNATCOLL.JSON.JSON_Value_Type;

      projectFile       : constant String := "projectFile";
      scenarioVariables : constant String := "scenarioVariables";
      defaultCharset    : constant String := "defaultCharset";
      enableDiagnostics : constant String := "enableDiagnostics";

      --  Default the charset to iso-8859-1, since this is the GNAT default
      Charset   : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String ("iso-8859-1");

      Ada       : constant LSP.Types.LSP_Any := Value.settings.Get ("ada");
      File      : LSP.Types.LSP_String;
      Variables : LSP.Types.LSP_Any;
      Errors    : LSP.Messages.ShowMessageParams;
   begin
      if Ada.Kind = GNATCOLL.JSON.JSON_Object_Type then
         if Ada.Has_Field (projectFile) then
            File := +Ada.Get (projectFile).Get;

            --  Drop uri scheme if present
            if LSP.Types.Starts_With (File, "file:") then
               File := Self.Context.URI_To_File (File);
            end if;
         end if;

         if Ada.Has_Field (scenarioVariables) and then
           Ada.Get (scenarioVariables).Kind  = GNATCOLL.JSON.JSON_Object_Type
         then
            Variables := Ada.Get (scenarioVariables);
         end if;

         if Ada.Has_Field (defaultCharset) then
            Charset := Ada.Get (defaultCharset);
         end if;

         --  It looks like the protocol does not allow clients to say whether
         --  or not they want diagnostics as part of
         --  InitializeParams.capabilities.textDocument. So we support
         --  deactivating of diagnostics via a setting here.
         if Ada.Has_Field (enableDiagnostics) then
            Self.Context.Set_Diagnostics_Enabled
              (Ada.Get (enableDiagnostics));
         end if;
      end if;

      Self.Context.Load_Project
        (File, Variables,
         Standard.Ada.Strings.Unbounded.To_String (Charset),
         Errors);

      if not LSP.Types.Is_Empty (Errors.message) then
         Self.Server.Show_Message (Errors);
      end if;
   end On_DidChangeConfiguration_Notification;

   ------------------------------------------
   -- On_Workspace_Execute_Command_Request --
   ------------------------------------------

   overriding function On_Workspace_Execute_Command_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.ExecuteCommandParams)
      return LSP.Messages.ExecuteCommand_Response
   is
      pragma Unreferenced (Self, Value);
      Response : LSP.Messages.ExecuteCommand_Response (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Messages.InternalError,
          message => To_LSP_String ("Not implemented"),
          data => <>));
      return Response;
   end On_Workspace_Execute_Command_Request;

   ----------------------------------
   -- On_Workspace_Symbols_Request --
   ----------------------------------

   overriding function On_Workspace_Symbols_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.WorkspaceSymbolParams)
      return LSP.Messages.Symbol_Response
   is
      pragma Unreferenced (Self, Value);
      Response : LSP.Messages.Symbol_Response (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Messages.InternalError,
          message => To_LSP_String ("Not implemented"),
          data => <>));
      return Response;
   end On_Workspace_Symbols_Request;

   ---------------------------
   -- On_Completion_Request --
   ---------------------------

   overriding function On_Completion_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Completion_Response
   is
      Document : constant LSP.Ada_Documents.Document_Access :=
        Self.Context.Get_Document (Value.textDocument.uri);
      Response : LSP.Messages.Completion_Response (Is_Error => False);
   begin
      Document.Get_Completions_At (Value.position, Response.result);
      return Response;
   end On_Completion_Request;

   ------------------
   -- Handle_Error --
   ------------------

   overriding procedure Handle_Error
     (Self : access Message_Handler) is
   begin
      --  Reload the context in case of unexpected errors.
      Self.Context.Reload;
   end Handle_Error;

end LSP.Ada_Handlers;

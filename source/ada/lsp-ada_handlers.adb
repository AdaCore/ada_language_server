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

with LSP.Types; use LSP.Types;

with LSP.Ada_Documents;
with LSP.Lal_Utils;
with LSP.Ada_Contexts; use LSP.Ada_Contexts;
with LSP.Messages.Server_Notifications;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Doc_Utils;

with URIs;

package body LSP.Ada_Handlers is

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
                 return LSP.Types.LSP_String renames
     LSP.Types.To_LSP_String;

   function Get_Node_Location
     (Node : Libadalang.Analysis.Ada_Node;
      Kind : LSP.Messages.AlsReferenceKind_Set := LSP.Messages.Empty_Set)
      return LSP.Messages.Location;
   --  Return the location of the given node. Populate alsKind field of the
   --  result with given Kind.

   procedure Send_Imprecise_Xref_Message
     (Self     : access Message_Handler;
      URI      : LSP.Messages.DocumentUri;
      Position : LSP.Messages.Position;
      Msg_Type : LSP.Messages.MessageType);
   --  Send a message of the given Msg_Type to the LSP client to warn the user
   --  of a possible imprecise result while computing xrefs on the given
   --  node.

   procedure Imprecise_Resolve_Name
     (Self       : access Message_Handler;
      In_Context : Context_Access;
      Position   : LSP.Messages.TextDocumentPositionParams'Class;
      Definition : out Libadalang.Analysis.Defining_Name;
      Msg_Type   : LSP.Messages.MessageType := LSP.Messages.Log);
   --  If node at given Position is a name, then resolve it.
   --  Send a message in case of a possible imprecise result.
   --  See description of Msg_Type in Send_Imprecise_Xref_Message comments.

   procedure Append_Location
     (Result : in out LSP.Messages.Location_Vector;
      Node   : Libadalang.Analysis.Ada_Node'Class;
      Kind   : LSP.Messages.AlsReferenceKind_Set := LSP.Messages.Empty_Set);
   --  Append given Node location to the Result.
   --  Do nothing if the item inside of an synthetic file (like __standard).
   --  Do not append if the location is already in Result.
   --  See description of Kind in Get_Node_Location comments.

   function To_File (URI : LSP.Messages.DocumentUri) return Virtual_File is
     (Create (+(URIs.Conversions.To_File (To_UTF_8_String (URI)))));
   --  Utility conversion function

   procedure Ensure_Context_Initialized
     (Self : access Message_Handler;
      Root : LSP.Types.LSP_String);
   --  This function makes sure that the contexts in Self are properly
   --  initialized, and, if they are not initialized, initialize them.
   --  This initializes two contexts in Self.Context, in this order:
   --   - a "project" context using Root as root directory
   --   - a "projectless" context

   ---------------------------
   -- Multi-context support --
   ---------------------------

   --  A set of ucilities for dealing with multiple contexts

   function Get_Best_Context_For_File
     (Contexts : Context_Lists.List;
      File     : Virtual_File) return Context_Access;
   --  Return the first context in Contexts which contains a project
   --  which knows about file.
   --  ??? Nest this into Get_Best_Context_For_URI or is it used
   --  somewhere else?

   function Get_Best_Context_For_URI
     (Contexts : Context_Lists.List;
      URI      : LSP.Messages.DocumentUri) return Context_Access;
   --  Return the Context most suitable to use for this URI: if a context
   --  contains an opened document for this URI, then return it, otherwise
   --  return the first context where the project tree knows about this file.
   --  defaulting on the last context in Contexts.

   function Get_Document
     (Contexts : Context_Lists.List;
      URI      : LSP.Messages.DocumentUri)
      return LSP.Ada_Documents.Document_Access;
   --  Return the document for URI in the first context in which it is open,
   --  null if it isn't open.

   -------------------------------
   -- Get_Best_Context_For_File --
   -------------------------------

   function Get_Best_Context_For_File
     (Contexts : Context_Lists.List;
      File     : Virtual_File) return Context_Access is
   begin
      for Context of Contexts loop
         if Context.Is_Part_Of_Project (File) then
            return Context;
         end if;
      end loop;

      return Contexts.Last_Element;
   end Get_Best_Context_For_File;

   ------------------------------
   -- Get_Best_Context_For_URI --
   ------------------------------

   function Get_Best_Context_For_URI
     (Contexts : Context_Lists.List;
      URI      : LSP.Messages.DocumentUri) return Context_Access is
   begin
      for Context of Contexts loop
         if Context.Has_Document (URI) then
            return Context;
         end if;
      end loop;

      return Get_Best_Context_For_File (Contexts, To_File (URI));
   end Get_Best_Context_For_URI;

   ------------------
   -- Get_Document --
   ------------------

   function Get_Document
     (Contexts : Context_Lists.List;
      URI      : LSP.Messages.DocumentUri)
      return LSP.Ada_Documents.Document_Access
   is
      C : constant Context_Access :=
        Get_Best_Context_For_URI (Contexts, URI);
   begin
      return C.Get_Document (URI);
   end Get_Document;

   ---------------------
   -- Append_Location --
   ---------------------

   procedure Append_Location
     (Result : in out LSP.Messages.Location_Vector;
      Node   : Libadalang.Analysis.Ada_Node'Class;
      Kind   : LSP.Messages.AlsReferenceKind_Set := LSP.Messages.Empty_Set)
   is
      function Is_Synthetic return Boolean;
      --  Check if Node is in a synthetic file (like "__standard").
      --  TODO: Replace this with LAL property as it will be available.

      ------------------
      -- Is_Synthetic --
      ------------------

      function Is_Synthetic return Boolean is
         Std  : constant String := "__standard";
         File : constant String := Node.Unit.Get_Filename;
      begin
         return File'Length >= Std'Length
           and then File (File'Last - Std'Length + 1 .. File'Last) = Std;
      end Is_Synthetic;

      Location : constant LSP.Messages.Location :=
        Get_Node_Location (Libadalang.Analysis.As_Ada_Node (Node), Kind);
   begin
      if not Is_Synthetic
        and then not Result.Contains (Location)
      then
         Result.Append (Location);
      end if;
   end Append_Location;

   -----------------------
   -- Get_Node_Location --
   -----------------------

   function Get_Node_Location
     (Node : Libadalang.Analysis.Ada_Node;
      Kind : LSP.Messages.AlsReferenceKind_Set := LSP.Messages.Empty_Set)
      return LSP.Messages.Location
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
                   (uri     => File_To_URI (+Node.Unit.Get_Filename),
                    span    =>
                      LSP.Messages.Span'(First_Position, Last_Position),
                    alsKind => Kind);

   begin
      return Location;
   end Get_Node_Location;

   ---------------------------------
   -- Send_Imprecise_Xref_Message --
   ---------------------------------

   procedure Send_Imprecise_Xref_Message
     (Self     : access Message_Handler;
      URI      : LSP.Messages.DocumentUri;
      Position : LSP.Messages.Position;
      Msg_Type : LSP.Messages.MessageType)
   is
      File : constant GNATCOLL.VFS.Virtual_File := To_File (URI);
   begin
      Self.Server.On_Show_Message
        ((Msg_Type,
         "Imprecise fallback used to compute cross-references on entity at:"
         & To_LSP_String
           (ASCII.LF & "   " & File.Display_Base_Name)
         & To_LSP_String
           (ASCII.LF & "   line:" & Position.line'Img)
         & To_LSP_String
           (ASCII.LF & "   column:" & Position.character'Img)));
   end Send_Imprecise_Xref_Message;

   ----------------------------
   -- Imprecise_Resolve_Name --
   ----------------------------

   procedure Imprecise_Resolve_Name
     (Self       : access Message_Handler;
      In_Context : Context_Access;
      Position   : LSP.Messages.TextDocumentPositionParams'Class;
      Definition : out Libadalang.Analysis.Defining_Name;
      Msg_Type   : LSP.Messages.MessageType := LSP.Messages.Log)
   is
      use type Libadalang.Analysis.Name;

      Name_Node : constant Libadalang.Analysis.Name :=
        LSP.Lal_Utils.Get_Node_As_Name (In_Context.Get_Node_At (Position));

      Imprecise  : Boolean;
   begin
      if Name_Node = Libadalang.Analysis.No_Name then
         return;
      end if;

      Definition := LSP.Lal_Utils.Resolve_Name
        (Name_Node,
         Imprecise => Imprecise);

      --  If we used the imprecise fallback to get to the definition, log it
      if Imprecise then
         Self.Send_Imprecise_Xref_Message
           (URI      => Position.textDocument.uri,
            Position => Position.position,
            Msg_Type => Msg_Type);
      end if;
   end Imprecise_Resolve_Name;

   -----------------------
   -- Exit_Notification --
   -----------------------

   overriding procedure On_Exit_Notification (Self : access Message_Handler) is
   begin
      Self.Server.Stop;
   end On_Exit_Notification;

   --------------------------------
   -- Ensure_Context_Initialized --
   --------------------------------

   procedure Ensure_Context_Initialized
     (Self : access Message_Handler;
      Root : LSP.Types.LSP_String)
   is
      C      : constant Context_Access := new Context (Self.Trace);
      Errors : LSP.Messages.ShowMessageParams;
   begin
      if Integer (Self.Contexts.Length) >= 2 then
         --  Rely on the fact that there are at least two contexts initialized
         --  as a guarantee that the initialization has been done.
         return;
      end if;

      if Integer (Self.Contexts.Length) = 1 then
         --  We should never have only one context, there should always
         --  be at least two contexts.
         raise Program_Error with "inconsistent context initialization";
      end if;

      Self.Trace.Trace ("No project loaded, creating default context ...");
      Self.Trace.Trace ("Root : " & To_UTF_8_String (Root));
      C.Initialize (Root);
      C.Load_Project
        (Empty_LSP_String, GNATCOLL.JSON.JSON_Null,

         --  We're loading a default project: set the default charset
         --  to latin-1, since this is the GNAT default.
         "iso-8859-1",
         Errors);

      if not LSP.Types.Is_Empty (Errors.message) then
         --  We might have encountered errors when loading the project, for
         --  instance for projects which need scenario variables to be set...
         --  but we should not report them to the user at this stage:
         --    - for project-aware clients, the expectation is that they
         --      will send the project information as part of the
         --      didChangeConfiguration notification
         --    - for clients that are not project aware, the burden is on them
         --      to indicate a directory where there is a single loadable
         --      project
         --  We change the error type to Log so that these get recorded
         --  in the log file.
         Errors.the_type := LSP.Messages.Log;
         Self.Server.On_Show_Message (Errors);
      end if;

      Self.Contexts.Prepend (C);

      --  Initialize the context that has no project, ie the last entry
      --  in Self.Contexts
      declare
         Projectless_Context : constant Context_Access :=
           new Context (Self.Trace);
      begin
         Projectless_Context.Initialize (Root);
         Self.Contexts.Append (Projectless_Context);
      end;
   end Ensure_Context_Initialized;

   ------------------------
   -- Initialize_Request --
   ------------------------

   overriding function On_Initialize_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.InitializeParams)
      return LSP.Messages.Server_Responses.Initialize_Response
   is
      Response : LSP.Messages.Server_Responses.Initialize_Response
        (Is_Error => False);
      Root     : LSP.Types.LSP_String;
   begin
      Response.result.capabilities.definitionProvider := True;
      Response.result.capabilities.typeDefinitionProvider := True;
      Response.result.capabilities.referencesProvider := True;
      Response.result.capabilities.documentSymbolProvider := True;
      Response.result.capabilities.renameProvider := True;
      Response.result.capabilities.textDocumentSync :=
        (Is_Set => True, Is_Number => True, Value => LSP.Messages.Full);
      Response.result.capabilities.completionProvider :=
        (True,
         (resolveProvider => (True, False),
          triggerCharacters => Empty_Vector & To_LSP_String (".")));
      Response.result.capabilities.hoverProvider := True;

      Response.result.capabilities.alsCalledByProvider := True;
      Response.result.capabilities.alsReferenceKinds :=
        (Is_Set => True,
         Value  => (Is_Server_Side => True, As_Flags => (others => True)));

      if not LSP.Types.Is_Empty (Value.rootUri) then
         Root := URI_To_File (Value.rootUri);
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

      Ensure_Context_Initialized (Self, Root);

      --  Log the context root
      Self.Trace.Trace ("Context root: " & To_UTF_8_String (Root));

      return Response;
   end On_Initialize_Request;

   -------------------------
   -- On_Shutdown_Request --
   -------------------------

   overriding function On_Shutdown_Request
     (Self  : access Message_Handler)
      return LSP.Messages.Server_Responses.Shutdown_Response
   is
      pragma Unreferenced (Self);
   begin
      return Response : LSP.Messages.Server_Responses.Shutdown_Response
        (Is_Error => False);
   end On_Shutdown_Request;

   ---------------------------
   -- On_CodeAction_Request --
   ---------------------------

   overriding function On_CodeAction_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.CodeActionParams)
      return LSP.Messages.Server_Responses.CodeAction_Response
   is
      pragma Unreferenced (Self, Value);
      Response : LSP.Messages.Server_Responses.CodeAction_Response
        (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Messages.InternalError,
          message => To_LSP_String ("Not implemented"),
          data => <>));
      return Response;
   end On_CodeAction_Request;

   --------------------------------
   -- On_Execute_Command_Request --
   --------------------------------

   overriding function On_Execute_Command_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.ExecuteCommandParams)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response
   is
      pragma Unreferenced (Self, Value);
      Response : LSP.Messages.Server_Responses.ExecuteCommand_Response
        (Is_Error => True);
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
      return LSP.Messages.Server_Responses.Location_Response
   is
      use Libadalang.Analysis;

      Response   : LSP.Messages.Server_Responses.Location_Response
        (Is_Error => False);

      function Find_Next_Part
        (Definition : Defining_Name) return Defining_Name;
      --  Find defining name of a completion if any

      function Find_First_Part
        (Definition : Defining_Name) return Defining_Name;
      --  Find defining name of a declaration for given completion

      procedure Resolve_In_Context (C : Context_Access);
      --  Utility function, appends to Resonse.result all results of the
      --  definition requests found in context C.

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

      ------------------------
      -- Resolve_In_Context --
      ------------------------

      procedure Resolve_In_Context (C : Context_Access) is
         Name_Node : constant Name := LSP.Lal_Utils.Get_Node_As_Name
           (C.Get_Node_At (Value));
         Definition : Defining_Name;
         Other_Part : Defining_Name;
      begin
         if Name_Node = No_Name then
            return;
         end if;

         --  Check is we are on some defining name
         Definition := LSP.Lal_Utils.Get_Name_As_Defining (Name_Node);

         if Definition = No_Defining_Name then
            Self.Imprecise_Resolve_Name
              (C, Value, Definition, LSP.Messages.Info);

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
      end Resolve_In_Context;

   begin
      for C of Self.Contexts loop
         Resolve_In_Context (C);
      end loop;

      return Response;
   end On_Definition_Request;

   --------------------------------
   -- On_Type_Definition_Request --
   --------------------------------

   overriding function On_Type_Definition_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Server_Responses.Location_Response
   is
      use Libadalang.Analysis;

      Response  : LSP.Messages.Server_Responses.Location_Response
        (Is_Error => False);
      Document  : constant LSP.Ada_Documents.Document_Access :=
        Get_Document (Self.Contexts, Value.textDocument.uri);
      Name_Node : constant Name :=
                    LSP.Lal_Utils.Get_Node_As_Name
                      (Document.Get_Node_At (Value.position));
      Type_Decl : Base_Type_Decl;
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

--        if Type_Decl.Kind in Ada_Anonymous_Type_Decl then
--           --  When LAL will be ready update this with
--           Type_Decl := Type_Decl.P_Pointed_Type or something
--        end if;

      Append_Location (Response.result, Type_Decl);

      return Response;
   end On_Type_Definition_Request;

   -------------------------------------------
   -- On_DidChangeTextDocument_Notification --
   -------------------------------------------

   overriding procedure On_DidChangeTextDocument_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidChangeTextDocumentParams)
   is
      function Skip_Did_Change return Boolean;
      --  Check if the following message in the queue is didChange for
      --  the same document

      ---------------------
      -- Skip_Did_Change --
      ---------------------

      function Skip_Did_Change return Boolean is
         use type LSP.Servers.Message_Access;

         subtype DidChangeTextDocument_Notification is LSP.Messages
           .Server_Notifications.DidChangeTextDocument_Notification;

         Next : constant LSP.Servers.Message_Access :=
           Self.Server.Look_Ahead_Message;
      begin
         if Next = null
           or else Next.all not in
             DidChangeTextDocument_Notification'Class
         then
            return False;
         end if;

         declare
            Object : DidChangeTextDocument_Notification'Class renames
              DidChangeTextDocument_Notification'Class (Next.all);
         begin
            if Object.params.textDocument.uri /= Value.textDocument.uri then
               return False;
            end if;
         end;

         return True;
      end Skip_Did_Change;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Document (Self.Contexts, Value.textDocument.uri);
      Diag     : LSP.Messages.PublishDiagnosticsParams;
   begin
      if Skip_Did_Change then
         --  Don't process the notification if next message overrides it
         return;
      end if;

      Document.Apply_Changes (Value.contentChanges);

      if Self.Diagnostics_Enabled then
         Document.Get_Errors (Diag.diagnostics);

         Diag.uri := Value.textDocument.uri;
         Self.Server.On_Publish_Diagnostics (Diag);
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
      for Context of Self.Contexts loop
         if Context.Has_Document (Value.textDocument.uri) then
            Context.Unload_Document (Value.textDocument);
         end if;
      end loop;

      --  Clean diagnostics up on closing document
      if Self.Diagnostics_Enabled then
         Diag.uri := Value.textDocument.uri;
         Self.Server.On_Publish_Diagnostics (Diag);
      end if;
   end On_DidCloseTextDocument_Notification;

   -----------------------------------------
   -- On_DidOpenTextDocument_Notification --
   -----------------------------------------

   overriding procedure On_DidOpenTextDocument_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidOpenTextDocumentParams)
   is
      Diag     : LSP.Messages.PublishDiagnosticsParams;
      Document : LSP.Ada_Documents.Document_Access;
   begin
      Self.Trace.Trace ("In Text_Document_Did_Open");
      Self.Trace.Trace ("Uri : " & To_UTF_8_String (Value.textDocument.uri));

      --  Some clients don't properly call initialize, in which case we want
      --  to call it anyway at the first open file request, using the
      --  directory containing the file being opened.
      Ensure_Context_Initialized
        (Self,
         To_LSP_String (Ada.Directories.Containing_Directory
           (To_UTF_8_String (URI_To_File (Value.textDocument.uri)))));

      --  Send notifications "loading document" / "done loading document"
      --  around the load of documents: this gets logged by the IDE,
      --  and helps tracking the performance of the language server.
      Self.Server.On_Show_Message
        ((LSP.Messages.Log,
         "loading document " & Value.textDocument.uri));

      declare
         Best_Context : constant Context_Access := Get_Best_Context_For_URI
           (Self.Contexts, Value.textDocument.uri);
      begin
         Document := Best_Context.Load_Document (Value.textDocument);

         Self.Server.On_Show_Message
           ((LSP.Messages.Log,
            "done loading document " & Value.textDocument.uri));

         if Self.Diagnostics_Enabled then
            Document.Get_Errors (Diag.diagnostics);

            Diag.uri := Value.textDocument.uri;
            Self.Server.On_Publish_Diagnostics (Diag);
         end if;
      end;
   end On_DidOpenTextDocument_Notification;

   --------------------------
   -- On_Highlight_Request --
   --------------------------

   overriding function On_Highlight_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Server_Responses.Highlight_Response
   is
      pragma Unreferenced (Self, Value);
      Response : LSP.Messages.Server_Responses.Highlight_Response
        (Is_Error => True);
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
      return LSP.Messages.Server_Responses.Hover_Response
   is
      use Libadalang.Analysis;
      use Libadalang.Common;

      Response           : LSP.Messages.Server_Responses.Hover_Response
        (Is_Error => False);

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

      C : constant Context_Access :=
        Get_Best_Context_For_URI (Self.Contexts, Value.textDocument.uri);
      --  For the Hover request, we're only interested in the "best"
      --  response value, not in the list of values for all contexts

   begin
      Self.Imprecise_Resolve_Name (C, Value, Defining_Name_Node);

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
      return LSP.Messages.Server_Responses.Location_Response
   is
      use Libadalang.Analysis;

      Response   : LSP.Messages.Server_Responses.Location_Response
        (Is_Error => False);

      procedure Process_Context (C : Context_Access);
      --  Process the references found in one context and append
      --  them to Response.results.

      function Get_Reference_Kind
        (Node : Ada_Node) return LSP.Messages.AlsReferenceKind_Set;
      --  Fetch reference kind for given node

      ------------------------
      -- Get_Reference_Kind --
      ------------------------

      function Get_Reference_Kind
        (Node : Ada_Node) return LSP.Messages.AlsReferenceKind_Set
      is
         Id     : constant Name := LSP.Lal_Utils.Get_Node_As_Name (Node);
         Result : LSP.Messages.AlsReferenceKind_Set := LSP.Messages.Empty_Set;
      begin
         begin
            Result.As_Flags (LSP.Messages.Write) := Id.P_Is_Write_Reference;
         exception
            when Libadalang.Common.Property_Error =>
               null;
         end;

         begin
            Result.As_Flags (LSP.Messages.Static_Call) := Id.P_Is_Static_Call;
         exception
            when Libadalang.Common.Property_Error =>
               null;
         end;

         begin
            Result.As_Flags (LSP.Messages.Dispatching_Call) :=
              Id.P_Is_Dispatching_Call;
         exception
            when Libadalang.Common.Property_Error =>
               null;
         end;

         return Result;
      end Get_Reference_Kind;

      ---------------------
      -- Process_Context --
      ---------------------

      procedure Process_Context (C : Context_Access) is
         Definition : Defining_Name;
      begin
         Self.Imprecise_Resolve_Name (C, Value, Definition);

         if Definition = No_Defining_Name then
            return;
         end if;

         declare
            References  : constant Base_Id_Array :=
              C.Find_All_References (Definition);
         begin
            for Node of References loop
               Append_Location
                 (Response.result,
                  Node,
                  Get_Reference_Kind (Node.As_Ada_Node));
            end loop;

            if Value.context.includeDeclaration then
               Append_Location
                 (Response.result,
                  Definition,
                  Get_Reference_Kind (Definition.As_Ada_Node));
            end if;
         end;
      end Process_Context;

   begin
      for C of Self.Contexts loop
         Process_Context (C);
      end loop;

      return Response;
   end On_References_Request;

   ------------------------------
   -- On_ALS_Called_By_Request --
   ------------------------------

   overriding function On_ALS_Called_By_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Server_Responses.ALS_Called_By_Response
   is
      use Libadalang.Analysis;
      use all type Libadalang.Common.Ada_Node_Kind_Type;

      Response   : LSP.Messages.Server_Responses.ALS_Called_By_Response
        (Is_Error => False);

      procedure Process_Context (C : Context_Access);
      --  Process the calls found in one context and append
      --  them to Response.results.

      ---------------------
      -- Process_Context --
      ---------------------

      procedure Process_Context (C : Context_Access) is
         Definition : Defining_Name;
      begin
         Self.Imprecise_Resolve_Name (C, Value, Definition);

         --  Attempt to resolve the name, return no results if we can't or if
         --  the name does not resolve to a subprogram.

         if Definition = No_Defining_Name
           or else Definition.P_Basic_Decl.Kind not in
             Ada_Subp_Decl | Ada_Subp_Body | Ada_Null_Subp_Decl
         then
            return;
         end if;

         declare
            Called  : constant LSP.Lal_Utils.References_By_Subprogram.Map :=
              LSP.Lal_Utils.Is_Called_By (C.all, Definition);

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
                  Subp_And_Refs.loc := Get_Node_Location (Ada_Node (Node));
                  Subp_And_Refs.name := To_LSP_String
                    (Langkit_Support.Text.To_UTF8 (Node.Text));
                  for Ref of Refs loop
                     Append_Location (Subp_And_Refs.refs, Ref);
                  end loop;
                  Response.result.Append (Subp_And_Refs);
               end;
               Next (C);
            end loop;
         end;
      end Process_Context;

   begin
      --  Find the references in all contexts
      for C of Self.Contexts loop
         Process_Context (C);
      end loop;

      return Response;
   end On_ALS_Called_By_Request;

   -------------------------------
   -- On_Signature_Help_Request --
   -------------------------------

   overriding function On_Signature_Help_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Server_Responses.SignatureHelp_Response
   is
      pragma Unreferenced (Self, Value);
      Response : LSP.Messages.Server_Responses.SignatureHelp_Response
        (Is_Error => True);
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
      return LSP.Messages.Server_Responses.Symbol_Response
   is
      --  The list of symbols for one document shouldn't depend
      --  on the project: we can just choose the best context for this.
      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Document (Self.Contexts, Value.textDocument.uri);
      Response : LSP.Messages.Server_Responses.Symbol_Response
        (Is_Error => False);

   begin
      Document.Get_Symbols (Response.result);
      return Response;
   end On_Document_Symbols_Request;

   -----------------------
   -- On_Rename_Request --
   -----------------------

   overriding function On_Rename_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.RenameParams)
      return LSP.Messages.Server_Responses.Rename_Response
   is
      use Libadalang.Analysis;

      Response   : LSP.Messages.Server_Responses.Rename_Response
        (Is_Error => False);

      procedure Process_Context (C : Context_Access);
      --  Process the rename request for the given context, and add
      --  the results to response.

      ---------------------
      -- Process_Context --
      ---------------------

      procedure Process_Context (C : Context_Access) is
         Position : constant LSP.Messages.TextDocumentPositionParams :=
           (Value.textDocument, Value.position);

         Name_Node : constant Name := LSP.Lal_Utils.Get_Node_As_Name
           (C.Get_Node_At (Position));

         Definition : Defining_Name;
         Imprecise  : Boolean;
         Empty      : LSP.Messages.TextEdit_Vector;
      begin
         if Name_Node = No_Name then
            return;
         end if;

         Definition := LSP.Lal_Utils.Resolve_Name
           (Name_Node,
            Imprecise => Imprecise);

         --  If we used the imprecise fallback to get to the definition, stop
         if Imprecise then
            return;
         end if;

         if Definition = No_Defining_Name then
            return;
         end if;

         declare
            References  : constant Base_Id_Array :=
              C.Find_All_References (Definition)
              --  Append Definition itself so that it is also renamed
              & Definition.P_Relative_Name.As_Base_Id;
         begin
            for Node of References loop
               declare
                  Location : constant LSP.Messages.Location :=
                    Get_Node_Location (Node => Node.As_Ada_Node);
                  Item : constant LSP.Messages.TextEdit :=
                    (span    => Location.span,
                     newText => Value.newName);
               begin
                  if not Response.result.changes.Contains (Location.uri) then
                     --  We haven't touched this document yet, create an empty
                     --  change list
                     Response.result.changes.Insert (Location.uri, Empty);
                  end if;

                  --  When iterating over all contexts (and therefore all
                  --  projects), it's possible to encounter the same
                  --  definitions more than once, so verify that the result
                  --  is not already recorded before adding it.
                  if not Response.result.changes
                    (Location.uri).Contains (Item)
                  then
                     Response.result.changes (Location.uri).Append (Item);
                  end if;
               end;
            end loop;

            return;
         end;
      end Process_Context;

   begin
      for C of Self.Contexts loop
         Process_Context (C);
      end loop;
      return Response;
   end On_Rename_Request;

   --------------------------------------------
   -- On_DidChangeConfiguration_Notification --
   --------------------------------------------

   overriding procedure On_DidChangeConfiguration_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidChangeConfigurationParams)
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
               File := URI_To_File (File);
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
            Self.Diagnostics_Enabled := Ada.Get (enableDiagnostics);
         end if;
      end if;

      --  Temporary behavior: at the moment, we support only one project
      --  tree loaded at the same time. So, Self.Contexts contains, in this
      --  order, the context for the current project, and the context for
      --  no projects.

      declare
         C : constant Context_Access := Self.Contexts.First_Element;
      begin
         C.Load_Project
           (File, Variables,
            Standard.Ada.Strings.Unbounded.To_String (Charset),
            Errors);
      end;

      if not LSP.Types.Is_Empty (Errors.message) then
         Self.Server.On_Show_Message (Errors);
      end if;
   end On_DidChangeConfiguration_Notification;

   ------------------------------------------
   -- On_Workspace_Execute_Command_Request --
   ------------------------------------------

   overriding function On_Workspace_Execute_Command_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.ExecuteCommandParams)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response
   is
      pragma Unreferenced (Self, Value);
      Response : LSP.Messages.Server_Responses.ExecuteCommand_Response
        (Is_Error => True);
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
      return LSP.Messages.Server_Responses.Symbol_Response
   is
      pragma Unreferenced (Self, Value);
      Response : LSP.Messages.Server_Responses.Symbol_Response
        (Is_Error => True);
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
      return LSP.Messages.Server_Responses.Completion_Response
   is
      --  We're completing only based on one context, ie one project
      --  tree: this seems reasonable. One further refinement could
      --  be to return only results that are available for all
      --  project contexts.
      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Document (Self.Contexts, Value.textDocument.uri);
      Response : LSP.Messages.Server_Responses.Completion_Response
        (Is_Error => False);
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
      --  Reload the contexts in case of unexpected errors.
      for C of Self.Contexts loop
         C.Reload;
      end loop;
   end Handle_Error;

end LSP.Ada_Handlers;

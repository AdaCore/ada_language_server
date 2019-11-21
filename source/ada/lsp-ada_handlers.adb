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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories;

with GNAT.Strings;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNATCOLL.Projects;
with GNATCOLL.JSON;
with GNATCOLL.Utils;             use GNATCOLL.Utils;
with GNATCOLL.VFS_Utils;         use GNATCOLL.VFS_Utils;

with LSP.Types; use LSP.Types;

with LSP.Ada_Documents;
with LSP.Common;       use LSP.Common;
with LSP.Lal_Utils;    use LSP.Lal_Utils;
with LSP.Ada_Contexts; use LSP.Ada_Contexts;
with LSP.Messages.Server_Notifications;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Doc_Utils;

with URIs;

package body LSP.Ada_Handlers is

   type Cancel_Countdown is mod 128;
   --  Counter to restrict frequency of Request.Canceled checks

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

   procedure Show_Message
     (Self : access Message_Handler;
      Text : String;
      Mode : LSP.Messages.MessageType := LSP.Messages.Error);
   --  Convenience function to send a message to the user.

   function Get_Unique_Progress_Token
     (Self      : access Message_Handler;
      Operation : String := "") return LSP_Number_Or_String;
   --  Return an unique token for indicating progress

   procedure Index_Files (Self : access Message_Handler);
   --  Index all loaded files in each context. Emit progresormation.

   ---------------------
   -- Project loading --
   ---------------------

   --  The heuristics that is used for loading a project is the following:
   --
   --     * if a project (and optionally a scenario) was specified by
   --       the user via the workspace/didChangeConfiguration request,
   --       attempt to use this. If this fails to load, report an error
   --       but do not attempt to load another project.
   --     => This case is handled by a call to Load_Project in
   --        On_DidChangeConfiguration_Notification.
   --
   --     * if no project was specified by the user, then look in the Root
   --       directory, mimicking the behavior of gprbuild:
   --           * if there are zero .gpr files in this directory, load the
   --             implicit project
   --           * if there is exactly one .gpr file in this directory, load
   --             it, returning an error if this failed
   --           * if there are more than one .gpr files in this directory,
   --             display an error
   --      => These cases are handled by Ensure_Project_Loaded
   --
   --  At any point where requests are made, Self.Contexts should
   --  contain, in this order:
   --   - one or more contexts, each one containing a non-aggregate project
   --       hierarchy.
   --   - a "projectless" context
   --  The creation of the "projectless" context is handled by
   --  Create_Projectless_Context
   --
   --  The attempt to load a project should be done in reaction to
   --  On_DidChangeConfiguration_Notification. However, the IDEs that
   --  are not configured specifically for this language server might
   --  not pass a .gpr file to didChangeConfiguration: for these IDEs,
   --  we fallback to loading the project the first time an editor is
   --  open.

   procedure Ensure_Project_Loaded
     (Self : access Message_Handler;
      Root : LSP.Types.LSP_String);
   --  This function makes sure that the contexts in Self are properly
   --  initialized and a project is loaded. If they are not initialized,
   --  initialize them.

   procedure Load_Project
     (Self     : access Message_Handler;
      GPR      : Virtual_File;
      Scenario : LSP.Types.LSP_Any;
      Charset  : String);
   --  Attempt to load the given project file, with the scenario provided.
   --  This unloads all currently loaded project contexts.

   procedure Create_Projectless_Context (Self : access Message_Handler);
   --  This creates the context for "no project" if it's not created already

   ---------------------------
   -- Multi-context support --
   ---------------------------

   --  A set of utilities for dealing with multiple contexts

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
         Self.Trace,
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

   ---------------------------
   -- Ensure_Project_Loaded --
   ---------------------------

   procedure Ensure_Project_Loaded
     (Self : access Message_Handler;
      Root : LSP.Types.LSP_String)
   is
      GPRs_Found : Natural := 0;
      Files      : File_Array_Access;
      GPR        : Virtual_File;
   begin
      if Integer (Self.Contexts.Length) >= 2 then
         --  Rely on the fact that there are at least two contexts initialized
         --  as a guarantee that the initialization has been done.
         return;
      end if;

      --  If we never passed through Initialize, this might be empty:
      --  initialize it now
      if Self.Root = No_File then
         Self.Root := Create (+To_UTF_8_String (Root));
      end if;

      Self.Trace.Trace ("Project loading ...");
      Self.Trace.Trace ("Root : " & To_UTF_8_String
                        (+Self.Root.Display_Full_Name));

      --  We're going to look for a project in Root: list all the files
      --  in this directory, looking for .gpr files.

      Files := Self.Root.Read_Dir (Files_Only);
      if Files /= null then
         for X of Files.all loop
            if Ends_With (+X.Base_Name, ".gpr") then
               GPRs_Found := GPRs_Found + 1;
               exit when GPRs_Found > 1;
               GPR := X;
            end if;
         end loop;
         Unchecked_Free (Files);
      end if;

      --  What we do depends on the number of .gpr files found:

      if GPRs_Found = 0 then
         --  We have found zero .gpr files: load the implicit project

         Self.Trace.Trace ("Loading the implicit project");
         declare
            C : constant Context_Access := new Context (Self.Trace);
            use GNATCOLL.Projects;
            Tree         : Project_Tree_Access;
            Root_Project : Project_Type_Access;
         begin
            Tree := new Project_Tree;
            C.Initialize;
            Load_Implicit_Project (Tree.all);
            Root_Project := new Project_Type'(Tree.Root_Project);
            C.Load_Project (Tree, Root_Project, "iso-8859-1");
            Self.Contexts.Prepend (C);
         end;
      elsif GPRs_Found = 1 then
         --  We have not found exactly one .gpr file: load the default
         --  project.
         Self.Trace.Trace ("Loading " & GPR.Display_Base_Name);
         Self.Load_Project (GPR, GNATCOLL.JSON.JSON_Null, "iso-8859-1");
      else
         --  We have found more than one project: warn the user!

         Self.Show_Message
           ("More than one .gpr found." & ASCII.LF &
              "Note: you can configure a project " &
              " through the ada.projectFile setting.");
      end if;
   end Ensure_Project_Loaded;

   --------------------------------
   -- Create_Projectless_Context --
   --------------------------------

   procedure Create_Projectless_Context (Self : access Message_Handler) is
      Projectless_Context : constant Context_Access :=
        new Context (Self.Trace);
   begin
      if not Self.Contexts.Is_Empty then
         --  We have already created the projectless context: we can return
         return;
      end if;

      Projectless_Context.Initialize;
      Self.Contexts.Append (Projectless_Context);
   end Create_Projectless_Context;

   ------------------------
   -- Initialize_Request --
   ------------------------

   overriding function On_Initialize_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Initialize_Request)
      return LSP.Messages.Server_Responses.Initialize_Response
   is
      Value    : LSP.Messages.InitializeParams renames Request.params;
      Response : LSP.Messages.Server_Responses.Initialize_Response
        (Is_Error => False);
      Root     : LSP.Types.LSP_String;
   begin
      Response.result.capabilities.definitionProvider := True;
      Response.result.capabilities.typeDefinitionProvider :=
        (Is_Set => True,
         Value => (Is_Boolean => True, Bool => True));
      Response.result.capabilities.referencesProvider := True;
      Response.result.capabilities.documentSymbolProvider := True;
      Response.result.capabilities.renameProvider :=
        (Is_Set => True,
         Value  => (prepareProvider => (Is_Set => False)));
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

      Self.Root := Create (+To_UTF_8_String (Root));

      --  Log the context root
      Self.Trace.Trace ("Context root: " & To_UTF_8_String (Root));

      Self.Create_Projectless_Context;

      return Response;
   end On_Initialize_Request;

   -------------------------
   -- On_Shutdown_Request --
   -------------------------

   overriding function On_Shutdown_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Shutdown_Request)
      return LSP.Messages.Server_Responses.Shutdown_Response
   is
      pragma Unreferenced (Self, Request);
   begin
      return Response : LSP.Messages.Server_Responses.Shutdown_Response
        (Is_Error => False);
   end On_Shutdown_Request;

   ---------------------------
   -- On_CodeAction_Request --
   ---------------------------

   overriding function On_CodeAction_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.CodeAction_Request)
      return LSP.Messages.Server_Responses.CodeAction_Response
   is
      pragma Unreferenced (Self, Request);
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
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response
   is
      pragma Unreferenced (Self, Request);
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
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Definition_Request)
      return LSP.Messages.Server_Responses.Location_Response
   is
      use Libadalang.Analysis;

      Value      : LSP.Messages.TextDocumentPositionParams renames
        Request.params;
      Response   : LSP.Messages.Server_Responses.Location_Response
        (Is_Error => False);
      Imprecise  : Boolean := False;

      procedure Resolve_In_Context (C : Context_Access);
      --  Utility function, appends to Resonse.result all results of the
      --  definition requests found in context C.

      ------------------------
      -- Resolve_In_Context --
      ------------------------

      procedure Resolve_In_Context (C : Context_Access) is
         Name_Node : constant Name := LSP.Lal_Utils.Get_Node_As_Name
           (C.Get_Node_At (Value));
         Definition : Defining_Name;
         Other_Part : Defining_Name;
         Manual_Fallback : Defining_Name;

      begin
         if Name_Node = No_Name then
            return;
         end if;

         --  Check is we are on some defining name
         Definition := Get_Name_As_Defining (Name_Node);

         if Definition = No_Defining_Name then
            Self.Imprecise_Resolve_Name
              (C, Value, Definition, LSP.Messages.Info);

            if Definition /= No_Defining_Name then
               Append_Location (Response.result, Definition);
            end if;
         else  --  If we are on a defining_name already
            Other_Part := Find_Next_Part (Definition, Self.Trace);

            if Other_Part = No_Defining_Name then
               --  No next part is found. Check first defining name
               Other_Part := Find_Canonical_Part (Definition, Self.Trace);
            end if;

            if Other_Part /= No_Defining_Name then
               Append_Location (Response.result, Other_Part);
            else
               --  We were on a defining name, but did not manage to find
               --  an answer using Find_Next_Part / Find_Canonical_Part.
               --  Use the manual fallback to attempt to find a good enough
               --  result.
               Manual_Fallback := Find_Other_Part_Fallback
                 (Definition, Self.Trace);

               if Manual_Fallback /= No_Defining_Name then
                  --  We have found a result using the imprecise heuristics.
                  --  We'll warn the user and send the result.
                  Imprecise := True;
                  Append_Location (Response.result, Manual_Fallback);
               end if;
            end if;
         end if;
      end Resolve_In_Context;

   begin
      for C of Self.Contexts loop
         Resolve_In_Context (C);

         exit when Request.Canceled;
      end loop;

      if Imprecise then
         Self.Show_Message
           ("The result of 'definition' is approximate.",
            LSP.Messages.Warning);
      end if;

      return Response;
   end On_Definition_Request;

   --------------------------------
   -- On_Type_Definition_Request --
   --------------------------------

   overriding function On_Type_Definition_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Type_Definition_Request)
      return LSP.Messages.Server_Responses.Location_Response
   is
      use Libadalang.Analysis;

      Value     : LSP.Messages.TextDocumentPositionParams renames
        Request.params;
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

      if Type_Decl = No_Basic_Decl or else Request.Canceled then
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

      --  Some clients don't properly call initialize, or don't pass the
      --  project to didChangeConfiguration: fallback here on loading a
      --  project in this directory, if needed.
      Self.Create_Projectless_Context;
      Ensure_Project_Loaded
        (Self,
         To_LSP_String (Ada.Directories.Containing_Directory
           (To_UTF_8_String (URI_To_File (Value.textDocument.uri)))));

      declare
         Best_Context : constant Context_Access := Get_Best_Context_For_URI
           (Self.Contexts, Value.textDocument.uri);
      begin
         Document := Best_Context.Load_Document (Value.textDocument);

         if Self.Diagnostics_Enabled then
            Document.Get_Errors (Diag.diagnostics);

            Diag.uri := Value.textDocument.uri;
            Self.Server.On_Publish_Diagnostics (Diag);
         end if;
      end;

      Self.Trace.Trace ("Finished Text_Document_Did_Open");
   end On_DidOpenTextDocument_Notification;

   --------------------------
   -- On_Highlight_Request --
   --------------------------

   overriding function On_Highlight_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Highlight_Request)
      return LSP.Messages.Server_Responses.Highlight_Response
   is
      pragma Unreferenced (Self, Request);
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
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Hover_Request)
      return LSP.Messages.Server_Responses.Hover_Response
   is
      use Libadalang.Analysis;
      use Libadalang.Common;

      Value    : LSP.Messages.TextDocumentPositionParams renames
        Request.params;
      Response : LSP.Messages.Server_Responses.Hover_Response
        (Is_Error => False);

      Defining_Name_Node : Defining_Name;
      Decl               : Basic_Decl;
      Subp_Spec_Node     : Base_Subp_Spec;
      Decl_Text          : LSP_String;
      Comments_Text      : LSP_String;
      Location_Text      : LSP_String;
      Decl_Unit_File     : Virtual_File;

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

      if Decl = No_Basic_Decl or else Request.Canceled then
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
         Response.result.contents.Vector.Append
           (LSP.Messages.MarkedString'
              (Is_String => False,
               value     => Decl_Text,
               language  => To_LSP_String ("ada")));

         --  Append the declaration's location

         Decl_Unit_File := GNATCOLL.VFS.Create (+Decl.Unit.Get_Filename);

         Location_Text := To_LSP_String
           ("at " & Decl_Unit_File.Display_Base_Name & " ("
            & GNATCOLL.Utils.Image
              (Integer (Decl.Sloc_Range.Start_Line), Min_Width => 1)
            & ":"
            & GNATCOLL.Utils.Image
              (Integer (Decl.Sloc_Range.Start_Column), Min_Width => 1)
            & ")");

         Response.result.contents.Vector.Append
           (LSP.Messages.MarkedString'
              (Is_String => True,
               value     => Location_Text));

         --  Append the comments associated with the basic declaration
         --  if any.

         Comments_Text := To_LSP_String
           (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode
              (Libadalang.Doc_Utils.Get_Documentation
                   (Decl).Doc.To_String));

         if Comments_Text /= Empty_LSP_String then
            Response.result.contents.Vector.Append
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
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.References_Request)
      return LSP.Messages.Server_Responses.Location_Response
   is
      use Libadalang.Analysis;

      Value      : LSP.Messages.ReferenceParams renames Request.params;
      Response   : LSP.Messages.Server_Responses.Location_Response
        (Is_Error => False);
      Imprecise  : Boolean := False;

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
            when E : Libadalang.Common.Property_Error =>
               Log (Self.Trace, E);
         end;

         begin
            Result.As_Flags (LSP.Messages.Static_Call) := Id.P_Is_Static_Call;
         exception
            when E : Libadalang.Common.Property_Error =>
               Log (Self.Trace, E);
         end;

         begin
            Result.As_Flags (LSP.Messages.Dispatching_Call) :=
              Id.P_Is_Dispatching_Call;
         exception
            when E : Libadalang.Common.Property_Error =>
               Log (Self.Trace, E);
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

         if Definition = No_Defining_Name or else Request.Canceled then
            return;
         end if;

         declare
            Count          : Cancel_Countdown := 0;
            This_Imprecise : Boolean;
            References     : constant Base_Id_Array :=
              C.Find_All_References (Definition, This_Imprecise);
         begin
            Imprecise := Imprecise or This_Imprecise;

            for Node of References loop
               Count := Count - 1;

               Append_Location
                 (Response.result,
                  Node,
                  Get_Reference_Kind (Node.As_Ada_Node));

               exit when Count = 0  and then Request.Canceled;
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

         exit when Request.Canceled;
      end loop;

      if Imprecise then
         Self.Show_Message
           ("The results of 'references' are approximate.",
            LSP.Messages.Warning);
      end if;

      return Response;
   end On_References_Request;

   ------------------------------
   -- On_ALS_Called_By_Request --
   ------------------------------

   overriding function On_ALS_Called_By_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Called_By_Request)
      return LSP.Messages.Server_Responses.ALS_Called_By_Response
   is
      use Libadalang.Analysis;

      Value      : LSP.Messages.TextDocumentPositionParams renames
        Request.params;
      Response   : LSP.Messages.Server_Responses.ALS_Called_By_Response
        (Is_Error => False);
      Imprecise  : Boolean := False;

      function Get_Reference_Kind
        (Node : Name) return LSP.Messages.AlsReferenceKind_Set;
      --  Query whether Node is a static or a dispatching call, and format
      --  this into an AlsReferenceKind_Set.

      ------------------------
      -- Get_Reference_Kind --
      ------------------------

      function Get_Reference_Kind
        (Node : Name) return LSP.Messages.AlsReferenceKind_Set
      is
         Result : LSP.Messages.AlsReferenceKind_Set := LSP.Messages.Empty_Set;
      begin
         begin
            Result.As_Flags (LSP.Messages.Static_Call) :=
              Node.P_Is_Static_Call;
            Result.As_Flags (LSP.Messages.Dispatching_Call) :=
              Node.P_Is_Dispatching_Call;
         exception
            when E : Libadalang.Common.Property_Error =>
               Log (Self.Trace, E);
         end;

         return Result;
      end Get_Reference_Kind;

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
         --  the name does not resolve to a callable object, like a subprogram
         --  or an entry.

         if Definition = No_Defining_Name
           or else not Definition.P_Basic_Decl.P_Is_Subprogram
           or else Request.Canceled
         then
            return;
         end if;

         declare
            This_Imprecise : Boolean;
            Called  : constant LSP.Lal_Utils.References_By_Subprogram.Map :=
              LSP.Lal_Utils.Find_All_Calls (C.all, Definition, This_Imprecise);

            use LSP.Lal_Utils.References_By_Subprogram;
            C     : Cursor := Called.First;
            Count : Cancel_Countdown := 0;
         begin
            Imprecise := Imprecise or This_Imprecise;

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
                     Append_Location (Subp_And_Refs.refs, Ref,
                                      Get_Reference_Kind (Ref.As_Name));
                     Count := Count - 1;

                     if Count = 0 and then Request.Canceled then
                        return;
                     end if;
                  end loop;

                  Response.result.Append (Subp_And_Refs);
                  Next (C);
               end;
            end loop;
         end;
      end Process_Context;

   begin
      --  Find the references in all contexts
      for C of Self.Contexts loop
         Process_Context (C);

         exit when Request.Canceled;
      end loop;

      if Imprecise then
         Self.Show_Message
           ("The results of 'called by' are approximate.",
            LSP.Messages.Warning);
      end if;

      return Response;
   end On_ALS_Called_By_Request;

   --------------------------
   -- On_ALS_Debug_Request --
   --------------------------

   overriding function On_ALS_Debug_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Debug_Request)
      return LSP.Messages.Server_Responses.ALS_Debug_Response is
   begin
      case Request.params.Kind is
         when LSP.Messages.Suspend_Execution =>
            declare
               Limit : constant Positive := Request.params.inputQueueLength;
            begin
               while Self.Server.Input_Queue_Length < Limit loop
                  delay 0.1;
               end loop;
            end;
      end case;

      return Response : LSP.Messages.Server_Responses.ALS_Debug_Response
        (Is_Error => False);
   end On_ALS_Debug_Request;

   -------------------------------
   -- On_Signature_Help_Request --
   -------------------------------

   overriding function On_Signature_Help_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Signature_Help_Request)
      return LSP.Messages.Server_Responses.SignatureHelp_Response
   is
      pragma Unreferenced (Self, Request);
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
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response
   is
      --  The list of symbols for one document shouldn't depend
      --  on the project: we can just choose the best context for this.
      Value    : LSP.Messages.DocumentSymbolParams renames Request.params;
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
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Rename_Request)
      return LSP.Messages.Server_Responses.Rename_Response
   is
      use Libadalang.Analysis;

      Value      : LSP.Messages.RenameParams renames Request.params;
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

         procedure Process_Comments
           (Node : Ada_Node;
            Uri  : LSP.Messages.DocumentUri);
         --  Iterate over all comments and include them in the responce when
         --  they contain a remaned word

         -----------------------
         --  Process_Comments --
         -----------------------

         procedure Process_Comments
           (Node : Ada_Node;
            Uri  : LSP.Messages.DocumentUri)
         is
            use Libadalang.Common;

            Token : Token_Reference := First_Token (Node.Unit);
            Name  : constant Wide_Wide_String :=
              Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String
                (Get_Last_Name (Name_Node));
            Span  : LSP.Messages.Span;

         begin
            while Token /= No_Token loop
               if Kind (Data (Token)) = Ada_Comment
                 and then Contains (Token, Name, Span)
               then
                  declare
                     C : constant LSP.Messages.TextEdit :=
                       (span    => Span,
                        newText => Value.newName);
                  begin
                     if not Response.result.changes (Uri).Contains (C) then
                        Response.result.changes (Uri).Append (C);
                     end if;
                  end;
               end if;

               Token := Next (Token);
            end loop;
         end Process_Comments;

      begin
         if Name_Node = No_Name then
            return;
         end if;

         Definition := LSP.Lal_Utils.Resolve_Name
           (Name_Node,
            Self.Trace,
            Imprecise => Imprecise);

         --  If we used the imprecise fallback to get to the definition, stop
         if Imprecise then
            return;
         end if;

         if Definition = No_Defining_Name or Request.Canceled then
            return;
         end if;

         declare
            Count       : Cancel_Countdown := 0;
            Imprecise   : Boolean;
            References  : constant Base_Id_Array :=
              C.Find_All_References (Definition, Imprecise)
              --  Append Definition itself so that it is also renamed
              & Definition.P_Relative_Name.As_Base_Id;
         begin
            if Imprecise then
               Self.Show_Message
                 ("References are not precise: renamed cancelled",
                  LSP.Messages.Warning);
               return;
            end if;

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

                     --  Process comments if it is needed
                     if Self.Refactoring.Renaming.In_Comments then
                        Process_Comments (Node.As_Ada_Node, Location.uri);
                     end if;
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

                  exit when Count = 0 and then Request.Canceled;

                  Count := Count - 1;
               end;
            end loop;
         end;
      end Process_Context;

   begin
      for C of Self.Contexts loop
         Process_Context (C);

         exit when Request.Canceled;
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
      enableIndexing    : constant String := "enableIndexing";
      renameInComments  : constant String := "renameInComments";

      Ada       : constant LSP.Types.LSP_Any := Value.settings.Get ("ada");
      File      : LSP.Types.LSP_String;
      Charset   : Unbounded_String;
      Variables : LSP.Types.LSP_Any;
   begin
      Self.Create_Projectless_Context;
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

         --  Similarly to diagnostics, we support selectively activating
         --  indexing in the parameters to this request.
         if Ada.Has_Field (enableIndexing) then
            Self.Indexing_Enabled := Ada.Get (enableIndexing);
         end if;

         if Ada.Has_Field (renameInComments) then
            Self.Refactoring.Renaming.In_Comments :=
              Ada.Get (renameInComments);
         end if;
      end if;

      if File /= Empty_LSP_String then
         --  The projectFile may be either an absolute path or a
         --  relative path; if so, we're assuming it's relative
         --  to Self.Root.
         declare
            Project_File : constant Filesystem_String :=
              +To_UTF_8_String (File);
            GPR : Virtual_File;
         begin
            if Is_Absolute_Path (Project_File) then
               GPR := Create (Project_File);
            else
               GPR := Create_From_Dir (Self.Root, Project_File);
            end if;

            Self.Load_Project (GPR, Variables, To_String (Charset));
         end;
      end if;

      Self.Ensure_Project_Loaded
        (To_LSP_String (Self.Root.Display_Full_Name));
   end On_DidChangeConfiguration_Notification;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Self     : access Message_Handler;
      GPR      : Virtual_File;
      Scenario : LSP.Types.LSP_Any;
      Charset  : String)
   is
      use GNATCOLL.Projects;
      use Ada.Containers;
      Project_Env   : Project_Environment_Access;
      Tree : Project_Tree_Access;
      Errors        : LSP.Messages.ShowMessageParams;
      Error_Text    : LSP.Types.LSP_String_Vector;

      procedure Create_Context_For_Non_Aggregate (P : Project_Type);
      procedure Add_Variable (Name : String; Value : GNATCOLL.JSON.JSON_Value);
      procedure On_Error (Text : String);

      ------------------
      -- Add_Variable --
      ------------------

      procedure Add_Variable
        (Name : String; Value : GNATCOLL.JSON.JSON_Value)
      is
         use type GNATCOLL.JSON.JSON_Value_Type;
      begin
         if Value.Kind = GNATCOLL.JSON.JSON_String_Type then
            Project_Env.Change_Environment (Name, Value.Get);
         end if;
      end Add_Variable;

      --------------
      -- On_Error --
      --------------

      procedure On_Error (Text : String) is
      begin
         LSP.Types.Append (Error_Text, LSP.Types.To_LSP_String (Text));
      end On_Error;

      --------------------------------------
      -- Create_Context_For_Non_Aggregate --
      --------------------------------------

      procedure Create_Context_For_Non_Aggregate (P : Project_Type) is
         C : constant Context_Access := new Context (Self.Trace);
         Project_Access : Project_Type_Access;
      begin
         C.Initialize;
         Project_Access := new Project_Type'(P);
         C.Load_Project (Tree    => Tree,
                         Root    => Project_Access,
                         Charset => Charset);
         Self.Contexts.Prepend (C);
      end Create_Context_For_Non_Aggregate;

   begin
      --  Unload all the contexts except the projectless context
      while Self.Contexts.Length > 1 loop
         declare
            C : constant Context_Access := Self.Contexts.First_Element;
         begin
            C.Unload;
         end;
         Self.Contexts.Delete_First;
      end loop;

      --  Now load the new project
      Errors.the_type := LSP.Messages.Warning;
      Initialize (Project_Env);
      if not Scenario.Is_Empty then
         Scenario.Map_JSON_Object (Add_Variable'Access);
      end if;

      begin
         Tree := new Project_Tree;
         Tree.Load (GPR, Project_Env, Errors => On_Error'Unrestricted_Access);
         if Tree.Root_Project.Is_Aggregate_Project then
            declare
               Aggregated : constant Project_Array_Access :=
                 Tree.Root_Project.Aggregated_Projects;
            begin
               for X of Aggregated.all loop
                  Create_Context_For_Non_Aggregate (X);
               end loop;
            end;
         else
            Create_Context_For_Non_Aggregate (Tree.Root_Project);
         end if;
      exception
         when E : Invalid_Project =>

            Self.Trace.Trace (E);
            Errors.the_type := LSP.Messages.Error;

            LSP.Types.Append
              (Errors.message,
               LSP.Types.To_LSP_String
                 ("Unable to load project file: " &
                  (+GPR.Full_Name.all)));
      end;

      --  Report the errors, if any
      if not Error_Text.Is_Empty then
         for Line of Error_Text loop
            LSP.Types.Append (Errors.message, Line);
         end loop;
         Self.Server.On_Show_Message (Errors);
      end if;

      --  Index the files immediately after loading a project
      Self.Indexing_Required := True;
   end Load_Project;

   -------------------------------
   -- Get_Unique_Progress_Token --
   -------------------------------

   function Get_Unique_Progress_Token
     (Self      : access Message_Handler;
      Operation : String := "") return LSP_Number_Or_String
   is

      Pid : constant String :=
        GNATCOLL.Utils.Image (Pid_To_Integer (Current_Process_Id), 1);
   begin
      Self.Token_Id := Self.Token_Id + 1;
      --  Generate an identifier that has little risk of collision with
      --  other language servers, or other occurrences of this server.
      --  (There is still a very small risk of collision with PID recyclings,
      --  but the consequences are acceptable.)
      return
        (Is_Number => False,
         String    => To_LSP_String
           ("ada_ls-"
            & Pid & "-" & Operation & "-"
            & GNATCOLL.Utils.Image (Self.Token_Id, 1)));
   end Get_Unique_Progress_Token;

   -----------------
   -- Index_Files --
   -----------------

   procedure Index_Files (Self : access Message_Handler) is
      type Context_And_Files is record
         Context : Context_Access;
         Sources : File_Array_Access;
      end record;
      type Context_And_Files_Array is array
        (Natural range <>) of Context_And_Files;

      Arr   : Context_And_Files_Array (1 .. Natural (Self.Contexts.Length));

      token : constant LSP.Types.LSP_Number_Or_String
        := Self.Get_Unique_Progress_Token ("indexing");

      procedure Emit_Progress_Begin;
      procedure Emit_Progress_Report (Percent : Natural);
      procedure Emit_Progress_End;
      --  Emit a message to inform that the indexing has begun / is in
      --  progress / has finished.

      -------------------------
      -- Emit_Progress_Begin --
      -------------------------

      procedure Emit_Progress_Begin is
         P : LSP.Messages.Progress_Params (LSP.Messages.Progress_Begin);
      begin
         P.Begin_Param.token := token;
         P.Begin_Param.value.title := LSP.Types.To_LSP_String ("Indexing");
         P.Begin_Param.value.percentage := (Is_Set => True, Value => 0);
         Self.Server.On_Progress (P);
      end Emit_Progress_Begin;

      --------------------------
      -- Emit_Progress_Report --
      --------------------------

      procedure Emit_Progress_Report (Percent : Natural) is
         P : LSP.Messages.Progress_Params (LSP.Messages.Progress_Report);
      begin
         P.Report_Param.token := token;
         P.Report_Param.value.percentage := (Is_Set => True, Value => Percent);
         Self.Server.On_Progress (P);
      end Emit_Progress_Report;

      -----------------------
      -- Emit_Progress_End --
      -----------------------

      procedure Emit_Progress_End is
         P : LSP.Messages.Progress_Params (LSP.Messages.Progress_End);
      begin
         P.End_Param.token := token;
         Self.Server.On_Progress (P);
      end Emit_Progress_End;

      Index           : Natural := 1;
      Total           : Natural := 0;
      Last_Percent    : Natural := 0;
      Current_Percent : Natural := 0;
   begin
      --  Prevent work if the indexing has been explicitly disabled
      if not Self.Indexing_Enabled then
         return;
      end if;

      --  Collect the contexts and their sources: we do this so that we are
      --  able to produce a progress bar covering the total number of files.

      for Context of Self.Contexts loop
         Arr (Index).Context := Context;
         Arr (Index).Sources := Context.List_Files;
         Total := Total + Arr (Index).Sources'Length;
         Index := Index + 1;
      end loop;

      Emit_Progress_Begin;

      Index := 0;
      for E of Arr loop
         for F of E.Sources.all loop
            Current_Percent := (Index * 100) / Total;
            --  If the value of the indexing increased by at least one percent,
            --  emit one progress report.
            if Current_Percent > Last_Percent then
               Emit_Progress_Report (Current_Percent);
               Last_Percent := Current_Percent;
            end if;

            E.Context.Index_File (F);
            Index := Index + 1;

            --  Check whether another request is pending. If so, pause the
            --  indexing; it will be resumed later as part of After_Request.
            --  if Self.Server.Input_Queue_Length > 0 then
            if Self.Server.Has_Pending_Work then
               Emit_Progress_End;
               return;
            end if;
         end loop;
      end loop;

      Emit_Progress_End;
      Self.Indexing_Required := False;
   end Index_Files;

   ------------------------------------------
   -- On_Workspace_Execute_Command_Request --
   ------------------------------------------

   overriding function On_Workspace_Execute_Command_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Workspace_Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response
   is
      pragma Unreferenced (Self, Request);
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
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Workspace_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response
   is
      pragma Unreferenced (Self, Request);
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
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Completion_Request)
      return LSP.Messages.Server_Responses.Completion_Response
   is
      --  We're completing only based on one context, ie one project
      --  tree: this seems reasonable. One further refinement could
      --  be to return only results that are available for all
      --  project contexts.
      Value    : LSP.Messages.TextDocumentPositionParams renames
        Request.params;
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

   ------------------
   -- Show_Message --
   ------------------

   procedure Show_Message
     (Self : access Message_Handler;
      Text : String;
      Mode : LSP.Messages.MessageType := LSP.Messages.Error) is
   begin
      Self.Server.On_Show_Message ((Mode, To_LSP_String (Text)));
   end Show_Message;

   -----------------
   -- Before_Work --
   -----------------

   overriding procedure Before_Work
     (Self    : access Message_Handler;
      Message : LSP.Messages.Message'Class) is null;

   ----------------
   -- After_Work --
   ----------------

   overriding procedure After_Work
     (Self    : access Message_Handler;
      Message : LSP.Messages.Message'Class)
   is
      pragma Unreferenced (Message);
   begin
     --  We have finished processing a request or notification:
     --  if it happens that indexing is required, do it now.
      if Self.Indexing_Required then
         Self.Index_Files;
      end if;
   end After_Work;

end LSP.Ada_Handlers;

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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Strings;
with GNATCOLL.JSON;
with GNATCOLL.Utils;             use GNATCOLL.Utils;
with GNATCOLL.VFS_Utils;         use GNATCOLL.VFS_Utils;

with LSP.Types; use LSP.Types;

with LSP.Common;       use LSP.Common;
with LSP.Lal_Utils;    use LSP.Lal_Utils;
with LSP.Ada_Contexts; use LSP.Ada_Contexts;
with LSP.Messages.Server_Notifications;

with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Doc_Utils;

with URIs;

package body LSP.Ada_Handlers is

   type Cancel_Countdown is mod 128;
   --  Counter to restrict frequency of Request.Canceled checks

   Is_Parent : constant LSP.Messages.AlsReferenceKind_Set :=
     (Is_Server_Side => True,
      As_Flags => (LSP.Messages.Parent => True, others => False));
   Is_Child : constant LSP.Messages.AlsReferenceKind_Set :=
     (Is_Server_Side => True,
      As_Flags => (LSP.Messages.Child => True, others => False));
   --  Convenient constants

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
                 return LSP.Types.LSP_String renames
     LSP.Types.To_LSP_String;

   procedure Send_Imprecise_Xref_Message
     (Self     : access Message_Handler;
      URI      : LSP.Messages.DocumentUri;
      Position : LSP.Messages.Position;
      Msg_Type : LSP.Messages.MessageType);
   --  Send a message of the given Msg_Type to the LSP client to warn the user
   --  of a possible imprecise result while computing xrefs on the given
   --  node.

   subtype Context_Access is LSP.Ada_Context_Sets.Context_Access;

   procedure Imprecise_Resolve_Name
     (Self       : access Message_Handler;
      In_Context : Context_Access;
      Position   : LSP.Messages.TextDocumentPositionParams'Class;
      Definition : out Libadalang.Analysis.Defining_Name;
      Msg_Type   : LSP.Messages.MessageType := LSP.Messages.Log);
   --  If node at given Position is a name, then resolve it.
   --  Send a message in case of a possible imprecise result.
   --  See description of Msg_Type in Send_Imprecise_Xref_Message comments.

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

   procedure Index_Files
     (Self             : access Message_Handler;
      Has_Pending_Work : Boolean);
   --  Index all loaded files in each context. Emit progress information.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (LSP.Ada_Documents.Document, Internal_Document_Access);

   procedure Release_Project_Info (Self : access Message_Handler);
   --  Release the memory associated to project information in Self

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
   --  contain one or more contexts, each one containing a non-aggregate
   --  project hierarchy.
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

   function Get_Open_Document
     (Self : access Message_Handler;
      URI  : LSP.Messages.DocumentUri)
      return LSP.Ada_Documents.Document_Access;
   --  Return the document for the given URI, assuming this document
   --  is open. Return null if this document is not open.

   -----------------------
   -- Get_Open_Document --
   -----------------------

   function Get_Open_Document
     (Self : access Message_Handler;
      URI  : LSP.Messages.DocumentUri)
      return LSP.Ada_Documents.Document_Access is
   begin
      if Self.Open_Documents.Contains (URI) then
         return LSP.Ada_Documents.Document_Access
           (Self.Open_Documents.Element (URI));
      else
         return null;
      end if;
   end Get_Open_Document;

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
        LSP.Lal_Utils.Get_Node_As_Name
          (In_Context.Get_Node_At
             (Get_Open_Document (Self, Position.textDocument.uri),
              Position));

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

   --------------------------
   -- Release_Project_Info --
   --------------------------

   procedure Release_Project_Info (Self : access Message_Handler) is
      use GNATCOLL.Projects;
   begin
      if Self.Project_Tree /= null then
         Self.Project_Tree.Unload;
         Free (Self.Project_Tree);
      end if;
      if Self.Project_Environment /= null then
         Free (Self.Project_Environment);
      end if;
   end Release_Project_Info;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Self : access Message_Handler) is
   begin
      --  Cleanup documents
      for Document of Self.Open_Documents loop
         Unchecked_Free (Document);
      end loop;
      Self.Open_Documents.Clear;

      --  Cleanup contexts
      Self.Contexts.Cleanup;

      --  Cleanup project and environment
      Self.Release_Project_Info;
   end Cleanup;

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
      if not Self.Contexts.Is_Empty then
         --  Rely on the fact that there is at least one context initialized
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
            C    : constant Context_Access := new Context (Self.Trace);
            Attr : GNAT.Strings.String_List (1 .. 1);
            use GNATCOLL.Projects;
         begin
            Self.Release_Project_Info;
            Initialize (Self.Project_Environment);
            Self.Project_Tree := new Project_Tree;
            C.Initialize;

            --  Note: we would call Load_Implicit_Project here, but this has
            --  two problems:
            --    - there is a bug under Windows where the files returned by
            --      Source_Files have an extraneous directory separator
            --    - the implicit project relies on the current working
            --      of the ALS, which imposes a restriction on clients, and
            --      is an extra pitfall for developers of this server
            --
            --  Instead, use Load_Empty_Project and set the source dir and
            --  language manually: this does not have these inconvenients.

            Load_Empty_Project
              (Self.Project_Tree.all, Self.Project_Environment);
            Attr := (1 => new String'("Ada"));
            Set_Attribute
              (Self.Project_Tree.Root_Project, Languages_Attribute, Attr);
            GNAT.Strings.Free (Attr (1));

            --  When there is no .gpr, create a project which loads all
            --  Ada sources in the current directory and subdirectories,
            --  recursively. Use the "**" syntax for this.
            Attr := (1 => new String'(Self.Root.Display_Full_Name & "**"));
            Set_Attribute
              (Self.Project_Tree.Root_Project,
               Source_Dirs_Attribute,
               Attr);
            GNAT.Strings.Free (Attr (1));
            Self.Project_Tree.Recompute_View;
            C.Load_Project (Self.Project_Tree,
                            Self.Project_Tree.Root_Project,
                            "iso-8859-1");
            Self.Contexts.Prepend (C);
         end;
      elsif GPRs_Found = 1 then
         --  We have not found exactly one .gpr file: load the default
         --  project.
         Self.Trace.Trace ("Loading " & GPR.Display_Base_Name);
         Self.Load_Project (GPR, No_Any, "iso-8859-1");
      else
         --  We have found more than one project: warn the user!

         Self.Show_Message
           ("More than one .gpr found." & ASCII.LF &
              "Note: you can configure a project " &
              " through the ada.projectFile setting.");
      end if;
   end Ensure_Project_Loaded;

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
      Response.result.capabilities.declarationProvider :=
        (Is_Set => True,
         Value => (Is_Boolean => True, Bool => True));
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

      if Value.capabilities.textDocument.documentSymbol.Is_Set
        and then Value.capabilities.textDocument.documentSymbol.Value
          .hierarchicalDocumentSymbolSupport = (True, True)
      then
         Self.Get_Symbols := LSP.Ada_Documents.Get_Symbol_Hierarchy'Access;
      else
         Self.Get_Symbols := LSP.Ada_Documents.Get_Symbols'Access;
      end if;

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

   ----------------------------
   -- On_Declaration_Request --
   ----------------------------

   overriding function On_Declaration_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Declaration_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response
   is

      Position   : LSP.Messages.TextDocumentPositionParams renames
        Request.params;
      Response   : LSP.Messages.Server_Responses.Location_Link_Response
        (Is_Error => False);
      Imprecise  : Boolean := False;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Position.textDocument.uri);

   begin
      for C of Self.Contexts.Contexts_For_URI (Position.textDocument.uri) loop
         C.Append_Declarations
           (Document,
            Position,
            Response.result,
            Imprecise);

         exit when Request.Canceled;
      end loop;

      if Imprecise then
         Self.Show_Message
           ("The result of 'declaration' is approximate.",
            LSP.Messages.Warning);
      end if;

      Sort_And_Remove_Duplicates (Response.result.Locations);
      return Response;
   end On_Declaration_Request;

   -------------------------------
   -- On_Implementation_Request --
   -------------------------------

   overriding function On_Implementation_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Implementation_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response
   is
      use Libadalang.Analysis;

      Position   : LSP.Messages.TextDocumentPositionParams renames
        Request.params;
      Response   : LSP.Messages.Server_Responses.Location_Link_Response
        (Is_Error => False);
      Imprecise  : Boolean := False;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Position.textDocument.uri);

      procedure Resolve_In_Context (C : Context_Access);
      --  Utility function to gather results on one context

      ------------------------
      -- Resolve_In_Context --
      ------------------------

      procedure Resolve_In_Context (C : Context_Access) is
         Name_Node      : constant Name := LSP.Lal_Utils.Get_Node_As_Name
           (C.Get_Node_At (Document, Position));

         procedure List_Bodies_Of
           (Definition : Defining_Name;
            Kind       : LSP.Messages.AlsReferenceKind_Set);
         --  List all the bodies of Definition, with the given kind as label

         --------------------
         -- List_Bodies_Of --
         --------------------

         procedure List_Bodies_Of
           (Definition : Defining_Name;
            Kind       : LSP.Messages.AlsReferenceKind_Set)
         is
            Next_Part  : Defining_Name;
            Loop_Count : Natural := 0;
            Parents    : constant Ada_Node_Array := Definition.Parents;
         begin
            --  If this happens to be the definition of a subprogram that
            --  does not call for a body, let's consider that this *is* the
            --  implementation. Return this, and do not attempt to look
            --  for secondary implementations in this case.
            if Parents'Length > 2 and then Parents (Parents'First + 2).Kind in
              Libadalang.Common.Ada_Null_Subp_Decl     --  "is null" procedure?
                | Libadalang.Common.Ada_Expr_Function  --  expression function?
            then
               Append_Location (Response.result, Definition, Kind);
               return;
            end if;

            --  If the definition that we found is a body, add this to the list
            if Parents'Length > 2 and then Parents (Parents'First + 2).Kind in
              Libadalang.Common.Ada_Subp_Body
            then
               Append_Location (Response.result, Definition, Kind);
            end if;

            Next_Part := Definition;

            --  Now that we have a definition, list all the implementations for
            --  this definition. We do this by iterating on Find_Next_Part
            loop
               --  Safety net, don't rely on the results making sense, since
               --  the code might be invalid.
               Next_Part := Find_Next_Part (Next_Part, Self.Trace);

               exit when Next_Part = No_Defining_Name;

               Append_Location (Response.result, Next_Part, Kind);

               Loop_Count := Loop_Count + 1;
               if Loop_Count > 5 then
                  Imprecise := True;
                  exit;
               end if;
            end loop;
         end List_Bodies_Of;

         Definition     : Defining_Name;
         This_Imprecise : Boolean;
         Decl           : Basic_Decl;

      begin
         if Name_Node = No_Name then
            return;
         end if;

         --  Find the definition
         Definition := Resolve_Name (Name_Node, Self.Trace, This_Imprecise);
         Imprecise := Imprecise or This_Imprecise;

         --  If we didn't find a definition, give up for this context
         if Definition = No_Defining_Name then
            return;
         end if;

         --  First list the bodies of this definition
         List_Bodies_Of (Definition, LSP.Messages.Empty_Set);

         --  Then list the bodies of the parent implementations
         Decl := Definition.P_Basic_Decl;
         for Subp of C.Find_All_Base_Declarations (Decl, This_Imprecise) loop
            List_Bodies_Of (Subp.P_Defining_Name, Is_Parent);
         end loop;
         Imprecise := Imprecise or This_Imprecise;

         --  And finally the bodies of child implementations
         for Subp of C.Find_All_Overrides (Decl, This_Imprecise) loop
            List_Bodies_Of (Subp.P_Defining_Name, Is_Child);
         end loop;
         Imprecise := Imprecise or This_Imprecise;
      end Resolve_In_Context;

   begin
      for C of Self.Contexts.Contexts_For_URI (Position.textDocument.uri) loop
         Resolve_In_Context (C);

         exit when Request.Canceled;
      end loop;

      if Imprecise then
         Self.Show_Message
           ("The result of 'implementation' is approximate.",
            LSP.Messages.Warning);
      end if;

      Sort_And_Remove_Duplicates (Response.result.Locations);
      return Response;
   end On_Implementation_Request;

   ---------------------------
   -- On_Definition_Request --
   ---------------------------

   overriding function On_Definition_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Definition_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response
   is
      use Libadalang.Analysis;

      Value      : LSP.Messages.TextDocumentPositionParams renames
        Request.params;
      Response   : LSP.Messages.Server_Responses.Location_Link_Response
        (Is_Error => False);
      Imprecise  : Boolean := False;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Value.textDocument.uri);

      procedure Resolve_In_Context (C : Context_Access);
      --  Utility function, appends to Resonse.result all results of the
      --  definition requests found in context C.

      ------------------------
      -- Resolve_In_Context --
      ------------------------

      procedure Resolve_In_Context (C : Context_Access) is
         Name_Node               : constant Name :=
                                     LSP.Lal_Utils.Get_Node_As_Name
                                       (C.Get_Node_At (Document, Value));
         Definition              : Defining_Name;
         Other_Part              : Defining_Name;
         Manual_Fallback         : Defining_Name;
         Decl_For_Find_Overrides : Basic_Decl;
      begin
         if Name_Node = No_Name then
            return;
         end if;

         --  Check if we are on some defining name
         Definition := Get_Name_As_Defining (Name_Node);

         if Definition = No_Defining_Name then
            Self.Imprecise_Resolve_Name
              (C, Value, Definition, LSP.Messages.Info);

            if Definition /= No_Defining_Name then
               Append_Location (Response.result, Definition);

               Decl_For_Find_Overrides := Definition.P_Basic_Decl;
            end if;
         else  --  If we are on a defining_name already
            Other_Part := Find_Next_Part (Definition, Self.Trace);

            Decl_For_Find_Overrides := Definition.P_Basic_Decl;

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

         if Decl_For_Find_Overrides /= Libadalang.Analysis.No_Basic_Decl then
            declare
               Imprecise_Over       : Boolean;
               Imprecise_Base       : Boolean;
               Overriding_Subps     : constant Basic_Decl_Array :=
                 C.Find_All_Overrides
                   (Decl_For_Find_Overrides,
                    Imprecise_Results => Imprecise_Over);
               Base_Subps           : constant Basic_Decl_Array :=
                 C.Find_All_Base_Declarations
                   (Decl_For_Find_Overrides,
                    Imprecise_Results => Imprecise_Base);
            begin
               for Subp of Base_Subps loop
                  Append_Location
                    (Response.result, Subp.P_Defining_Name, Is_Parent);
               end loop;
               for Subp of Overriding_Subps loop
                  Append_Location
                    (Response.result, Subp.P_Defining_Name, Is_Child);
               end loop;
               Imprecise := Imprecise or Imprecise_Over or Imprecise_Base;
            end;
         end if;
      end Resolve_In_Context;

   begin
      for C of Self.Contexts.Contexts_For_URI (Value.textDocument.uri) loop
         Resolve_In_Context (C);

         exit when Request.Canceled;
      end loop;

      if Imprecise then
         Self.Show_Message
           ("The result of 'definition' is approximate.",
            LSP.Messages.Warning);
      end if;

      Sort_And_Remove_Duplicates (Response.result.Locations);
      return Response;
   end On_Definition_Request;

   --------------------------------
   -- On_Type_Definition_Request --
   --------------------------------

   overriding function On_Type_Definition_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Type_Definition_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response
   is
      use Libadalang.Analysis;

      Position   : LSP.Messages.TextDocumentPositionParams renames
        Request.params;
      Response   : LSP.Messages.Server_Responses.Location_Link_Response
        (Is_Error => False);
      Imprecise  : Boolean := False;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Position.textDocument.uri);

      procedure Resolve_In_Context (C : Context_Access);
      --  Utility function to gather results on one context

      ------------------------
      -- Resolve_In_Context --
      ------------------------

      procedure Resolve_In_Context (C : Context_Access) is
         Name_Node      : constant Name := LSP.Lal_Utils.Get_Node_As_Name
             (C.Get_Node_At (Document, Position));
         Definition     : Defining_Name;
         Type_Decl : Base_Type_Decl;
      begin
         if Name_Node = No_Name then
            return;
         end if;

         if Name_Node.P_Is_Defining then
            --  Special case if Name_Node is defining, for instance on the X in
            --      X : My_Type;
            declare
               Def_Name : constant Defining_Name :=
                 Name_Node.P_Enclosing_Defining_Name;
               Type_Expr : constant Libadalang.Analysis.Type_Expr :=
                 Def_Name.P_Basic_Decl.P_Type_Expression;
            begin
               if not Type_Expr.Is_Null then
                  Definition := Resolve_Name
                    (Type_Expr.P_Type_Name, Self.Trace, Imprecise);
               end if;
            end;
         else
            --  Name_Node is not defining. In this case we can rely on
            --  P_Expression_Type.
            Type_Decl := Name_Node.P_Expression_Type;

            --  P_Expression_Type returns the entire expression: narrow the
            --  result down to the type declaration. Here we assume that the
            --  first defining name in this expression is the name of the type.
            if Type_Decl /= No_Type_Decl then
               Definition := Type_Decl.P_Defining_Name;
            end if;
         end if;

         if Definition /= No_Defining_Name then
            Append_Location (Response.result, Definition);
         end if;
      end Resolve_In_Context;

   begin
      for C of Self.Contexts.Contexts_For_URI (Position.textDocument.uri) loop
         Resolve_In_Context (C);

         exit when Request.Canceled;
      end loop;

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
        Get_Open_Document (Self, Value.textDocument.uri);
      Diag     : LSP.Messages.PublishDiagnosticsParams;
      Diags_Already_Published : Boolean := False;
   begin
      if Skip_Did_Change then
         --  Don't process the notification if next message overrides it
         return;
      end if;

      Document.Apply_Changes (Value.contentChanges);

      --  Reindex the document in each of the contexts where it is relevant

      for Context
        of Self.Contexts.Contexts_For_URI (Value.textDocument.uri)
      loop
         Context.Index_Document (Document.all);

         --  Emit diagnostics - do this for only one context
         if Self.Diagnostics_Enabled
           and then not Diags_Already_Published
         then
            Document.Get_Errors (Context.all, Diag.diagnostics);
            Diag.uri := Value.textDocument.uri;
            Self.Server.On_Publish_Diagnostics (Diag);
            Diags_Already_Published := True;
         end if;
      end loop;
   end On_DidChangeTextDocument_Notification;

   ------------------------------------------
   -- On_DidCloseTextDocument_Notification --
   ------------------------------------------

   overriding procedure On_DidCloseTextDocument_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidCloseTextDocumentParams)
   is
      Diag     : LSP.Messages.PublishDiagnosticsParams;
      Document : Internal_Document_Access;
   begin
      if Self.Open_Documents.Contains (Value.textDocument.uri) then
         Document := Self.Open_Documents.Element (Value.textDocument.uri);
         Unchecked_Free (Document);
         Self.Open_Documents.Delete (Value.textDocument.uri);
      else
         --  We have received a didCloseTextDocument but the document was
         --  not open: this is not supposed to happen, log it.

         Self.Trace.Trace
           ("received a didCloseTextDocument for non-open document with uri: "
            & To_UTF_8_String (Value.textDocument.uri));
      end if;

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
      URI    : LSP.Messages.DocumentUri renames Value.textDocument.uri;
      Object : constant Internal_Document_Access :=
        new LSP.Ada_Documents.Document (Self.Trace);
   begin
      Self.Trace.Trace ("In Text_Document_Did_Open");
      Self.Trace.Trace ("Uri : " & To_UTF_8_String (URI));

      --  Some clients don't properly call initialize, or don't pass the
      --  project to didChangeConfiguration: fallback here on loading a
      --  project in this directory, if needed.
      Ensure_Project_Loaded
        (Self,
         To_LSP_String (Ada.Directories.Containing_Directory
           (To_UTF_8_String (URI_To_File (URI)))));

      --  We have received a document: add it to the documents container
      Object.Initialize (URI, Value.textDocument.text);
      Self.Open_Documents.Insert (URI, Object);

      --  Index the document in all the contexts where it is relevant
      declare
         Diag : LSP.Messages.PublishDiagnosticsParams;
         Diags_Already_Published : Boolean := False;
      begin
         for Context of Self.Contexts.Contexts_For_URI (URI) loop
            Context.Index_Document (Object.all);

            if Self.Diagnostics_Enabled
              and then not Diags_Already_Published
            then
               Object.Get_Errors (Context.all, Diag.diagnostics);
               Diag.uri := Value.textDocument.uri;
               Self.Server.On_Publish_Diagnostics (Diag);

               --  Publish diagnostics only for one context,
               --  to avoid emitting too much noise.
               Diags_Already_Published := True;
            end if;
         end loop;
      end;

      Self.Trace.Trace ("Finished Text_Document_Did_Open");
   end On_DidOpenTextDocument_Notification;

   ------------------------------
   -- On_Folding_Range_Request --
   ------------------------------

   overriding function On_Folding_Range_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Folding_Range_Request)
      return LSP.Messages.Server_Responses.FoldingRange_Response
   is
      pragma Unreferenced (Self, Request);
      Response : LSP.Messages.Server_Responses.FoldingRange_Response
        (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Messages.InternalError,
          message => To_LSP_String ("Not implemented"),
          data => <>));
      return Response;
   end On_Folding_Range_Request;

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

      C : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);
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
         Decl_Text := Get_Hover_Text (Decl);
      else

         --  Try to retrieve the subprogram spec node, if any: if it's a
         --  subprogram node that does not have any separate declaration we
         --  only want to display its specification, not the body.
         Subp_Spec_Node := Decl.P_Subp_Spec_Or_Null;

         if Subp_Spec_Node /= No_Base_Subp_Spec then
            Decl_Text := Get_Hover_Text (Subp_Spec_Node);

            --  Append the aspects to the declaration text, if any.
            declare
               Aspects      : constant Aspect_Spec := Decl.F_Aspects;
               Aspects_Text : LSP_String;
            begin
               if not Aspects.Is_Null then
                  for Aspect of Aspects.F_Aspect_Assocs loop
                     if Aspects_Text /= Empty_LSP_String then
                        --  need to add "," for the highlighting
                        Append (Aspects_Text, To_LSP_String (","));
                     end if;

                     Append (Aspects_Text, Get_Hover_Text (Aspect));
                  end loop;

                  if Aspects_Text /= Empty_LSP_String then
                     Decl_Text := Decl_Text
                       & To_LSP_String (ASCII.LF & "with")
                       & Aspects_Text;
                  end if;
               end if;
            end;

         else
            Decl_Text := Get_Hover_Text (Decl);
         end if;
      end if;

      if Decl_Text = Empty_LSP_String then
         return Response;
      end if;

      --  Append the whole declaration text to the response

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
      use Libadalang.Common;

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

      function Is_End_Label (Node : Ada_Node) return Boolean
      is
        (not Node.Parent.Is_Null
         and then
           (Node.Parent.Kind in Ada_End_Name
            or else (Node.Parent.Kind in Ada_Dotted_Name
                     and then not Node.Parent.Parent.Is_Null
                     and then Node.Parent.Parent.Kind in Ada_End_Name)));
      --  Return True if the node belongs to an end label node.
      --  Used to filter out end label references.

      function Is_Type_Derivation (Node : Ada_Node) return Boolean
      is
        (not Node.Parent.Is_Null
         and then
           (Node.Parent.Kind in Ada_Subtype_Indication_Range
            and then not Node.Parent.Parent.Is_Null
            and then Node.Parent.Parent.Kind in Ada_Derived_Type_Def_Range));
      --  Return True if the node belongs to derived type declaration.

      ------------------------
      -- Get_Reference_Kind --
      ------------------------

      function Get_Reference_Kind
        (Node : Ada_Node) return LSP.Messages.AlsReferenceKind_Set
      is
         use LSP.Messages;

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

         begin
            Result.As_Flags (LSP.Messages.Child) :=
              Is_Type_Derivation (Id.As_Ada_Node);
         exception
            when E : Libadalang.Common.Property_Error =>
               Log (Self.Trace, E);
         end;

         --  If the result has not any set flags at this point, flag it as a
         --  simple reference.
         if Result.As_Flags = AlsReferenceKind_Array'(others => False) then
            Result.As_Flags (LSP.Messages.Simple) := True;
         end if;

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
               if not Is_End_Label (Node.As_Ada_Node) then
                  Count := Count - 1;

                  Append_Location
                    (Response.result,
                     Node,
                     Get_Reference_Kind (Node.As_Ada_Node));
               end if;

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
      for C of Self.Contexts.Contexts_For_URI (Value.textDocument.uri) loop
         Process_Context (C);

         exit when Request.Canceled;
      end loop;

      if Imprecise then
         Self.Show_Message
           ("The results of 'references' are approximate.",
            LSP.Messages.Warning);
      end if;

      Sort_And_Remove_Duplicates (Response.result);
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
                  Sort_And_Remove_Duplicates (Subp_And_Refs.refs);
                  Response.result.Append (Subp_And_Refs);
                  Next (C);
               end;
            end loop;
         end;
      end Process_Context;

   begin
      --  Find the references in all contexts
      for C of Self.Contexts.Contexts_For_URI (Value.textDocument.uri) loop
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

   -----------------------------------
   -- On_Color_Presentation_Request --
   -----------------------------------

   overriding function On_Color_Presentation_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Color_Presentation_Request)
      return LSP.Messages.Server_Responses.ColorPresentation_Response
   is
      pragma Unreferenced (Self, Request);
      Response : LSP.Messages.Server_Responses.ColorPresentation_Response
        (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Messages.InternalError,
          message => To_LSP_String ("Not implemented"),
          data => <>));
      return Response;
   end On_Color_Presentation_Request;

   -------------------------------
   -- On_Document_Color_Request --
   -------------------------------

   overriding function On_Document_Color_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Color_Request)
      return LSP.Messages.Server_Responses.DocumentColor_Response
   is
      pragma Unreferenced (Self, Request);
      Response : LSP.Messages.Server_Responses.DocumentColor_Response
        (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Messages.InternalError,
          message => To_LSP_String ("Not implemented"),
          data => <>));
      return Response;
   end On_Document_Color_Request;

   -------------------------------
   -- On_Document_Links_Request --
   -------------------------------

   overriding function On_Document_Links_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Links_Request)
      return LSP.Messages.Server_Responses.Links_Response
   is
      pragma Unreferenced (Self, Request);
      Response : LSP.Messages.Server_Responses.Links_Response
        (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Messages.InternalError,
          message => To_LSP_String ("Not implemented"),
          data => <>));
      return Response;
   end On_Document_Links_Request;

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
        Get_Open_Document (Self, Value.textDocument.uri);
      Context  : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);

   begin
      return Result : LSP.Messages.Server_Responses.Symbol_Response :=
        (Is_Error => False,
         result   => <>,
         error    => (Is_Set => False),
         others   => <>)
      do
         Self.Get_Symbols (Document.all, Context.all, Result.result);
      end return;
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

      Value     : LSP.Messages.RenameParams renames Request.params;
      Response  : LSP.Messages.Server_Responses.Rename_Response
        (Is_Error => False);

      Document  : constant LSP.Ada_Documents.Document_Access :=
                   Get_Open_Document (Self, Value.textDocument.uri);

      procedure Process_Context (C : Context_Access);
      --  Process the rename request for the given context, and add
      --  the results to response.

      ---------------------
      -- Process_Context --
      ---------------------

      procedure Process_Context (C : Context_Access) is
         Position : constant LSP.Messages.TextDocumentPositionParams :=
                      (Value.textDocument, Value.position);

         Name_Node  : constant Name := LSP.Lal_Utils.Get_Node_As_Name
           (C.Get_Node_At (Document, Position));

         Definition : Defining_Name;
         Imprecise  : Boolean;

         Empty      : LSP.Messages.TextEdit_Vector;

         procedure Process_Comments
           (Node : Ada_Node;
            Uri  : LSP.Messages.DocumentUri);
         --  Iterate over all comments and include them in the response when
         --  they contain a renamed word

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
            Count      : Cancel_Countdown := 0;
            References : constant Base_Id_Array :=
                           C.Get_References_For_Renaming
                             (Definition,
                              Imprecise_Results => Imprecise);
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
      for C of Self.Contexts.Contexts_For_URI (Value.textDocument.uri) loop
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

      projectFile            : constant String := "projectFile";
      scenarioVariables      : constant String := "scenarioVariables";
      defaultCharset         : constant String := "defaultCharset";
      enableDiagnostics      : constant String := "enableDiagnostics";
      enableIndexing         : constant String := "enableIndexing";
      renameInComments       : constant String := "renameInComments";

      Ada       : constant LSP.Types.LSP_Any := Value.settings.Get ("ada");
      File      : LSP.Types.LSP_String;
      Charset   : Unbounded_String;
      Variables : LSP.Types.LSP_Any;
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

         --  Similarly to diagnostics, we support selectively activating
         --  indexing in the parameters to this request.
         if Ada.Has_Field (enableIndexing) then
            Self.Indexing_Enabled := Ada.Get (enableIndexing);
         end if;

         --  Retrieve the different textDocument/rename options if specified

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
            Self.Project_Environment.Change_Environment (Name, Value.Get);
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
      begin
         C.Initialize;
         C.Load_Project (Tree    => Self.Project_Tree,
                         Root    => P,
                         Charset => Charset);
         Self.Contexts.Prepend (C);
      end Create_Context_For_Non_Aggregate;

   begin
      --  Unload all the contexts
      Self.Contexts.Cleanup;

      --  Unload the project tree and the project environment
      Self.Release_Project_Info;

      --  Now load the new project
      Errors.the_type := LSP.Messages.Warning;
      Initialize (Self.Project_Environment);
      if not Scenario.Is_Empty then
         Scenario.Map_JSON_Object (Add_Variable'Access);
      end if;

      begin
         Self.Project_Tree := new Project_Tree;
         Self.Project_Tree.Load
           (GPR,
            Self.Project_Environment,
            Errors => On_Error'Unrestricted_Access);
         if Self.Project_Tree.Root_Project.Is_Aggregate_Project then
            declare
               Aggregated : Project_Array_Access :=
                 Self.Project_Tree.Root_Project.Aggregated_Projects;
            begin
               for X of Aggregated.all loop
                  Create_Context_For_Non_Aggregate (X);
               end loop;
               Unchecked_Free (Aggregated);
            end;
         else
            Create_Context_For_Non_Aggregate (Self.Project_Tree.Root_Project);
         end if;
      exception
         when E : Invalid_Project =>
            Self.Release_Project_Info;

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

      --  Reindex all open documents immediately after project reload, so
      --  that navigation from editors is accurate.
      for Document of Self.Open_Documents loop
         for Context of Self.Contexts.Contexts_For_URI (Document.URI) loop
            Context.Index_Document (Document.all);
         end loop;
      end loop;

      --  Reindex the files from disk in the background after a project reload
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

   procedure Index_Files
     (Self             : access Message_Handler;
      Has_Pending_Work : Boolean)
   is

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
      Total           : constant Natural := Self.Contexts.Total_Source_Files;
      Last_Percent    : Natural := 0;
      Current_Percent : Natural := 0;
      Context         : Context_Access;
   begin
      --  Prevent work if the indexing has been explicitly disabled
      if not Self.Indexing_Enabled then
         return;
      end if;

      Emit_Progress_Begin;

      for E in Self.Contexts.Each_Context loop
         Context := LSP.Ada_Context_Sets.Context_Lists.Element (E);

         for F of Context.List_Files loop
            Current_Percent := (Index * 100) / Total;
            --  If the value of the indexing increased by at least one percent,
            --  emit one progress report.
            if Current_Percent > Last_Percent then
               Emit_Progress_Report (Current_Percent);
               Last_Percent := Current_Percent;
            end if;

            Context.Index_File (F);
            Index := Index + 1;

            --  Check whether another request is pending. If so, pause the
            --  indexing; it will be resumed later as part of After_Request.
            --  if Self.Server.Input_Queue_Length > 0 then
            if Has_Pending_Work then
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
        Get_Open_Document (Self, Value.textDocument.uri);
      Context  : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);
      Response : LSP.Messages.Server_Responses.Completion_Response
        (Is_Error => False);
   begin
      Document.Get_Completions_At
        (Context.all, Value.position, Response.result);
      return Response;
   end On_Completion_Request;

   ------------------
   -- Handle_Error --
   ------------------

   overriding procedure Handle_Error
     (Self : access Message_Handler) is
   begin
      --  Reload the contexts in case of unexpected errors.
      Self.Contexts.Reload_All_Contexts;
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
     (Self             : access Message_Handler;
      Message          : LSP.Messages.Message'Class;
      Has_Pending_Work : Boolean)
   is
      pragma Unreferenced (Message);
   begin
     --  We have finished processing a request or notification:
     --  if it happens that indexing is required, do it now.
      if Self.Indexing_Required then
         Self.Index_Files (Has_Pending_Work);
      end if;
   end After_Work;

end LSP.Ada_Handlers;

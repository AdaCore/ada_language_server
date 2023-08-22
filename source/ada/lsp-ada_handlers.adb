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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with GNATCOLL.Traces;

with Libadalang.Analysis;
with Libadalang.Common;

with Laltools.Common;

with Langkit_Support.Slocs;

with LSP.Ada_Completions;
with LSP.Ada_Completions.Aspects;
with LSP.Ada_Completions.Attributes;
with LSP.Ada_Completions.End_Names;
with LSP.Ada_Completions.Keywords;
with LSP.Ada_Completions.Names;
with LSP.Ada_Completions.Parameters;
with LSP.Ada_Completions.Pragmas;
with LSP.Ada_Completions.Use_Clauses;
with LSP.Ada_Contexts;
with LSP.Ada_Handlers.Call_Hierarchy;
with LSP.Ada_Handlers.Invisibles;
with LSP.Ada_Handlers.Locations;
with LSP.Ada_Handlers.Project_Diagnostics;
with LSP.Ada_Handlers.Project_Loading;
with LSP.Diagnostic_Sources;
with LSP.Enumerations;
with LSP.Generic_Cancel_Check;
with LSP.GNATCOLL_Tracers.Handle;
with LSP.Server_Notifications.DidChange;
with LSP.Servers;
with LSP.Utils;

package body LSP.Ada_Handlers is

   pragma Style_Checks ("o");  --  check subprogram bodies in alphabetical ordr

   subtype AlsReferenceKind_Array is
     LSP.Ada_Handlers.Locations.AlsReferenceKind_Array;

   function Is_Parent return AlsReferenceKind_Array is
     ([LSP.Ada_Handlers.Locations.Parent => True, others => False]);

   function Is_Child return AlsReferenceKind_Array is
     ([LSP.Ada_Handlers.Locations.Child => True, others => False]);

   function Contexts_For_URI
     (Self : access Message_Handler;
      URI  : LSP.Structures.DocumentUri)
      return LSP.Ada_Context_Sets.Context_Lists.List;
   --  Return a list of contexts that are suitable for the given File/URI:
   --  a list of all contexts where the file is known to be part of the
   --  project tree, or is a runtime file for this project. If the file
   --  is not known to any project, return an empty list.

   procedure Publish_Diagnostics
     (Self              : in out Message_Handler'Class;
      Document          : not null LSP.Ada_Documents.Document_Access;
      Other_Diagnostics : LSP.Structures.Diagnostic_Vector :=
        LSP.Structures.Empty;
      Force             : Boolean := False);
   --  Publish diagnostic messages for given document if needed.
   --  Other_Diagnostics can be used to specify punctual diagnostics not coming
   --  from sources that analyze files when being opened or modified.
   --  When Force is True, the diagnostics will always be sent, not matter if
   --  they have changed or not.

   procedure Clean_Diagnostics
     (Self     : in out Message_Handler'Class;
      Document : not null LSP.Ada_Documents.Document_Access);
   --  Clean diagnostics up for the document

   function To_DocumentUri (X : VSS.Strings.Virtual_String)
     return LSP.Structures.DocumentUri is (X with null record);

   function To_DocumentUri
     (X : LSP.Structures.URI)
      return LSP.Structures.DocumentUri is
     (VSS.Strings.Virtual_String (X) with null record);

   EmptyDocumentUri : constant LSP.Structures.DocumentUri :=
     To_DocumentUri (VSS.Strings.Empty_Virtual_String);

   procedure Log_Method_In
     (Self : in out Message_Handler;
      Name : String;
      URI  : LSP.Structures.DocumentUri := EmptyDocumentUri);

   procedure Log_Method_Out
     (Self : in out Message_Handler;
      Name : String);
   --  Save method in/out in a log file

   function To_LSP_Location
     (Self : in out Message_Handler'Class;
      Node : Libadalang.Analysis.Ada_Node'Class)
      return LSP.Structures.Location
        renames LSP.Ada_Handlers.Locations.To_LSP_Location;

   function Get_Node_At
     (Self     : in out Message_Handler'Class;
      Context  : LSP.Ada_Contexts.Context;
      Value    : LSP.Structures.TextDocumentPositionParams'Class)
      return Libadalang.Analysis.Ada_Node
        renames LSP.Ada_Handlers.Locations.Get_Node_At;

   procedure Append_Location
     (Self   : in out Message_Handler;
      Result : in out LSP.Structures.Location_Vector;
      Node   : Libadalang.Analysis.Ada_Node'Class;
      Ignore : AlsReferenceKind_Array := LSP.Ada_Handlers.Locations.Empty)
        renames LSP.Ada_Handlers.Locations.Append_Location;

   function Imprecise_Resolve_Name
     (Self     : in out Message_Handler'Class;
      Context  : LSP.Ada_Contexts.Context;
      Position : LSP.Structures.TextDocumentPositionParams'Class)
        return Libadalang.Analysis.Defining_Name;

   -----------------------
   -- Clean_Diagnostics --
   -----------------------

   procedure Clean_Diagnostics
     (Self     : in out Message_Handler'Class;
      Document : not null LSP.Ada_Documents.Document_Access)
   is
      Diag : LSP.Structures.PublishDiagnosticsParams;
   begin
      if Self.Configuration.Diagnostics_Enabled then
         Diag.uri := Document.URI;
         Self.Sender.On_PublishDiagnostics_Notification (Diag);
      end if;
   end Clean_Diagnostics;

   ----------------------
   -- Contexts_For_URI --
   ----------------------

   function Contexts_For_URI
     (Self : access Message_Handler;
      URI  : LSP.Structures.DocumentUri)
      return LSP.Ada_Context_Sets.Context_Lists.List
   is
      function Is_A_Source (Self : LSP.Ada_Contexts.Context) return Boolean is
        (Self.Is_Part_Of_Project (URI));
      --  Return True if URI is a source of the project held by Context

      File : constant GNATCOLL.VFS.Virtual_File := Self.To_File (URI);
   begin
      --  If the file does not exist on disk, assume this is a file
      --  being created and, as a special convenience in this case,
      --  assume it could belong to any project.
      if not File.Is_Regular_File
      --  If the file is a runtime file for the loaded project environment,
      --  all projects can see it.
        or else Self.Project_Predefined_Sources.Contains (File)
      then
         return Self.Contexts.Each_Context;
      end if;

      --  List contexts where File is a source of the project hierarchy
      return Self.Contexts.Each_Context (Is_A_Source'Unrestricted_Access);
   end Contexts_For_URI;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Internal_Document_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (LSP.Ada_Documents.Document, Internal_Document_Access);
   begin
      Self.Cleanup;
      Unchecked_Free (Self);
   end Free;

   -----------------------
   -- Get_Open_Document --
   -----------------------

   function Get_Open_Document
     (Self  : in out Message_Handler;
      URI   : LSP.Structures.DocumentUri;
      Force : Boolean := False)
      return LSP.Ada_Documents.Document_Access
   is
      File : constant GNATCOLL.VFS.Virtual_File := Self.To_File (URI);
   begin
      Project_Loading.Ensure_Project_Loaded (Self);

      if Self.Open_Documents.Contains (File) then
         return LSP.Ada_Documents.Document_Access
           (Self.Open_Documents.Element (File));
      elsif Force then
         declare
            Document : constant Internal_Document_Access :=
              new LSP.Ada_Documents.Document (Self.Tracer);
         begin
            Document.Initialize (URI, VSS.Strings.Empty_Virtual_String, null);
            return LSP.Ada_Documents.Document_Access (Document);
         end;
      else
         return null;
      end if;
   end Get_Open_Document;

   ----------------------------
   -- Imprecise_Resolve_Name --
   ----------------------------

   function Imprecise_Resolve_Name
     (Self     : in out Message_Handler'Class;
      Context  : LSP.Ada_Contexts.Context;
      Position : LSP.Structures.TextDocumentPositionParams'Class)
        return Libadalang.Analysis.Defining_Name
   is
      Trace     : constant GNATCOLL.Traces.Trace_Handle :=
        LSP.GNATCOLL_Tracers.Handle (Self.Tracer.all);

      Name_Node  : constant Libadalang.Analysis.Name :=
        Laltools.Common.Get_Node_As_Name
          (Self.Get_Node_At (Context, Position));

      Imprecise : Boolean;
   begin
      if Name_Node.Is_Null then
         return Libadalang.Analysis.No_Defining_Name;
      end if;

      return Laltools.Common.Resolve_Name
        (Name_Node,
         Trace,
         Imprecise => Imprecise);
   end Imprecise_Resolve_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Message_Handler'Class;
      Incremental_Text_Changes : Boolean) is
   begin
      Self.Incremental_Text_Changes := Incremental_Text_Changes;
   end Initialize;

   -------------------
   -- Log_Method_In --
   -------------------

   procedure Log_Method_In
     (Self : in out Message_Handler;
      Name : String;
      URI  : LSP.Structures.DocumentUri := EmptyDocumentUri) is
   begin
      if not URI.Is_Empty then
         Self.Tracer.Trace ("In Message_Handler " & Name & " URI:");
         Self.Tracer.Trace_Text (URI);
      else
         Self.Tracer.Trace ("In Message_Handler " & Name);
      end if;
   end Log_Method_In;

   --------------------
   -- Log_Method_Out --
   --------------------

   procedure Log_Method_Out
     (Self : in out Message_Handler;
      Name : String) is
   begin
      Self.Tracer.Trace ("Out Message_Handler " & Name);
   end Log_Method_Out;

   ---------------------------
   -- On_Completion_Request --
   ---------------------------

   overriding procedure On_Completion_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionParams)
   is
      --  We're completing only based on one context, ie one project
      --  tree: this seems reasonable. One further refinement could
      --  be to return only results that are available for all
      --  project contexts.

      Context  : constant LSP.Ada_Context_Sets.Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Value.textDocument.uri);

      Names    : LSP.Ada_Completions.Completion_Maps.Map;

      --  If lazy computation for the 'detail' and 'documentation' fields is
      --  supported by the client, set the Compute_Doc_And_Details flag to
      --  False.
      Compute_Doc_And_Details : constant Boolean :=
        not Self.Client.Resolve_Lazily;

      P1 : aliased LSP.Ada_Completions.Aspects.Aspect_Completion_Provider;
      P2 : aliased LSP.Ada_Completions.Pragmas.Pragma_Completion_Provider;
      P3 : aliased LSP.Ada_Completions.Keywords.Keyword_Completion_Provider;
      P4 : aliased
        LSP.Ada_Completions.Attributes.Attributes_Completion_Provider;

      P5 : aliased LSP.Ada_Completions.Names.Name_Completion_Provider
        (Self.Configuration.Use_Completion_Snippets);
      P6 : aliased LSP.Ada_Handlers.Invisibles.Invisible_Completion_Provider
        (Self'Access, Context);
      P7 : aliased
        LSP.Ada_Completions.Parameters.Parameter_Completion_Provider
          (Context                  => Context,
           Document                 => Document,
           Compute_Doc_And_Details  => Compute_Doc_And_Details,
           Named_Notation_Threshold =>
             Self.Configuration.Named_Notation_Threshold);
      P8 : aliased LSP.Ada_Completions.End_Names.End_Name_Completion_Provider;
      P9 : aliased
        LSP.Ada_Completions.Use_Clauses.Use_Clause_Completion_Provider;
      Providers : constant LSP.Ada_Completions.Completion_Provider_List :=
        [P1'Unchecked_Access,
         P2'Unchecked_Access,
         P3'Unchecked_Access,
         P4'Unchecked_Access,
         P5'Unchecked_Access,
         P6'Unchecked_Access,
         P7'Unchecked_Access,
         P8'Unchecked_Access,
         P9'Unchecked_Access];

      Sloc  : Langkit_Support.Slocs.Source_Location;
      Token : Libadalang.Common.Token_Reference;
      Node  : Libadalang.Analysis.Ada_Node;

      Response : LSP.Structures.Completion_Result
        (Kind => LSP.Structures.Varian_2);
   begin
      Response.Varian_2.isIncomplete := False;

      Document.Get_Completion_Node
        (Context  => Context.all,
         Position => Value.position,
         Sloc     => Sloc,
         Token    => Token,
         Node     => Node);

      Document.Get_Completions_At
        (Context   => Context.all,
         Providers => Providers,
         Sloc      => Sloc,
         Token     => Token,
         Node      => Node,
         Names     => Names,
         Result    => Response.Varian_2);

      LSP.Ada_Completions.Write_Completions
        (Context                  => Context.all,
         Document                 => Document.all,
         Sloc                     => Sloc,
         Node                     => Node,
         Names                    => Names,
         Named_Notation_Threshold =>
           Self.Configuration.Named_Notation_Threshold,
         Compute_Doc_And_Details  => Compute_Doc_And_Details,
         Result                   => Response.Varian_2.items);

      Self.Sender.On_Completion_Response (Id, Response);
   end On_Completion_Request;

   ----------------------------
   -- On_Declaration_Request --
   ----------------------------

   overriding procedure On_Declaration_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DeclarationParams)
   is
      use Libadalang.Analysis;
      use all type LSP.Ada_Configurations.
        DisplayMethodAncestryOnNavigationPolicy;

      procedure Resolve_In_Context (C : LSP.Ada_Context_Sets.Context_Access);
      --  Utility function, appends to Vector all results of the
      --  declaration requests found in context C.

      Response   : LSP.Structures.Declaration_Result (LSP.Structures.Varian_1);
      Vector     : LSP.Structures.Location_Vector renames Response.Varian_1;

      Display_Method_Policy : constant
        LSP.Ada_Configurations.DisplayMethodAncestryOnNavigationPolicy :=
          Self.Configuration.Display_Method_Ancestry_Policy;

      ------------------------
      -- Resolve_In_Context --
      ------------------------

      procedure Resolve_In_Context (C : LSP.Ada_Context_Sets.Context_Access) is
         Trace      : constant GNATCOLL.Traces.Trace_Handle :=
           LSP.GNATCOLL_Tracers.Handle (Self.Tracer.all);

         Name_Node               : constant Name :=
           Laltools.Common.Get_Node_As_Name (Self.Get_Node_At (C.all, Value));

         Definition              : Libadalang.Analysis.Defining_Name;
         --  A defining name that corresponds to Name_Node
         First_Part              : Libadalang.Analysis.Defining_Name;
         --  "Canonical part" of Definition
         Prev_Part               : Libadalang.Analysis.Defining_Name;
         --  A previous name for Definition
         Decl_For_Find_Overrides : Libadalang.Analysis.Basic_Decl :=
           Libadalang.Analysis.No_Basic_Decl;

         On_Defining_Name        : Boolean := False;
         --  Set to True if we are on a denfining name node

         Is_Imprecise            : Boolean;
      begin
         if Name_Node.Is_Null then
            return;
         end if;

         --  Check if we are on some defining name
         Definition := Laltools.Common.Get_Name_As_Defining (Name_Node);

         if Definition.Is_Null then
            --  If we aren't on a defining_name already then try to resolve
            Definition := Laltools.Common.Resolve_Name
              (Name_Node, Trace, Is_Imprecise);
         else
            On_Defining_Name := True;
         end if;

         if Definition.Is_Null then
            return;  --  Name resolution fails, nothing to do.
         end if;

         --  Display the method ancestry in three cases:
         --
         --   . When the preference is set to Always
         --
         --   . When we are on a usage node (e.g: subprogram call) and if the
         --     preference is set to Usage_And_Abstract_Only
         --
         --   . When we are on a defining name node and if the preference is
         --     set to Definition_Only

         if Display_Method_Policy = Always
           or else (Display_Method_Policy = Usage_And_Abstract_Only
                   and then not On_Defining_Name)
           or else (Display_Method_Policy = Definition_Only
                   and then On_Defining_Name)
         then
            First_Part := Laltools.Common.Find_Canonical_Part (Definition, Trace);

            if First_Part.Is_Null then
               Decl_For_Find_Overrides := Definition.P_Basic_Decl;
            else
               Decl_For_Find_Overrides := First_Part.P_Basic_Decl;
            end if;
         end if;

         begin
            Prev_Part := Definition.P_Previous_Part;
         exception
            when E :  Libadalang.Common.Property_Error =>
               Self.Tracer.Trace_Exception (E);
               Prev_Part := Libadalang.Analysis.No_Defining_Name;
         end;

         if not Prev_Part.Is_Null then
            --  We have found previous part, return it.
            Self.Append_Location (Vector, Prev_Part);
         elsif not Definition.Is_Null then
            --  No previous part, return definition itself.
            Self.Append_Location (Vector, Definition);
         end if;

         if not Decl_For_Find_Overrides.Is_Null then
            declare
               Overridings : constant Libadalang.Analysis.Basic_Decl_Array :=
                 C.Find_All_Overrides
                   (Decl_For_Find_Overrides,
                    Imprecise_Results => Is_Imprecise);

               Bases       : constant Libadalang.Analysis.Basic_Decl_Array :=
                 C.Find_All_Base_Declarations
                   (Decl_For_Find_Overrides,
                    Imprecise_Results => Is_Imprecise);
            begin
               for Subp of Bases loop
                  Self.Append_Location
                    (Vector, Subp.P_Defining_Name, Is_Parent);
               end loop;

               for Subp of Overridings loop
                  Self.Append_Location
                    (Vector, Subp.P_Defining_Name, Is_Child);
               end loop;
            end;
         end if;
      end Resolve_In_Context;

   begin
      --  Override the displayMethodAncestryOnNavigation global configuration
      --  flag if there is on embedded in the request.
      --  if Value.alsDisplayMethodAncestryOnNavigation.Is_Set then
      --     Display_Method_Ancestry_Policy :=
      --       Value.alsDisplayMethodAncestryOnNavigation.Value;
      --  end if;

      for C of Self.Contexts_For_URI (Value.textDocument.uri) loop
         Resolve_In_Context (C);

         exit when Self.Is_Canceled.all;
      end loop;

      --  Sort_And_Remove_Duplicates (Response.result.Locations);

      Self.Sender.On_Declaration_Response (Id, Response);
   end On_Declaration_Request;

   ---------------------------
   -- On_Definition_Request --
   ---------------------------

   overriding procedure On_Definition_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DefinitionParams)
   is
      use Libadalang.Analysis;
      use all type LSP.Ada_Configurations.
        DisplayMethodAncestryOnNavigationPolicy;

      Trace      : constant GNATCOLL.Traces.Trace_Handle :=
        LSP.GNATCOLL_Tracers.Handle (Self.Tracer.all);

      Response   : LSP.Structures.Definition_Result (LSP.Structures.Varian_1);
      Vector     : LSP.Structures.Location_Vector renames Response.Varian_1;

      Imprecise  : Boolean := False;

      Display_Method_Ancestry_Policy : constant
        LSP.Ada_Configurations.DisplayMethodAncestryOnNavigationPolicy :=
          Self.Configuration.Display_Method_Ancestry_Policy;

      procedure Resolve_In_Context (C : LSP.Ada_Context_Sets.Context_Access);
      --  Utility function, appends to Vector all results of the
      --  definition requests found in context C.

      ------------------------
      -- Resolve_In_Context --
      ------------------------

      procedure Resolve_In_Context (C : LSP.Ada_Context_Sets.Context_Access) is
         use Libadalang.Common;

         Name_Node               : constant Name :=
           Laltools.Common.Get_Node_As_Name (Self.Get_Node_At (C.all, Value));

         Definition              : Defining_Name;
         Other_Part              : Defining_Name;
         Manual_Fallback         : Defining_Name;
         Definition_Node         : Basic_Decl := No_Basic_Decl;
         Decl_For_Find_Overrides : Basic_Decl := No_Basic_Decl;
         Entry_Decl_Node         : Entry_Decl := No_Entry_Decl;
      begin
         if Name_Node.Is_Null then
            return;
         end if;

         --  Check if we are on some defining name
         Definition := Laltools.Common.Get_Name_As_Defining (Name_Node);

         if Definition.Is_Null then
            Definition := Laltools.Common.Resolve_Name
              (Name_Node,
               Trace,
               Imprecise => Imprecise);

            if not Definition.Is_Null then
               Self.Append_Location (Vector, Definition);

               if Display_Method_Ancestry_Policy
                  in Usage_And_Abstract_Only | Always
               then
                  Decl_For_Find_Overrides := Definition.P_Basic_Decl;
               end if;
            end if;
         else  --  If we are on a defining_name already
            Other_Part := Laltools.Common.Find_Next_Part (Definition, Trace);

            Definition_Node := Definition.P_Basic_Decl;

            --  Search for overriding subprograms only if we are on an
            --  abstract subprogram.
            if Display_Method_Ancestry_Policy /= Never
              and then
                (Display_Method_Ancestry_Policy /= Usage_And_Abstract_Only
                  or else Definition_Node.Kind in Ada_Abstract_Subp_Decl_Range)
            then
               Decl_For_Find_Overrides := Definition_Node;
            end if;

            --  Search for accept statements only if we are on an entry
            if Definition_Node.Kind in Ada_Entry_Decl_Range then
               Entry_Decl_Node := Definition_Node.As_Entry_Decl;

            elsif Definition_Node.Kind in
              Ada_Single_Task_Type_Decl_Range | Ada_Protected_Type_Decl_Range
            then
               --  These node types are not handled by Find_Next_Part
               --  (LAL design limitations)
               declare
                  Other_Part_For_Decl : constant Basic_Decl :=
                    Laltools.Common.Find_Next_Part_For_Decl
                      (Definition_Node, Trace);
               begin
                  if not Other_Part_For_Decl.Is_Null then
                     Other_Part := Other_Part_For_Decl.P_Defining_Name;
                  end if;
               end;
            end if;

            if Other_Part.Is_Null then
               --  No next part is found. Check first defining name
               Other_Part := Laltools.Common.Find_Canonical_Part
                 (Definition, Trace);
            end if;

            if not Other_Part.Is_Null then
               Self.Append_Location (Vector, Other_Part);

            else
               --  We were on a defining name, but did not manage to find
               --  an answer using Find_Next_Part / Find_Canonical_Part.
               --  Use the manual fallback to attempt to find a good enough
               --  result.
               Manual_Fallback := Laltools.Common.Find_Other_Part_Fallback
                 (Definition, Trace);

               if not Manual_Fallback.Is_Null then
                  --  We have found a result using the imprecise heuristics.
                  --  We'll warn the user and send the result.
                  Imprecise := True;
                  Self.Append_Location (Vector, Manual_Fallback);
               end if;
            end if;
         end if;

         if not Decl_For_Find_Overrides.Is_Null then
            declare
               Overridings : constant Basic_Decl_Array :=
                 C.Find_All_Overrides
                   (Decl_For_Find_Overrides,
                    Imprecise_Results => Imprecise);

               Bases       : constant Basic_Decl_Array :=
                 C.Find_All_Base_Declarations
                   (Decl_For_Find_Overrides,
                    Imprecise_Results => Imprecise);
            begin
               for Subp of Bases loop
                  Self.Append_Location
                    (Vector, Subp.P_Defining_Name, Is_Parent);
               end loop;

               for Subp of Overridings loop
                  Self.Append_Location
                    (Vector, Subp.P_Defining_Name, Is_Child);
               end loop;
            end;
         end if;

         if not Entry_Decl_Node.Is_Null then
            for Accept_Node of Entry_Decl_Node.P_Accept_Stmts loop
               Self.Append_Location
                 (Vector, Accept_Node.F_Body_Decl.F_Name);
            end loop;
         end if;
      end Resolve_In_Context;

   begin
      --  Override the displayMethodAncestryOnNavigation global configuration
      --  flag if there is on embedded in the request.
      --  if Value.alsDisplayMethodAncestryOnNavigation.Is_Set then
      --     Display_Method_Ancestry_Policy :=
      --       Value.alsDisplayMethodAncestryOnNavigation.Value;
      --  end if;

      for C of Self.Contexts_For_URI (Value.textDocument.uri) loop
         Resolve_In_Context (C);

         exit when Self.Is_Canceled.all;
      end loop;

      --  Sort_And_Remove_Duplicates (Vector);

      Self.Sender.On_Definition_Response (Id, Response);
   end On_Definition_Request;

   -------------------------------
   -- On_DidChange_Notification --
   -------------------------------

   overriding procedure On_DidChange_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidChangeTextDocumentParams)
   is
      use type LSP.Ada_Documents.Document_Access;

      function Skip_Did_Change return Boolean;
      --  Check if the following message in the queue is didChange for
      --  the same document

      URI      : LSP.Structures.DocumentUri renames Value.textDocument.uri;
      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, URI);

      ---------------------
      -- Skip_Did_Change --
      ---------------------

      function Skip_Did_Change return Boolean is
         use type LSP.Servers.Server_Message_Access;

         subtype DidChange_Notification is
           LSP.Server_Notifications.DidChange.Notification;

         Next : constant LSP.Servers.Server_Message_Access :=
           LSP.Servers.Server'Class (Self.Sender.all).Look_Ahead_Message;
      begin

         if Next = null
           or else Next.all not in DidChange_Notification'Class
         then
            return False;
         end if;

         declare
            use GNATCOLL.VFS;
            Object      : DidChange_Notification'Class renames
              DidChange_Notification'Class (Next.all);
            Object_File : constant Virtual_File := Self.To_File
              (Object.Params.textDocument.uri);
            Value_File  : constant Virtual_File := Self.To_File (URI);
         begin
            if Object_File /= Value_File then
               return False;
            end if;
         end;

         return True;
      end Skip_Did_Change;

   begin
      if Document = null then
         Self.Tracer.Trace
           ("Unexpected null document in On_DidChange_Notification");
         return;
      end if;

      if Self.Incremental_Text_Changes then
         --  If we are applying incremental changes, we can't skip the
         --  call to Apply_Changes, since this would break synchronization.
         Document.Apply_Changes
           (Value.textDocument.version,
            Value.contentChanges);

         --  However, we should skip the Indexing part if the next change in
         --  the queue will re-change the text document.
         if Skip_Did_Change then
            return;
         end if;
      else
         --  If we are not applying incremental changes, we can skip
         --  Apply_Changes: the next change will contain the full text.
         if Skip_Did_Change then
            return;
         end if;

         Document.Apply_Changes
           (Value.textDocument.version,
            Value.contentChanges);
      end if;

      --  Reindex the document in each of the contexts where it is relevant

      for Context of Self.Contexts_For_URI (URI) loop
         Context.Index_Document (Document.all);
      end loop;

      --  Emit diagnostics
      Self.Publish_Diagnostics (Document);
   end On_DidChange_Notification;

   --------------------------------------------
   -- On_DidChangeConfiguration_Notification --
   --------------------------------------------

   overriding procedure On_DidChangeConfiguration_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidChangeConfigurationParams)
   is
      Reload : Boolean;
   begin
      Self.Configuration.Read_JSON (Value.settings, Reload);

      --  Always reload project if Project_Tree isn't ready
      Reload := Reload or not Self.Project_Tree.Is_Defined;

      if Reload then
         LSP.Ada_Handlers.Project_Loading.Reload_Project (Self);
      end if;
   end On_DidChangeConfiguration_Notification;

   -----------------------------------------------
   -- On_DidChangeWorkspaceFolders_Notification --
   -----------------------------------------------

   overriding procedure On_DidChangeWorkspaceFolders_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidChangeWorkspaceFoldersParams)
   is
      use type LSP.Ada_Documents.Document_Access;

      URI  : LSP.Structures.DocumentUri;
      File : GNATCOLL.VFS.Virtual_File;

      procedure Process_Created_File;
      --  Processes a created file

      procedure Process_Deleted_File;
      --  Processes a deleted file

      --------------------------
      -- Process_Created_File --
      --------------------------

      procedure Process_Created_File
      is
         use VSS.Strings.Conversions;

         Contexts : constant LSP.Ada_Context_Sets.Context_Lists.List :=
           Self.Contexts_For_URI (URI);

         function Has_Dir
           (Context : LSP.Ada_Contexts.Context)
            return Boolean
         is (Context.List_Source_Directories.Contains (File.Dir));
         --  Return True if File is in a source directory of the project held
         --  by Context.

      begin
         --  If the file was created by the client, then the DidCreateFiles
         --  notification might have been received from it. In that case,
         --  Contexts wont be empty, and all we need to do is check if
         --  there's an open document. If there is, it takes precedence over
         --  the filesystem.
         --  If Contexts is empty, then we need to check if is a new source
         --  that needs to be added. For instance, a source that was moved
         --  to the the project source directories.

         if Contexts.Is_Empty then
            for Context of Self.Contexts.Each_Context
              (Has_Dir'Unrestricted_Access)
            loop
               Context.Include_File (File);
               Context.Index_File (File);

               Self.Tracer.Trace
                 ("Included " & File.Display_Base_Name
                  & " in context " & To_UTF_8_String (Context.Id));
            end loop;

         else
            if Self.Get_Open_Document (URI) = null then
               for Context of Contexts loop
                  Context.Index_File (File);
               end loop;
            end if;
         end if;
      end Process_Created_File;

      ---------------------------
      -- Process_Deleted_Files --
      ---------------------------

      procedure Process_Deleted_File is
      begin
         if Self.Get_Open_Document (URI) = null then
            --  If there is no document, remove from the sources list
            --  and reindex the file for each context where it is
            --  relevant.
            for C of Self.Contexts_For_URI (URI) loop
               C.Exclude_File (File);
               C.Index_File (File);
            end loop;
         end if;
      end Process_Deleted_File;

   begin
      --  Look through each change, filtering non Ada source files
      for Change of Value.event.added loop
         URI  := To_DocumentUri (Change.uri);
         File := Self.To_File (URI);
         Process_Created_File;
      end loop;

      for Change of Value.event.removed loop
         URI  := To_DocumentUri (Change.uri);
         File := Self.To_File (URI);
         Process_Deleted_File;
      end loop;
   end On_DidChangeWorkspaceFolders_Notification;

   ------------------------------
   -- On_DidClose_Notification --
   ------------------------------

   overriding procedure On_DidClose_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidCloseTextDocumentParams)
   is
      URI      : LSP.Structures.DocumentUri renames Value.textDocument.uri;
      File     : constant GNATCOLL.VFS.Virtual_File := Self.To_File (URI);
      Document : Internal_Document_Access;
   begin
      if Self.Open_Documents.Contains (File) then
         Document := Self.Open_Documents.Element (File);

         --  Remove the URI from the set of open documents now: this way,
         --  the call to Flush_Document below will not attempt to reindex
         --  from an open document, but from the file on disk.
         Self.Open_Documents.Delete (File);

         for Context of Self.Contexts_For_URI (URI) loop
            Context.Flush_Document (File);
         end loop;

         Free (Document);

      else
         --  We have received a didCloseTextDocument but the document was
         --  not open: this is not supposed to happen, log it.

         Self.Tracer.Trace
           ("received a On_DidClose_Notification for non-open document "
            & "with uri: ");
         Self.Tracer.Trace_Text (URI);
      end if;

      --  Clean diagnostics up on closing document
      Self.Clean_Diagnostics
        (LSP.Ada_Documents.Document_Access (Document));
   end On_DidClose_Notification;

   ------------------------------------
   -- On_DidCreateFiles_Notification --
   ------------------------------------

   overriding procedure On_DidCreateFiles_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.CreateFilesParams) is
   begin
      Self.Log_Method_In ("On_DidCreateFiles_Notification");

      --  New sources were created on this project, so recompute its view

      Self.Project_Tree.Update_Sources (With_Runtime => True);

      --  For each created file of Value.files:
      --  - find the contexts that contains its directory
      --  - add it to those contexts
      --  - index it on those contexts

      for File of Value.files loop
         declare
            use VSS.Strings.Conversions;

            Created_File : constant GNATCOLL.VFS.Virtual_File :=
              Self.To_File (To_DocumentUri (File.uri));

            function Has_Dir
              (Context : LSP.Ada_Contexts.Context)
                  return Boolean
            is (Context.List_Source_Directories.Contains
                (Created_File.Dir));
            --  Return True if Old_File is a source of the project held by
            --  Context.

         begin
            for Context of Self.Contexts.Each_Context
              (Has_Dir'Unrestricted_Access)
            loop
               Context.Include_File (Created_File);
               Context.Index_File (Created_File);

               Self.Tracer.Trace
                 ("Included " & Created_File.Display_Base_Name
                  & " in context " & To_UTF_8_String (Context.Id));
            end loop;
         end;
      end loop;

      Self.Log_Method_Out ("On_DidCreateFiles_Notification");
   end On_DidCreateFiles_Notification;

   ------------------------------------
   -- On_DidDeleteFiles_Notification --
   ------------------------------------

   overriding procedure On_DidDeleteFiles_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DeleteFilesParams) is
   begin
      Self.Log_Method_In ("On_DidDeleteFiles_Notification");

      --  Some project sources were deleted, so recompute its view

      Self.Project_Tree.Update_Sources (With_Runtime => True);

      --  For each delete file of Value.files:
      --  - find the contexts that contains it
      --  - remove it from those contexts
      --  - re-index it on those contexts so that an empty unit is reparsed

      for File of Value.files loop
         declare
            Deleted_File : constant GNATCOLL.VFS.Virtual_File :=
              Self.To_File (To_DocumentUri (File.uri));

            function Has_File
              (Context : LSP.Ada_Contexts.Context)
               return Boolean
            is (Context.Is_Part_Of_Project (To_DocumentUri (File.uri)));
            --  Return True if Old_File is a source of the project held by
            --  Context.

         begin
            for Context of Self.Contexts.Each_Context
              (Has_File'Unrestricted_Access)
            loop
               Context.Exclude_File (Deleted_File);
               Context.Index_File (Deleted_File);

               Self.Tracer.Trace
                 ("Excluded " & Deleted_File.Display_Base_Name
                  & " from context "
                  & VSS.Strings.Conversions.To_UTF_8_String (Context.Id));
            end loop;
         end;
      end loop;

      Self.Log_Method_Out ("On_DidDeleteFiles_Notification");
   end On_DidDeleteFiles_Notification;

   -----------------------------
   -- On_DidOpen_Notification --
   -----------------------------

   overriding procedure On_DidOpen_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidOpenTextDocumentParams)
   is
      URI    : LSP.Structures.DocumentUri renames Value.textDocument.uri;
      File   : constant GNATCOLL.VFS.Virtual_File := Self.To_File (URI);
      Object : constant Internal_Document_Access :=
        new LSP.Ada_Documents.Document (Self.Tracer);
      Diag   : constant LSP.Diagnostic_Sources.Diagnostic_Source_Access :=
        new LSP.Ada_Handlers.Project_Diagnostics.Diagnostic_Source
          (Self'Unchecked_Access);
   begin
      Self.Log_Method_In ("Text_Document_Did_Open", URI);

      --  Some clients don't properly call initialize, or don't pass the
      --  project to didChangeConfiguration: fallback here on loading a
      --  project in this directory, if needed.
      Self.Client.Set_Root_If_Empty
        (VSS.Strings.Conversions.To_Virtual_String
           (File.Dir.Display_Full_Name));

      Project_Loading.Ensure_Project_Loaded (Self);

      --  We have received a document: add it to the documents container
      Object.Initialize (URI, Value.textDocument.text, Diag);
      Self.Open_Documents.Include (File, Object);

      --  Handle the case where we're loading the implicit project: do
      --  we need to add the directory in which the document is open?

      if Self.Project_Status in Implicit_Project_Loaded then
         declare
            Dir : constant GNATCOLL.VFS.Virtual_File := Self.To_File (URI).Dir;
         begin
            if not Self.Project_Dirs_Loaded.Contains (Dir) then
               --  We do need to add this directory
               Self.Project_Dirs_Loaded.Insert (Dir);
               Project_Loading.Reload_Implicit_Project_Dirs (Self);
            end if;
         end;
      end if;

      --  Index the document in all the contexts where it is relevant
      for Context of Self.Contexts_For_URI (URI) loop
         Context.Index_Document (Object.all);
      end loop;

      --  Emit diagnostics
      Self.Publish_Diagnostics (LSP.Ada_Documents.Document_Access (Object));

      Self.Log_Method_Out ("Text_Document_Did_Open");
   end On_DidOpen_Notification;

   ------------------------------------
   -- On_DidRenameFiles_Notification --
   ------------------------------------

   overriding procedure On_DidRenameFiles_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.RenameFilesParams)
   is
      use LSP.Ada_Context_Sets;

      package URI_Contexts_Maps is new
        Ada.Containers.Indefinite_Hashed_Maps
          (Key_Type        => LSP.Structures.DocumentUri,
           Element_Type    => Context_Lists.List,
           Hash            => LSP.Structures.Get_Hash,
           Equivalent_Keys => LSP.Structures."=",
           "="             => Context_Lists."=");

      subtype URI_Contexts_Map is URI_Contexts_Maps.Map;

      URIs_Contexts : URI_Contexts_Map;

   begin
      Self.Log_Method_In ("On_DidRenameFiles_Notification");

      --  Some project sources were renamed, so recompute its view

      Self.Project_Tree.Update_Sources (With_Runtime => True);

      --  For each oldUri of Value.files:
      --  - map it to a list of context that contains it
      --  - remove it from those contexts
      --  - re-index it on those contexts so that an empty unit is reparsed

      for File_Rename of Value.files loop
         declare
            use VSS.Strings.Conversions;

            Old_File : constant GNATCOLL.VFS.Virtual_File :=
              Self.To_File (To_DocumentUri (File_Rename.oldUri));

            function Has_File
              (Context : LSP.Ada_Contexts.Context)
               return Boolean
            is (Context.Is_Part_Of_Project
                (To_DocumentUri (File_Rename.oldUri)));
            --  Return True if Old_File is a source of the project held by
            --  Context.

            URI_Contexts : Context_Lists.List;

         begin
            for Context of Self.Contexts.Each_Context
              (Has_File'Unrestricted_Access)
            loop
               URI_Contexts.Append (Context);
               Context.Exclude_File (Old_File);
               Context.Index_File (Old_File);

               Self.Tracer.Trace
                 ("Excluded " & Old_File.Display_Full_Name
                  & " from context " & To_UTF_8_String (Context.Id));
            end loop;

            URIs_Contexts.Insert
              (To_DocumentUri (File_Rename.oldUri), URI_Contexts);
         end;
      end loop;

      --  For each (oldUri, newUri) tuple:
      --  - add newUri to all contexts that contained oldUri
      --  - index the newUri (using the appriate method depending if
      --    (there's an open document of not)

      for File_Rename of Value.files loop
         declare
            use VSS.Strings.Conversions;
            use type LSP.Ada_Documents.Document_Access;

            New_File : constant GNATCOLL.VFS.Virtual_File :=
              Self.To_File (To_DocumentUri (File_Rename.newUri));
            Document : constant LSP.Ada_Documents.Document_Access :=
              Get_Open_Document (Self, To_DocumentUri (File_Rename.newUri));
            Is_Document_Open : constant Boolean := Document /= null;

         begin
            for Context of URIs_Contexts.Constant_Reference
              (To_DocumentUri (File_Rename.oldUri))
            loop
               Context.Include_File (New_File);
               if Is_Document_Open then
                  Context.Index_Document (Document.all);
               else
                  Context.Index_File (New_File);
               end if;
               Self.Tracer.Trace
                 ("Included " & New_File.Display_Base_Name & " in context "
                  & To_UTF_8_String (Context.Id));
            end loop;
         end;
      end loop;

      Self.Log_Method_Out ("On_DidRenameFiles_Notification");
   end On_DidRenameFiles_Notification;

   -----------------------------
   -- On_FoldingRange_Request --
   -----------------------------

   overriding procedure On_FoldingRange_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.FoldingRangeParams)
   is
      use type LSP.Ada_Documents.Document_Access;

      Context  : constant LSP.Ada_Context_Sets.Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);
      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Value.textDocument.uri);
      Response : LSP.Structures.FoldingRange_Vector_Or_Null;

   begin
      if Document /= null then
         Document.Get_Folding_Blocks
           (Context.all,
            Self.Client.Line_Folding_Only,
            Self.Configuration.Folding_Comments,
            Self.Is_Canceled,
            Response);

         if Self.Is_Canceled.all then
            Response.Clear;
         end if;
         Self.Sender.On_FoldingRange_Response (Id, Response);

      else
         Self.Sender.On_Error_Response
           (Id, (code => LSP.Enumerations.InternalError,
                 message => "Document is not opened"));
      end if;
   end On_FoldingRange_Request;

   -------------------------------
   -- On_Implementation_Request --
   -------------------------------

   overriding procedure On_Implementation_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ImplementationParams)
   is

      Trace : constant GNATCOLL.Traces.Trace_Handle :=
        LSP.GNATCOLL_Tracers.Handle (Self.Tracer.all);

      Response : LSP.Structures.Definition_Result (LSP.Structures.Varian_1);

      Vector : LSP.Structures.Location_Vector renames Response.Varian_1;

      Display_Method_Ancestry_Policy : constant
        LSP.Ada_Configurations.DisplayMethodAncestryOnNavigationPolicy :=
          Self.Configuration.Display_Method_Ancestry_Policy;

      procedure Resolve_In_Context (C : LSP.Ada_Context_Sets.Context_Access);
      --  Utility function to gather results on one context

      ------------------------
      -- Resolve_In_Context --
      ------------------------

      procedure Resolve_In_Context (C : LSP.Ada_Context_Sets.Context_Access) is

         use all type LSP.Ada_Configurations.
           DisplayMethodAncestryOnNavigationPolicy;

         use Libadalang.Common;

         Name_Node : constant Libadalang.Analysis.Name :=
           Laltools.Common.Get_Node_As_Name (Self.Get_Node_At (C.all, Value));

         procedure Update_Response
           (Bodies : Laltools.Common.Bodies_List.List;
            Ignore : AlsReferenceKind_Array);
         --  Utility function to update response with the bodies

         ---------------------
         -- Update_Response --
         ---------------------

         procedure Update_Response
           (Bodies : Laltools.Common.Bodies_List.List;
            Ignore : AlsReferenceKind_Array)
         is
         begin
            for E of Bodies loop
               Self.Append_Location (Vector, E);
            end loop;
         end Update_Response;

         Definition : Libadalang.Analysis.Defining_Name;
         Imprecise  : Boolean;
         Decl       : Libadalang.Analysis.Basic_Decl;

      begin
         if Name_Node.Is_Null then
            return;
         end if;

         --  Find the definition
         Definition := Laltools.Common.Resolve_Name
           (Name_Node, Trace, Imprecise);

         --  If we didn't find a definition, give up for this context
         if Definition.Is_Null then
            return;
         end if;

         --  First list the bodies of this definition
         Update_Response
           (Laltools.Common.List_Bodies_Of (Definition, Trace, Imprecise),
            LSP.Ada_Handlers.Locations.Empty);

         --  Then list the bodies of the parent implementations
         Decl := Definition.P_Basic_Decl;

         --  Display overriding/overridden subprograms depending on the
         --  displayMethodAncestryOnNavigation flag.
         if Display_Method_Ancestry_Policy in Definition_Only | Always
           or else
             (Display_Method_Ancestry_Policy = Usage_And_Abstract_Only
                     and then Decl.Kind in Ada_Abstract_Subp_Decl_Range)
         then
            for Subp of C.Find_All_Base_Declarations (Decl, Imprecise)
            loop
               Update_Response
                 (Laltools.Common.List_Bodies_Of
                    (Subp.P_Defining_Name, Trace, Imprecise),
                  Is_Parent);
            end loop;

            --  And finally the bodies of child implementations
            for Subp of C.Find_All_Overrides (Decl, Imprecise) loop
               Update_Response
                 (Laltools.Common.List_Bodies_Of
                    (Subp.P_Defining_Name, Trace, Imprecise),
                  Is_Child);
            end loop;
         end if;
      end Resolve_In_Context;

   begin
      --  Override the displayMethodAncestryOnNavigation global configuration
      --  flag if there is on embedded in the request.
      --  if Value.alsDisplayMethodAncestryOnNavigation.Is_Set then
      --     Display_Method_Ancestry_Policy :=
      --       Value.alsDisplayMethodAncestryOnNavigation.Value;
      --  end if;

      for C of Self.Contexts_For_URI (Value.textDocument.uri) loop
         Resolve_In_Context (C);

         exit when Self.Is_Canceled.all;
      end loop;

      --  Sort_And_Remove_Duplicates (Response.result.Locations);

      Self.Sender.On_Implementation_Response (Id, Response);
   end On_Implementation_Request;

   ------------------------------
   -- On_IncomingCalls_Request --
   ------------------------------

   overriding procedure On_IncomingCalls_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyIncomingCallsParams)
   is
      use Libadalang.Analysis;

      procedure Process_Context (C : LSP.Ada_Contexts.Context);
      --  Process the subprogram found in one context and append corresponding
      --  calls to Response.

      Response   : LSP.Structures.CallHierarchyIncomingCall_Vector;

      Item : LSP.Structures.CallHierarchyItem renames
        Value.item;

      Position : constant LSP.Structures.TextDocumentPositionParams :=
        (textDocument => (uri => Item.uri),
         position     => Item.selectionRange.start);

      Filter : Call_Hierarchy.File_Span_Sets.Set;

      ---------------------
      -- Process_Context --
      ---------------------

      procedure Process_Context (C : LSP.Ada_Contexts.Context) is
         Definition : Defining_Name;
      begin
         Definition := Self.Imprecise_Resolve_Name (C, Position);

         --  Attempt to resolve the name, return no results if we can't or if
         --  the name does not resolve to a callable object, like a subprogram
         --  or an entry.

         if not Definition.Is_Null
           and then Definition.P_Basic_Decl.P_Is_Subprogram
           and then not Self.Is_Canceled.all
         then
            Call_Hierarchy.Find_Incoming_Calls
              (Self, Response, Filter, C, Definition);
         end if;

      end Process_Context;

   begin
      --  Find the references in all contexts
      for C of Self.Contexts_For_URI (Item.uri) loop
         Process_Context (C.all);

         exit when Self.Is_Canceled.all;
      end loop;

      Self.Sender.On_IncomingCalls_Response (Id, Response);
   end On_IncomingCalls_Request;

   ---------------------------
   -- On_Initialize_Request --
   ---------------------------

   overriding procedure On_Initialize_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InitializeParams)
   is
      Response : LSP.Structures.InitializeResult;
   begin
      Self.Client.Initialize (Value);

      Response.capabilities := Self.Client.To_Server_Capabilities
        (Self.Incremental_Text_Changes);

      Self.Sender.On_Initialize_Response (Id, Response);
   end On_Initialize_Request;

   ------------------------------
   -- On_OutgoingCalls_Request --
   ------------------------------

   overriding procedure On_OutgoingCalls_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyOutgoingCallsParams)
   is
      use Libadalang.Analysis;

      procedure Process_Context (C : LSP.Ada_Contexts.Context);
      --  Process the subprogram found in one context and append corresponding
      --  calls to Response.

      Response   : LSP.Structures.CallHierarchyOutgoingCall_Vector;

      Item : LSP.Structures.CallHierarchyItem renames
        Value.item;

      Position : constant LSP.Structures.TextDocumentPositionParams :=
        (textDocument => (uri => Item.uri),
         position     => Item.selectionRange.start);

      Filter : Call_Hierarchy.File_Span_Sets.Set;

      ---------------------
      -- Process_Context --
      ---------------------

      procedure Process_Context (C : LSP.Ada_Contexts.Context) is
         Definition : Defining_Name;
      begin
         Definition := Self.Imprecise_Resolve_Name (C, Position);

         --  Attempt to resolve the name, return no results if we can't or if
         --  the name does not resolve to a callable object, like a subprogram
         --  or an entry.

         if not Definition.Is_Null
           and then Definition.P_Basic_Decl.P_Is_Subprogram
           and then not Self.Is_Canceled.all
         then
            Call_Hierarchy.Find_Outgoing_Calls
              (Self, Response, Filter, Definition);
         end if;
      end Process_Context;

   begin
      --  Find the references in all contexts
      for C of Self.Contexts_For_URI (Item.uri) loop
         Process_Context (C.all);

         exit when Self.Is_Canceled.all;
      end loop;

      Self.Sender.On_OutgoingCalls_Response (Id, Response);
   end On_OutgoingCalls_Request;

   -------------------------------------
   -- On_PrepareCallHierarchy_Request --
   -------------------------------------

   overriding procedure On_PrepareCallHierarchy_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyPrepareParams)
   is
      Response  : LSP.Structures.CallHierarchyItem_Vector;

      C    : constant LSP.Ada_Context_Sets.Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);
      --  For the PrepareCallHierarchy request, we're only interested in the
      --  "best" response value, not in the list of values for all contexts

      Name : constant Libadalang.Analysis.Defining_Name :=
        Self.Imprecise_Resolve_Name (C.all, Value);

      Decl : Libadalang.Analysis.Basic_Decl;
      Next : Libadalang.Analysis.Basic_Decl;
   begin
      if not Name.Is_Null then
         Decl := Name.P_Basic_Decl;

         if not Decl.Is_Null and then Decl.P_Is_Subprogram then

            Next := Decl.P_Next_Part_For_Decl;
            Decl := (if Next.Is_Null then Decl else Next);

            declare
               Span : constant LSP.Structures.A_Range :=
                 Self.To_LSP_Location (Decl).a_range;

               Node : constant Libadalang.Analysis.Defining_Name :=
                 Decl.P_Defining_Name;

               Location : constant LSP.Structures.Location :=
                 Self.To_LSP_Location (Node);

               Item : constant LSP.Structures.CallHierarchyItem :=
                 (name           => VSS.Strings.To_Virtual_String (Node.Text),
                  kind           => Utils.Get_Decl_Kind (Decl),
                  tags           => <>,
                  detail         => Utils.Node_Location_Image (Node),
                  uri            => Location.uri,
                  a_range        => Span,
                  selectionRange => Location.a_range,
                  data           => <>);
            begin

               Response.Append (Item);
            end;
         end if;
      end if;

      Self.Sender.On_PrepareCallHierarchy_Response (Id, Response);
   end On_PrepareCallHierarchy_Request;

   ----------------------------
   -- On_Server_Notification --
   ----------------------------

   overriding procedure On_Server_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Server_Notifications.Server_Notification'Class) is
   begin
      Value.Visit_Server_Receiver (Self);
   exception
      when E : others =>
         Self.Tracer.Trace_Exception (E, "On_Server_Notification");
   end On_Server_Notification;

   -----------------------
   -- On_Server_Request --
   -----------------------

   overriding procedure On_Server_Request
     (Self  : in out Message_Handler;
      Value : LSP.Server_Requests.Server_Request'Class)
   is
      package Canceled is new LSP.Generic_Cancel_Check (Value'Access, 127);
   begin
      Self.Implemented := True;
      Self.Is_Canceled := Canceled.Has_Been_Canceled'Unrestricted_Access;

      Value.Visit_Server_Receiver (Self);

      if not Self.Implemented then
         Self.Sender.On_Error_Response
           (Value.Id,
            (code    => LSP.Enumerations.MethodNotFound,
             message => "Not implemented"));
      end if;

   exception
      when E : others =>
         declare
            Message : constant VSS.Strings.Virtual_String :=
              VSS.Strings.Conversions.To_Virtual_String
                ("Exception: " &
                   Ada.Exceptions.Exception_Name (E) & " (" &
                     Ada.Exceptions.Exception_Message (E) & ")");

         begin
            Self.Tracer.Trace_Exception (E, "On_Server_Request");

            Self.Sender.On_Error_Response
              (Value.Id,
               (code    => LSP.Enumerations.InternalError,
                message => Message));

         end;
   end On_Server_Request;

   -------------------------
   -- On_Shutdown_Request --
   -------------------------

   overriding procedure On_Shutdown_Request
     (Self : in out Message_Handler;
      Id   : LSP.Structures.Integer_Or_Virtual_String)
   is
      Result : LSP.Structures.Null_Record;

   begin
      --  Suspend files/runtime indexing after shutdown requst
      Self.Indexing_Enabled := False;

      Self.Sender.On_Shutdown_Response (Id, Result);
   end On_Shutdown_Request;

   -------------------------------
   -- On_TypeDefinition_Request --
   -------------------------------

   overriding procedure On_TypeDefinition_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeDefinitionParams)
   is

      Response   : LSP.Structures.Definition_Result (LSP.Structures.Varian_1);
      Vector     : LSP.Structures.Location_Vector renames Response.Varian_1;
      Imprecise  : Boolean := False;

      procedure Resolve_In_Context (C : LSP.Ada_Context_Sets.Context_Access);
      --  Utility function to gather results on one context

      ------------------------
      -- Resolve_In_Context --
      ------------------------

      procedure Resolve_In_Context (C : LSP.Ada_Context_Sets.Context_Access) is
         Trace      : constant GNATCOLL.Traces.Trace_Handle :=
           LSP.GNATCOLL_Tracers.Handle (Self.Tracer.all);

         Name_Node  : constant Libadalang.Analysis.Name :=
           Laltools.Common.Get_Node_As_Name (Self.Get_Node_At (C.all, Value));

         Definition : Libadalang.Analysis.Defining_Name;
         Type_Decl  : Libadalang.Analysis.Base_Type_Decl;
      begin
         if Name_Node.Is_Null then
            return;
         end if;

         if Name_Node.P_Is_Defining then
            --  Special case if Name_Node is defining, for instance on the X in
            --      X : My_Type;
            declare
               Def_Name : constant Libadalang.Analysis.Defining_Name :=
                 Name_Node.P_Enclosing_Defining_Name;

               Type_Expr : constant Libadalang.Analysis.Type_Expr :=
                 Def_Name.P_Basic_Decl.P_Type_Expression;
            begin
               if not Type_Expr.Is_Null then
                  Definition := Laltools.Common.Resolve_Name
                    (Type_Expr.P_Type_Name, Trace, Imprecise);
               end if;
            end;
         else
            --  Name_Node is not defining. In this case we can rely on
            --  P_Expression_Type.
            Type_Decl := Name_Node.P_Expression_Type;

            --  P_Expression_Type returns the entire expression: narrow the
            --  result down to the type declaration. Here we assume that the
            --  first defining name in this expression is the name of the type.
            if not Type_Decl.Is_Null then
               Definition := Type_Decl.P_Defining_Name;
            end if;
         end if;

         if not Definition.Is_Null then
            Self.Append_Location (Vector, Definition);
         end if;
      end Resolve_In_Context;

   begin
      for C of Self.Contexts_For_URI (Value.textDocument.uri) loop
         Resolve_In_Context (C);

         exit when Self.Is_Canceled.all;
      end loop;

      Self.Sender.On_Definition_Response (Id, Response);
   end On_TypeDefinition_Request;

   -------------------------
   -- Publish_Diagnostics --
   -------------------------

   procedure Publish_Diagnostics
     (Self              : in out Message_Handler'Class;
      Document          : not null LSP.Ada_Documents.Document_Access;
      Other_Diagnostics : LSP.Structures.Diagnostic_Vector :=
        LSP.Structures.Empty;
      Force             : Boolean := False)
   is
      Changed : Boolean;
      Diag    : LSP.Structures.PublishDiagnosticsParams;
   begin
      if Self.Configuration.Diagnostics_Enabled then
         Document.Get_Errors
           (Context => Self.Contexts.Get_Best_Context (Document.URI).all,
            Changed => Changed,
            Errors  => Diag.diagnostics,
            Force   => Force);

         Diag.diagnostics.Append_Vector (Other_Diagnostics);

         if Changed or else not Other_Diagnostics.Is_Empty then
            Diag.uri := Document.URI;
            Self.Sender.On_PublishDiagnostics_Notification (Diag);
         end if;
      end if;
   end Publish_Diagnostics;

end LSP.Ada_Handlers;

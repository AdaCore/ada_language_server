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

with Ada.Exceptions;
with Ada.Tags.Generic_Dispatching_Constructor;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;
with GNATCOLL.Traces;

with VSS.Characters.Latin;
with VSS.Strings.Formatters.Integers;
with VSS.Strings.Formatters.Strings;
with VSS.Strings.Templates;
with VSS.JSON.Streams;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Helpers;

with Laltools.Common;

with Langkit_Support.Slocs;

with LAL_Refactor.Extract_Subprogram;
with LAL_Refactor.Introduce_Parameter;
with LAL_Refactor.Pull_Up_Declaration;
with LAL_Refactor.Refactor_Imports;
with LAL_Refactor.Replace_Type;
with LAL_Refactor.Sort_Dependencies;
with LAL_Refactor.Subprogram_Signature.Change_Parameters_Default_Value;
with LAL_Refactor.Subprogram_Signature.Change_Parameters_Type;
with LAL_Refactor.Subprogram_Signature.Remove_Parameter;
with LAL_Refactor.Suppress_Separate;

with LSP.Ada_Completions.Aspects;
with LSP.Ada_Completions.Attributes;
with LSP.Ada_Completions.End_Names;
with LSP.Ada_Completions.Keywords;
with LSP.Ada_Completions.Names;
with LSP.Ada_Completions.Parameters;
with LSP.Ada_Completions.Pragmas;
with LSP.Ada_Completions.Use_Clauses;
with LSP.Ada_Completions;
with LSP.Ada_Contexts;
with LSP.Ada_Documentation;
with LSP.Ada_Handlers.Call_Hierarchy;
with LSP.Ada_Handlers.Invisibles;
with LSP.Ada_Handlers.Locations;
with LSP.Ada_Handlers.Named_Parameters_Commands;
with LSP.Ada_Handlers.Project_Diagnostics;
with LSP.Ada_Handlers.Project_Loading;
with LSP.Ada_Handlers.Refactor.Add_Parameter;
with LSP.Ada_Handlers.Refactor.Change_Parameter_Mode;
with LSP.Ada_Handlers.Refactor.Change_Parameters_Default_Value;
with LSP.Ada_Handlers.Refactor.Change_Parameters_Type;
with LSP.Ada_Handlers.Refactor.Extract_Subprogram;
with LSP.Ada_Handlers.Refactor.Imports_Commands;
with LSP.Ada_Handlers.Refactor.Introduce_Parameter;
with LSP.Ada_Handlers.Refactor.Move_Parameter;
with LSP.Ada_Handlers.Refactor.Pull_Up_Declaration;
with LSP.Ada_Handlers.Refactor.Remove_Parameter;
with LSP.Ada_Handlers.Refactor.Replace_Type;
with LSP.Ada_Handlers.Refactor.Sort_Dependencies;
with LSP.Ada_Handlers.Refactor.Suppress_Seperate;
with LSP.Ada_Handlers.Renaming;
with LSP.Commands;
with LSP.Constants;
with LSP.Diagnostic_Sources;
with LSP.Enumerations;
with LSP.Errors;
with LSP.Generic_Cancel_Check;
with LSP.GNATCOLL_Tracers.Handle;
with LSP.Server_Notifications.DidChange;
with LSP.Servers;
with LSP.Structures.LSPAny_Vectors;
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

   -----------------------------
   -- Allocate_Progress_Token --
   -----------------------------

   function Allocate_Progress_Token
     (Self      : in out Message_Handler'Class;
      Operation : VSS.Strings.Virtual_String)
      return LSP.Structures.ProgressToken
   is
      Token_Template : VSS.Strings.Templates.Virtual_String_Template :=
        "ada_ls-{}-{}-{}";

   begin
      Self.Token_Id := Self.Token_Id + 1;
      --  Generate an identifier that has little risk of collision with
      --  other language servers, or other occurrences of this server.
      --  (There is still a very small risk of collision with PID recyclings,
      --  but the consequences are acceptable.)

      return
        (Is_Integer     => False,
         Virtual_String =>
           Token_Template.Format
             (VSS.Strings.Formatters.Integers.Image
                (GNAT.OS_Lib.Pid_To_Integer (GNAT.OS_Lib.Current_Process_Id)),
              VSS.Strings.Formatters.Strings.Image (Operation),
              VSS.Strings.Formatters.Integers.Image (Self.Token_Id)));
   end Allocate_Progress_Token;

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

   -----------------------
   -- Contexts_For_File --
   -----------------------

   function Contexts_For_File
     (Self : access Message_Handler;
      File : GNATCOLL.VFS.Virtual_File)
      return LSP.Ada_Context_Sets.Context_Lists.List
   is
      function Is_A_Source (Self : LSP.Ada_Contexts.Context) return Boolean is
        (Self.Is_Part_Of_Project (File));
      --  Return True if File is a source of the project held by Context

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
   end Contexts_For_File;

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

   -----------------------
   -- Get_Project_Stamp --
   -----------------------

   function Get_Project_Stamp
     (Self : Message_Handler'Class)
      return Project_Stamp is (Self.Project_Stamp);

   -------------------------------
   -- Get_Open_Document_Version --
   -------------------------------

   function Get_Open_Document_Version
     (Self : in out Message_Handler;
      URI  : LSP.Structures.DocumentUri)
      return LSP.Structures.OptionalVersionedTextDocumentIdentifier
   is
      use type LSP.Ada_Documents.Document_Access;
      Document : constant LSP.Ada_Documents.Document_Access :=
        Self.Get_Open_Document (URI);

   begin
      --  If the target textDocument hasn't been opened in the editor
      --  then ALS hasn't received an open notification before. Therefore
      --  Target_Text_Document will be null.
      --  In that case, its VersionedTextDocumentIdentifier.version will
      --  be null.

      if Document = null then
         return (URI, LSP.Structures.Integer_Or_Null'(Is_Null => True));

      else
         return
           (uri     => Document.Versioned_Identifier.uri,
            version => (Is_Null => False,
                        Value   => Document.Versioned_Identifier.version));
      end if;
   end Get_Open_Document_Version;

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

   ----------------------
   -- Is_Open_Document --
   ----------------------

   function Is_Open_Document
     (Self : Message_Handler;
      File : GNATCOLL.VFS.Virtual_File) return Boolean is
   begin
      return Self.Open_Documents.Contains (File);
   end Is_Open_Document;

   -----------------
   -- Is_Shutdown --
   -----------------

   function Is_Shutdown
     (Self : Message_Handler'Class) return Boolean is (Self.Shutdown);

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
   -- On_CodeAction_Request --
   ---------------------------

   overriding procedure On_CodeAction_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeActionParams)
   is

      use Libadalang.Common;

      procedure Analyse_In_Context
        (Context  : LSP.Ada_Context_Sets.Context_Access;
         Document : LSP.Ada_Documents.Document_Access;
         Result   : out LSP.Structures.Command_Or_CodeAction_Vector;
         Found    : in out Boolean);
      --  Perform refactoring ananlysis given Document in the Context.
      --  Return Found = True if some refactoring is possible. Populate
      --  Result with Code_Actions in this case.

      function Has_Assoc_Without_Designator
        (Node : Libadalang.Analysis.Basic_Assoc_List) return Boolean;
      --  Check if Node is Basic_Assoc_List that contains at least one
      --  ParamAssoc without a designator.

      procedure Analyse_Node
        (Context : LSP.Ada_Context_Sets.Context_Access;
         Node    : Libadalang.Analysis.Ada_Node;
         Result  : out LSP.Structures.Command_Or_CodeAction_Vector;
         Found   : in out Boolean);
      --  Look for a possible refactoring in given Node.
      --  Return Found = True if some refactoring is possible. Populate
      --  Result with Code_Actions in this case. Return Done = True if futher
      --  analysis has no sense.

      procedure Append_Project_Status_Code_Actions
        (Result : in out LSP.Structures.Command_Or_CodeAction_Vector);
      --  Append project status code action if needed

      ------------------------
      -- Analyse_In_Context --
      ------------------------

      procedure Analyse_In_Context
        (Context  : LSP.Ada_Context_Sets.Context_Access;
         Document : LSP.Ada_Documents.Document_Access;
         Result   : out LSP.Structures.Command_Or_CodeAction_Vector;
         Found    : in out Boolean)
      is
         Node : constant Libadalang.Analysis.Ada_Node :=
           Document.Get_Node_At (Context.all, Value.a_range.start);
      begin
         if Node.Is_Null then
            Found := False;
            return;
         end if;

         Analyse_Node (Context, Node, Result, Found);
      end Analyse_In_Context;

      ------------------
      -- Analyse_Node --
      ------------------

      procedure Analyse_Node
        (Context : LSP.Ada_Context_Sets.Context_Access;
         Node    : Libadalang.Analysis.Ada_Node;
         Result  : out LSP.Structures.Command_Or_CodeAction_Vector;
         Found   : in out Boolean)
      is
         procedure Change_Parameters_Type_Code_Action;
         --  Checks if the Change Parameters Type refactoring tool is avaiable,
         --  and if so, appends a Code Action with its Command.

         procedure Change_Parameters_Default_Value_Code_Action;
         --  Checks if the Change Parameters Default Value refactoring tool is
         --  avaiable, and if so, appends a Code Action with its Command.

         procedure Extract_Subprogram_Code_Action;
         --  Checks if the Extract Subprogram refactoring tool is available,
         --  and if so, appends a Code Action with its Command.

         procedure Introduce_Parameter_Code_Action;
         --  Checks if the Introduce Parameter refactoring tool is available,
         --  and if so, appends a Code Action with its Command.

         procedure Import_Package_Code_Action;
         --  Checks if the Import Package code assist is available,
         --  and if so, appends a Code Aciton with its Command.

         procedure Named_Parameters_Code_Action;
         --  Checks if the Named Parameters refactoring is available, and if
         --  so, appends a Code Action with its Command.

         procedure Pull_Up_Declaration_Code_Action;
         --  Checks if the Pull Up Declaration refactoring tool is available,
         --  and if so, appends a Code Action with its Command.

         procedure Replace_Type_Code_Action;
         --  Checks if the Replace Type refactoring tool is available,
         --  and if so, appends a Code Action with its Command.

         procedure Sort_Dependencies_Code_Action;
         --  Checks if the Sort Dependencies refactoring tool is available,
         --  and if so, appends a Code Action with its Command.

         -------------------------------------------------
         -- Change_Parameters_Default_Value_Code_Action --
         -------------------------------------------------

         procedure Change_Parameters_Default_Value_Code_Action is
            use Langkit_Support.Slocs;
            use LAL_Refactor.Subprogram_Signature.
                  Change_Parameters_Default_Value;
            use LSP.Ada_Handlers.Refactor.Change_Parameters_Default_Value;

            Span : constant Source_Location_Range :=
              (Langkit_Support.Slocs.Line_Number
                 (Value.a_range.start.line) + 1,
               Langkit_Support.Slocs.Line_Number
                 (Value.a_range.an_end.line) + 1,
               Column_Number (Value.a_range.start.character) + 1,
               Column_Number (Value.a_range.an_end.character) + 1);

            Change_Parameters_Default_Value_Command : Command;

         begin
            if Is_Change_Parameters_Default_Value_Available
                 (Unit                             => Node.Unit,
                  Parameters_Source_Location_Range => Span)
            then
               Change_Parameters_Default_Value_Command.Append_Code_Action
                 (Context         => Context,
                  Commands_Vector => Result,
                  Where           =>
                    (uri     => Value.textDocument.uri,
                     a_range => Value.a_range));

               Found := True;
            end if;
         end Change_Parameters_Default_Value_Code_Action;

         ----------------------------------------
         -- Change_Parameters_Type_Code_Action --
         ----------------------------------------

         procedure Change_Parameters_Type_Code_Action is
            use Langkit_Support.Slocs;
            use LAL_Refactor.Subprogram_Signature.Change_Parameters_Type;
            use LSP.Ada_Handlers.Refactor.Change_Parameters_Type;

            Span : constant Source_Location_Range :=
              (Langkit_Support.Slocs.Line_Number
                 (Value.a_range.start.line) + 1,
               Langkit_Support.Slocs.Line_Number
                 (Value.a_range.an_end.line) + 1,
               Column_Number (Value.a_range.start.character) + 1,
               Column_Number (Value.a_range.an_end.character) + 1);

            Syntax_Rules : Laltools.Common.Grammar_Rule_Vector;

            Change_Parameters_Type_Command : Command;

         begin
            if Is_Change_Parameters_Type_Available
                 (Unit                             => Node.Unit,
                  Parameters_Source_Location_Range => Span,
                  New_Parameter_Syntax_Rules       => Syntax_Rules)
            then
               Change_Parameters_Type_Command.Append_Code_Action
                 (Context         => Context,
                  Commands_Vector => Result,
                  Where           =>
                    (uri     => Value.textDocument.uri,
                     a_range => Value.a_range),
                  Syntax_Rules    => Syntax_Rules);

               Found := True;
            end if;
         end Change_Parameters_Type_Code_Action;

         ------------------------------------
         -- Extract_Subprogram_Code_Action --
         ------------------------------------

         procedure Extract_Subprogram_Code_Action is
            use LSP.Ada_Handlers.Refactor.Extract_Subprogram;
            use Langkit_Support.Slocs;
            use LAL_Refactor.Extract_Subprogram;
            use type LSP.Structures.Position;

            Single_Location : constant Boolean :=
              Value.a_range.start = Value.a_range.an_end;

            Section_To_Extract_SLOC : constant Source_Location_Range :=
              (Langkit_Support.Slocs.Line_Number
                 (Value.a_range.start.line) + 1,
               Langkit_Support.Slocs.Line_Number
                 (Value.a_range.an_end.line) + 1,
               Column_Number (Value.a_range.start.character) + 1,
               Column_Number (Value.a_range.an_end.character) + 1);

            Available_Subprogram_Kinds : Available_Subprogram_Kinds_Type;

            Extract_Subprogram_Command : Command;

         begin
            if not Single_Location then
               if Is_Extract_Subprogram_Available
                 (Node.Unit,
                  Section_To_Extract_SLOC,
                  Available_Subprogram_Kinds)
               then
                  if Available_Subprogram_Kinds (Ada_Subp_Kind_Procedure) then
                     Extract_Subprogram_Command.Append_Code_Action
                       (Context         => Context,
                        Commands_Vector => Result,
                        Where           =>
                          (Value.textDocument.uri,
                           Value.a_range),
                        Subprogram_Kind => Ada_Subp_Kind_Procedure);
                  end if;

                  if Available_Subprogram_Kinds (Ada_Subp_Kind_Function) then
                     Extract_Subprogram_Command.Append_Code_Action
                       (Context         => Context,
                        Commands_Vector => Result,
                        Where           =>
                          (Value.textDocument.uri,
                           Value.a_range),
                        Subprogram_Kind => Ada_Subp_Kind_Function);
                  end if;

                  Found := True;
               end if;
            end if;
         end Extract_Subprogram_Code_Action;

         --------------------------------
         -- Import_Package_Code_Action --
         --------------------------------

         procedure Import_Package_Code_Action is
            use Libadalang.Analysis;
            use LAL_Refactor.Refactor_Imports;
            use LSP.Structures;

            Single_Location : constant Boolean :=
              Value.a_range.start = Value.a_range.an_end;

            Units_Vector : Libadalang.Helpers.Unit_Vectors.Vector;
            Units_Array  : constant Analysis_Unit_Array :=
              Context.Analysis_Units;

            Import_Suggestions : Import_Suggestions_Vector.Vector;

            function Is_Import_Suggestions_Available
              (This_Node : Ada_Node'Class)
               return Boolean;
            --  Checks if This_Node is a suitable node to get import
            --  suggestions. A suitable node must be an identifier, non
            --  defining and if it resolves, it must be to a declaration not
            --  declared in the standard package.
            --  This function also prepares Units_Vector with the right units
            --  where suggestions should be searched for.

            -------------------------------------
            -- Is_Import_Suggestions_Available --
            -------------------------------------

            function Is_Import_Suggestions_Available
              (This_Node : Ada_Node'Class)
               return Boolean
            is
               Aux_Node              : Ada_Node :=
                 (if This_Node.Is_Null then No_Ada_Node
                  else This_Node.As_Ada_Node);
               Referenced_Definition : Defining_Name := No_Defining_Name;

            begin
               --  Only get suggestions for Identifiers or Dotted_Names
               if Aux_Node.Is_Null
                 or else Aux_Node.Kind not in
                   Ada_Identifier_Range | Ada_Dotted_Name_Range
               then
                  return False;
               end if;

               --  Get the full Dotted_Name if applicable
               while not Aux_Node.Is_Null
                 and then not Aux_Node.Parent.Is_Null
                 and then Aux_Node.Parent.Kind in Ada_Dotted_Name_Range
               loop
                  Aux_Node := Aux_Node.Parent;
               end loop;

               --  Defining names do not need prefixes
               if Aux_Node.Is_Null or else Aux_Node.As_Name.P_Is_Defining then
                  return False;
               end if;

               Referenced_Definition :=
                 Aux_Node.As_Name.P_Referenced_Defining_Name;

               --  Declarations in the standard package do not need prefixes
               if not Referenced_Definition.Is_Null then
                  if Referenced_Definition.Unit = Node.P_Standard_Unit then
                     return False;
                  end if;
               end if;

               if Referenced_Definition.Is_Null then
                  --  The name could not be resolved so a full search needs to
                  --  be done.

                  for U of Units_Array loop
                     Units_Vector.Append (U);
                  end loop;

                  --  Add runtime analysis units for this context
                  --  ??? If possible, this should be cached.

                  for F in Self.Project_Predefined_Sources.Iterate loop
                     declare
                        VF : GNATCOLL.VFS.Virtual_File renames
                          LSP.Ada_File_Sets.File_Sets.Element (F);
                     begin
                        Units_Vector.Append
                          (Context.LAL_Context.Get_From_File
                             (VF.Display_Full_Name,
                              --  ??? What is the charset for predefined
                              --  files?
                              ""));
                     end;
                  end loop;

               else
                  --  Libadalang sometimes can resolve names that are not
                  --  withed.
                  --  For instance, with Ada.Text_IO, resolve
                  --  Ada.Text_IO.Put_Line, remove the Ada.Text_IO and then
                  --  resolve again Ada.Text_IO.Put_Line. Even though
                  --  Ada.Text_IO is no longer withed, Libadalang is still
                  --  able to resolve Put_Line.
                  --  For such cases, include only Referenced_Definition's
                  --  Analysis_Units and the tool will suggest the prefixes
                  --  (there can be more than one, for instance, when there
                  --  are nested packages.
                  Units_Vector.Append (Referenced_Definition.Unit);
               end if;

               return True;
            exception
               when others => return False;
            end Is_Import_Suggestions_Available;

         begin
            if not Single_Location
              or else not Is_Import_Suggestions_Available (Node)
            then
               return;
            end if;

            --  Get suggestions for all reachable declarations.
            --  Each suggestion contains a with clause and a
            --  prefix.

            Import_Suggestions :=
              Get_Import_Suggestions (Node, Units_Vector);

            --  Create a new codeAction command for each suggestion

            for Suggestion of Import_Suggestions loop
               declare
                  Command : LSP.Ada_Handlers.Refactor.Imports_Commands.Command;
               begin
                  Command.Append_Suggestion
                    (Context         => Context,
                     Where           =>
                       LSP.Utils.Get_Node_Location (Node),
                     Commands_Vector => Result,
                     Suggestion      => Suggestion);
               end;
            end loop;

            if not Import_Suggestions.Is_Empty then
               Found := True;
            end if;
         end Import_Package_Code_Action;

         -------------------------------------
         -- Introduce_Parameter_Code_Action --
         -------------------------------------

         procedure Introduce_Parameter_Code_Action is
            use Langkit_Support.Slocs;
            use LAL_Refactor.Introduce_Parameter;
            use LSP.Ada_Handlers.Refactor.Introduce_Parameter;

            Span : constant Source_Location_Range :=
              (Langkit_Support.Slocs.Line_Number
                 (Value.a_range.start.line) + 1,
               Langkit_Support.Slocs.Line_Number
                 (Value.a_range.an_end.line) + 1,
               Column_Number (Value.a_range.start.character) + 1,
               Column_Number (Value.a_range.an_end.character) + 1);

            Introduce_Parameter_Command : Command;

         begin
            if Is_Introduce_Parameter_Available
                 (Unit       => Node.Unit,
                  SLOC_Range => Span)
            then
               Introduce_Parameter_Command.Append_Code_Action
                 (Context         => Context,
                  Commands_Vector => Result,
                  Where           =>
                    (uri     => Value.textDocument.uri,
                     a_range => Value.a_range));

               Found := True;
            end if;
         end Introduce_Parameter_Code_Action;

         ----------------------------------
         -- Named_Parameters_Code_Action --
         ----------------------------------

         procedure Named_Parameters_Code_Action is
            Aux_Node : Libadalang.Analysis.Ada_Node := Node;
            Done     : Boolean := False;
            --  We propose only one choice of Named_Parameters refactoring per
            --  request. So, if a user clicks on `1` in `A (B (1))` we propose
            --  the refactoring for B (1), but not for A (...) call. We
            --  consider this as better user experience.
            --
            --  This boolean filter to detect such refactoring duplication.

            procedure Append_Command (Node : Libadalang.Analysis.Ada_Node);
            --  Contruct a command and append it to Result

            --------------------
            -- Append_Command --
            --------------------

            procedure Append_Command (Node : Libadalang.Analysis.Ada_Node) is
               use LSP.Ada_Handlers.Named_Parameters_Commands;

               Named_Parameters_Command : LSP.Ada_Handlers.
                 Named_Parameters_Commands.Command;

            begin
               Named_Parameters_Command.Append_Suggestion
                 (Context             => Context,
                  Commands_Vector     => Result,
                  Where               => LSP.Utils.Get_Node_Location (Node),
                  Versioned_Documents => Self.Client.Versioned_Documents);

               Done  := True;
               Found := True;
            end Append_Command;

         begin
            while not Done and then not Aux_Node.Is_Null loop
               case Aux_Node.Kind is
                  when Libadalang.Common.Ada_Stmt
                     | Libadalang.Common.Ada_Basic_Decl =>

                     Done := True;

                  when Libadalang.Common.Ada_Basic_Assoc_List =>
                     if Has_Assoc_Without_Designator
                          (Aux_Node.As_Basic_Assoc_List)
                     then
                        Append_Command (Aux_Node);
                     end if;

                  when Libadalang.Common.Ada_Call_Expr =>
                     declare
                        List : constant Libadalang.Analysis.Ada_Node :=
                          Aux_Node.As_Call_Expr.F_Suffix;

                     begin
                        if not List.Is_Null
                          and then List.Kind in
                                     Libadalang.Common.Ada_Basic_Assoc_List
                          and then Has_Assoc_Without_Designator
                                     (List.As_Basic_Assoc_List)
                        then
                           Append_Command (List);
                        end if;
                     end;
                  when others =>
                     null;
               end case;

               Aux_Node := Aux_Node.Parent;
            end loop;
         end Named_Parameters_Code_Action;

         -------------------------------------
         -- Pull_Up_Declaration_Code_Action --
         -------------------------------------

         procedure Pull_Up_Declaration_Code_Action is
            use Langkit_Support.Slocs;
            use Libadalang.Analysis;
            use LAL_Refactor.Pull_Up_Declaration;
            use LSP.Ada_Handlers.Refactor.Pull_Up_Declaration;
            use LSP.Structures;

            --  This code action is not available when a range of text is
            --  selected.

            Single_Location : constant Boolean :=
              Value.a_range.start = Value.a_range.an_end;
            Location        : constant Source_Location :=
              (Langkit_Support.Slocs.Line_Number
                 (Value.a_range.start.line) + 1,
               Column_Number (Value.a_range.start.character) + 1);

            Pull_Up_Declaration_Command :
              LSP.Ada_Handlers.Refactor.Pull_Up_Declaration.Command;

         begin
            if Single_Location
              and then Is_Pull_Up_Declaration_Available (Node.Unit, Location)
            then
               Pull_Up_Declaration_Command.Append_Code_Action
                 (Context                     => Context,
                  Commands_Vector             => Result,
                  Where                       =>
                    (uri     => Value.textDocument.uri,
                     a_range => Value.a_range));

               Found := True;
            end if;
         end Pull_Up_Declaration_Code_Action;

         ------------------------------
         -- Replace_Type_Code_Action --
         ------------------------------

         procedure Replace_Type_Code_Action is
            use LSP.Ada_Handlers.Refactor.Replace_Type;
            use LAL_Refactor.Replace_Type;

            use Langkit_Support.Slocs;

            Location : constant Source_Location :=
              (Langkit_Support.Slocs.Line_Number
                 (Value.a_range.start.line) + 1,
               Column_Number (Value.a_range.start.character) + 1);

            Replace_Type_Command :
              LSP.Ada_Handlers.Refactor.Replace_Type.Command;

         begin
            if Is_Replace_Type_Available (Node.Unit, Location) then
               Replace_Type_Command.Append_Code_Action
                 (Context         => Context,
                  Commands_Vector => Result,
                  Where           =>
                    (Value.textDocument.uri,
                     Value.a_range));

               Found := True;
            end if;
         end Replace_Type_Code_Action;

         -----------------------------------
         -- Sort_Dependencies_Code_Action --
         -----------------------------------

         procedure Sort_Dependencies_Code_Action is
            use Langkit_Support.Slocs;
            use Libadalang.Analysis;
            use LAL_Refactor.Sort_Dependencies;
            use LSP.Ada_Handlers.Refactor.Sort_Dependencies;
            use LSP.Structures;

            Location        : constant Source_Location :=
              (Langkit_Support.Slocs.Line_Number
                 (Value.a_range.start.line) + 1,
               Column_Number (Value.a_range.start.character) + 1);

            Sort_Dependencies_Command :
              LSP.Ada_Handlers.Refactor.Sort_Dependencies.Command;

         begin
            if Is_Sort_Dependencies_Available (Node.Unit, Location) then
               Sort_Dependencies_Command.Append_Code_Action
                 (Context         => Context,
                  Commands_Vector => Result,
                  Where           =>
                    (uri     => Value.textDocument.uri,
                     a_range => Value.a_range));

               Found := True;
            end if;
         end Sort_Dependencies_Code_Action;

      begin
         Named_Parameters_Code_Action;

         Sort_Dependencies_Code_Action;

         Import_Package_Code_Action;

         --  Refactoring Code Actions

         --  Extract Subprogram
         Extract_Subprogram_Code_Action;

         --  Pull Up Declaration
         Pull_Up_Declaration_Code_Action;

         --  These refactorings are only available for clients that can
         --  provide user inputs:
         --  - Add Parameter
         --  - Change Parameters Type
         --  - Change Parameters Default Value

         --  Add Parameter
         if Self.Client.Refactoring_Add_Parameter then
            declare
               use LSP.Ada_Handlers.Refactor.Add_Parameter;
               use Libadalang.Analysis;
               use LAL_Refactor.Subprogram_Signature;
               use Langkit_Support.Slocs;
               use type LSP.Structures.Position;

               --  This code action is not available when a range of text is
               --  selected.

               Single_Location             : constant Boolean :=
                 Value.a_range.start = Value.a_range.an_end;
               Location                    : constant Source_Location :=
                 (if Single_Location then
                    (Langkit_Support.Slocs.Line_Number
                         (Value.a_range.start.line) + 1,
                     Column_Number (Value.a_range.start.character) + 1)
                  else
                     No_Source_Location);

               Requires_Full_Specification : Boolean;

               Add_Parameter_Commad : Command;

            begin
               if Single_Location
                 and then Is_Add_Parameter_Available
                   (Node.Unit,
                    Location,
                    Requires_Full_Specification)
               then
                  Add_Parameter_Commad.Append_Code_Action
                    (Context                     => Context,
                     Commands_Vector             => Result,
                     Where                       =>
                       (Value.textDocument.uri,
                        Value.a_range),
                     Requires_Full_Specification =>
                       Requires_Full_Specification);

                  Found := True;
               end if;
            end;
         end if;

         --  Change Parameters Type
         if Self.Client.Refactoring_Change_Parameters_Type then
            Change_Parameters_Type_Code_Action;
         end if;

         --  Change Parameters Default Value
         if Self.Client.Refactoring_Change_Parameters_Default_Value then
            Change_Parameters_Default_Value_Code_Action;
         end if;

         --  Remove Parameter
         declare
            use LSP.Ada_Handlers.Refactor.Remove_Parameter;
            use Libadalang.Analysis;
            use LAL_Refactor.Subprogram_Signature;
            use LAL_Refactor.Subprogram_Signature.Remove_Parameter;

            Target_Subp              : Basic_Decl := No_Basic_Decl;
            Parameter_Indices_Range  : Parameter_Indices_Range_Type;
            Remove_Parameter_Command : Command;

         begin
            if Is_Remove_Parameter_Available
              (Node, Target_Subp, Parameter_Indices_Range)
            then
               Remove_Parameter_Command.Append_Code_Action
                 (Context            => Context,
                  Commands_Vector    => Result,
                  Target_Subp        => Target_Subp,
                  Parameters_Indices => Parameter_Indices_Range);

               Found := True;
            end if;
         end;

         --  Move Parameter
         declare
            use LSP.Ada_Handlers.Refactor.Move_Parameter;
            use Libadalang.Analysis;
            use LAL_Refactor.Subprogram_Signature;

            Target_Subp            : Basic_Decl := No_Basic_Decl;
            Parameter_Index        : Positive;
            Move_Directions        : Move_Direction_Availability_Type;
            Move_Parameter_Command : Command;

         begin
            if Is_Move_Parameter_Available
              (Node, Target_Subp, Parameter_Index, Move_Directions)
            then
               for Direction in Move_Direction_Type loop
                  if Move_Directions (Direction) then
                     Move_Parameter_Command.Append_Code_Action
                       (Context          => Context,
                        Commands_Vector  => Result,
                        Target_Subp      => Target_Subp,
                        Parameter_Index  => Parameter_Index,
                        Move_Direction   => Direction);
                  end if;
               end loop;

               Found := True;
            end if;
         end;

         --  Change Parameter Mode
         declare
            use LSP.Ada_Handlers.Refactor.Change_Parameter_Mode;
            use Libadalang.Analysis;
            use LAL_Refactor.Subprogram_Signature;

            Target_Subp                   : Basic_Decl := No_Basic_Decl;
            Target_Parameters_Indices     : Parameter_Indices_Range_Type;
            Mode_Alternatives             : Mode_Alternatives_Type;
            Change_Parameter_Mode_Command : Command;

         begin
            if Is_Change_Mode_Available
              (Node, Target_Subp, Target_Parameters_Indices, Mode_Alternatives)
            then
               for Alternative of Mode_Alternatives loop
                  Change_Parameter_Mode_Command.Append_Code_Action
                    (Context            => Context,
                     Commands_Vector    => Result,
                     Target_Subp        => Target_Subp,
                     Parameters_Indices => Target_Parameters_Indices,
                     New_Mode           => Alternative);
               end loop;

               Found := True;
            end if;
         end;

         --  Introduce Parameter
         Introduce_Parameter_Code_Action;

         --  Suppress Subprogram
         declare
            use LSP.Ada_Handlers.Refactor.Suppress_Seperate;
            use Libadalang.Analysis;
            use LAL_Refactor.Suppress_Separate;

            Target_Separate           : Basic_Decl := No_Basic_Decl;
            Suppress_Separate_Command : Command;
         begin
            if Is_Suppress_Separate_Available (Node, Target_Separate) then
               Suppress_Separate_Command.Append_Code_Action
                 (Context         => Context,
                  Commands_Vector => Result,
                  Target_Separate => Target_Separate);

               Found := True;
            end if;
         end;

         --  Replace Type
         if Self.Client.Refactoring_Replace_Type then
            Replace_Type_Code_Action;
         end if;
      end Analyse_Node;

      ----------------------------------------
      -- Append_Project_Status_Code_Actions --
      ----------------------------------------

      procedure Append_Project_Status_Code_Actions
        (Result : in out LSP.Structures.Command_Or_CodeAction_Vector)
      is
         use type VSS.Strings.Virtual_String;

         Diagnostics : LSP.Structures.Diagnostic_Vector;

      begin
         for Item of Value.context.diagnostics loop
            if Item.source = "project" then
               Diagnostics.Append (Item);
            end if;
         end loop;

         case Self.Project_Status is
            when Valid_Project_Configured | Alire_Project =>
               null;
            when No_Runtime_Found =>
               --  TODO: Provide help with the compiler installation
               null;
            when Single_Project_Found | Multiple_Projects_Found =>
               declare
                  Item    : LSP.Structures.CodeAction;
                  Command : LSP.Structures.Command;
                  Arg     : constant VSS.JSON.Streams.JSON_Stream_Element :=
                    VSS.JSON.Streams.JSON_Stream_Element'
                      (Kind         => VSS.JSON.Streams.String_Value,
                       String_Value => "ada.projectFile");
               begin
                  Command.title := "Open settings for ada.projectFile";
                  Command.command := "workbench.action.openSettings";
                  Command.arguments.Append (Arg);

                  Item :=
                    (title       => Command.title,
                     kind        => (True, LSP.Enumerations.QuickFix),
                     diagnostics => Diagnostics,
                     disabled    => (Is_Set => False),
                     edit        => (Is_Set => False),
                     isPreferred => LSP.Constants.True,
                     command     => (True, Command),
                     data        => <>);

                  Result.Append
                    (LSP.Structures.Command_Or_CodeAction'
                       (Is_Command => False, CodeAction => Item));
               end;
            when No_Project_Found =>
               declare
                  Title  : constant VSS.Strings.Virtual_String :=
                    "Create a default project file (default.gpr)";
                  URI    : constant LSP.Structures.DocumentUri :=
                    To_DocumentUri
                      (VSS.Strings.Conversions.To_Virtual_String
                         (GNATCOLL.VFS.Create_From_UTF8
                         (VSS.Strings.Conversions.To_UTF_8_String
                            (Self.Client.Root)).Join
                              ("default.gpr").Display_Full_Name));

                  Create : constant LSP.Structures.
                    documentChanges_OfWorkspaceEdit_Item :=
                      (Kind    => LSP.Structures.create,
                       create  => (uri    => URI,
                                   others => <>));

                  Text   : constant LSP.Structures.
                    TextEdit_Or_AnnotatedTextEdit :=
                      (Is_TextEdit => True,
                       TextEdit    =>
                         (a_range => ((0, 0), (0, 0)),
                          newText => "project Default is end Default;"));
                  Insert : LSP.Structures.
                    documentChanges_OfWorkspaceEdit_Item :=
                    (LSP.Structures.Variant_1,
                     (textDocument => (uri => URI, others => <>),
                      edits        => <>));

                  Item   : LSP.Structures.CodeAction;
                  Edit   : LSP.Structures.WorkspaceEdit;
               begin
                  Insert.Variant_1.edits.Append (Text);
                  Edit.documentChanges.Append (Create);
                  Edit.documentChanges.Append (Insert);
                  Item :=
                    (title       => Title,
                     kind        => (True, LSP.Enumerations.QuickFix),
                     diagnostics => Diagnostics,
                     disabled    => (Is_Set => False),
                     edit        => (True, Edit),
                     isPreferred => LSP.Constants.True,
                     command     => (Is_Set => False),
                     data        => <>);

                  Result.Append
                    (LSP.Structures.Command_Or_CodeAction'
                       (Is_Command => False, CodeAction => Item));
               end;
            when Invalid_Project_Configured =>
               null;
         end case;
      end Append_Project_Status_Code_Actions;

      ----------------------------------
      -- Has_Assoc_Without_Designator --
      ----------------------------------

      function Has_Assoc_Without_Designator
        (Node : Libadalang.Analysis.Basic_Assoc_List) return Boolean
      is
         Found : Boolean := False;

         function Process_Type_Expr
           (TE : Libadalang.Analysis.Type_Expr)
            return Boolean;
         --  Returns True if TE is associated to an access of a subprogram

         -----------------------
         -- Process_Type_Expr --
         -----------------------

         function Process_Type_Expr
           (TE : Libadalang.Analysis.Type_Expr)
            return Boolean
         is
            TD : Libadalang.Analysis.Base_Type_Decl;
            --  If TE is not an anonymous type then we'll need to know its
            --  declaration.

         begin
            case TE.Kind is
               when Ada_Subtype_Indication_Range =>
                  TD := TE.As_Subtype_Indication.P_Designated_Type_Decl;

                  if TD.Is_Null
                    or else not (TD.Kind in Ada_Type_Decl)
                  then
                     return False;
                  end if;

                  case TD.As_Type_Decl.F_Type_Def.Kind is
                     when Ada_Access_To_Subp_Def_Range =>
                        --  Confirmation that TD is an access to a subprogram

                        return True;

                     when Ada_Array_Type_Def_Range =>
                        --  If TD is an array type, then it might be an array
                        --  of accesses to subprograms. Therefore, recursively
                        --  call Process_Type_Expr to check the type of the
                        --  components of the array.

                        return Process_Type_Expr
                          (TD.As_Type_Decl.F_Type_Def.As_Array_Type_Def.
                             F_Component_Type.F_Type_Expr);

                     when others =>
                        return False;
                  end case;

               when Ada_Anonymous_Type_Range =>
                  return TE.As_Anonymous_Type.F_Type_Decl.F_Type_Def.Kind in
                    Ada_Access_To_Subp_Def_Range;

               when others =>
                  return False;

            end case;
         end Process_Type_Expr;

      begin
         for J of Node loop
            if J.Kind in Libadalang.Common.Ada_Param_Assoc and then
              J.As_Param_Assoc.F_Designator.Is_Null
            then
               Found := True;
               exit;
            end if;
         end loop;

         if not Found then
            return False;
         end if;

         declare
            Expr : constant Libadalang.Analysis.Ada_Node := Node.Parent;
            Name : Libadalang.Analysis.Name;
            Decl : Libadalang.Analysis.Basic_Decl;
         begin
            case Expr.Kind is
               when Libadalang.Common.Ada_Call_Expr =>
                  Name := Expr.As_Call_Expr.F_Name;
               when others =>
                  return False;
            end case;

            Decl := Name.P_Referenced_Decl;

            if Decl.Is_Null then
               return False;
            end if;

            --  For Ada_Param_Spec, Ada_Component_Decl or Object_Decl nodes,
            --  check the type definition of Decl. Named parameters can be
            --  added if Decl's type is a (possibly anonymous) access to a
            --  subprogram.

            case Decl.Kind is
               when Libadalang.Common.Ada_Base_Subp_Body
                  | Libadalang.Common.Ada_Basic_Subp_Decl =>
                  return True;

               when Libadalang.Common.Ada_Param_Spec_Range =>
                  return Process_Type_Expr (Decl.As_Param_Spec.F_Type_Expr);

               when Libadalang.Common.Ada_Component_Decl_Range =>
                  return Process_Type_Expr
                    (Decl.As_Component_Decl.F_Component_Def.F_Type_Expr);

               when  Libadalang.Common.Ada_Object_Decl_Range =>
                  --  This can either be an object which type is an access
                  --  to a subprogram or an array of accesses to
                  --  subprograms.
                  return Process_Type_Expr (Decl.As_Object_Decl.F_Type_Expr);

               when others =>
                  return False;
            end case;
         end;
      end Has_Assoc_Without_Designator;

      use type LSP.Ada_Documents.Document_Access;
      use type LSP.Structures.Position;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Value.textDocument.uri);

      Response : LSP.Structures.Command_Or_CodeAction_Vector_Or_Null;

      Found : Boolean := False;
   begin
      if Document = null then
         Self.Tracer.Trace
           ("Unexpected null document in On_CodeAction_Request");
         Self.Sender.On_CodeAction_Response (Id, Response);
         return;
      end if;

      --  Find any context where we can do some refactoring
      for C of Self.Contexts_For_URI (Value.textDocument.uri) loop
         Analyse_In_Context (C, Document, Response, Found);

         exit when Self.Is_Canceled.all or else Found;
      end loop;

      if Value.a_range.start = LSP.Constants.Empty then
         Append_Project_Status_Code_Actions (Response);
      end if;

      Self.Sender.On_CodeAction_Response (Id, Response);
   end On_CodeAction_Request;

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
        (Kind => LSP.Structures.Variant_2);
   begin
      Response.Variant_2.isIncomplete := False;

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
         Result    => Response.Variant_2);

      LSP.Ada_Completions.Write_Completions
        (Context                  => Context.all,
         Document                 => Document.all,
         Sloc                     => Sloc,
         Node                     => Node,
         Names                    => Names,
         Named_Notation_Threshold =>
           Self.Configuration.Named_Notation_Threshold,
         Compute_Doc_And_Details  => Compute_Doc_And_Details,
         Result                   => Response.Variant_2.items);

      Self.Sender.On_Completion_Response (Id, Response);
   end On_Completion_Request;

   -----------------------------------
   -- On_Completion_Resolve_Request --
   -----------------------------------

   overriding procedure On_Completion_Resolve_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionItem)
   is
      Context  : LSP.Ada_Context_Sets.Context_Access;
      Node     : Libadalang.Analysis.Ada_Node;
      C        : LSP.Structures.JSON_Event_Vectors.Cursor;
      Location : LSP.Structures.Location;
      Response : LSP.Structures.CompletionItem;

   begin
      --  Return immediately if we don't have data that allows us to compute
      --  additional information for the given item.
      --  This is the case when all the completion item's fields have already
      --  been computed.
      if Value.data.Is_Empty then
         Self.Sender.On_Completion_Resolve_Response (Id, Value);
      end if;

      C := Value.data.First;
      Location := LSP.Structures.LSPAny_Vectors.From_Any (C);
      Context  := Self.Contexts.Get_Best_Context (Location.uri);
      Node     := Get_Node_At
        (Self, Context.all,
         LSP.Structures.TextDocumentPositionParams'
           (textDocument => (uri => Location.uri),
            position     => Location.a_range.start));

      --  Retrieve the Basic_Decl from the completion item's SLOC
      while not Node.Is_Null
        and then Node.Kind not in Libadalang.Common.Ada_Basic_Decl
      loop
         Node := Node.Parent;
      end loop;

      --  Compute the completion item's details
      if not Node.Is_Null then
         declare
            use type VSS.Strings.Virtual_String;

            BD           : constant Libadalang.Analysis.Basic_Decl :=
              Node.As_Basic_Decl;
            Qual_Text    : VSS.Strings.Virtual_String;
            Loc_Text     : VSS.Strings.Virtual_String;
            Doc_Text     : VSS.Strings.Virtual_String;
            Decl_Text    : VSS.Strings.Virtual_String;
            Aspects_Text : VSS.Strings.Virtual_String;

         begin
            LSP.Ada_Documentation.Get_Tooltip_Text
              (BD                 => BD,
               Style              => Self.Configuration.Documentation_Style,
               Qualifier_Text     => Qual_Text,
               Location_Text      => Loc_Text,
               Documentation_Text => Doc_Text,
               Declaration_Text   => Decl_Text,
               Aspects_Text       => Aspects_Text);

            Response.detail := Decl_Text;

            if not Doc_Text.Is_Empty then
               Loc_Text.Append (2 * VSS.Characters.Latin.Line_Feed);
               Loc_Text.Append (Doc_Text);
            end if;

            Response.documentation :=
              (Is_Set => True,
               Value  => LSP.Structures.Virtual_String_Or_MarkupContent'
                 (Is_Virtual_String => True,
                  Virtual_String    => Loc_Text));
         end;
      end if;

      Self.Sender.On_Completion_Resolve_Response (Id, Response);
   end On_Completion_Resolve_Request;

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

      Response   : LSP.Structures.Declaration_Result (LSP.Structures.Variant_1);
      Vector     : LSP.Structures.Location_Vector renames Response.Variant_1;

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

      Response   : LSP.Structures.Definition_Result (LSP.Structures.Variant_1);
      Vector     : LSP.Structures.Location_Vector renames Response.Variant_1;

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
        Ada.Containers.Hashed_Maps
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

   ----------------------------------
   -- On_DocumentHighlight_Request --
   ----------------------------------

   overriding procedure On_DocumentHighlight_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentHighlightParams)
   is
      Response : LSP.Structures.DocumentHighlight_Vector_Or_Null;

      procedure Compute_Response;

      function Get_Highlight_Kind
        (Node : Libadalang.Analysis.Ada_Node)
         return LSP.Structures.DocumentHighlightKind_Optional;
      --  Fetch highlight kind for given node

      ----------------------
      -- Compute_Response --
      ----------------------

      procedure Compute_Response is

         use type LSP.Ada_Documents.Document_Access;

         Context       : constant LSP.Ada_Context_Sets.Context_Access :=
           Self.Contexts.Get_Best_Context (Value.textDocument.uri);
         Document      : constant LSP.Ada_Documents.Document_Access :=
           Get_Open_Document (Self, Value.textDocument.uri);
         File          : constant GNATCOLL.VFS.Virtual_File :=
           Self.To_File (Value.textDocument.uri);
         Defining_Name : constant Libadalang.Analysis.Defining_Name :=
           Imprecise_Resolve_Name (Self, Context.all, Value);

         procedure Append_To_Response
           (Node   : Libadalang.Analysis.Base_Id;
            Kind   : Libadalang.Common.Ref_Result_Kind;
            Cancel : in out Boolean);
         --  Called on each found reference. Used to append the reference to
         --  the final result.

         ------------------------
         -- Append_To_Response --
         ------------------------

         procedure Append_To_Response
           (Node   : Libadalang.Analysis.Base_Id;
            Kind   : Libadalang.Common.Ref_Result_Kind;
            Cancel : in out Boolean)
         is
            pragma Unreferenced (Kind);
            pragma Unreferenced (Cancel);

         begin
            if not Laltools.Common.Is_End_Label (Node.As_Ada_Node) then
               LSP.Ada_Handlers.Locations.Append_Location
                 (Result   => Response,
                  Document => Document,
                  File     => File,
                  Node     => Node,
                  Kind     => Get_Highlight_Kind (Node.As_Ada_Node));
            end if;
         end Append_To_Response;

      begin
         if Document = null
           or Defining_Name.Is_Null
           or Self.Is_Canceled.all
         then
            return;
         end if;

         --  Find all references will return all the references except the
         --  declaration ...

         Document.Find_All_References
           (Context    => Context.all,
            Definition => Defining_Name,
            Callback   => Append_To_Response'Access);

         --  ... add it manually

         LSP.Ada_Handlers.Locations.Append_Location
           (Result   => Response,
            Document => Document,
            File     => File,
            Node     => Defining_Name,
            Kind     => Get_Highlight_Kind (Defining_Name.As_Ada_Node));
      end Compute_Response;

      ------------------------
      -- Get_Highlight_Kind --
      ------------------------

      function Get_Highlight_Kind
        (Node : Libadalang.Analysis.Ada_Node)
         return LSP.Structures.DocumentHighlightKind_Optional
      is
         Id : constant Libadalang.Analysis.Name :=
           Laltools.Common.Get_Node_As_Name (Node);

      begin
         if Id.P_Is_Write_Reference then
            return (Is_Set => True, Value => LSP.Enumerations.Write);

         else
            return (Is_Set => True, Value  => LSP.Enumerations.Read);
         end if;
      end Get_Highlight_Kind;

   begin
      Compute_Response;
      Self.Sender.On_DocumentHighlight_Response (Id, Response);
   end On_DocumentHighlight_Request;

   -------------------------------
   -- On_ExecuteCommand_Request --
   -------------------------------

   function Create_Command is new Ada.Tags.Generic_Dispatching_Constructor
     (T           => LSP.Commands.Command,
      Parameters  => LSP.Structures.LSPAny_Vector,
      Constructor => LSP.Commands.Create);

   overriding procedure On_ExecuteCommand_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ExecuteCommandParams)
   is
      use type Ada.Tags.Tag;

      Tag   : Ada.Tags.Tag := Ada.Tags.No_Tag;
      Error : LSP.Errors.ResponseError_Optional;

   begin
      if not Value.command.Is_Empty then
         Tag := Ada.Tags.Internal_Tag
           (VSS.Strings.Conversions.To_UTF_8_String (Value.command));
      end if;

      if Tag = Ada.Tags.No_Tag then
         Self.Sender.On_Error_Response
           (Id, (code    => LSP.Constants.InternalError,
                 message => "Unknown command"));
         return;
      end if;

      declare
         Command : constant LSP.Commands.Command'Class :=
           Create_Command (Tag, Value.arguments'Unrestricted_Access);
      begin
         Command.Execute
           (Handler => Self'Access,
            Sender  => Self.Sender,
            Id      => Id,
            Error   => Error);

         if Error.Is_Set then
            Self.Sender.On_Error_Response (Id, Error.Value);
         else
            --  No particular response in case of success.
            Self.Sender.On_ExecuteCommand_Response (Id, (Is_Null => True));
         end if;
      end;
   end On_ExecuteCommand_Request;

   ---------------------------
   -- On_Exits_Notification --
   ---------------------------

   overriding procedure On_Exits_Notification
     (Self : in out Message_Handler) is
   begin
      LSP.Servers.Server'Class (Self.Sender.all).Stop;
   end On_Exits_Notification;

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
           (Id, (code => LSP.Constants.InternalError,
                 message => "Document is not opened"));
      end if;
   end On_FoldingRange_Request;

   ---------------------
   -- On_Full_Request --
   ---------------------

   overriding procedure On_Full_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensParams)
   is
      use type LSP.Ada_Documents.Document_Access;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Self.Get_Open_Document (Value.textDocument.uri);

      Context  : constant LSP.Ada_Context_Sets.Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);

      Response : LSP.Structures.SemanticTokens_Or_Null (Is_Null => False);

      Result   : LSP.Structures.Natural_Vector renames
        Response.Value.data;
   begin
      if Document /= null then
         Result := Document.Get_Tokens (Context.all, Self.Highlighter);
      end if;

      Self.Sender.On_Full_Response (Id, Response);
   end On_Full_Request;

   ----------------------
   -- On_Hover_Request --
   ----------------------

   overriding procedure On_Hover_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.HoverParams)
   is

      Response : LSP.Structures.Hover_Or_Null;

      procedure Compute_Response;

      ----------------------
      -- Compute_Response --
      ----------------------

      procedure Compute_Response is
         Context            : constant LSP.Ada_Context_Sets.Context_Access :=
           Self.Contexts.Get_Best_Context (Value.textDocument.uri);
         --  For the Hover request, we're only interested in the "best"
         --  response value, not in the list of values for all contexts

         Defining_Name_Node : constant Libadalang.Analysis.Defining_Name :=
           Self.Imprecise_Resolve_Name (Context.all, Value);
         Decl               : constant Libadalang.Analysis.Basic_Decl :=
           (if Defining_Name_Node.Is_Null
            then Libadalang.Analysis.No_Basic_Decl
            else Defining_Name_Node.P_Basic_Decl);
         --  Associated basic declaration, if any

         Decl_Text          : VSS.Strings.Virtual_String;
         Qualifier_Text     : VSS.Strings.Virtual_String;
         Comments_Text      : VSS.Strings.Virtual_String;
         Location_Text      : VSS.Strings.Virtual_String;
         Aspects_Text       : VSS.Strings.Virtual_String;

      begin
         if Decl.Is_Null or else Self.Is_Canceled.all then
            return;
         end if;

         LSP.Ada_Documentation.Get_Tooltip_Text
           (BD                 => Decl,
            Style              => Context.Get_Documentation_Style,
            Declaration_Text   => Decl_Text,
            Qualifier_Text     => Qualifier_Text,
            Location_Text      => Location_Text,
            Documentation_Text => Comments_Text,
            Aspects_Text       => Aspects_Text);

         if Decl_Text.Is_Empty then
            return;
         end if;

         Response := (Is_Null => False, others => <>);
         Response.Value.contents := (Is_MarkupContent => False, others => <>);

         --  Append the whole declaration text to the response

         Response.Value.contents.MarkedString_Vector.Append
           (LSP.Structures.MarkedString'
              (Is_Virtual_String => False,
               value             => Decl_Text,
               language          => "ada"));

         --  Append qualifier text if any

         if not Qualifier_Text.Is_Empty then
            Response.Value.contents.MarkedString_Vector.Append
              (LSP.Structures.MarkedString'
                 (Is_Virtual_String => True,
                  Virtual_String    => Qualifier_Text));
         end if;

         --  Append the declaration's location.
         --
         --  In addition, append the project's name if we are dealing with an
         --  aggregate project.

         Location_Text := LSP.Utils.Node_Location_Image (Decl);

         if Self.Project_Tree.Root_Project.Kind in GPR2.Aggregate_Kind then
            Location_Text.Append (VSS.Characters.Latin.Line_Feed);
            Location_Text.Append ("As defined in project ");
            Location_Text.Append (Context.Id);
            Location_Text.Append (" (other projects skipped).");
         end if;

         Response.Value.contents.MarkedString_Vector.Append
           (LSP.Structures.MarkedString'
              (Is_Virtual_String => True,
               Virtual_String    => Location_Text));

         --  Append the comments associated with the basic declaration if any.

         if not Comments_Text.Is_Empty then
            Response.Value.contents.MarkedString_Vector.Append
              (LSP.Structures.MarkedString'
                 (Is_Virtual_String => False,
                  language          => "plaintext",
                  value             => Comments_Text));
         end if;

         --  Append text of aspects

         if not Aspects_Text.Is_Empty then
            Response.Value.contents.MarkedString_Vector.Append
              (LSP.Structures.MarkedString'
                 (Is_Virtual_String => False,
                  value             => Aspects_Text,
                  language          => "ada"));
         end if;
      end Compute_Response;

   begin
      Compute_Response;
      Self.Sender.On_Hover_Response (Id, Response);
   end On_Hover_Request;

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

      Response : LSP.Structures.Definition_Result (LSP.Structures.Variant_1);

      Vector : LSP.Structures.Location_Vector renames Response.Variant_1;

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
      Token_Types     : LSP.Structures.Virtual_String_Vector;
      Token_Motifiers : LSP.Structures.Virtual_String_Vector;
   begin
      Self.Client.Initialize (Value);

      Self.Highlighter.Initialize
        (Self.Client, Token_Types, Token_Motifiers);

      Response.capabilities := Self.Client.To_Server_Capabilities
        (Self.Incremental_Text_Changes,
         Token_Types,
         Token_Motifiers);

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

   ------------------------------
   -- On_PrepareRename_Request --
   ------------------------------

   overriding procedure On_PrepareRename_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.PrepareRenameParams)
   is
      Trace : constant GNATCOLL.Traces.Trace_Handle :=
        LSP.GNATCOLL_Tracers.Handle (Self.Tracer.all);

      Response : LSP.Structures.PrepareRenameResult_Or_Null (Is_Null => False);

      Context : constant LSP.Ada_Context_Sets.Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);
      --  For the prepareRename request, we're only interested in the "best"
      --  context to check that we are able to rename the name.

      Name_Node  : constant Libadalang.Analysis.Name :=
        Laltools.Common.Get_Node_As_Name
          (Self.Get_Node_At (Context.all, Value));

      Defining_Name : Libadalang.Analysis.Defining_Name;

      Imprecise : Boolean;
   begin
      if not Name_Node.Is_Null then
         Defining_Name := Laltools.Common.Resolve_Name
           (Name_Node,
            Trace,
            Imprecise => Imprecise);
      end if;

      if not Name_Node.Is_Null
        and then not Defining_Name.Is_Null
        and then not Imprecise
      then
         --  Success only if the node is a name and can be resolved precisely
         Response.Value.Variant_1 := Self.To_LSP_Location (Name_Node).a_range;

      end if;

      Self.Sender.On_PrepareRename_Response (Id, Response);
   end On_PrepareRename_Request;

   ---------------------------
   -- On_References_Request --
   ---------------------------

   overriding procedure On_References_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ReferenceParams)
   is
      use all type LSP.Ada_Handlers.Locations.AlsReferenceKind;

      Response   : LSP.Structures.Location_Vector_Or_Null;
      Imprecise  : Boolean := False;

      Additional_Kinds : AlsReferenceKind_Array :=
        [others => False];

      procedure Process_Context (C : LSP.Ada_Context_Sets.Context_Access);
      --  Process the references found in one context and append
      --  them to Response.results.

      function Get_Reference_Kind
        (Node               : Libadalang.Analysis.Ada_Node'Class;
         Is_Overriding_Decl : Boolean := False)
         return AlsReferenceKind_Array;
      --  Fetch reference kind for given node.

      ------------------------
      -- Get_Reference_Kind --
      ------------------------

      function Get_Reference_Kind
        (Node               : Libadalang.Analysis.Ada_Node'Class;
         Is_Overriding_Decl : Boolean := False)
         return AlsReferenceKind_Array
      is
         use type AlsReferenceKind_Array;

         Id     : constant Libadalang.Analysis.Name :=
           Laltools.Common.Get_Node_As_Name (Node.As_Ada_Node);

         Result : AlsReferenceKind_Array := [others => False];
      begin
         begin
            Result (Write) := Id.P_Is_Write_Reference;
         exception
            when E : Libadalang.Common.Property_Error =>
               Self.Tracer.Trace_Exception (E);
         end;

         begin
            Result (Access_Ref) :=
              Laltools.Common.Is_Access_Ref (Id.As_Ada_Node);
         exception
            when E : Libadalang.Common.Property_Error =>
               Self.Tracer.Trace_Exception (E);
         end;

         begin
            Result (Static_Call) := Id.P_Is_Static_Call;
         exception
            when E : Libadalang.Common.Property_Error =>
               Self.Tracer.Trace_Exception (E);
         end;

         begin
            Result (Dispatching_Call) :=
              Id.P_Is_Dispatching_Call;
         exception
            when E : Libadalang.Common.Property_Error =>
               Self.Tracer.Trace_Exception (E);
         end;

         begin
            Result (Child) :=
              Laltools.Common.Is_Type_Derivation (Id.As_Ada_Node);
         exception
            when E : Libadalang.Common.Property_Error =>
               Self.Tracer.Trace_Exception (E);
         end;

         Result (Overriding_Decl) := Is_Overriding_Decl;

         --  If the result has not any set flags at this point, flag it as a
         --  simple reference.
         if Result = [Result'Range => False] then
            Result (Simple) := True;
         end if;

         --  Apply additional kinds
         Result := Result or Additional_Kinds;

         return Result;
      end Get_Reference_Kind;

      ---------------------
      -- Process_Context --
      ---------------------

      procedure Process_Context (C : LSP.Ada_Context_Sets.Context_Access) is
         procedure Callback
           (Node   : Libadalang.Analysis.Base_Id;
            Kind   : Libadalang.Common.Ref_Result_Kind;
            Cancel : in out Boolean);

         procedure Callback
           (Node   : Libadalang.Analysis.Base_Id;
            Kind   : Libadalang.Common.Ref_Result_Kind;
            Cancel : in out Boolean)
         is
            pragma Unreferenced (Kind);
         begin
            if not Laltools.Common.Is_End_Label (Node.As_Ada_Node) then

               Self.Append_Location
                 (Response,
                  Node,
                  Get_Reference_Kind (Node));
            end if;

            Cancel := Self.Is_Canceled.all;
         end Callback;

         Definition : Libadalang.Analysis.Defining_Name;

         use Libadalang.Common;
      begin

         Definition := Self.Imprecise_Resolve_Name (C.all, Value);

         if Definition.Is_Null or else Self.Is_Canceled.all then
            return;
         end if;

         --  Set additional "reference" kind for enumeration literal
         declare
            Decl : constant Libadalang.Analysis.Basic_Decl :=
              Libadalang.Analysis.P_Basic_Decl (Definition);
         begin
            if not Decl.Is_Null
              and then Libadalang.Analysis.Kind (Decl) = Ada_Enum_Literal_Decl
            then
               Additional_Kinds (Simple) := True;
            end if;

            --  Find all the references
            C.Find_All_References (Definition, Callback'Access);

            --  Find all the overriding declarations, if any
            for Subp of C.Find_All_Overrides (Decl, Imprecise) loop
               Self.Append_Location
                 (Response,
                  Subp.P_Defining_Name,
                  Get_Reference_Kind
                    (Definition,
                     Is_Overriding_Decl => True));
            end loop;

            if Value.context.includeDeclaration then
               Self.Append_Location
                 (Response,
                  Definition,
                  Get_Reference_Kind (Definition));
            end if;
         end;
      end Process_Context;

   begin
      for C of Self.Contexts_For_URI (Value.textDocument.uri) loop
         Process_Context (C);

         exit when Self.Is_Canceled.all;
      end loop;

      --  Sort_And_Remove_Duplicates (Response.result);

      Self.Sender.On_References_Response (Id, Response);
   end On_References_Request;

   -----------------------
   -- On_Rename_Request --
   -----------------------

   overriding procedure On_Rename_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.RenameParams)
   is
      Response : LSP.Structures.WorkspaceEdit_Or_Null (Is_Null => False);

      Position : constant LSP.Structures.TextDocumentPositionParams :=
        (textDocument => Value.textDocument,
         position => Value.position);

      Filter : LSP.Ada_Handlers.Renaming.Edit_Sets.Set;
      --  When iterating over all contexts (and therefore all projects), it's
      --  possible to encounter the same Text_Edit more than once, so this
      --  stores all the unique edits

      Errors : LAL_Refactor.Refactoring_Diagnostic_Vector;

   begin
      for C of Self.Contexts_For_URI (Value.textDocument.uri) loop
         declare
            Name_Node : constant Libadalang.Analysis.Name :=
              Laltools.Common.Get_Node_As_Name
                (Self.Get_Node_At (C.all, Position));
         begin
            LSP.Ada_Handlers.Renaming.Process_Context
              (Self,
               C,
               Name_Node,
               New_Name   => Value.newName,
               Filter     => Filter,
               Result     => Response.Value,
               Errors     => Errors);

            if not Errors.Is_Empty then
               declare
                  Template : VSS.Strings.Templates.Virtual_String_Template :=
                    "Can't rename identifier '{}'";

                  Message : constant VSS.Strings.Virtual_String :=
                    Template.Format
                      (VSS.Strings.Formatters.Strings.Image
                         (VSS.Strings.To_Virtual_String (Name_Node.Text)));

                  Diag_Params : LSP.Structures.PublishDiagnosticsParams;
                  Diagnostic  : LSP.Structures.Diagnostic;
               begin
                  Diagnostic.a_range :=
                    Self.To_LSP_Location (Name_Node).a_range;
                  Diagnostic.severity := LSP.Constants.Error;
                  Diagnostic.source := "Ada";

                  if Self.Client.Supports_Related_Diagnostics then

                     Diagnostic.message := Message;

                     for Problem of Errors loop
                        Diagnostic.relatedInformation.Append
                          (LSP.Structures.DiagnosticRelatedInformation'
                             (location =>
                                LSP.Ada_Handlers.Locations.To_LSP_Location
                                (Self,
                                 C.all,
                                 Problem.Filename,
                                 Problem.Location),

                              message  =>
                                VSS.Strings.Conversions.To_Virtual_String
                                  (Problem.Info)));
                     end loop;
                  else
                     Diagnostic.message :=
                       VSS.Strings.Conversions.To_Virtual_String
                         (Errors.First_Element.Info);

                  end if;

                  Diag_Params.uri := Value.textDocument.uri;
                  Diag_Params.diagnostics.Append (Diagnostic);
                  Self.Sender.On_PublishDiagnostics_Notification (Diag_Params);
                  exit;
               end;
            end if;
         end;
      end loop;

      if Errors.Is_Empty then
         Self.Sender.On_Rename_Response (Id, Response);
      else
         Self.Sender.On_Error_Response
           (Id,
            (code    => LSP.Constants.RequestFailed,
             message => <>));
      end if;
   end On_Rename_Request;

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
            (code    => LSP.Constants.MethodNotFound,
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
               (code    => LSP.Constants.InternalError,
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
      Self.Shutdown := True;
      Self.Sender.On_Shutdown_Response (Id, Result);
   end On_Shutdown_Request;

   ------------------------------
   -- On_SignatureHelp_Request --
   ------------------------------

   overriding procedure On_SignatureHelp_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SignatureHelpParams)
   is
      procedure Compute_Response;

      Response : LSP.Structures.SignatureHelp_Or_Null (Is_Null => False);

      ----------------------
      -- Compute_Response --
      ----------------------

      procedure Compute_Response is
         Context  : constant LSP.Ada_Context_Sets.Context_Access :=
           Self.Contexts.Get_Best_Context (Value.textDocument.uri);
         Document : constant LSP.Ada_Documents.Document_Access :=
           Self.Get_Open_Document (Value.textDocument.uri);
         Location : constant Langkit_Support.Slocs.Source_Location :=
           Document.Get_Source_Location (Value.position);

         Position : LSP.Structures.Position := Value.position;
         Node     : Libadalang.Analysis.Ada_Node;

      begin
         --  Move the cursor to the previous character: this is more resilient
         --  to invalid code.

         if Position.character > 0 then
            Position.character := @ - 1;
         end if;

         Node := Document.Get_Node_At (Context.all, Position);

         declare
            Name_Node : constant Libadalang.Analysis.Name :=
              Laltools.Common.Get_Node_As_Name (Node);

         begin
            --  Is this a type cast?

            if not Name_Node.Is_Null
              and then not Name_Node.P_Name_Designated_Type.Is_Null
            --  Does the cast make sense?
            --   and then Active_Position = 0
            --  Do we have the previous signatures?
              and then Value.context.Is_Set
              and then Value.context.Value.activeSignatureHelp.Is_Set
            then
               --  At this point, the user is writing a typecast in a previous
               --  signature => keep showing the previous signatures.

               Response.Value := Value.context.Value.activeSignatureHelp.Value;

               return;
            end if;
         end;

         --  Try to get signatures before the cursor location
         --  i.e "Foo (1,|" => "Foo (1|,"

         LSP.Ada_Completions.Parameters.Propose_Signatures
           (Context         => Context,
            Node            => Node,
            Cursor          => Location,
            Prev_Signatures => Value.context,
            Res             => Response.Value);

         --  Retry to get signature in the previous non whitespace token
         --  i.e. "Foo (1, 2 + |" => "Foo (1, 2 +|"

         if Response.Value.signatures.Is_Empty then
            declare
               use all type Libadalang.Common.Token_Kind;
               use type Libadalang.Common.Token_Reference;

               Token : Libadalang.Common.Token_Reference :=
                 Document.Get_Token_At (Context.all, Position);

            begin
               if Token /= Libadalang.Common.No_Token
                 and then Libadalang.Common.Kind
                            (Libadalang.Common.Data (Token)) = Ada_Whitespace
               then
                  Token :=
                    Libadalang.Common.Previous
                      (Token, Exclude_Trivia => True);
               end if;

               Position := LSP.Ada_Handlers.Locations.Start_Position (Token);
            end;

            Node := Document.Get_Node_At (Context.all, Position);
            LSP.Ada_Completions.Parameters.Propose_Signatures
              (Context         => Context,
               Node            => Node,
               Cursor          => Location,
               Prev_Signatures => Value.context,
               Res             => Response.Value);
         end if;

         --  Retry to get signatures in the cursor position.
         --  It handles the edge case of nested function closing
         --  i.e. "Foo (Bar (1)|"

         if Response.Value.signatures.Is_Empty then
            Node := Document.Get_Node_At (Context.all, Value.position);
            LSP.Ada_Completions.Parameters.Propose_Signatures
              (Context         => Context,
               Node            => Node,
               Cursor          => Location,
               Prev_Signatures => Value.context,
               Res             => Response.Value);
         end if;
      end Compute_Response;

   begin
      Compute_Response;
      Self.Sender.On_SignatureHelp_Response (Id, Response);
   end On_SignatureHelp_Request;

   -----------------------------
   -- On_Tokens_Range_Request --
   -----------------------------

   overriding procedure On_Tokens_Range_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensRangeParams)
   is
      use type LSP.Ada_Documents.Document_Access;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Self.Get_Open_Document (Value.textDocument.uri);

      Context  : constant LSP.Ada_Context_Sets.Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);

      Response : LSP.Structures.SemanticTokens_Or_Null (Is_Null => False);

      Result   : LSP.Structures.Natural_Vector renames
        Response.Value.data;
   begin
      if Document /= null then
         Result := Document.Get_Tokens
           (Context.all, Self.Highlighter, Value.a_range);
      end if;

      Self.Sender.On_Full_Response (Id, Response);
   end On_Tokens_Range_Request;

   -------------------------------
   -- On_TypeDefinition_Request --
   -------------------------------

   overriding procedure On_TypeDefinition_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeDefinitionParams)
   is

      Response   : LSP.Structures.Definition_Result (LSP.Structures.Variant_1);
      Vector     : LSP.Structures.Location_Vector renames Response.Variant_1;
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

   -----------------------
   -- To_Workspace_Edit --
   -----------------------

   function To_Workspace_Edit
     (Self   : in out Message_Handler'Class;
      Edits  : LAL_Refactor.Refactoring_Edits;
      Rename : Boolean := False)
      return LSP.Structures.WorkspaceEdit
   is
      File_URI   : LSP.Structures.DocumentUri;
      Text_Edits : LSP.Structures.TextEdit_Vector;

      use LAL_Refactor;
      use LSP.Structures;

      Text_Edits_Cursor     : Text_Edit_Ordered_Maps.Cursor :=
        Edits.Text_Edits.First;
      File_Deletions_Cursor : Unbounded_String_Ordered_Sets.Cursor :=
        Edits.File_Deletions.First;

      function To_TextEdit
        (E : LAL_Refactor.Text_Edit)
         return LSP.Structures.TextEdit is
        (LSP.Structures.TextEdit'
           (LSP.Utils.To_Range (E.Location),
            VSS.Strings.Conversions.To_Virtual_String (E.Text)));

   begin
      return WE : LSP.Structures.WorkspaceEdit do
         --  Text edits

         while Text_Edit_Ordered_Maps.Has_Element (Text_Edits_Cursor) loop
            Text_Edits.Clear;

            for Edit of Text_Edit_Ordered_Maps.Element (Text_Edits_Cursor) loop
               Text_Edits.Append (To_TextEdit (Edit));
            end loop;

            File_URI := To_DocumentUri
              (VSS.Strings.Conversions.To_Virtual_String
                 (Text_Edit_Ordered_Maps.Key (Text_Edits_Cursor)));

            --  If `workspace.workspaceEdit.documentChanges` client capability
            --  was true, then use `TextDocumentEdit[]` instead of
            --  `TextEdit[]`.

            if Self.Client.Versioned_Documents then
               declare
                  Annotaded_Edits : TextEdit_Or_AnnotatedTextEdit_Vector;

               begin
                  Annotaded_Edits.Reserve_Capacity (Text_Edits.Capacity);
                  for X of Text_Edits loop
                     Annotaded_Edits.Append
                       (TextEdit_Or_AnnotatedTextEdit'
                          (Is_TextEdit       => False,
                           AnnotatedTextEdit => (X with annotationId => <>)));
                  end loop;

                  WE.documentChanges.Append
                    (documentChanges_OfWorkspaceEdit_Item'(
                     (Kind     => Variant_1,
                      Variant_1 => TextDocumentEdit'
                        (textDocument => Self.Get_Open_Document_Version
                           (File_URI),
                         edits        => Annotaded_Edits))));
               end;
            else
               WE.changes.Insert (File_URI, Text_Edits);
            end if;

            Text_Edit_Ordered_Maps.Next (Text_Edits_Cursor);
         end loop;

         --  Resource operations are only supported if
         --  `workspace.workspaceEdit.documentChanges` is True since they
         --  must be sent in the `documentChanges` field.
         --  `workspace.workspaceEdit.resourceOperations` client capability
         --  must be checked in order to know which kind of operations are
         --  supported.

         --  File creations

         if Self.Client.Versioned_Documents
           and then Self.Client.Resource_Create_Supported
         then
            for File_Creation of Edits.File_Creations loop
               WE.documentChanges.Append
                 (documentChanges_OfWorkspaceEdit_Item'(
                  (Kind   => create,
                   create => CreateFile'
                     (uri    => To_DocumentUri
                        (VSS.Strings.Conversions.To_Virtual_String
                             (File_Creation.Filepath)),
                      others => <>))));

               declare
                  Annotaded_Edits : TextEdit_Or_AnnotatedTextEdit_Vector;
                  Content : constant TextEdit := TextEdit'
                    (a_range    => ((0, 0), (0, 0)),
                     newText =>
                       VSS.Strings.Conversions.To_Virtual_String
                         (File_Creation.Content));

               begin
                  Annotaded_Edits.Append
                    (TextEdit_Or_AnnotatedTextEdit'
                       (Is_TextEdit => True, TextEdit => Content));

                  WE.documentChanges.Append
                    (documentChanges_OfWorkspaceEdit_Item'(
                     (Kind     => Variant_1,
                      Variant_1 => TextDocumentEdit'
                        (edits => Annotaded_Edits,
                         others => <>))));
               end;
            end loop;
         end if;

         --  File deletions

         if Self.Client.Versioned_Documents
           and then Self.Client.Resource_Delete_Supported
         then
            while Unbounded_String_Ordered_Sets.Has_Element
              (File_Deletions_Cursor)
            loop
               File_URI := To_DocumentUri
                 (VSS.Strings.Conversions.To_Virtual_String
                    (Unbounded_String_Ordered_Sets.Element
                         (File_Deletions_Cursor)));

               WE.documentChanges.Append
                 (documentChanges_OfWorkspaceEdit_Item'(
                  (Kind   => LSP.Structures.rename,
                   rename => RenameFile'
                     (oldUri       => File_URI,
                      newUri       =>
                        (if Rename
                         then File_URI & ".bak"
                         else File_URI),
                      others => <>))));

               Unbounded_String_Ordered_Sets.Next (File_Deletions_Cursor);
            end loop;
         end if;

         --  File renames

         if Self.Client.Versioned_Documents
           and then Self.Client.Resource_Rename_Supported
         then
            for File_Rename of Edits.File_Renames loop
               WE.documentChanges.Append
                 (documentChanges_OfWorkspaceEdit_Item'(
                  (Kind   => LSP.Structures.rename,
                   rename => RenameFile'
                     (oldUri => To_DocumentUri
                        (VSS.Strings.Conversions.To_Virtual_String
                             (File_Rename.Filepath)),
                      newUri => To_DocumentUri
                        (VSS.Strings.Conversions.To_Virtual_String
                             (File_Rename.New_Name)),
                      others => <>))));
            end loop;
         end if;
      end return;
   end To_Workspace_Edit;

end LSP.Ada_Handlers;

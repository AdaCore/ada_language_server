------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2024, AdaCore                          --
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

with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Source_Reference;
with LSP.Constants;
with LSP.Enumerations;
with LSP.Utils;
with URIs;
with VSS.JSON;
with VSS.JSON.Streams;
with VSS.Strings;
with VSS.Strings.Conversions;

package body LSP.Ada_Project_Loading is

   function Load_Status_Message
     (Project : Project_Status_Type)
      return VSS.Strings.Virtual_String;
   --  Message describing the project status

   function Load_Status_Severity
     (Project : Project_Status_Type)
      return LSP.Enumerations.DiagnosticSeverity;
   --  Messages severity of the project status

   function Has_Diagnostics
     (Project : Project_Status_Type) return Boolean;
   --  Return True if Project has diagnostics

   function Has_Pertinent_GPR2_Messages
     (Project : Project_Status_Type) return Boolean;
   --  Return True if GPR2_Messages has warnings or errors

   -------------------------
   -- Load_Status_Message --
   -------------------------

   function Load_Status_Message
     (Project : Project_Status_Type)
      return VSS.Strings.Virtual_String is
   begin
      case Project.Status is
         when Valid_Project =>
            if Project.Project_Type = Single_Project_Found then
               return VSS.Strings.To_Virtual_String
                 ("A unique project in the root directory was found"
                  & " and loaded but it was not explicitly configured.");
            else
               return VSS.Strings.Empty_Virtual_String;
            end if;
         when No_Project =>
            return VSS.Strings.To_Virtual_String
              ("No project was found in the root directory."
               & " Please create a GPR project file"
               & " and add it to the configuration.");
         when Project_Not_Found =>
            return VSS.Strings.Conversions.To_Virtual_String
              ("The configured project "
               & URIs.Conversions.From_File
                 (Project.Project_File.Display_Full_Name)
               & " does not exist.");
         when Multiple_Projects =>
            return VSS.Strings.To_Virtual_String
              ("No project was loaded because"
               & " multiple project files were found in the root directory."
               & " Please change the configuration"
               & " to point to a single project file.");
         when Invalid_Project =>
            return VSS.Strings.To_Virtual_String
              ("The project file has errors and could not be loaded.");
         when Warning_In_Project =>
            return VSS.Strings.Empty_Virtual_String;
      end case;
   end Load_Status_Message;

   --------------------------
   -- Load_Status_Severity --
   --------------------------

   function Load_Status_Severity
     (Project : Project_Status_Type)
      return LSP.Enumerations.DiagnosticSeverity is
   begin
      case Project.Status is
         when Valid_Project =>
            if not Project.Has_Runtime then
               return LSP.Enumerations.Warning;
            else
               return LSP.Enumerations.Hint;
            end if;
         when Warning_In_Project | No_Project =>
            return LSP.Enumerations.Warning;
         when Multiple_Projects .. Project_Not_Found =>
            return LSP.Enumerations.Error;
      end case;
   end Load_Status_Severity;

   ---------------------
   -- Get_Diagnostics --
   ---------------------

   function Get_Diagnostics
     (Project : Project_Status_Type)
      return LSP.Structures.Diagnostic_Vector
   is
      use LSP.Structures;

      Result            : LSP.Structures.Diagnostic_Vector;
      Parent_Diagnostic : LSP.Structures.Diagnostic;
      GPR2_Messages     : GPR2.Log.Object renames Project.GPR2_Messages;

      procedure Create_Project_Loading_Diagnostic;
      --  Create a parent diagnostic for the project loading status.

      procedure Append_GPR2_Diagnostics;
      --  Append the GPR2 messages to the given parent diagnostic, if any.

      procedure Append_Runtime_Diagnostic;
      --  Append a diagnostic if no runtime has been found for the project

      ---------------------------------------
      -- Create_Project_Loading_Diagnostic --
      ---------------------------------------

      procedure Create_Project_Loading_Diagnostic is
         Sloc : constant LSP.Structures.A_Range :=
           (start  => (0, 0),
            an_end => (0, 0));
      begin
         --  Initialize the parent diagnostic.
         Parent_Diagnostic.a_range := ((0, 0), (0, 0));
         Parent_Diagnostic.source := "project";
         Parent_Diagnostic.severity :=
           (True, Load_Status_Severity (Project));

         --  If we don't have any GPR2 messages, display the project loading
         --  status message in the parent diagnostic directly.
         --  Otherwise display a generic message in the parent amnd append it
         --  to its children, along with the other GPR2 messages.
         if GPR2_Messages.Is_Empty then
            Parent_Diagnostic.message :=
              Load_Status_Message (Project);
         else
            declare
               Project_File : GNATCOLL.VFS.Virtual_File renames
                 Project.Project_File;
               URI          : constant LSP.Structures.DocumentUri :=
                 (VSS.Strings.Conversions.To_Virtual_String
                    (URIs.Conversions.From_File
                         (Project_File.Display_Full_Name)) with null record);
            begin
               Parent_Diagnostic.message := "Project Problems";
               Parent_Diagnostic.relatedInformation.Append
                 (LSP.Structures.DiagnosticRelatedInformation'
                    (location =>
                         LSP.Structures.Location'
                       (uri    => URI, a_range => Sloc,
                        others => <>),
                     message  =>
                       Load_Status_Message (Project)));
            end;
         end if;
      end Create_Project_Loading_Diagnostic;

      -----------------------------
      -- Append_GPR2_Diagnostics --
      -----------------------------

      procedure Append_GPR2_Diagnostics is
         use GPR2.Message;
         use LSP.Enumerations;
      begin
         for Msg of GPR2_Messages loop
            if Msg.Level in GPR2.Message.Warning .. GPR2.Message.Error then
               declare
                  Sloc : constant GPR2.Source_Reference.Object :=
                    GPR2.Message.Sloc (Msg);
                  File : constant GPR2.Path_Name.Object :=
                    (if Sloc.Is_Defined and then Sloc.Has_Source_Reference
                     then
                        GPR2.Path_Name.Create_File
                       (GPR2.Filename_Type (Sloc.Filename))
                     else
                        GPR2.Path_Name.Undefined);
               begin
                  --  Display a diagnostic for GPR2 messages only if the file
                  --  attached to the message is defined.
                  if File.Is_Defined and then File.Has_Value then
                     Parent_Diagnostic.relatedInformation.Append
                       (LSP.Structures.DiagnosticRelatedInformation'
                          (location => LSP.Structures.Location'
                               (uri     => LSP.Utils.To_URI (File),
                                a_range => LSP.Utils.To_Range (Sloc),
                                others  => <>),
                           message  =>
                             VSS.Strings.Conversions.To_Virtual_String
                               (Msg.Message)));
                  end if;
               end;

               --  If we have one error in the GPR2 messages, the parent
               --  diagnostic's severity should be "error" too, otherwise
               --  "warning".
               if Msg.Level = GPR2.Message.Error then
                  Parent_Diagnostic.severity :=
                    (True, LSP.Enumerations.Error);
               elsif Parent_Diagnostic.severity.Value /=
                 LSP.Enumerations.Error
               then
                  Parent_Diagnostic.severity :=
                    (True, LSP.Enumerations.Warning);
               end if;
            end if;
         end loop;
      end Append_GPR2_Diagnostics;

      -------------------------------
      -- Append_Runtime_Diagnostic --
      -------------------------------

      procedure Append_Runtime_Diagnostic is
      begin
         if Project.Status not in No_Project .. Project_Not_Found
           and then not Project.Has_Runtime
         then
            Parent_Diagnostic.relatedInformation.Append
              (LSP.Structures.DiagnosticRelatedInformation'
                 (location =>
                      (uri     => "",
                       a_range => (start  => (0, 0),
                                   an_end => (0, 0)),
                       others  => <>),
                  message  => VSS.Strings.Conversions.To_Virtual_String
                    ("The project was loaded, but no Ada runtime found. "
                     & "Please check the installation of the Ada compiler.")));
         end if;
      end Append_Runtime_Diagnostic;

   begin

      if not Has_Diagnostics (Project) then
         return Result;
      end if;

      Create_Project_Loading_Diagnostic;
      Append_GPR2_Diagnostics;
      Append_Runtime_Diagnostic;
      Result.Append (Parent_Diagnostic);
      return Result;
   end Get_Diagnostics;

   -------------------------
   -- Has_New_Diagnostics --
   -------------------------

   function Has_New_Diagnostics
     (Old_Project : Project_Status_Type;
      New_Project : Project_Status_Type)
      return Boolean is
   begin
      return
        Old_Project.Status /= New_Project.Status
        or else (Old_Project.Project_Type /= New_Project.Project_Type
                 and then Has_Pertinent_GPR2_Messages (New_Project));
   end Has_New_Diagnostics;

   ---------------------
   -- Has_Diagnostics --
   ---------------------

   function Has_Diagnostics
     (Project : Project_Status_Type) return Boolean is
   begin
      if not Project.Has_Runtime then
         return True;
      end if;

      case Project.Project_Type is
         when Single_Project_Found .. Implicit_Project =>
            --  Even if the project is valid this is not an expected one
            return True;
         when others =>
            return Project.Status /= Valid_Project
              or else Has_Pertinent_GPR2_Messages (Project);
      end case;
   end Has_Diagnostics;

   ---------------------------------
   -- Has_Pertinent_GPR2_Messages --
   ---------------------------------

   function Has_Pertinent_GPR2_Messages
     (Project : Project_Status_Type) return Boolean is
   begin
      for Msg of Project.GPR2_Messages loop
         if Msg.Level in GPR2.Message.Warning .. GPR2.Message.Error then
            return True;
         end if;
      end loop;

      return False;
   end Has_Pertinent_GPR2_Messages;

   ---------------------
   -- Set_Load_Status --
   ---------------------

   procedure Set_Load_Status
     (Project : in out Project_Status_Type;
      Status  : Project_Status) is
   begin
      Project.Status := Status;
   end Set_Load_Status;

   ----------------------
   -- Set_Project_Type --
   ----------------------

   procedure Set_Project_Type
     (Project      : in out Project_Status_Type;
      Project_Type : Project_Types) is
   begin
      Project.Project_Type := Project_Type;
   end Set_Project_Type;

   ---------------------
   -- Set_Has_Runtime --
   ---------------------

   procedure Set_Has_Runtime
     (Project      : in out Project_Status_Type;
      Has_Runtime  : Boolean) is
   begin
      Project.Has_Runtime := Has_Runtime;
   end Set_Has_Runtime;

   -----------------------
   -- Set_GPR2_Messages --
   -----------------------

   procedure Set_GPR2_Messages
     (Project       : in out Project_Status_Type;
      GPR2_Messages : GPR2.Log.Object) is
   begin
      Project.GPR2_Messages := GPR2_Messages;
   end Set_GPR2_Messages;

   ----------------------
   -- Set_Project_File --
   ----------------------

   procedure Set_Project_File
     (Project      : in out Project_Status_Type;
      Project_File : GNATCOLL.VFS.Virtual_File) is
   begin
      Project.Project_File := Project_File;
   end Set_Project_File;

   --------------------------
   -- Is_Implicit_Fallback --
   --------------------------

   function Is_Implicit_Fallback
     (Project : Project_Status_Type) return Boolean is
   begin
      return Project.Project_Type = Implicit_Project;
   end Is_Implicit_Fallback;

   -----------------------
   -- Is_Project_Loaded --
   -----------------------

   function Is_Project_Loaded
     (Project : Project_Status_Type) return Boolean is
   begin
      return Project.Status = Valid_Project
        or else Project.Status = Warning_In_Project;
   end Is_Project_Loaded;

   ---------------------------------
   -- Project_Status_Code_Actions --
   ---------------------------------

   procedure Project_Status_Code_Actions
     (Result      : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Project     : Project_Status_Type;
      Diagnostics : LSP.Structures.Diagnostic_Vector;
      Default_URI : LSP.Structures.DocumentUri) is
   begin
      case Project.Status is
         when Valid_Project =>
            null;
         when Multiple_Projects
            | Project_Not_Found =>
            declare
               Item    : LSP.Structures.CodeAction;
               Command : LSP.Structures.Command;
               Arg     : constant VSS.JSON.Streams.JSON_Stream_Element :=
                 VSS.JSON.Streams.JSON_Stream_Element'
                   (Kind         => VSS.JSON.Streams.String_Value,
                    String_Value => "ada.projectFile");
            begin
               Command.title :=
                 "Open settings to set ada.projectFile to a valid project";
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
         when No_Project =>
            declare
               Title  : constant VSS.Strings.Virtual_String :=
                 "Create a default project file (default.gpr)";

               Create : constant LSP.Structures.
                 documentChanges_OfWorkspaceEdit_Item :=
                   (Kind    => LSP.Structures.create,
                    create  => (uri    => Default_URI,
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
                    (textDocument => (uri => Default_URI, others => <>),
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
         when Invalid_Project | Warning_In_Project =>
            null;
      end case;
   end Project_Status_Code_Actions;

end LSP.Ada_Project_Loading;

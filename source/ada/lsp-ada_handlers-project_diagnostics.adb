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

with GPR2.Source_Reference;
with GPR2.Message;
with GPR2.Path_Name;

with VSS.Strings;

with LSP.Enumerations;
with LSP.Utils;

package body LSP.Ada_Handlers.Project_Diagnostics is

   Project_Loading_Status_Messages : constant array (Load_Project_Status)
     of VSS.Strings.Virtual_String :=
       (Single_Project_Found       =>
          VSS.Strings.To_Virtual_String
            ("Unique project in root directory was found and "
             & "loaded, but it wasn't explicitly configured."),
        No_Runtime_Found           =>
          VSS.Strings.To_Virtual_String
            ("The project was loaded, but no Ada runtime found. "
             & "Please check the installation of the Ada compiler."),
        No_Project_Found           =>
          VSS.Strings.To_Virtual_String
            ("No project found in root directory. "
             & "Please create a project file and add it to the "
             & "configuration."),
        Multiple_Projects_Found    =>
          VSS.Strings.To_Virtual_String
            ("No project was loaded, because more than one "
             & "project file has been found in the root directory. "
             & "Please change configuration to point a correct project "
             & "file."),
        Invalid_Project_Configured =>
          VSS.Strings.To_Virtual_String
            ("Project file has errors and can't be loaded."),
        others                     => VSS.Strings.Empty_Virtual_String);
   --  The diagnostics' messages depending on the project loading status.

   Project_Loading_Status_Severities : constant array (Load_Project_Status)
     of LSP.Enumerations.DiagnosticSeverity :=
       (Valid_Project_Configured   => LSP.Enumerations.Hint,
        Alire_Project              => LSP.Enumerations.Hint,
        Single_Project_Found       => LSP.Enumerations.Hint,
        No_Runtime_Found           => LSP.Enumerations.Warning,
        Multiple_Projects_Found    => LSP.Enumerations.Error,
        No_Project_Found           => LSP.Enumerations.Error,
        Invalid_Project_Configured => LSP.Enumerations.Error);
   --  The diagnostics' severities depending on the project loading status.

   --------------------
   -- Get_Diagnostic --
   --------------------

   overriding procedure Get_Diagnostic
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context;
      Errors  : out LSP.Structures.Diagnostic_Vector)
   is
      use LSP.Structures;

      Parent_Diagnostic : LSP.Structures.Diagnostic;
      GPR2_Messages     : GPR2.Log.Object renames
        Self.Handler.Project_Status.GPR2_Messages;

      procedure Create_Project_Loading_Diagnostic;
      --  Create a parent diagnostic for the project loading status.

      procedure Append_GPR2_Diagnostics;
      --  Append the GPR2 messages to the given parent diagnostic, if any.

      ---------------------------------------
      -- Create_Project_Loading_Diagnostic --
      ---------------------------------------

      procedure Create_Project_Loading_Diagnostic is
         Project_File : GNATCOLL.VFS.Virtual_File renames
           Self.Handler.Project_Status.Project_File;
         URI          : constant LSP.Structures.DocumentUri :=
           Self.Handler.To_URI (Project_File.Display_Full_Name);
         Sloc         : constant LSP.Structures.A_Range :=
           (start  => (0, 0),
            an_end => (0, 0));
      begin
         --  Initialize the parent diagnostic.
         Parent_Diagnostic.a_range := ((0, 0), (0, 0));
         Parent_Diagnostic.source := "project";
         Parent_Diagnostic.severity :=
           (True, Project_Loading_Status_Severities (Self.Last_Status));

         --  If we don't have any GPR2 messages, display the project loading
         --  status message in the parent diagnostic directly.
         --  Otherwise display a generic message in the parent amnd append it
         --  to its children, along with the other GPR2 messages.
         if GPR2_Messages.Is_Empty then
            Parent_Diagnostic.message := Project_Loading_Status_Messages
              (Self.Last_Status);
         else
            Parent_Diagnostic.message := "Project Problems";
            Parent_Diagnostic.relatedInformation.Append
              (LSP .Structures.DiagnosticRelatedInformation'
                 (location => LSP.Structures.Location'
                      (uri     => URI,
                       a_range => Sloc,
                       others  => <>),
                  message  => Project_Loading_Status_Messages
                    (Self.Last_Status)));
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
                    (if Sloc.Is_Defined and then Sloc.Has_Source_Reference then
                        GPR2.Path_Name.Create_File
                          (GPR2.Filename_Type (Sloc.Filename))
                     else
                        Self.Handler.Project_Tree.Root_Path);
               begin
                  Parent_Diagnostic.relatedInformation.Append
                    (LSP .Structures.DiagnosticRelatedInformation'
                       (location => LSP.Structures.Location'
                            (uri     => LSP.Utils.To_URI (File),
                             a_range => LSP.Utils.To_Range (Sloc),
                             others  => <>),
                        message  => VSS.Strings.Conversions.To_Virtual_String
                          (Msg.Message)));
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

   begin
      Self.Last_Status := Self.Handler.Project_Status.Load_Status;

      --  If we have a valid project return immediately: we want to display
      --  diagnostics only if there is an issue to solve or a potential
      --  enhancement.
      if Self.Last_Status = Valid_Project_Configured
        or else (Self.Last_Status = Alire_Project and then GPR2_Messages.Is_Empty)
      then
         return;
      end if;

      Create_Project_Loading_Diagnostic;
      Append_GPR2_Diagnostics;

      Errors.Append (Parent_Diagnostic);
   end Get_Diagnostic;

   ------------------------
   -- Has_New_Diagnostic --
   ------------------------

   overriding function Has_New_Diagnostic
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context)
      return Boolean
   is
      pragma Unreferenced (Context);
   begin
      return
        (Self.Last_Status /= Self.Handler.Project_Status.Load_Status
         or else not Self.Handler.Project_Status.GPR2_Messages.Is_Empty);
   end Has_New_Diagnostic;

end LSP.Ada_Handlers.Project_Diagnostics;

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

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;

with GNATCOLL.JSON;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with URIs;
with LSP.Ada_Unit_Providers;
with LSP.Common; use LSP.Common;

with Libadalang.Common;
with Langkit_Support.Slocs;

package body LSP.Ada_Contexts is

   function Get_Charset (Self : Context'Class) return String;
   --  Return the charset with which the context was initialized

   procedure Free is new Ada.Unchecked_Deallocation
     (LSP.Ada_Documents.Document,
      Internal_Document_Access);

   function Analysis_Units
     (Self : Context) return Libadalang.Analysis.Analysis_Unit_Array;
   --  Return the analysis units for all Ada sources known to this context

   -----------------
   -- File_To_URI --
   -----------------

   function File_To_URI
     (File : LSP.Types.LSP_String) return LSP.Types.LSP_String
   is
      Result : constant URIs.URI_String :=
        URIs.Conversions.From_File (LSP.Types.To_UTF_8_String (File));
   begin
      return LSP.Types.To_LSP_String (Result);
   end File_To_URI;

   --------------------
   -- Analysis_Units --
   --------------------

   function Analysis_Units
     (Self : Context) return Libadalang.Analysis.Analysis_Unit_Array
   is
      Source_Units : Libadalang.Analysis.Analysis_Unit_Array
        (Self.Source_Files'Range);
   begin
      for N in Self.Source_Files'Range loop
         Source_Units (N) := Self.LAL_Context.Get_From_File
           (Self.Source_Files (N).Display_Full_Name,
            Charset => Self.Get_Charset);
      end loop;
      return Source_Units;
   end Analysis_Units;

   -------------------------
   -- Find_All_References --
   -------------------------

   function Find_All_References
     (Self              : Context;
      Definition        : Libadalang.Analysis.Defining_Name;
      Imprecise_Results : out Boolean)
      return Libadalang.Analysis.Base_Id_Array
   is
      Units : constant Libadalang.Analysis.Analysis_Unit_Array :=
        Self.Analysis_Units;
   begin
      Imprecise_Results := False;

      --  Make two attempts: first with precise results, then with the
      --  imprecise_fallback.
      begin
         return Definition.P_Find_All_References (Units);
      exception
         when E : Libadalang.Common.Property_Error =>
            Imprecise_Results := True;
            Log (Self.Trace, E, "in Find_All_References (precise)");
            return Definition.P_Find_All_References
              (Units, Imprecise_Fallback => True);
      end;
   exception
      when E : Libadalang.Common.Property_Error =>
         Log (Self.Trace, E, "in Find_All_References (imprecise)");
         return (1 .. 0 => <>);
   end Find_All_References;

   ------------------------
   -- Find_All_Overrides --
   ------------------------

   function Find_All_Overrides
     (Self              : Context;
      Decl              : Libadalang.Analysis.Basic_Decl;
      Imprecise_Results : out Boolean)
      return Libadalang.Analysis.Basic_Decl_Array
   is
      Units : constant Libadalang.Analysis.Analysis_Unit_Array :=
                Self.Analysis_Units;
   begin
      Imprecise_Results := False;

      --  Make two attempts: first with precise results, then with the
      --  imprecise_fallback.
      begin
         return Decl.P_Find_All_Overrides (Units);
      exception
         when E : Libadalang.Common.Property_Error =>
            Imprecise_Results := True;
            Log (Self.Trace, E, "in Find_All_Overrides (precise)");
            return Decl.P_Find_All_Overrides
              (Units, Imprecise_Fallback => True);
      end;
   exception
      when E : Libadalang.Common.Property_Error =>
         Log (Self.Trace, E, "in Find_All_Overrides (imprecise)");
         return (1 .. 0 => <>);
   end Find_All_Overrides;

   --------------------
   -- Find_All_Calls --
   --------------------

   function Find_All_Calls
     (Self              : Context;
      Definition        : Libadalang.Analysis.Defining_Name;
      Imprecise_Results : out Boolean)
      return Libadalang.Analysis.Base_Id_Array
   is
      Units : constant Libadalang.Analysis.Analysis_Unit_Array :=
        Self.Analysis_Units;
   begin
      Imprecise_Results := False;

      --  Make two attempts: first with precise results, then with the
      --  imprecise_fallback.
      begin
         return Definition.P_Find_All_Calls (Units);
      exception
         when E : Libadalang.Common.Property_Error =>
            Imprecise_Results := True;
            Log (Self.Trace, E, "in Is_Called_By (precise)");
            return Definition.P_Find_All_Calls
              (Units, Imprecise_Fallback => True);
      end;
   exception
      when E : Libadalang.Common.Property_Error =>
         Log (Self.Trace, E, "in Is_Called_By (imprecise)");
         return (1 .. 0 => <>);
   end Find_All_Calls;

   ------------------
   -- Has_Document --
   ------------------

   function Has_Document
     (Self : Context;
      URI  : LSP.Messages.DocumentUri) return Boolean is
   begin
      return Self.Documents.Contains (URI);
   end Has_Document;

   ------------------
   -- Get_Document --
   ------------------

   function Get_Document
     (Self : Context;
      URI  : LSP.Messages.DocumentUri)
        return LSP.Ada_Documents.Document_Access
   is
      Object : constant Internal_Document_Access := Self.Documents (URI);
   begin
      return LSP.Ada_Documents.Document_Access (Object);
   end Get_Document;

   -----------------
   -- Get_Charset --
   -----------------

   function Get_Charset (Self : Context'Class) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Self.Charset);
   end Get_Charset;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Context) is
   begin
      Self.Charset := Ada.Strings.Unbounded.To_Unbounded_String
        ("iso-8859-1");
      Self.LAL_Context := Libadalang.Analysis.Create_Context
        (Unit_Provider => Self.Unit_Provider,
         With_Trivia   => True,
         Charset       => Self.Get_Charset);
   end Initialize;

   ------------------------
   -- Is_Part_Of_Project --
   ------------------------

   function Is_Part_Of_Project
     (Self : Context;
      File : Virtual_File) return Boolean is
   begin
      --  If there is no project, it's easy to answer this question
      if Self.Project_Tree = null then
         return False;
      end if;

      declare
         Set   : constant File_Info_Set :=
           Self.Project_Tree.Info_Set (File);
         First : constant File_Info'Class :=
           File_Info'Class (Set.First_Element);
      begin
         return First.Project /= No_Project;
      end;
   end Is_Part_Of_Project;

   -------------------
   -- Load_Document --
   -------------------

   function Load_Document
     (Self : aliased in out Context;
      Item : LSP.Messages.TextDocumentItem)
      return LSP.Ada_Documents.Document_Access
   is
      Object : constant Internal_Document_Access :=
        new LSP.Ada_Documents.Document (Self'Unchecked_Access);
   begin
      Object.Initialize (Self.LAL_Context, Item);
      Self.Documents.Insert (Item.uri, Object);

      declare
         Name : constant LSP.Types.LSP_String := URI_To_File (Item.uri);
         File : constant Virtual_File := Create
           (Filesystem_String (LSP.Types.To_UTF_8_String (Name)),
            Normalize => True);
      begin
         if not Self.Is_Part_Of_Project (File) then
            --  File that we are loading doesn't belong to the project, so
            --  firstly recompute project view just in case this is a new file
            --  in the project. Then update list of source files.

            if Self.Project_Tree /= null then
               Self.Project_Tree.Recompute_View;
            end if;

            Self.Update_Source_Files;
         end if;
      end;

      return LSP.Ada_Documents.Document_Access (Object);
   end Load_Document;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Self     : in out Context;
      Tree     : not null GNATCOLL.Projects.Project_Tree_Access;
      Root     : not null GNATCOLL.Projects.Project_Type_Access;
      Charset  : String) is
   begin
      Self.Charset := Ada.Strings.Unbounded.To_Unbounded_String (Charset);

      Self.Project_Tree := Tree;
      declare
         Provider : LSP.Ada_Unit_Providers.Unit_Provider
             (Self.Project_Tree, Root);
      begin
         Self.Unit_Provider.Set (Provider);
      end;

      Self.Reload;
      Self.Update_Source_Files;
   end Load_Project;

   ------------
   -- Reload --
   ------------

   procedure Reload (Self : in out Context) is
   begin
      Self.LAL_Context := Libadalang.Analysis.Create_Context
        (Unit_Provider => Self.Unit_Provider,
         With_Trivia   => True,
         Charset       => Self.Get_Charset);

      --  After re-creation of the LAL context all Analysis_Units become
      --  outdated, let's refresh all of them
      for Document of Self.Documents loop
         Document.Reload (Self.LAL_Context);
      end loop;
   end Reload;

   ------------
   -- Unload --
   ------------

   procedure Unload (Self : in out Context) is
   begin
      for Document of Self.Documents loop
         Free (Document);
      end loop;
      Self.Documents.Clear;
   end Unload;

   ---------------------
   -- Unload_Document --
   ---------------------

   procedure Unload_Document
     (Self : in out Context;
      Item : LSP.Messages.TextDocumentIdentifier)
   is
      Document : Internal_Document_Access :=
        Self.Documents.Element (Item.uri);
   begin
      Self.Documents.Delete (Item.uri);
      Free (Document);
   end Unload_Document;

   -------------------------
   -- Update_Source_Files --
   -------------------------

   procedure Update_Source_Files (Self : in out Context) is
   begin
      if Self.Project_Tree = null then
         return;
      end if;

      declare
         All_Sources : File_Array_Access :=
           Self.Project_Tree.Root_Project.Source_Files (Recursive => True);
         All_Ada_Sources : File_Array (1 .. All_Sources'Length);
         Free_Index      : Natural := All_Ada_Sources'First;
         Set             : File_Info_Set;
      begin
         --  Iterate through all sources, returning only those that have Ada as
         --  language.
         for J in All_Sources'Range loop
            Set := Self.Project_Tree.Info_Set (All_Sources (J));
            if not Set.Is_Empty then
               --  The file can be listed in several projects with different
               --  Info_Sets, in the case of aggregate project. However, assume
               --  that the language is the same in all projects, so look only
               --  at the first entry in the set.
               declare
                  Info : constant File_Info'Class :=
                    File_Info'Class (Set.First_Element);
               begin
                  if To_Lower (Info.Language) = "ada" then
                     All_Ada_Sources (Free_Index) := All_Sources (J);
                     Free_Index := Free_Index + 1;
                  end if;
               end;
            end if;
         end loop;

         Unchecked_Free (All_Sources);
         Unchecked_Free (Self.Source_Files);

         Self.Source_Files :=
           new File_Array'(All_Ada_Sources (1 .. Free_Index - 1));
      end;
   end Update_Source_Files;

   -----------------
   -- URI_To_File --
   -----------------

   function URI_To_File
     (URI : LSP.Types.LSP_String) return LSP.Types.LSP_String
   is
      To     : constant URIs.URI_String := LSP.Types.To_UTF_8_String (URI);
      Result : constant String := URIs.Conversions.To_File (To);
   begin
      return LSP.Types.To_LSP_String (Result);
   end URI_To_File;

   -----------------
   -- Get_Node_At --
   -----------------

   function Get_Node_At
     (Self     : Context;
      Position : LSP.Messages.TextDocumentPositionParams'Class)
      return Libadalang.Analysis.Ada_Node
   is
      use type Libadalang.Analysis.Ada_Node;
      use type Langkit_Support.Slocs.Line_Number;
      use type Langkit_Support.Slocs.Column_Number;

      Unit : Libadalang.Analysis.Analysis_Unit;

      URI : constant LSP.Messages.DocumentUri := Position.textDocument.uri;
      File : Virtual_File;
   begin
      --  We're about to get a node from an analysis unit. Either the document
      --  is open for it, in which case we read the document, or the
      --  document is not open for it. In this case, resolve this only
      --  if the file belongs to the project: we don't want to pollute the
      --  LAL context with units that are not in the project.

      if Self.Has_Document (URI) then
         return Self.Get_Document (URI).Get_Node_At (Position.position);
      else
         File := Create
           (Filesystem_String
              (LSP.Types.To_UTF_8_String (URI_To_File (URI))),
            Normalize => True);

         if not Self.Is_Part_Of_Project (File) then
            return Libadalang.Analysis.No_Ada_Node;
         else
            Unit := Self.LAL_Context.Get_From_File
              (LSP.Types.To_UTF_8_String (URI_To_File (URI)),
               Charset => Self.Get_Charset);

            if Unit.Root = Libadalang.Analysis.No_Ada_Node then
               return Libadalang.Analysis.No_Ada_Node;
            end if;

            return Unit.Root.Lookup
              ((Line   => Langkit_Support.Slocs.Line_Number
                (Position.position.line) + 1,
                Column => Langkit_Support.Slocs.Column_Number
                  (Position.position.character) + 1));
         end if;
      end if;
   end Get_Node_At;

   ----------------
   -- List_Files --
   ----------------

   function List_Files
     (Self : Context) return GNATCOLL.VFS.File_Array_Access is
   begin
      return Self.Source_Files;
   end List_Files;

   ----------------
   -- Index_File --
   ----------------

   procedure Index_File (Self : Context; File : GNATCOLL.VFS.Virtual_File) is
      Ignored : Libadalang.Analysis.Analysis_Unit;
   begin
      Ignored := Self.LAL_Context.Get_From_File
        (File.Display_Full_Name, Charset => Self.Get_Charset);
   end Index_File;

end LSP.Ada_Contexts;

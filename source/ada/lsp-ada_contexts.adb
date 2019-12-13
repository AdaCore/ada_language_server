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

with GNATCOLL.JSON;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with URIs;
with LSP.Ada_Unit_Providers;
with LSP.Common; use LSP.Common;
with LSP.Lal_Utils;

with Libadalang.Common;
with Langkit_Support.Slocs;

package body LSP.Ada_Contexts is

   function Get_Charset (Self : Context'Class) return String;
   --  Return the charset with which the context was initialized

   function Analysis_Units
     (Self : Context) return Libadalang.Analysis.Analysis_Unit_Array;
   --  Return the analysis units for all Ada sources known to this context

   -------------------------
   -- Append_Declarations --
   -------------------------

   procedure Append_Declarations
     (Self      : Context;
      Document  : LSP.Ada_Documents.Document_Access;
      Position  : LSP.Messages.TextDocumentPositionParams;
      Result    : in out LSP.Messages.Location_Vector;
      Imprecise : in out Boolean)
   is
      use type Libadalang.Analysis.Name;
      use type Libadalang.Analysis.Defining_Name;

      Name_Node : constant Libadalang.Analysis.Name :=
        LSP.Lal_Utils.Get_Node_As_Name
          (Self.Get_Node_At (Document, Position));

      Definition              : Libadalang.Analysis.Defining_Name;
      --  A defining name that corresponds to Name_Node
      First_Part              : Libadalang.Analysis.Defining_Name;
      --  "Canonical part" of Definition
      Prev_Part               : Libadalang.Analysis.Defining_Name;
      --  A previous name for Definition
      Decl_For_Find_Overrides : Libadalang.Analysis.Basic_Decl;
   begin
      if Name_Node = Libadalang.Analysis.No_Name then
         return;
      end if;

      --  Check if we are on some defining name
      Definition := LSP.Lal_Utils.Get_Name_As_Defining (Name_Node);

      if Definition = Libadalang.Analysis.No_Defining_Name then
         --  If we aren't on a defining_name already then try to resolve
         declare
            Is_Imprecise : Boolean;
         begin
            Definition := LSP.Lal_Utils.Resolve_Name
              (Name_Node, Self.Trace, Is_Imprecise);

            Imprecise := Imprecise or Is_Imprecise;
         end;
      end if;

      if Definition = Libadalang.Analysis.No_Defining_Name then
         return;  --  Name resolution fails, nothing to do.
      end if;

      First_Part := LSP.Lal_Utils.Find_Canonical_Part (Definition, Self.Trace);

      if First_Part = Libadalang.Analysis.No_Defining_Name then
         Decl_For_Find_Overrides := Definition.P_Basic_Decl;
      else
         Decl_For_Find_Overrides := First_Part.P_Basic_Decl;
      end if;

      begin
         Prev_Part := Definition.P_Previous_Part;
      exception
         when E :  Libadalang.Common.Property_Error =>
            Log (Self.Trace, E);
            Prev_Part := Libadalang.Analysis.No_Defining_Name;
      end;

      if Prev_Part /= Libadalang.Analysis.No_Defining_Name then
         --  We have found previous part, return it.
         LSP.Lal_Utils.Append_Location (Result, Prev_Part);
      elsif Definition /= Libadalang.Analysis.No_Defining_Name then
         --  No previous part, return definition itself.
         LSP.Lal_Utils.Append_Location (Result, Definition);
      end if;

      declare
         Is_Imprecise     : Boolean;
         Overriding_Subps : constant Libadalang.Analysis.Basic_Decl_Array :=
           Self.Find_All_Overrides
             (Decl_For_Find_Overrides,
              Imprecise_Results => Is_Imprecise);
      begin
         for Subp of Overriding_Subps loop
            LSP.Lal_Utils.Append_Location (Result, Subp.P_Defining_Name);
         end loop;

         Imprecise := Imprecise or Is_Imprecise;
      end;
   end Append_Declarations;

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

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Self     : in out Context;
      Tree     : not null GNATCOLL.Projects.Project_Tree_Access;
      Root     : Project_Type;
      Charset  : String) is
   begin
      Self.Charset := Ada.Strings.Unbounded.To_Unbounded_String (Charset);

      Self.Project_Tree := Tree;
      declare
         Provider : LSP.Ada_Unit_Providers.Unit_Provider
           (Self.Project_Tree, new Project_Type'(Root));
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
   end Reload;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Context) is
   begin
      Unchecked_Free (Self.Source_Files);
   end Free;

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
      Document : LSP.Ada_Documents.Document_Access;
      Position : LSP.Messages.TextDocumentPositionParams'Class)
      return Libadalang.Analysis.Ada_Node
   is
      use type Libadalang.Analysis.Ada_Node;
      use type LSP.Ada_Documents.Document_Access;
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

      if Document /= null then
         return Document.Get_Node_At (Self, Position.position);
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

   --------------------
   -- Index_Document --
   --------------------

   procedure Index_Document
     (Self     : Context;
      Document : LSP.Ada_Documents.Document)
   is
      File  : constant LSP.Types.LSP_String := URI_To_File (Document.URI);
      Unit  : Libadalang.Analysis.Analysis_Unit;
   begin
      Unit := Self.LAL_Context.Get_From_Buffer
        (Filename => LSP.Types.To_UTF_8_String (File),
         --  Change.text is always encoded in UTF-8, as per the protocol
         Charset  => "utf-8",
         Buffer   => LSP.Types.To_UTF_8_Unbounded_String (Document.Text));

      --  After creating an analysis unit, populate the lexical env with it:
      --  we do this to allow Libadalang to do some work in reaction to
      --  a file being open in the IDE, in order to speed up the response
      --  to user queries.
      Libadalang.Analysis.Populate_Lexical_Env (Unit);
   end Index_Document;

end LSP.Ada_Contexts;

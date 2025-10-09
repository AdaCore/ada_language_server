------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2022, AdaCore                     --
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

with Ada.Characters.Handling;     use Ada.Characters.Handling;

with GNATCOLL.Traces;             use GNATCOLL.Traces;
with GNATCOLL.VFS;                use GNATCOLL.VFS;

with GPR2;
with GPR2.Containers;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Build.Source;
with GPR2.Build.Source.Sets;

with VSS.Strings.Conversions;

with Libadalang.Common;           use Libadalang.Common;
with Langkit_Support.Slocs;

with Langkit_Support.Text;

with URIs;
with LSP.Ada_Id_Iterators;

package body LSP.Ada_Contexts is

   Indexing_Debug_Trace : constant Trace_Handle :=
     Create ("ALS.INDEXING.DEBUG", Off);

   use type Libadalang.Analysis.Analysis_Unit;

   type LSP_Context_Event_Handler_Type
   is new Libadalang.Analysis.Event_Handler_Interface with record
      Tracer : LSP.Tracers.Tracer_Access;
   end record;
   --  LAL event handler used to log units that have notbeen found when
   --  requested.

   overriding procedure Unit_Requested_Callback
     (Self               : in out LSP_Context_Event_Handler_Type;
      Context            : Libadalang.Analysis.Analysis_Context'Class;
      Name               : Langkit_Support.Text.Text_Type;
      From               : Libadalang.Analysis.Analysis_Unit'Class;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean);

   overriding procedure Unit_Parsed_Callback
     (Self     : in out LSP_Context_Event_Handler_Type;
      Context  : Libadalang.Analysis.Analysis_Context'Class;
      Unit     : Libadalang.Analysis.Analysis_Unit'Class;
      Reparsed : Boolean)
   is null;

   overriding procedure Release (Self : in out LSP_Context_Event_Handler_Type)
   is null;

   function Get_Charset (Self : Context'Class) return String;
   --  Return the charset with which the context was initialized

   procedure Find_All_References_In_Hierarchy
     (Self       : Context;
      Decl       : Libadalang.Analysis.Basic_Decl;
      Imprecise  : in out Boolean;
      Callback   : not null access procedure
        (Base_Id : Libadalang.Analysis.Base_Id;
         Kind    : Libadalang.Common.Ref_Result_Kind;
         Cancel  : in out Boolean));
   --  When called on a tagged type primitive declaration, iterate over all the
   --  references to the base primitives it inherits and all the references to
   --  the overriding ones.

   -----------------
   -- URI_To_File --
   -----------------

   function URI_To_File
     (Self : Context;
      URI  : LSP.Structures.DocumentUri)
      return GNATCOLL.VFS.Virtual_File
   is
     (GNATCOLL.VFS.Create_From_UTF8
        (URIs.Conversions.To_File
             (VSS.Strings.Conversions.To_UTF_8_String (URI),
              Self.Follow_Symlinks)));

   -----------------------------
   -- Unit_Requested_Callback --
   -----------------------------

   overriding procedure Unit_Requested_Callback
     (Self               : in out LSP_Context_Event_Handler_Type;
      Context            : Libadalang.Analysis.Analysis_Context'Class;
      Name               : Langkit_Support.Text.Text_Type;
      From               : Libadalang.Analysis.Analysis_Unit'Class;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean) is
   begin
      if not Found then
         Self.Tracer.Trace
           ("Failed to request the following unit: "
            & Langkit_Support.Text.To_UTF8 (Name));
      end if;
   end Unit_Requested_Callback;

   ------------
   -- Get_AU --
   ------------

   function Get_AU
     (Self    : Context;
      File    : GNATCOLL.VFS.Virtual_File;
      Reparse : Boolean := False) return Libadalang.Analysis.Analysis_Unit is
   begin
      return Self.LAL_Context.Get_From_File
        (File.Display_Full_Name,
         Charset => Self.Get_Charset,
         Reparse => Reparse);
   end Get_AU;

   --------------------
   -- Analysis_Units --
   --------------------

   function Analysis_Units
     (Self : Context) return Libadalang.Analysis.Analysis_Unit_Array
   is
      Source_Units : Libadalang.Analysis.Analysis_Unit_Array
        (1 .. Self.Source_Files.Length);
      Index : Natural := Source_Units'First;
   begin
      for File in Self.Source_Files.Iterate loop
         Source_Units (Index) := Self.Get_AU
           (LSP.Ada_File_Sets.File_Sets.Element (File));
         Index := Index + 1;
      end loop;
      return Source_Units;
   end Analysis_Units;

   ----------------------------
   -- List_Source_Externsion --
   ----------------------------

   function List_Source_Extensions
     (Self : Context) return LSP.Ada_File_Sets.Extension_Sets.Set is
   begin
      return Self.Extension_Set;
   end List_Source_Extensions;

   -------------------------
   -- Find_All_References --
   -------------------------

   procedure Find_All_References
     (Self       : Context;
      Definition : Libadalang.Analysis.Defining_Name;
      Callback   : not null access procedure
        (Base_Id : Libadalang.Analysis.Base_Id;
         Kind    : Libadalang.Common.Ref_Result_Kind;
         Cancel  : in out Boolean))
   is
      Units : constant Libadalang.Analysis.Analysis_Unit_Array :=
        Self.Analysis_Units;
   begin
      LSP.Ada_Id_Iterators.Find_All_References (Definition, Units, Callback);
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Tracer.Trace_Exception (E, "in Find_All_References");
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

      if Decl.Is_Null then
         return (1 .. 0 => <>);
      end if;

      --  Make two attempts: first with precise results, then with the
      --  imprecise_fallback.
      begin
         return Decl.P_Find_All_Overrides (Units);
      exception
         when E : Libadalang.Common.Property_Error =>
            Imprecise_Results := True;
            Self.Tracer.Trace_Exception (E, "in Find_All_Overrides (precise)");
            return Decl.P_Find_All_Overrides
              (Units, Imprecise_Fallback => True);
      end;
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Tracer.Trace_Exception (E, "in Find_All_Overrides (imprecise)");
         return (1 .. 0 => <>);
   end Find_All_Overrides;

   --------------------------------
   -- Find_All_Base_Declarations --
   --------------------------------

   function Find_All_Base_Declarations
     (Self              : Context;
      Decl              : Libadalang.Analysis.Basic_Decl;
      Imprecise_Results : out Boolean)
      return Libadalang.Analysis.Basic_Decl_Array
   is
      use Libadalang.Analysis;
      use type Langkit_Support.Slocs.Source_Location;

      function Equal (Left, Right : Libadalang.Analysis.Ada_Node'Class)
                      return Boolean is
        (Left.Unit.Get_Filename =
           Right.Unit.Get_Filename and then
         Langkit_Support.Slocs.Start_Sloc (Left.Sloc_Range) =
             Langkit_Support.Slocs.Start_Sloc (Right.Sloc_Range));
   begin
      Imprecise_Results := False;

      if Decl.Is_Null then
         return (1 .. 0 => <>);
      end if;

      declare
         Lal_Result : constant Basic_Decl_Array :=
                        Decl.P_Base_Subp_Declarations;
         Our_Result : Basic_Decl_Array
           (Lal_Result'First .. Lal_Result'Last - 1);
         Index      : Positive := 1;
      begin
         --  Libadalang returns an empty array if this is not a subprogram
         --  that's a primitive of a tagged type
         if Lal_Result'Length = 0 then
            return (1 .. 0 => <>);
         end if;

         --  The result returned by Libadalang includes self; we want to remove
         --  this from the list.
         for J of Lal_Result loop
            if not Equal (J, Decl) then
               Our_Result (Index) := J;
               Index := Index + 1;
            end if;
         end loop;

         return Our_Result;
      end;

   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Tracer.Trace_Exception (E, "in Find_All_Base_Declarations");
         Imprecise_Results := True;
         return (1 .. 0 => <>);
   end Find_All_Base_Declarations;

   --------------------------------------
   -- Find_All_References_In_Hierarchy --
   --------------------------------------

   procedure Find_All_References_In_Hierarchy
     (Self       : Context;
      Decl       : Libadalang.Analysis.Basic_Decl;
      Imprecise  : in out Boolean;
      Callback   : not null access procedure
        (Base_Id : Libadalang.Analysis.Base_Id;
         Kind    : Libadalang.Common.Ref_Result_Kind;
         Cancel  : in out Boolean))
   is
      use type Libadalang.Analysis.Basic_Decl_Array;

      Is_Param : constant Boolean :=
        Decl.Kind in Libadalang.Common.Ada_Param_Spec_Range;

      Parents   : constant Libadalang.Analysis.Ada_Node_Array := Decl.Parents;

      Subp_Decl : constant Libadalang.Analysis.Basic_Decl :=
        (if Is_Param then
            Parents (Parents'First + 4).As_Basic_Decl
         else
            Decl);

      Overriding_Decls : constant Libadalang.Analysis.Basic_Decl_Array :=
        Self.Find_All_Overrides
          (Subp_Decl,
           Imprecise_Results => Imprecise);

      Base_Decls       : constant Libadalang.Analysis.Basic_Decl_Array :=
        Self.Find_All_Base_Declarations
          (Subp_Decl,
           Imprecise_Results => Imprecise);

      Hierarchy        : constant Libadalang.Analysis.Basic_Decl_Array :=
        Overriding_Decls & Base_Decls;
   begin
      if Is_Param then
         LSP.Ada_Id_Iterators.Find_All_Param_References_In_Hierarchy
           (Param     => Decl.As_Param_Spec,
            Hierarchy => Hierarchy,
            Units     => Self.Analysis_Units,
            Callback  => Callback);
      else
         LSP.Ada_Id_Iterators.Find_All_Subp_References_In_Hierarchy
           (Hierarchy => Hierarchy,
            Tracer    => Self.Tracer.all,
            Callback  => Callback);
      end if;
   end Find_All_References_In_Hierarchy;

   --------------------
   -- Flush_Document --
   --------------------

   procedure Flush_Document
     (Self : in out Context;
      File : GNATCOLL.VFS.Virtual_File) is
   begin
      --  Make LAL reload file from disk and then update index
      Self.Index_File (File, Reparse => True);
   end Flush_Document;

   ---------------------------------
   -- Get_References_For_Renaming --
   ---------------------------------

   procedure Get_References_For_Renaming
     (Self              : Context;
      Definition        : Libadalang.Analysis.Defining_Name;
      Imprecise_Results : out Boolean;
      Callback          : not null access procedure
        (Base_Id : Libadalang.Analysis.Base_Id;
         Kind    : Libadalang.Common.Ref_Result_Kind;
         Cancel  : in out Boolean))
   is
      Cancel : Boolean := False;
      Decl   : constant Libadalang.Analysis.Basic_Decl :=
         Definition.P_Basic_Decl;

   begin
      --  Make sure to initialize the "out" variable Imprecise_Results
      Imprecise_Results := False;

      if Decl.Is_Null then
         return;
      end if;

      Self.Find_All_References (Definition, Callback);

      --  Append Definition itself so that it is also renamed
      Callback
        (Base_Id => Definition.P_Relative_Name.As_Base_Id,
         Kind    => Libadalang.Common.Precise,
         Cancel  => Cancel);

      if Cancel then
         return;
      end if;

      --  Append the references in overriding and base declaractions in case
      --  we are dealing with tagged type primitives or a parameter of a
      --  tagged type primitive.
      Self.Find_All_References_In_Hierarchy
        (Decl, Imprecise_Results, Callback);
   end Get_References_For_Renaming;

   --------------------
   -- Find_All_Calls --
   --------------------

   procedure Find_All_Calls
     (Self       : Context;
      Definition : Libadalang.Analysis.Defining_Name;
      Callback   : not null access procedure
        (Base_Id : Libadalang.Analysis.Base_Id;
         Kind    : Libadalang.Common.Ref_Result_Kind;
         Cancel  : in out Boolean))
   is
      Cancel : Boolean := False;

      Units : constant Libadalang.Analysis.Analysis_Unit_Array :=
        Self.Analysis_Units;
   begin
      for Item of Definition.P_Find_All_Calls (Units, Follow_Renamings => True)
      loop
         Callback
           (Base_Id => Libadalang.Analysis.Ref (Item).As_Base_Id,
            Kind    => Libadalang.Analysis.Kind (Item),
            Cancel  => Cancel);

         exit when Cancel;
      end loop;
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Tracer.Trace_Exception (E, "in Is_Called_By");
   end Find_All_Calls;

   ---------------------------
   -- Find_All_Env_Elements --
   ---------------------------

   function Find_All_Env_Elements
     (Self     : Context;
      Name     : Libadalang.Analysis.Name;
      Seq      : Boolean := True;
      Seq_From : Libadalang.Analysis.Ada_Node'Class :=
        Libadalang.Analysis.No_Ada_Node)
      return Laltools.Common.Node_Vectors.Vector
   is
      No_Duplicate : Laltools.Common.Node_Sets.Set :=
        Laltools.Common.Node_Sets.Empty_Set;
      Res          : Laltools.Common.Node_Vectors.Vector :=
        Laltools.Common.Node_Vectors.Empty_Vector;
   begin
      for U_Node of
        Libadalang.Analysis.P_All_Env_Elements (Name, Seq, Seq_From)
      loop
         --  For performance reason use a Set to detect duplicate and ignore
         --  them.
         if not No_Duplicate.Contains (U_Node) then
            No_Duplicate.Insert (U_Node);
            Res.Append (U_Node);
         end if;
      end loop;
      return Res;
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Tracer.Trace_Exception (E, "in Find_All_Env_Elements");
         return Laltools.Common.Node_Vectors.Empty_Vector;
   end Find_All_Env_Elements;

   --------------------
   -- Get_Any_Symbol --
   --------------------

   procedure Get_Any_Symbol
     (Self        : Context;
      Pattern     : LSP.Search.Search_Pattern'Class;
      Only_Public : Boolean;
      Callback : not null access procedure
        (File : GNATCOLL.VFS.Virtual_File;
         Name : Libadalang.Analysis.Defining_Name;
         Stop : in out Boolean);
      Unit_Prefix : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String)
   is
      function Get_Defining_Name
        (File : GNATCOLL.VFS.Virtual_File;
         Loc  : Langkit_Support.Slocs.Source_Location)
         return Libadalang.Analysis.Defining_Name;
      --  Find a Defining_Name at the given location Loc in a unit of File

      -----------------------
      -- Get_Defining_Name --
      -----------------------

      function Get_Defining_Name
        (File : GNATCOLL.VFS.Virtual_File;
         Loc  : Langkit_Support.Slocs.Source_Location)
         return Libadalang.Analysis.Defining_Name
      is
         Unit : constant Libadalang.Analysis.Analysis_Unit :=
                  Self.Get_AU (File);
         Name : constant Libadalang.Analysis.Name :=
           Laltools.Common.Get_Node_As_Name (Unit.Root.Lookup (Loc));
      begin
         return Laltools.Common.Get_Name_As_Defining (Name);
      end Get_Defining_Name;

   begin
      Self.Source_Files.Get_Any_Symbol
        (Pattern           => Pattern,
         Only_Public       => Only_Public,
         Get_Defining_Name => Get_Defining_Name'Access,
         Callback          => Callback,
         Unit_Prefix       => Unit_Prefix);
   end Get_Any_Symbol;

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
     (Self                : in out Context;
      File_Reader         : File_Reader_Interface'Class;
      Follow_Symlinks     : Boolean;
      Style               : GNATdoc.Comments.Options.Documentation_Style;
      As_Fallback_Context : Boolean := False) is
   begin
      Self.Follow_Symlinks := Follow_Symlinks;
      Self.Charset := Ada.Strings.Unbounded.To_Unbounded_String
        ("iso-8859-1");
      Self.Reader_Reference := Create_File_Reader_Reference (File_Reader);

      --  Tab stop is set 1 to disable "visible character guessing" by LAL.
      Self.LAL_Context := Libadalang.Analysis.Create_Context
        (Unit_Provider => Self.Unit_Provider,
         File_Reader   => Self.Reader_Reference,
         With_Trivia   => True,
         Charset       => Self.Get_Charset,
         Tab_Stop      => 1);
      Self.Style := Style;
      Self.Is_Fallback_Context := As_Fallback_Context;
   end Initialize;

   -------------------------
   -- Is_Fallback_Context --
   -------------------------

   function Is_Fallback_Context (Self : Context) return Boolean
   is (Self.Is_Fallback_Context);

   ------------------------
   -- Is_Part_Of_Project --
   ------------------------

   function Is_Part_Of_Project
     (Self : Context;
      URI : LSP.Structures.DocumentUri) return Boolean is
   begin
      return Self.Is_Part_Of_Project (Self.URI_To_File (URI));
   end Is_Part_Of_Project;

   ------------------------
   -- Is_Part_Of_Project --
   ------------------------

   function Is_Part_Of_Project
     (Self : Context;
      File : Virtual_File) return Boolean is
   begin
      return Self.Source_Files.Contains (File);
   end Is_Part_Of_Project;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Self     : in out Context;
      Provider : Libadalang.Project_Provider.GPR2_Provider_And_Projects;
      Tree     : GPR2.Project.Tree.Object;
      Charset  : String)
   is
      procedure Update_Source_Files;
      --  Update the value of Self.Source_Files

      -------------------------
      -- Update_Source_Files --
      -------------------------

      procedure Update_Source_Files is

         procedure Process_Closure
           (Root     : GPR2.Project.View.Object;
            Callback : not null access procedure
              (View : GPR2.Project.View.Object));
         --  Process the closure of Root with the given callback.
         --  Similar to GPR2.Project.View.Closure, but, in addition,
         --  when encountering an extending project, consider the
         --  project it is extending to be part of the closure.

         procedure Add_Sources_From_View (View : GPR2.Project.View.Object);
         --  Add the sources from the given view to the context

         procedure Add_Dirs_From_View (View : GPR2.Project.View.Object);
         --  Add the source directories from the given view to the context

         procedure Insert_Source (Source : GPR2.Build.Source.Object);
         --  Insert Source in Self.Source_Files

         -------------------
         -- Insert_Source --
         -------------------

         procedure Insert_Source (Source : GPR2.Build.Source.Object)
         is
            Path : constant Virtual_File :=
              Source.Path_Name.Virtual_File;
         begin
            if not Self.Source_Files.Contains (Path) then
               Self.Source_Files.Include (Path);
            end if;
         end Insert_Source;

         ---------------------
         -- Process_Closure --
         ---------------------

         procedure Process_Closure
           (Root     : GPR2.Project.View.Object;
            Callback :
              not null access procedure (View : GPR2.Project.View.Object)) is
         begin
            --  Process the closure of the root project, including any
            --  aggregated library and the projects that it might extend.
            for View
              of Root.Closure
                   (Include_Self       => True,
                    Include_Extended   => True,
                    Include_Aggregated => True)
              when not View.Is_Runtime
            loop
               Callback (View);
            end loop;
         end Process_Closure;

         ---------------------------
         -- Add_Sources_From_View --
         ---------------------------

         procedure Add_Sources_From_View (View : GPR2.Project.View.Object) is
            Sources : constant GPR2.Build.Source.Sets.Object := View.Sources;
            use type GPR2.Language_Id;
         begin
            for Source of Sources loop
               if Source.Language = GPR2.Ada_Language then
                  Insert_Source (Source);
               end if;
            end loop;
         end Add_Sources_From_View;

         ------------------------
         -- Add_Dirs_From_View --
         ------------------------

         procedure Add_Dirs_From_View (View : GPR2.Project.View.Object) is
         begin
            for Dir of View.Source_Directories loop
               Self.Source_Dirs.Include (Dir.Virtual_File);
            end loop;
         end Add_Dirs_From_View;

      begin
         Self.Source_Files.Clear;
         Self.Source_Dirs.Clear;

         --  Iterate on all the projects coexisting inside the Provider
         --  By design there is no source collision so merge them.
         for Project of Provider.Projects loop
            Process_Closure (Project, Add_Sources_From_View'Access);
            Process_Closure (Project, Add_Dirs_From_View'Access);
         end loop;

         Self.Extension_Set.Clear;

         for C in Tree.Iterate
           (Kind => (GPR2.Project.I_Runtime       => True,
                     GPR2.Project.I_Configuration => False,
                     others                       => True))
         loop
            declare
               View                : constant GPR2.Project.View.Object :=
                 GPR2.Project.Tree.Element (C);
               Spec_Suffix         : constant String :=
                 View.Spec_Suffix (GPR2.Ada_Language).Value.Text;
               Body_Suffix         : constant String :=
                 View.Body_Suffix (GPR2.Ada_Language).Value.Text;
               Separate_Suffix     : constant String :=
                 View.Separate_Suffix.Value.Text;
            begin
               Self.Extension_Set.Include
                 (VSS.Strings.Conversions.To_Virtual_String (Spec_Suffix));
               Self.Extension_Set.Include
                 (VSS.Strings.Conversions.To_Virtual_String (Body_Suffix));
               Self.Extension_Set.Include
                 (VSS.Strings.Conversions.To_Virtual_String (Separate_Suffix));
            end;
         end loop;
      end Update_Source_Files;

   begin
      --  Use the full path for the ID to avoid conflict when project are
      --  sharing the same name. For example for GNATTest stubs.
      Self.Id := VSS.Strings.Conversions.To_Virtual_String
        (URIs.Conversions.From_File
           (String (Provider.Projects.First_Element.Path_Name.Value)));
      Self.Tree := Tree;
      Self.Charset := Ada.Strings.Unbounded.To_Unbounded_String (Charset);

      Self.Unit_Provider := Provider.Provider;

      Self.Event_Handler := Libadalang.Analysis.Create_Event_Handler_Reference
        (LSP_Context_Event_Handler_Type'(Tracer => Self.Tracer));

      Self.Reload;
      Update_Source_Files;

      --  Choose the first project in case of aggregate context, assuming
      --  they all share the gnatformat options.
      Self.Format_Options :=
        Gnatformat.Configuration.From_Project
          (Provider.Projects.First_Element);
   end Load_Project;

   ------------
   -- Reload --
   ------------

   procedure Reload (Self : in out Context) is
   begin
      Self.LAL_Context := Libadalang.Analysis.Create_Context
        (Unit_Provider => Self.Unit_Provider,
         File_Reader   => Self.Reader_Reference,
         With_Trivia   => True,
         Charset       => Self.Get_Charset,
         Tab_Stop      => 1);
      --  Tab stop is set 1 to disable "visible character guessing" by LAL.
   end Reload;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Context) is
   begin
      Self.Source_Files.Clear;
      Self.Source_Dirs.Clear;
      Self.Tree := GPR2.Project.Tree.Undefined;
   end Free;

   --------
   -- Id --
   --------

   function Id (Self : Context) return VSS.Strings.Virtual_String is
   begin
      return Self.Id;
   end Id;

   ----------------
   -- Index_File --
   ----------------

   procedure Index_File
     (Self    : in out Context;
      File    : GNATCOLL.VFS.Virtual_File;
      Reparse : Boolean := True;
      PLE     : Boolean := True)
   is
   begin
      --  Add a trace before the call to Get_AU, so we can see in the traces
      --  the memory being consumed by Get_AU + Indexing for this file.
      Trace
        (Indexing_Debug_Trace,
         "Indexing " & (if PLE then "(PLE) " else "") &
           File.Display_Full_Name);

      declare
         Unit : Libadalang.Analysis.Analysis_Unit;
      begin
         Unit := Self.Get_AU (File, Reparse => Reparse);

         if Unit = Libadalang.Analysis.No_Analysis_Unit then
            Trace (Indexing_Debug_Trace, "No AU found: not indexing");
            return;
         end if;

         Self.Source_Files.Index_File (File, Unit);

         if PLE then
            Libadalang.Analysis.Populate_Lexical_Env (Unit);
         end if;

         Trace
           (Indexing_Debug_Trace,
            "Done indexing." & Integer'Image (Unit.Diagnostics'Length) &
              " diagnostic(s) found.");
      end;
   end Index_File;

   ---------------------------
   -- Add_Invisible_Symbols --
   ---------------------------

   procedure Add_Invisible_Symbols
     (Self : in out Context;
      File : GNATCOLL.VFS.Virtual_File;
      Unit : Libadalang.Analysis.Analysis_Unit) is
   begin
      Self.Source_Files.Index_File (File, Unit);
   end Add_Invisible_Symbols;

   ------------------
   -- Include_File --
   ------------------

   procedure Include_File
     (Self    : in out Context;
      File    : GNATCOLL.VFS.Virtual_File) is
   begin
      Self.Source_Files.Include (File);
   end Include_File;

   ------------------
   -- Exclude_File --
   ------------------

   procedure Exclude_File
     (Self    : in out Context;
      File    : GNATCOLL.VFS.Virtual_File) is
   begin
      Self.Source_Files.Exclude (File);
   end Exclude_File;

   --------------------
   -- Index_Document --
   --------------------

   procedure Index_Document
     (Self     : in out Context;
      Document : in out LSP.Ada_Documents.Document)
   is
      Filename : constant GNATCOLL.VFS.Virtual_File :=
        Self.URI_To_File (Document.URI);
   begin
      --  Reset cache of symbols to avoid access to stale references
      Document.Reset_Symbol_Cache;

      --  Index the file, calling Populate_Lexical_Env on it to speed up the
      --  response to user queries.
      Self.Index_File
        (File    => Filename,
         Reparse => True,
         PLE     => True);
   end Index_Document;

   -------------
   -- Charset --
   -------------

   function Charset (Self : Context) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Self.Charset);
   end Charset;

   ------------------------------
   -- Project_Attribute_Values --
   ------------------------------

   function Project_Attribute_Values
     (View              : GPR2.Project.View.Object;
      Attribute         : GPR2.Q_Attribute_Id;
      Index             : String := "";
      Is_List_Attribute : out Boolean;
      Is_Known          : out Boolean)
      return VSS.String_Vectors.Virtual_String_Vector
   is
      use GPR2.Project.Registry.Attribute;

      Attribute_Index : constant GPR2.Project.Attribute_Index.Object :=
        (if Index = "" then GPR2.Project.Attribute_Index.Undefined
         else GPR2.Project.Attribute_Index.Create (Index));

      Attribute_Value : GPR2.Project.Attribute.Object;

      function Convert
        (Values : GPR2.Containers.Source_Value_List)
         return VSS.String_Vectors.Virtual_String_Vector;

      -------------
      -- Convert --
      -------------

      function Convert
        (Values : GPR2.Containers.Source_Value_List)
         return VSS.String_Vectors.Virtual_String_Vector
      is
         Result : VSS.String_Vectors.Virtual_String_Vector;
      begin
         for Value of Values loop
            Result.Append (VSS.Strings.Conversions.To_Virtual_String (Value.Text));
         end loop;

         return Result;
      end Convert;

   begin
      Is_Known := False;

      if View.Is_Defined then

         if View.Check_Attribute
           (Name   => Attribute,
            Index  => Attribute_Index,
            Result => Attribute_Value)
         then
            Is_List_Attribute := (Attribute_Value.Kind = List);
            Is_Known := True;
            return Convert (Attribute_Value.Values);
         end if;
      end if;

      return [];
   end Project_Attribute_Values;

   -----------------------------
   -- Project_Attribute_Value --
   -----------------------------

   function Project_Attribute_Value
     (View         : GPR2.Project.View.Object;
      Attribute    : GPR2.Q_Attribute_Id;
      Index        : String := "";
      Default      : String := "") return String
   is
      Dummy      : Boolean;
      Is_Known   : Boolean;
      Values : constant VSS.String_Vectors.Virtual_String_Vector :=
        Project_Attribute_Values
          (View              => View,
           Attribute         => Attribute,
           Index             => Index,
           Is_List_Attribute => Dummy,
           Is_Known          => Is_Known);
   begin
      if Is_Known then
         return VSS.Strings.Conversions.To_UTF_8_String (Values.First_Element);
      else
         return Default;
      end if;
   end Project_Attribute_Value;

   -----------------------------
   -- Project_Attribute_Value --
   -----------------------------

   function Project_Attribute_Value
     (Self         : Context;
      Attribute    : GPR2.Q_Attribute_Id;
      Index        : String := "";
      Default      : String := "") return String is
   begin
      return Project_Attribute_Value
        (View         => Self.Tree.Root_Project,
         Attribute    => Attribute,
         Index        => Index,
         Default      => Default);
   end Project_Attribute_Value;

end LSP.Ada_Contexts;

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

with GNAT.Strings;

with GNATCOLL.Projects;           use GNATCOLL.Projects;
with GNATCOLL.Traces;             use GNATCOLL.Traces;
with GNATCOLL.VFS;                use GNATCOLL.VFS;

with VSS.Strings.Conversions;

with URIs;
with LSP.Ada_Id_Iterators;
with LSP.Common;                  use LSP.Common;
with LSP.Lal_Utils;               use LSP.Lal_Utils;

with Libadalang.Common;           use Libadalang.Common;
with Libadalang.Project_Provider;

with Langkit_Support.Slocs;

with Utils.Command_Lines.Common;

package body LSP.Ada_Contexts is

   Indexing_Trace   : constant Trace_Handle := Create ("ALS.INDEXING", Off);

   Formatting_Trace : constant Trace_Handle := Create ("ALS.FORMATTING", On);

   use type Libadalang.Analysis.Analysis_Unit;

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

   function URI_To_File
     (Self : Context;
      URI  : LSP.Types.LSP_URI)
      return Ada.Strings.UTF_Encoding.UTF_8_String
        is (URIs.Conversions.To_File (LSP.Types.To_UTF_8_String (URI),
            Self.Follow_Symlinks));

   function URI_To_File
     (Self : Context;
      URI  : LSP.Types.LSP_URI)
      return GNATCOLL.VFS.Virtual_File
   is
     (GNATCOLL.VFS.Create_From_UTF8 (Self.URI_To_File (URI)));

   procedure Update_Pp_Formatting_Options
     (Pp_Options  : in out Utils.Command_Lines.Command_Line;
      LSP_Options : LSP.Messages.FormattingOptions);
   --  Update the gnatpp formatting options using the LSP ones.
   --  Options that are explicitly specified in the .gpr file take precedence
   --  over LSP options.

   -------------------------
   -- Append_Declarations --
   -------------------------

   procedure Append_Declarations
     (Self                    : Context;
      Document                : LSP.Ada_Documents.Document_Access;
      Position                : LSP.Messages.TextDocumentPositionParams;
      Display_Method_Ancestry_Policy :
         LSP.Messages.AlsDisplayMethodAncestryOnNavigationPolicy;
      Result                  : in out LSP.Messages.Location_Or_Link_Vector;
      Imprecise               : in out Boolean)
   is
      use LSP.Messages;
      use Libadalang.Analysis;

      Name_Node : constant Libadalang.Analysis.Name :=
        Laltools.Common.Get_Node_As_Name
          (Self.Get_Node_At (Document, Position));

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
   begin
      if Name_Node = Libadalang.Analysis.No_Name then
         return;
      end if;

      --  Check if we are on some defining name
      Definition := Laltools.Common.Get_Name_As_Defining (Name_Node);

      if Definition = Libadalang.Analysis.No_Defining_Name then
         --  If we aren't on a defining_name already then try to resolve
         declare
            Is_Imprecise : Boolean;
         begin
            Definition := Laltools.Common.Resolve_Name
              (Name_Node, Self.Trace, Is_Imprecise);

            Imprecise := Imprecise or Is_Imprecise;
         end;
      else
         On_Defining_Name := True;
      end if;

      if Definition = Libadalang.Analysis.No_Defining_Name then
         return;  --  Name resolution fails, nothing to do.
      end if;

      First_Part := Laltools.Common.Find_Canonical_Part
        (Definition, Self.Trace);

      --  Display the method ancestry in three cases:
      --
      --   . When the preference is set to Always
      --
      --   . When we are on a usage node (e.g: subprogram call) and if the
      --     preference is set to Usage_And_Abstract_Only
      --
      --   . When we are on a defining name node and if the preference is
      --     set to Definition_Only

      if Display_Method_Ancestry_Policy = Always
        or else (Display_Method_Ancestry_Policy = Usage_And_Abstract_Only
                        and then not On_Defining_Name)
        or else (Display_Method_Ancestry_Policy = Definition_Only
                        and then On_Defining_Name)
      then
         if First_Part = Libadalang.Analysis.No_Defining_Name then
            Decl_For_Find_Overrides := Definition.P_Basic_Decl;
         else
            Decl_For_Find_Overrides := First_Part.P_Basic_Decl;
         end if;
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
         Imprecise_Over       : Boolean;
         Imprecise_Base       : Boolean;
         Overriding_Subps     : constant Basic_Decl_Array :=
                                  Self.Find_All_Overrides
                                    (Decl_For_Find_Overrides,
                                     Imprecise_Results => Imprecise_Over);
         Base_Subps           : constant Basic_Decl_Array :=
                                  Self.Find_All_Base_Declarations
                                    (Decl_For_Find_Overrides,
                                     Imprecise_Results => Imprecise_Base);
      begin
         for Subp of Base_Subps loop
            Append_Location
              (Result, Subp.P_Defining_Name, LSP.Common.Is_Parent);
         end loop;
         for Subp of Overriding_Subps loop
            Append_Location
              (Result, Subp.P_Defining_Name, LSP.Common.Is_Child);
         end loop;
         Imprecise := Imprecise or Imprecise_Over or Imprecise_Base;
      end;
   end Append_Declarations;

   ------------
   -- Get_AU --
   ------------

   function Get_AU
     (Self    : Context;
      File    : GNATCOLL.VFS.Virtual_File;
      Reparse : Boolean := False) return Libadalang.Analysis.Analysis_Unit is
   begin
      if not Is_Ada_File (Self.Tree, File) then
         return Libadalang.Analysis.No_Analysis_Unit;
      end if;

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

   -----------------------------
   -- List_Source_Directories --
   -----------------------------

   function List_Source_Directories
     (Self                     : Context;
      Include_Externally_Built : Boolean := False)
      return LSP.Ada_File_Sets.File_Sets.Set is
   begin
      if Include_Externally_Built then
         return Self.External_Source_Dirs;
      else
         return Self.Source_Dirs;
      end if;
   end List_Source_Directories;

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
         Log (Self.Trace, E, "in Find_All_References");
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
            Log (Self.Trace, E, "in Find_All_Overrides (precise)");
            return Decl.P_Find_All_Overrides
              (Units, Imprecise_Fallback => True);
      end;
   exception
      when E : Libadalang.Common.Property_Error =>
         Log (Self.Trace, E, "in Find_All_Overrides (imprecise)");
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

   begin
      Imprecise_Results := False;

      if Decl.Is_Null then
         return (1 .. 0 => <>);
      end if;

      declare
         Lal_Result : constant Basic_Decl_Array :=
                        Decl.P_Base_Subp_Declarations;
         Our_Result : Basic_Decl_Array (1 .. Lal_Result'Length - 1);
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
            if J /= Decl then
               Our_Result (Index) := J;
               Index := Index + 1;
            end if;
         end loop;

         return Our_Result;
      end;

   exception
      when E : Libadalang.Common.Property_Error =>
         Log (Self.Trace, E, "in Find_All_Base_Declarations");
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
            Trace     => Self.Trace,
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
         Log (Self.Trace, E, "in Is_Called_By");
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
         Log (Self.Trace, E, "in Find_All_Env_Elements");
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
         Stop : in out Boolean))
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
        (Pattern, Only_Public, Get_Defining_Name'Access, Callback);
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
      As_Fallback_Context : Boolean := False) is
   begin
      Self.Follow_Symlinks := Follow_Symlinks;
      Self.Charset := Ada.Strings.Unbounded.To_Unbounded_String
        ("iso-8859-1");
      Self.Reader_Reference := Create_File_Reader_Reference (File_Reader);
      Self.LAL_Context := Libadalang.Analysis.Create_Context
        (Unit_Provider => Self.Unit_Provider,
         File_Reader   => Self.Reader_Reference,
         With_Trivia   => True,
         Charset       => Self.Get_Charset,
         Tab_Stop      => 1);
      --  Tab stop is set 1 to disable "visible character guessing" by LAL.
      Self.Is_Fallback_Context := As_Fallback_Context;
   end Initialize;

   ------------------------
   -- Is_Part_Of_Project --
   ------------------------

   function Is_Part_Of_Project
     (Self : Context;
      File : Virtual_File) return Boolean is
   begin
      return Self.Is_Fallback_Context
        or else Self.Source_Files.Contains (File);
   end Is_Part_Of_Project;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Self     : in out Context;
      Tree     : not null GNATCOLL.Projects.Project_Tree_Access;
      Root     : Project_Type;
      Charset  : String)
   is
      procedure Update_Source_Files;
      --  Update the value of Self.Source_Files
      procedure Pretty_Printer_Setup;
      --  Setup PP_Options object

      -------------------------
      -- Update_Source_Files --
      -------------------------

      procedure Update_Source_Files is
         All_Sources : File_Array_Access :=
           Root.Source_Files (Recursive => True);
         All_Ada_Sources : File_Array (1 .. All_Sources'Length);
         Free_Index      : Natural := All_Ada_Sources'First;
      begin
         --  Iterate through all sources, returning only those that have Ada
         --  as language.
         for J in All_Sources'Range loop
            if Is_Ada_File (Self.Tree, All_Sources (J)) then
               All_Ada_Sources (Free_Index) := All_Sources (J);
               Free_Index := Free_Index + 1;
            end if;
         end loop;

         Unchecked_Free (All_Sources);
         Self.Source_Files.Clear;

         for Index in 1 .. Free_Index - 1 loop
            Self.Source_Files.Include (All_Ada_Sources (Index));
         end loop;

         Self.Source_Dirs.Clear;
         Self.External_Source_Dirs.Clear;

         for Dir of Source_Dirs
           (Project                  => Root,
            Recursive                => True,
            Include_Externally_Built => False)
         loop
            Self.Source_Dirs.Include (Dir);
         end loop;

         for Dir of Source_Dirs
           (Project                  => Root,
            Recursive                => True,
            Include_Externally_Built => True)
         loop
            Self.External_Source_Dirs.Include (Dir);
         end loop;
      end Update_Source_Files;

      --------------------------
      -- Pretty_Printer_Setup --
      --------------------------

      procedure Pretty_Printer_Setup
      is
         use type GNAT.Strings.String_Access;
         Options   : GNAT.Strings.String_List_Access;
         Validated : GNAT.Strings.String_List_Access;
         Last      : Integer;
         Default   : Boolean;
      begin
         Root.Switches
           (In_Pkg           => "Pretty_Printer",
            File             => GNATCOLL.VFS.No_File,
            Language         => "ada",
            Value            => Options,
            Is_Default_Value => Default);

         --  Initialize an gnatpp command line object
         Last := Options'First - 1;
         for Item of Options.all loop
            if Item /= null
              and then Item.all /= ""
            then
               Last := Last + 1;
            end if;
         end loop;

         Validated := new GNAT.Strings.String_List (Options'First .. Last);
         Last      := Options'First - 1;
         for Item of Options.all loop
            if Item /= null
              and then Item.all /= ""
            then
               Last := Last + 1;
               Validated (Last) := new String'(Item.all);
            end if;
         end loop;

         Utils.Command_Lines.Parse
           (Validated,
            Self.PP_Options,
            Phase              => Utils.Command_Lines.Cmd_Line_1,
            Callback           => null,
            Collect_File_Names => False,
            Ignore_Errors      => True);

         GNAT.Strings.Free (Options);
         GNAT.Strings.Free (Validated);

         --  Set UTF-8 encoding
         Utils.Command_Lines.Common.Set_WCEM (Self.PP_Options, "8");
      end Pretty_Printer_Setup;

   begin
      Self.Id := VSS.Strings.Conversions.To_Virtual_String (Root.Name);
      Self.Tree := Tree;
      Self.Charset := Ada.Strings.Unbounded.To_Unbounded_String (Charset);

      Self.Unit_Provider :=
        Libadalang.Project_Provider.Create_Project_Unit_Provider
          (Tree             => Tree,
           Project          => Root,
           Env              => Get_Environment (Root),
           Is_Project_Owner => False);

      Self.Reload;
      Update_Source_Files;
      Pretty_Printer_Setup;
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

   ----------------------------------
   -- Update_Pp_Formatting_Options --
   ----------------------------------

   procedure Update_Pp_Formatting_Options
     (Pp_Options  : in out Utils.Command_Lines.Command_Line;
      LSP_Options : LSP.Messages.FormattingOptions)
   is
      Pp_Indentation : constant Natural :=
        Pp.Command_Lines.Pp_Nat_Switches.Arg
          (Pp_Options, Pp.Command_Lines.Indentation);
      Pp_No_Tab      : constant Boolean :=
        Pp.Command_Lines.Pp_Flag_Switches.Arg
          (Pp_Options, Pp.Command_Lines.No_Tab);
   begin
      --  Check if intentation and 'no tab' policy options have been explictly
      --  set in the project.
      --  If it's not the case, use the LSP options.

      if not Pp.Command_Lines.Pp_Nat_Switches.Explicit
        (Pp_Options, Pp.Command_Lines.Indentation)
      then
         Pp.Command_Lines.Pp_Nat_Switches.Set_Arg
           (Pp_Options,
            Pp.Command_Lines.Indentation,
            Natural (LSP_Options.tabSize));

      elsif Pp_Indentation /= Natural (LSP_Options.tabSize) then
         Formatting_Trace.Trace
           ("Project file defines an indentation "
            & "of" & Pp_Indentation'Img & ", while LSP defines an "
            & "indentation of" & LSP_Options.tabSize'Img & ".");
      end if;

      if not Pp.Command_Lines.Pp_Flag_Switches.Explicit
        (Pp_Options, Pp.Command_Lines.No_Tab)
      then
         Pp.Command_Lines.Pp_Flag_Switches.Set_Arg
           (Pp_Options,
            Pp.Command_Lines.No_Tab,
            LSP_Options.insertSpaces);

      elsif Pp_No_Tab /= LSP_Options.insertSpaces then
         Formatting_Trace.Trace
           ("Project file no tab policy is set to " & Pp_No_Tab'Img
            & ", while LSP is set to " & LSP_Options.insertSpaces'Img);
      end if;
   end Update_Pp_Formatting_Options;

   ------------
   -- Format --
   ------------

   procedure Format
     (Self     : in out Context;
      Document : LSP.Ada_Documents.Document_Access;
      Span     : LSP.Messages.Span;
      Options  : LSP.Messages.FormattingOptions;
      Edit     : out LSP.Messages.TextEdit_Vector;
      Success  : out Boolean;
      Messages : out VSS.String_Vectors.Virtual_String_Vector) is
   begin
      --  Take into account the options set by the request only if the
      --  corresponding GPR switches are not explicitly set.

      Update_Pp_Formatting_Options
        (Pp_Options  => Self.PP_Options,
         LSP_Options => Options);

      Success := Document.Formatting
        (Context  => Self,
         Span     => Span,
         Cmd      => Self.PP_Options,
         Edit     => Edit,
         Messages => Messages);
   end Format;

   ------------------
   -- Range_Format --
   ------------------

   procedure Range_Format
     (Self     : in out Context;
      Document : LSP.Ada_Documents.Document_Access;
      Span     : LSP.Messages.Span;
      Options  : LSP.Messages.FormattingOptions;
      Edit     : out LSP.Messages.TextEdit_Vector;
      Success  : out Boolean;
      Messages : out VSS.String_Vectors.Virtual_String_Vector) is
   begin
      --  Take into account the options set by the request only if the
      --  corresponding GPR switches are not explicitly set.

      Update_Pp_Formatting_Options
        (Pp_Options  => Self.PP_Options,
         LSP_Options => Options);

      Success := Document.Range_Formatting
        (Context    => Self,
         Span       => Span,
         PP_Options => Self.PP_Options,
         Edit       => Edit,
         Messages   => Messages);
   end Range_Format;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Context) is
   begin
      Self.Source_Files.Clear;
      Self.Source_Dirs.Clear;
      Self.Tree := null;

      --  Destroy GnatPP command line
      Utils.Command_Lines.Clear (Self.PP_Options);
   end Free;

   -----------------
   -- Get_Node_At --
   -----------------

   function Get_Node_At
     (Self         : Context;
      Document     : LSP.Ada_Documents.Document_Access;
      Position     : LSP.Messages.TextDocumentPositionParams'Class;
      Project_Only : Boolean := True;
      Previous     : Boolean := False)
      return Libadalang.Analysis.Ada_Node
   is
      use type Libadalang.Analysis.Ada_Node;
      use type LSP.Ada_Documents.Document_Access;
      use type Langkit_Support.Slocs.Line_Number;
      use type Langkit_Support.Slocs.Column_Number;

      Unit : Libadalang.Analysis.Analysis_Unit;

      URI      : constant LSP.Messages.DocumentUri :=
        Position.textDocument.uri;
      Name     : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
        Self.URI_To_File (URI);
      File     : constant Virtual_File := Create_From_UTF8 (Name);
      Col_Incr : constant Langkit_Support.Slocs.Column_Number :=
        (if Previous then 0 else 1);
   begin
      --  We're about to get a node from an analysis unit. Either the document
      --  is open for it, in which case we read the document, or the
      --  document is not open for it. In this case, resolve this only
      --  if the file belongs to the project (unless if Project_Only is False):
      --  we don't want to pollute the LAL context with units that are not in
      --  the project.

      if Document /= null then
         return Document.Get_Node_At
           (Self, Position.position, Previous => Previous);
      elsif not Project_Only or else Self.Is_Part_Of_Project (File) then
         Unit := Self.Get_AU (File);

         if Unit.Root = Libadalang.Analysis.No_Ada_Node then
            return Libadalang.Analysis.No_Ada_Node;
         end if;

         return Unit.Root.Lookup
           ((Line   => Langkit_Support.Slocs.Line_Number
             (Position.position.line) + 1,
             Column => Langkit_Support.Slocs.Column_Number
               (Position.position.character) + Col_Incr));
         --  ??? Incorrect conversion of UTF16 offset to Column_Number

      else
         return Libadalang.Analysis.No_Ada_Node;
      end if;
   end Get_Node_At;

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
        (Indexing_Trace,
         "Indexing " & (if PLE then "(PLE) " else "") &
           File.Display_Full_Name);

      declare
         Unit : Libadalang.Analysis.Analysis_Unit;
      begin
         Unit := Self.Get_AU (File, Reparse => Reparse);

         if Unit = Libadalang.Analysis.No_Analysis_Unit then
            Trace (Indexing_Trace, "No AU found: not indexing");
            return;
         end if;

         Self.Source_Files.Index_File (File, Unit);

         if PLE then
            Libadalang.Analysis.Populate_Lexical_Env (Unit);
         end if;

         Trace
           (Indexing_Trace,
            "Done indexing." & Integer'Image (Unit.Diagnostics'Length) &
              " diagnostic(s) found.");
      end;
   end Index_File;

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
      Filename : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
        Self.URI_To_File (Document.URI);
   begin
      --  Reset cache of symbols to avoid access to stale references
      Document.Reset_Symbol_Cache;

      --  Index the file, calling Populate_Lexical_Env on it to speed up the
      --  response to user queries.
      Self.Index_File
        (File    => Create_From_UTF8 (Filename),
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

   -----------------------------
   -- Project_Attribute_Value --
   -----------------------------

   function Project_Attribute_Value
     (Self         : Context;
      Attribute    : Attribute_Pkg_String;
      Index        : String := "";
      Default      : String := "";
      Use_Extended : Boolean := False) return String
   is (if Self.Tree = null then Default
       else Root_Project (Self.Tree.all).
              Attribute_Value (Attribute, Index, Default,  Use_Extended));

end LSP.Ada_Contexts;

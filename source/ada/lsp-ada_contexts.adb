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

with Ada.Characters.Handling;     use Ada.Characters.Handling;

with GNATCOLL.JSON;
with GNATCOLL.Projects;           use GNATCOLL.Projects;
with GNATCOLL.VFS;                use GNATCOLL.VFS;

with URIs;
with LSP.Lal_Utils;               use LSP.Lal_Utils;

with Libadalang.Common;           use Libadalang.Common;
with Libadalang.Project_Provider;
with Langkit_Support.Slocs;

package body LSP.Ada_Contexts is

   function Get_Charset (Self : Context'Class) return String;
   --  Return the charset with which the context was initialized

   function Find_All_References_In_Hierarchy
     (Self              : Context;
      Decl              : Libadalang.Analysis.Basic_Decl;
      Imprecise_Results : out Boolean)
      return Base_Id_Array;
   --  When called on a tagged type primitive declaration, return all the
   --  references to the base primitives it inherits and all the references to
   --  the overriding ones.
   --  Imprecise_Results is set to True if we don't know whether the results
   --  are precise.

   -------------------------
   -- Append_Declarations --
   -------------------------

   procedure Append_Declarations
     (Self      : Context;
      Document  : LSP.Ada_Documents.Document_Access;
      Position  : LSP.Messages.TextDocumentPositionParams;
      Result    : in out LSP.Messages.Location_Or_Link_Vector;
      Imprecise : in out Boolean)
   is
      use Libadalang.Analysis;

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
        (1 .. Integer (Self.Source_Files.Length));
      Index : Natural := Source_Units'First;
   begin
      for File of Self.Source_Files loop
         Source_Units (Index) := Self.LAL_Context.Get_From_File
           (File.Display_Full_Name,
            Charset => Self.Get_Charset);
         Index := Index + 1;
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
      return Base_Id_Array
   is
      use Libadalang.Analysis;

      Units : constant Libadalang.Analysis.Analysis_Unit_Array :=
        Self.Analysis_Units;
   begin
      Imprecise_Results := False;

      declare
         Refs : constant Ref_Result_Array :=
           Definition.P_Find_All_References (Units);
         R    : Base_Id_Array (Refs'Range);
      begin
         for I in Refs'Range loop
            R (I) := Ref (Refs (I)).As_Base_Id;
            Imprecise_Results := Imprecise_Results
              or Kind (Refs (I)) = Imprecise;
         end loop;
         return R;
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

   function Find_All_References_In_Hierarchy
     (Self              : Context;
      Decl              : Libadalang.Analysis.Basic_Decl;
      Imprecise_Results : out Boolean)
      return Base_Id_Array
   is
      use Libadalang.Analysis;

      function Find_All_Subp_References_In_Hierarchy
        (Hierarchy : Basic_Decl_Array) return Base_Id_Array;
      --  Return all the references of Decl in the given hierarchy

      function Find_All_Param_References_In_Hierarchy
        (Param     : Param_Spec;
         Hierarchy : Basic_Decl_Array;
         Idx       : Positive) return Base_Id_Array;
      --  Recursive function that returns all the references of the given
      --  parameter's name in the hierarchy.

      -------------------------------------------
      -- Find_All_Subp_References_In_Hierarchy --
      -------------------------------------------

      function Find_All_Subp_References_In_Hierarchy
        (Hierarchy : Basic_Decl_Array) return Base_Id_Array
      is
         References       : Base_Id_Array
           (1 .. Hierarchy'Length * 3);
         Subp_Body_Name   : Defining_Name;
         Subp_Body_Node   : Subp_Body;
         Last             : Positive := References'First;
      begin

         for Subp_Decl of Hierarchy loop
            References (Last) := Subp_Decl.P_Defining_Name.F_Name.As_Base_Id;
            Last := Last + 1;

            --  Try to get the corresponding body
            Subp_Body_Name := Lal_Utils.Find_Next_Part
              (Subp_Decl.P_Defining_Name, Self.Trace);

            --  If there is a body, append the body's begin and end labels
            --  to the result.
            if not Subp_Body_Name.Is_Null then
               References (Last) := Subp_Body_Name.F_Name.As_Base_Id;

               Subp_Body_Node := Subp_Body_Name.Parent.Parent.As_Subp_Body;
               References (Last + 1) :=
                 Subp_Body_Node.F_End_Name.F_Name.As_Base_Id;

               Last := Last + 2;
            end if;
         end loop;

         return References (References'First .. Last - 1);
      end Find_All_Subp_References_In_Hierarchy;

      --------------------------------------------
      -- Find_All_Param_References_In_Hierarchy --
      --------------------------------------------

      function Find_All_Param_References_In_Hierarchy
        (Param     : Param_Spec;
         Hierarchy : Basic_Decl_Array;
         Idx       : Positive) return Base_Id_Array
      is
         Param_Name_Id : constant Defining_Name :=
                           Param.F_Ids.List_Child (1);
         Subp_Decl     : Basic_Subp_Decl;
      begin

         if Idx > Hierarchy'Last then
            return (1 .. 0 => No_Base_Id);
         end if;

         Subp_Decl := Hierarchy (Idx).As_Basic_Subp_Decl;

         --  Iterate on all the parameters of the subprogram and find the
         --  parameter with the same name
         for Param of Subp_Decl.P_Subp_Decl_Spec.P_Params loop
            if Param_Name_Id.Text = Param.F_Ids.List_Child (1).Text then

               --  Return all the references to the parameter and call this
               --  function recursively to do the same thing on the next
               --  subprogram of the hierarchy.
               return
                 Param.F_Ids.List_Child (1).F_Name.As_Base_Id
                 & Self.Find_All_References
                      (Definition        => Param.F_Ids.List_Child (1),
                       Imprecise_Results => Imprecise_Results)
                 & Find_All_Param_References_In_Hierarchy
                 (Param     => Param,
                  Hierarchy => Hierarchy,
                  Idx       => Idx + 1);
            end if;
         end loop;

         return (1 .. 0 => No_Base_Id);
      end Find_All_Param_References_In_Hierarchy;

   begin
      Imprecise_Results := False;

      if Decl.Is_Null then
         return (1 .. 0 => <>);
      end if;

      declare
         Is_Param         : constant Boolean :=
                              Decl.Kind in Ada_Param_Spec_Range;
         Parents          : constant Ada_Node_Array := Decl.Parents;
         Subp_Decl        : constant Basic_Decl :=
                              (if Is_Param then
                                  Parents (Parents'First + 4).As_Basic_Decl
                               else
                                  Decl);
         Overriding_Decls : constant Basic_Decl_Array :=
                                   Self.Find_All_Overrides
                                     (Subp_Decl,
                                      Imprecise_Results => Imprecise_Results);
         Base_Decls       : constant Basic_Decl_Array :=
                                   Self.Find_All_Base_Declarations
                                     (Subp_Decl,
                                      Imprecise_Results => Imprecise_Results);
         Hierarchy        : constant Basic_Decl_Array :=
                                   Overriding_Decls & Base_Decls;
      begin
         if Is_Param then
            return Find_All_Param_References_In_Hierarchy
              (Param     => Decl.As_Param_Spec,
               Hierarchy => Hierarchy,
               Idx       => Hierarchy'First);
         else
            return Find_All_Subp_References_In_Hierarchy (Hierarchy);
         end if;
      end;
   end Find_All_References_In_Hierarchy;

   ---------------------------------
   -- Get_References_For_Renaming --
   ---------------------------------

   function Get_References_For_Renaming
     (Self              : Context;
      Definition        : Libadalang.Analysis.Defining_Name;
      Imprecise_Results : out Boolean)
      return Base_Id_Array
   is
      use Libadalang.Analysis;

      Imprecise_For_Refs       : Boolean;
      Imprecise_For_Hierarchy  : Boolean;
      Decl                     : constant Basic_Decl :=
         Definition.P_Basic_Decl;
      References               : constant Base_Id_Array :=
         Self.Find_All_References (Definition, Imprecise_For_Refs)

         --  Append Definition itself so that it is also renamed
         & Definition.P_Relative_Name.As_Base_Id

         --  Append the references in overriding and base declaractions in case
         --  we are dealing with tagged type primitives or a parameter of a
         --  tagged type primitive.
         & Self.Find_All_References_In_Hierarchy
            (Decl,
             Imprecise_Results => Imprecise_For_Hierarchy);
   begin
      Imprecise_Results := Imprecise_For_Refs or Imprecise_For_Hierarchy;

      if Imprecise_Results then
         return (1 .. 0 => <>);
      end if;

      return References;
   end Get_References_For_Renaming;

   --------------------
   -- Find_All_Calls --
   --------------------

   function Find_All_Calls
     (Self              : Context;
      Definition        : Libadalang.Analysis.Defining_Name;
      Imprecise_Results : out Boolean)
      return Base_Id_Array
   is
      use Libadalang.Analysis;

      Units : constant Libadalang.Analysis.Analysis_Unit_Array :=
        Self.Analysis_Units;
   begin
      Imprecise_Results := False;

      declare
         Refs    : constant Ref_Result_Array
           := Definition.P_Find_All_Calls (Units);
         R       : Base_Id_Array (Refs'Range);
      begin
         for I in Refs'Range loop
            R (I) := Ref (Refs (I)).As_Base_Id;
            Imprecise_Results := Imprecise_Results
              or Kind (Refs (I)) = Imprecise;
         end loop;
         return R;
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
      return Self.Source_Files.Contains (File);
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

      -------------------------
      -- Update_Source_Files --
      -------------------------

      procedure Update_Source_Files is
         All_Sources : File_Array_Access :=
           Root.Source_Files (Recursive => True);
         All_Ada_Sources : File_Array (1 .. All_Sources'Length);
         Free_Index      : Natural := All_Ada_Sources'First;
         Set             : File_Info_Set;
      begin
         --  Iterate through all sources, returning only those that have Ada
         --  as language.
         for J in All_Sources'Range loop
            Set := Tree.Info_Set (All_Sources (J));
            if not Set.Is_Empty then
               --  The file can be listed in several projects with different
               --  Info_Sets, in the case of aggregate projects. However,
               --  assume that the language is the same in all projects,
               --  so look only at the first entry in the set.
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
         Self.Source_Files.Clear;

         for Index in 1 .. Free_Index - 1 loop
            Self.Source_Files.Include (All_Ada_Sources (Index));
         end loop;
      end Update_Source_Files;

   begin
      Self.Id := LSP.Types.To_LSP_String (Root.Name);

      Self.Charset := Ada.Strings.Unbounded.To_Unbounded_String (Charset);

      Self.Unit_Provider :=
        Libadalang.Project_Provider.Create_Project_Unit_Provider
          (Tree             => Tree,
           Project          => Root,
           Env              => Get_Environment (Root),
           Is_Project_Owner => False);

      Self.Reload;
      Update_Source_Files;
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
      Self.Source_Files.Clear;
   end Free;

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

   --------
   -- Id --
   --------

   function Id (Self : Context) return LSP.Types.LSP_String is
   begin
      return Self.Id;
   end Id;

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

------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2024, AdaCore                     --
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

pragma Ada_2022;

with Ada.Containers.Generic_Anonymous_Array_Sort;

with GNATCOLL.Traces;
with GNATCOLL.VFS;

with LSP.Utils;

with VSS.JSON.Pull_Readers.Simple;
with VSS.JSON.Streams;
with VSS.Strings.Conversions;
with VSS.Text_Streams.File_Input;
with VSS.Transformers.Casing;

package body LSP.Ada_Configurations is

   Doc_Style_Values : constant VSS.String_Vectors.Virtual_String_Vector :=
     [for Item in GNATdoc.Comments.Options.Documentation_Style =>
        VSS.Strings.To_Virtual_String (Item'Wide_Wide_Image).Transform
          (VSS.Transformers.Casing.To_Lowercase)];

   Display_Method_Values : constant VSS.String_Vectors.Virtual_String_Vector :=
     [for Item in LSP.Enumerations.AlsDisplayMethodAncestryOnNavigationPolicy
        => VSS.Strings.To_Virtual_String (Item'Wide_Wide_Image).Transform
          (VSS.Transformers.Casing.To_Lowercase)];

   function "+" (X : VSS.Strings.Virtual_String'Class) return String renames
     VSS.Strings.Conversions.To_UTF_8_String;

   ALS_COMPLETION_FORMATTING : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("ALS.COMPLETION.FORMATTING", Default => GNATCOLL.Traces.On);
   --  Used in Completion_Formatting/LSP.Ada_Completions.Pretty_Print_Snippet

   Partial_Gnatpp_Trace : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       (Unit_Name => "ALS.PARTIAL_GNATPP",
        Default   => GNATCOLL.Traces.On);
   --  Trace to enable/disable using partial Gnatpp in the rangeFormatting
   --  request.

   On_Type_Formatting_Trace : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       (Unit_Name => "ALS.ON_TYPE_FORMATTING",
        Default   => GNATCOLL.Traces.On);
   --  Trace to enable/disable ALS from providing the
   --  documentOnTypeFormattingProvider capability.

   procedure Skip_Value
     (JSON  : LSP.Structures.LSPAny;
      Index : in out Positive);

   procedure Parse_Ada
     (Self   : in out Configuration'Class;
      JSON   : LSP.Structures.LSPAny;
      From   : Positive;
      Reload : out Boolean);

   ----------------
   -- Build_Path --
   ----------------

   function Build_Path
     (Self : Configuration'Class;
      File : GPR2.Path_Name.Object)
      return GPR2.Path_Name.Object
   is
      Result : GPR2.Path_Name.Object;

      Relocate_Build_Tree : constant GNATCOLL.VFS.Virtual_File :=
                              LSP.Utils.To_Virtual_File
                                (Self.Relocate_Build_Tree);

      Root_Dir            : constant GNATCOLL.VFS.Virtual_File :=
                              LSP.Utils.To_Virtual_File (Self.Relocate_Root);

   begin
      if not Self.Relocate_Build_Tree.Is_Empty then
         Result := GPR2.Path_Name.Create (Relocate_Build_Tree);

         if not Self.Relocate_Root.Is_Empty and then File.Is_Defined
         then
            if not Root_Dir.Is_Absolute_Path then
               Result :=
                 GPR2.Path_Name.Create_Directory
                   (File.Relative_Path
                    (GPR2.Path_Name.Create (Root_Dir)),
                    GPR2.Filename_Type
                      (Result.Value));
            end if;
         end if;
      end if;
      return Result;
   end Build_Path;

   ---------------------------
   -- Completion_Formatting --
   ---------------------------

   function Completion_Formatting return Boolean is
   begin
      return ALS_COMPLETION_FORMATTING.Is_Active;
   end Completion_Formatting;

   ------------------------
   -- On_Type_Formatting --
   ------------------------

   function On_Type_Formatting return Boolean is
   begin
      return On_Type_Formatting_Trace.Is_Active;
   end On_Type_Formatting;

   ---------------
   -- Parse_Ada --
   ---------------

   procedure Parse_Ada
     (Self   : in out Configuration'Class;
      JSON   : LSP.Structures.LSPAny;
      From   : Positive;
      Reload : out Boolean)
   is
      use all type VSS.JSON.JSON_Number_Kind;
      use all type VSS.JSON.Streams.JSON_Stream_Element_Kind;
      use type VSS.String_Vectors.Virtual_String_Vector;

      Index : Positive := From;
      Variables_Names  : VSS.String_Vectors.Virtual_String_Vector;
      Variables_Values : VSS.String_Vectors.Virtual_String_Vector;
      Follow_Symlinks  : constant Boolean := Self.Follow_Symlinks;

      procedure Parse_Variables (From : Positive);
      procedure Swap_Variables (Left, Right : Positive);

      procedure Set
        (Target : in out VSS.Strings.Virtual_String;
         Value  : VSS.Strings.Virtual_String);
      --  If Target /= Value then assign Target and set Reload to Trues

      ---------------------
      -- Parse_Variables --
      ---------------------

      procedure Parse_Variables (From : Positive) is
         Name  : VSS.Strings.Virtual_String;
         Value : VSS.Strings.Virtual_String;
         Index : Positive := From + 1;
      begin
         while Index <= JSON.Last_Index
           and then JSON (Index).Kind = Key_Name
         loop
            Name := JSON (Index).Key_Name;
            Index := Index + 1;

            Value := (if JSON (Index).Kind = String_Value
                      then JSON (Index).String_Value else "");

            Index := Index + 1;

            Variables_Names.Append (Name);
            Variables_Values.Append (Value);
         end loop;
      end Parse_Variables;

      ---------
      -- Set --
      ---------

      procedure Set
        (Target : in out VSS.Strings.Virtual_String;
         Value  : VSS.Strings.Virtual_String) is
      begin
         if Target /= Value then
            Target := Value;
            Reload := True;
         end if;
      end Set;

      --------------------
      -- Swap_Variables --
      --------------------

      procedure Swap_Variables (Left, Right : Positive) is
         Name : constant VSS.Strings.Virtual_String := Variables_Names (Left);

         Value : constant VSS.Strings.Virtual_String :=
           Variables_Values (Left);
      begin
         Variables_Names.Replace (Left, Variables_Names (Right));
         Variables_Values.Replace (Left, Variables_Values (Right));
         Variables_Names.Replace (Right, Name);
         Variables_Values.Replace (Right, Value);
      end Swap_Variables;

      function Less (Left, Right : Positive) return Boolean is
         (Variables_Names (Left) < Variables_Names (Right));

      procedure Sort_Variables is
        new Ada.Containers.Generic_Anonymous_Array_Sort
          (Positive, Less, Swap_Variables);

      Name : VSS.Strings.Virtual_String;
   begin
      Reload := False;
      Index := Index + 1;  --  skip start object

      while Index <= JSON.Last_Index
        and then JSON (Index).Kind = Key_Name
      loop
         Name := JSON (Index).Key_Name;
         Index := Index + 1;

         if Name = "relocateBuildTree"
           and then JSON (Index).Kind = String_Value
         then
            Set (Self.Relocate_Build_Tree, JSON (Index).String_Value);

         elsif Name = "rootDir"
           and then JSON (Index).Kind = String_Value
         then
            Set (Self.Relocate_Root, JSON (Index).String_Value);

         elsif Name = "projectFile"
           and then JSON (Index).Kind = String_Value
         then
            Set (Self.Project_File, JSON (Index).String_Value);

         elsif Name = "scenarioVariables"
           and then JSON (Index).Kind = Start_Object
         then
            Parse_Variables (Index);
            Sort_Variables (1, Variables_Names.Length);

            Reload := Reload or else
              Variables_Names /= Self.Variables_Names or else
              Variables_Values /= Self.Variables_Values;

            Self.Variables_Names := Variables_Names;
            Self.Variables_Values := Variables_Values;

            --  Replace Context with user provided values
            Self.Context.Clear;
            for J in 1 .. Variables_Names.Length loop
               Self.Context.Insert
                 (GPR2.External_Name_Type
                    (VSS.Strings.Conversions.To_UTF_8_String
                         (Variables_Names (J))),
                  VSS.Strings.Conversions.To_UTF_8_String
                    (Variables_Values (J)));
            end loop;

         elsif Name = "defaultCharset"
           and then JSON (Index).Kind = String_Value
         then
            Set (Self.Charset, JSON (Index).String_Value);

         elsif Name = "enableDiagnostics"
           and then JSON (Index).Kind = Boolean_Value
         then
            Self.Diagnostics_Enabled := JSON (Index).Boolean_Value;

         elsif Name = "enableIndexing"
           and then JSON (Index).Kind = Boolean_Value
         then
            Self.Indexing_Enabled := JSON (Index).Boolean_Value;

         elsif Name = "renameInComments"
           and then JSON (Index).Kind = Boolean_Value
         then
            Self.Rename_In_Comments := JSON (Index).Boolean_Value;

         elsif Name = "namedNotationThreshold"
           and then JSON (Index).Kind = Number_Value
           and then JSON (Index).Number_Value.Kind = JSON_Integer
         then
            Self.Named_Notation_Threshold :=
              Natural (JSON (Index).Number_Value.Integer_Value);

         elsif Name = "foldComments"
           and then JSON (Index).Kind = Boolean_Value
         then
            Self.Folding_Comments := JSON (Index).Boolean_Value;

         elsif Name = "displayMethodAncestryOnNavigation"
           and then JSON (Index).Kind = String_Value
           and then Display_Method_Values.Contains
             (JSON (Index).String_Value)
         then
            Self.Method_Ancestry_Policy :=
              LSP.Enumerations.AlsDisplayMethodAncestryOnNavigationPolicy
                'Value (+JSON (Index).String_Value);

         elsif Name = "followSymlinks"
           and then JSON (Index).Kind = Boolean_Value
         then
            Self.Follow_Symlinks := JSON (Index).Boolean_Value;
            Reload := Reload or else Follow_Symlinks /= Self.Follow_Symlinks;

         elsif Name = "documentationStyle"
           and then JSON (Index).Kind = String_Value
           and then Doc_Style_Values.Contains (JSON (Index).String_Value)
         then
            Self.Documentation_Style :=
              GNATdoc.Comments.Options.Documentation_Style'Value
                (+JSON (Index).String_Value);

         elsif Name = "useCompletionSnippets"
           and then JSON (Index).Kind = Boolean_Value
         then
            Self.Use_Completion_Snippets := JSON (Index).Boolean_Value;

         elsif Name = "insertWithClauses"
           and then JSON (Index).Kind = Boolean_Value
         then
            Self.Insert_With_Clauses := JSON (Index).Boolean_Value;

         elsif Name = "logThreshold"
           and then JSON (Index).Kind = Number_Value
           and then JSON (Index).Number_Value.Kind = JSON_Integer
         then
            Self.Log_Threshold :=
              Natural (JSON (Index).Number_Value.Integer_Value);

         elsif Name = "onTypeFormatting"
           and then JSON (Index).Kind = Start_Object
         then
            Name := JSON (Index + 1).Key_Name;

            if Name = "indentOnly"
              and then JSON (Index + 2).Kind = Boolean_Value
            then
               Self.Indent_Only := JSON (Index + 2).Boolean_Value;

            end if;
         end if;

         Skip_Value (JSON, Index);
      end loop;
   end Parse_Ada;

   --------------------
   -- Partial_GNATPP --
   --------------------

   function Partial_GNATPP return Boolean is
   begin
      return Partial_Gnatpp_Trace.Is_Active;
   end Partial_GNATPP;

   ---------------
   -- Read_File --
   ---------------

   procedure Read_File
     (Self : in out Configuration'Class;
      File : VSS.Strings.Virtual_String)
   is
      Input   : aliased VSS.Text_Streams.File_Input.File_Input_Text_Stream;
      Reader  : VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
      JSON    : LSP.Structures.LSPAny;
      Ignore  : Boolean;
   begin
      Input.Open (File, "utf-8");
      Reader.Set_Stream (Input'Unchecked_Access);
      Reader.Read_Next;
      pragma Assert (Reader.Is_Start_Document);
      Reader.Read_Next;

      while not Reader.Is_End_Document loop
         JSON.Append (Reader.Element);
         Reader.Read_Next;
      end loop;

      Self.Parse_Ada (JSON, JSON.First_Index, Ignore);
   end Read_File;

   ---------------
   -- Read_JSON --
   ---------------

   procedure Read_JSON
     (Self   : in out Configuration'Class;
      JSON   : LSP.Structures.LSPAny;
      Reload : out Boolean)
   is
      use all type VSS.JSON.Streams.JSON_Stream_Element_Kind;
      Index : Positive := JSON.First_Index + 1;

   begin
      Reload := False;

      if JSON.Is_Empty or else JSON.First_Element.Kind /= Start_Object then
         return;
      end if;

      while Index < JSON.Last_Index
        and then JSON (Index).Kind = Key_Name
      loop
         declare
            Is_Ada : constant Boolean := JSON (Index).Key_Name = "ada";
         begin
            Index := Index + 1;

            if Is_Ada and then
              Index <= JSON.Last_Index and then
              JSON (Index).Kind = Start_Object
            then
               Self.Parse_Ada (JSON, Index, Reload);
               exit;
            else
               Skip_Value (JSON, Index);
            end if;
         end;

      end loop;
   end Read_JSON;

   ----------------
   -- Skip_Value --
   ----------------

   procedure Skip_Value
     (JSON  : LSP.Structures.LSPAny;
      Index : in out Positive)
   is
      use all type VSS.JSON.Streams.JSON_Stream_Element_Kind;
      Level : Natural := 0;
   begin
      while Index <= JSON.Last_Index loop
         Level := (case JSON (Index).Kind is
                      when Start_Object | Start_Array => Level + 1,
                      when End_Object | End_Array     => Level - 1,
                      when others                     => Level);

         Index := Index + 1;

         exit when Level = 0;
      end loop;
   end Skip_Value;

end LSP.Ada_Configurations;

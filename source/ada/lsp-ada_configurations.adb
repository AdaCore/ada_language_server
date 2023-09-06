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

pragma Ada_2022;

with Ada.Containers.Generic_Anonymous_Array_Sort;

with GNATCOLL.Traces;

with VSS.Strings.Conversions;
with VSS.JSON.Streams;

package body LSP.Ada_Configurations is

   Doc_Style_Values : constant VSS.String_Vectors.Virtual_String_Vector :=
     [for Item in GNATdoc.Comments.Options.Documentation_Style =>
        VSS.Strings.To_Virtual_String (Item'Wide_Wide_Image).To_Lowercase];

   Display_Method_Values : constant VSS.String_Vectors.Virtual_String_Vector :=
     [for Item in DisplayMethodAncestryOnNavigationPolicy =>
        VSS.Strings.To_Virtual_String (Item'Wide_Wide_Image).To_Lowercase];

   function "+" (X : VSS.Strings.Virtual_String'Class) return String renames
     VSS.Strings.Conversions.To_UTF_8_String;

   ALS_COMPLETION_FORMATTING : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("ALS.COMPLETION.FORMATTING", Default => GNATCOLL.Traces.On);
   --  Used in Completion_Formatting/LSP.Ada_Completions.Pretty_Print_Snippet

   ---------------------------
   -- Completion_Formatting --
   ---------------------------

   function Completion_Formatting return Boolean is
   begin
      return ALS_COMPLETION_FORMATTING.Is_Active;
   end Completion_Formatting;

   ---------------
   -- Read_JSON --
   ---------------

   procedure Read_JSON
     (Self   : in out Configuration'Class;
      JSON   : LSP.Structures.LSPAny;
      Reload : out Boolean)
   is
      use all type VSS.JSON.JSON_Number_Kind;
      use all type VSS.JSON.Streams.JSON_Stream_Element_Kind;
      use type VSS.Strings.Virtual_String;
      use type VSS.String_Vectors.Virtual_String_Vector;

      Index            : Positive := JSON.First_Index;
      Variables_Names  : VSS.String_Vectors.Virtual_String_Vector;
      Variables_Values : VSS.String_Vectors.Virtual_String_Vector;

      procedure Swap_Variables (Left, Right : Positive);

      procedure Parse_Ada;
      procedure Skip_Value;
      procedure Set
        (Target : in out VSS.Strings.Virtual_String;
         Value  : VSS.Strings.Virtual_String);

      ---------------
      -- Parse_Ada --
      ---------------

      procedure Parse_Ada is
         procedure Parse_Variables (From : Positive);

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

         Name : VSS.Strings.Virtual_String;
      begin
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
                 DisplayMethodAncestryOnNavigationPolicy'Value
                   (+JSON (Index).String_Value);

            elsif Name = "followSymlinks"
              and then JSON (Index).Kind = Boolean_Value
            then
               Self.Follow_Symlinks := JSON (Index).Boolean_Value;

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

            Skip_Value;
         end loop;
      end Parse_Ada;

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

      ----------------
      -- Skip_Value --
      ----------------

      procedure Skip_Value is
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
   begin
      Reload := False;

      if JSON.Is_Empty or else JSON (Index).Kind /= Start_Object then
         return;
      else
         Index := Index + 1;
      end if;

      while Index <= JSON.Last_Index
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
               Parse_Ada;
            else
               Skip_Value;
            end if;
         end;

      end loop;

      Sort_Variables (1, Variables_Names.Length);

      Reload := Reload or else
        Variables_Names /= Self.Variables_Names or else
        Variables_Values /= Self.Variables_Values;

      Self.Variables_Names := Variables_Names;
      Self.Variables_Values := Variables_Values;
   end Read_JSON;

end LSP.Ada_Configurations;

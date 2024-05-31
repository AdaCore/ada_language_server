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

with GNATdoc.Comments.Options;

private with VSS.Characters.Latin;
with VSS.Strings;
with VSS.String_Vectors;

with LSP.Enumerations;
with LSP.Structures;

with GPR2.Context;
with GPR2.Path_Name;

package LSP.Ada_Configurations is

   type Configuration is tagged private;

   procedure Read_JSON
     (Self   : in out Configuration'Class;
      JSON   : LSP.Structures.LSPAny;
      Reload : out Boolean);

   procedure Read_File
     (Self : in out Configuration'Class;
      File : VSS.Strings.Virtual_String);

   function Project_File
     (Self : Configuration'Class) return VSS.Strings.Virtual_String;
   --  The project file, if provided by the user on Configuration/Init

   function Charset
     (Self : Configuration'Class) return VSS.Strings.Virtual_String;
   --  A character set for Libadalang

   function Relocate_Build_Tree
     (Self : Configuration'Class) return VSS.Strings.Virtual_String;
   --  Value of `relocateBuildTree`. See `--relocate-build-tree[=dir]`
   --  of `gprbuild`.

   function Relocate_Root
     (Self : Configuration'Class) return VSS.Strings.Virtual_String;
   --  Value of `rootDir`. See `--root-dir=dir` of `gprbuild`.

   function Named_Notation_Threshold
     (Self : Configuration'Class) return Natural;
   --  Defines the number of parameters/components at which point named
   --  notation is used for subprogram/aggregate completion snippets.

   function Log_Threshold (Self : Configuration'Class) return Natural;
   --  Maximum number of logs (should be > to the number of servers run
   --  simultaneously)

   function Diagnostics_Enabled (Self : Configuration'Class) return Boolean;
   --  Whether to publish diagnostics

   function Indexing_Enabled (Self : Configuration'Class) return Boolean;
   --  Whether to index sources in the background. This should be True
   --  for normal use, and can be disabled for debug or testing purposes.

   function Rename_In_Comments (Self : Configuration'Class) return Boolean;
   function Folding_Comments (Self : Configuration'Class) return Boolean;

   function Use_Completion_Snippets
     (Self : Configuration'Class) return Boolean;
   --  True if we should use snippets for completion (e.g:
   --  subprogram calls).

   function Insert_With_Clauses
     (Self : Configuration'Class) return Boolean;
   --  True if completion is allowed to insert automatically with-clauses for
   --  invisible symbols.

   function Indent_Only (Self : Configuration'Class) return Boolean;

   function Follow_Symlinks (Self : Configuration'Class) return Boolean;
   --  False if the client disables symlink following. In this case
   --  URIs from client should match file names reported by LAL and
   --  GNATCOLL.Project.

   function Documentation_Style (Self : Configuration'Class)
     return GNATdoc.Comments.Options.Documentation_Style;

   function Display_Method_Ancestry_Policy (Self : Configuration'Class)
     return LSP.Enumerations.AlsDisplayMethodAncestryOnNavigationPolicy;

   function Build_Path
     (Self : Configuration'Class; File : GPR2.Path_Name.Object)
      return GPR2.Path_Name.Object;
   --  Convert Self.Relocate_Build_Tree, Self.Relocate_Root & File to
   --  GPR2.Project.Tree.Load procedures Build_Path parameter.

   function Context (Self : Configuration'Class) return GPR2.Context.Object;
   --  Convert Configuration scenario variables to
   --  GPR2.Project.Tree.Load procedures Context parameter.

   function Completion_Formatting return Boolean;
   --  Used in LSP.Ada_Completions.Pretty_Print_Snippet

   function Partial_GNATPP return Boolean;
   --  Whether partial GNATPP is enabled.

   function On_Type_Formatting return Boolean;
   --  Whether onTypeFormatting is enabled.

   function On_Type_Formatting_Settings
     return LSP.Structures.DocumentOnTypeFormattingOptions;

private

   use type VSS.Strings.Virtual_String;

   type Configuration is tagged record
      Project_File             : VSS.Strings.Virtual_String;
      Charset                  : VSS.Strings.Virtual_String;
      Relocate_Build_Tree      : VSS.Strings.Virtual_String;
      Relocate_Root            : VSS.Strings.Virtual_String;
      Named_Notation_Threshold : Natural := 3;
      Log_Threshold            : Natural := 10;
      Diagnostics_Enabled      : Boolean := True;
      Indexing_Enabled         : Boolean := True;
      Rename_In_Comments       : Boolean := False;
      Folding_Comments         : Boolean := True;
      Use_Completion_Snippets  : Boolean := True;
      Indent_Only              : Boolean := True;
      Follow_Symlinks          : Boolean := True;
      Insert_With_Clauses      : Boolean := True;

      Documentation_Style      : GNATdoc.Comments.Options.Documentation_Style
        := GNATdoc.Comments.Options.GNAT;

      Method_Ancestry_Policy   :
        LSP.Enumerations.AlsDisplayMethodAncestryOnNavigationPolicy :=
          LSP.Enumerations.Usage_And_Abstract_Only;

      Variables_Names          : VSS.String_Vectors.Virtual_String_Vector;
      Variables_Values         : VSS.String_Vectors.Virtual_String_Vector;

      Context                  : GPR2.Context.Object;
   end record;

   function Project_File
     (Self : Configuration'Class) return VSS.Strings.Virtual_String is
       (Self.Project_File);

   function Charset
     (Self : Configuration'Class) return VSS.Strings.Virtual_String is
       (Self.Charset);

   function Relocate_Build_Tree
     (Self : Configuration'Class) return VSS.Strings.Virtual_String is
       (Self.Relocate_Build_Tree);

   function Relocate_Root
     (Self : Configuration'Class) return VSS.Strings.Virtual_String is
       (Self.Relocate_Root);

   function Diagnostics_Enabled (Self : Configuration'Class) return Boolean is
      (Self.Diagnostics_Enabled);

   function Indexing_Enabled (Self : Configuration'Class) return Boolean is
      (Self.Indexing_Enabled);

   function Use_Completion_Snippets
     (Self : Configuration'Class) return Boolean
   is
     (Self.Use_Completion_Snippets);

   function Insert_With_Clauses
     (Self : Configuration'Class) return Boolean
   is
     (Self.Insert_With_Clauses);

   function Follow_Symlinks (Self : Configuration'Class) return Boolean is
      (Self.Follow_Symlinks);

   function Named_Notation_Threshold
     (Self : Configuration'Class) return Natural is
       (Self.Named_Notation_Threshold);

   function Log_Threshold (Self : Configuration'Class) return Natural is
     (Self.Log_Threshold);

   function Rename_In_Comments (Self : Configuration'Class) return Boolean is
     (Self.Rename_In_Comments);

   function Folding_Comments (Self : Configuration'Class) return Boolean is
     (Self.Folding_Comments);

   function Indent_Only (Self : Configuration'Class) return Boolean is
     (Self.Indent_Only);

   function Documentation_Style (Self : Configuration'Class)
     return GNATdoc.Comments.Options.Documentation_Style is
       (Self.Documentation_Style);

   function Display_Method_Ancestry_Policy (Self : Configuration'Class)
     return LSP.Enumerations.AlsDisplayMethodAncestryOnNavigationPolicy is
       (Self.Method_Ancestry_Policy);

   function On_Type_Formatting_Settings
     return LSP.Structures.DocumentOnTypeFormattingOptions is
     (firstTriggerCharacter => 1 * VSS.Characters.Latin.Line_Feed,
      moreTriggerCharacter  => <>);

   function Context (Self : Configuration'Class) return GPR2.Context.Object
   is (Self.Context);

end LSP.Ada_Configurations;

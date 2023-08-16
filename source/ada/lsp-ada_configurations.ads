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

with VSS.Strings;
with VSS.String_Vectors;

with LSP.Structures;

package LSP.Ada_Configurations is

   type Configuration is tagged limited private;

   procedure Read_JSON
     (Self   : in out Configuration'Class;
      JSON   : LSP.Structures.LSPAny;
      Reload : out Boolean);

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

   function Indent_Only (Self : Configuration'Class) return Boolean;

   function Follow_Symlinks (Self : Configuration'Class) return Boolean;
   --  False if the client disables symlink following. In this case
   --  URIs from client should match file names reported by LAL and
   --  GNATCOLL.Project.

   function Documentation_Style (Self : Configuration'Class)
     return GNATdoc.Comments.Options.Documentation_Style;

   type Variable_List is record
      Names  : VSS.String_Vectors.Virtual_String_Vector;
      Values : VSS.String_Vectors.Virtual_String_Vector;
   end record;

   function Scenario_Variables
     (Self : Configuration'Class) return Variable_List;
   --  Scenario variables, if provided by the user on Configuration/Init

   type DisplayMethodAncestryOnNavigationPolicy is
     (Never, Usage_And_Abstract_Only, Definition_Only, Always);

   function Display_Method_Ancestry_Policy (Self : Configuration'Class)
     return DisplayMethodAncestryOnNavigationPolicy;

private

   type Configuration is tagged limited record
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

      Documentation_Style      : GNATdoc.Comments.Options.Documentation_Style
        := GNATdoc.Comments.Options.GNAT;

      Method_Ancestry_Policy   : DisplayMethodAncestryOnNavigationPolicy :=
        Usage_And_Abstract_Only;

      Variables_Names          : VSS.String_Vectors.Virtual_String_Vector;
      Variables_Values         : VSS.String_Vectors.Virtual_String_Vector;
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

   function Scenario_Variables
     (Self : Configuration'Class) return Variable_List is
       ((Self.Variables_Names, Self.Variables_Values));

   function Diagnostics_Enabled (Self : Configuration'Class) return Boolean is
      (Self.Diagnostics_Enabled);

   function Indexing_Enabled (Self : Configuration'Class) return Boolean is
      (Self.Indexing_Enabled);

   function Use_Completion_Snippets
     (Self : Configuration'Class) return Boolean is
       (Self.Use_Completion_Snippets);

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
     return DisplayMethodAncestryOnNavigationPolicy is
       (Self.Method_Ancestry_Policy);

end LSP.Ada_Configurations;

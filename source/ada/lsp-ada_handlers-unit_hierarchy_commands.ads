------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2026, AdaCore                          --
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

--  Return Ada library units and their source parts across the loaded project
--  tree.

with LSP.Ada_Commands;
with LSP.Errors;
with LSP.Server_Jobs;

package LSP.Ada_Handlers.Unit_Hierarchy_Commands is

   --  Execute with no arguments. The result is null when no project tree is
   --  available. Otherwise it has the following JSON shape:
   --
   --    {
   --      "root-project": Project,
   --      "namespaces": [Project, ...],
   --      "units": [
   --        {
   --          "namespace-id": string,
   --          "name": string,
   --          "simple-name": string,
   --          "parent-name": string,
   --          "kind": "package" | "subprogram" | "unknown",
   --          "is-generic"?: boolean,
   --          "is-instantiation"?: boolean,
   --          "is-renaming"?: boolean,
   --          "parts": [
   --            {
   --              "kind": "specification" | "body" | "separate" | "unknown",
   --              "uri": DocumentUri,
   --              "unit-index": number,
   --              "project": Project,
   --              "separate-name"?: string,
   --              "range"?: Range,
   --              "selection-range"?: Range
   --            }, ...
   --          ]
   --        }, ...
   --      ]
   --    }
   --
   --  Project contains id, name, an optional uri, is-runtime, and
   --  is-externally-built. IDs are GPR2 view IDs, as in the Project View.
   --  A runtime project may have no project file URI.
   --
   --  Each unit is identified by its namespace-id and qualified name. Its
   --  namespace-id refers to a project in namespaces. Shared dependencies can
   --  appear in more than one namespace, so names alone do not identify units.
   --  Names come from GPR2 and use its canonical casing. For A.B.C, simple-name
   --  is C and parent-name is A.B. Root units have an empty parent-name, and
   --  a parent unit need not have source files in the result.
   --  Namespaces keep unrelated definitions in aggregate projects distinct.
   --  GPR2 selects the visible source parts, including project extensions
   --  and runtime overrides. Runtime and externally built units are included.
   --
   --  Only library units are listed, including generic units, instantiations,
   --  renamings, and library subprograms. Nested declarations are not listed.
   --  Separates belong to their library unit and have a separate-name
   --  relative to it. A unit-index of zero means a single-unit source;
   --  other indices are one-based positions in a multi-unit source.
   --
   --  Unit kind and declaration flags come from the specification when it
   --  exists, or from the body otherwise. Part kind comes from GPR2 and does
   --  not depend on parsing the source.
   --
   --  Each part's range covers its declaration; selection-range covers the
   --  defining name, or the declaration if no defining name is available.
   --  Ranges use zero-based LSP lines and UTF-16 columns, with an excluded end.
   --  Ranges may be absent when no declaration can be found. Parser recovery
   --  can still provide ranges for malformed source. If the declaration used
   --  for unit metadata is unavailable, the unit kind is unknown and its
   --  declaration flags are absent. Source metadata is still returned.

   type Command is new LSP.Ada_Commands.Command with null record;

   overriding
   function Create
     (Any : not null access LSP.Structures.LSPAny_Vector) return Command;

   overriding
   procedure Execute
     (Self     : Command;
      Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Response : in out LSP.Structures.LSPAny_Or_Null;
      Error    : in out LSP.Errors.ResponseError_Optional);

   overriding
   function Priority (Self : Command) return LSP.Server_Jobs.Job_Priority
   is (LSP.Server_Jobs.Low);

   for Command'External_Tag use "als-unit-hierarchy-information";

end LSP.Ada_Handlers.Unit_Hierarchy_Commands;

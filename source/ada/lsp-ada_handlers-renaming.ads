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

with Libadalang.Analysis;

with LAL_Refactor;

with LSP.Ada_Context_Sets;

package LSP.Ada_Handlers.Renaming is

   subtype File_Edit is LSP.Structures.documentChanges_OfWorkspaceEdit_Item;

   function Hash (X : File_Edit) return Ada.Containers.Hash_Type;

   package Edit_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => File_Edit,
      Hash                => Hash,
      Equivalent_Elements => LSP.Structures."=",
      "="                 => LSP.Structures."=");

   procedure Process_Context
     (Self      : in out Message_Handler'Class;
      C         : LSP.Ada_Context_Sets.Context_Access;
      Name_Node : Libadalang.Analysis.Name;
      New_Name  : VSS.Strings.Virtual_String;
      Filter    : in out Edit_Sets.Set;
      Result    : in out LSP.Structures.WorkspaceEdit;
      Errors    : in out LAL_Refactor.Refactoring_Diagnostic_Vector);
   --  Process the rename request for the given context, and add the
   --  edits to Result using Filter to avoid duplicates.

end LSP.Ada_Handlers.Renaming;

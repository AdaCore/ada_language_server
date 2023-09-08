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

with VSS.Strings;

with LSP.Structures;

package LSP.Ada_Client_Capabilities is

   type Client_Capability is tagged limited private;
   --  This type holds client initialization response and provides handy
   --  queries on the client capabilities

   procedure Initialize
     (Self  : in out Client_Capability'Class;
      Value : LSP.Structures.InitializeParams);
   --  Save initialze parameters

   function To_Server_Capabilities
     (Self : Client_Capability'Class;
      Incremental_Text_Changes : Boolean;
      Commands                 : LSP.Structures.Virtual_String_Vector;
      Token_Types              : LSP.Structures.Virtual_String_Vector;
      Token_Modifiers          : LSP.Structures.Virtual_String_Vector)
      return LSP.Structures.ServerCapabilities;
   --  Calculate server capabilities by client Capability and configuation
   --  options.

   function Root (Self : Client_Capability'Class)
     return VSS.Strings.Virtual_String;
   --  rootUri (or rootPath)
   --  if not rootUri/rootPath is provided, then it means no folder is open.
   --  Return an empty string in this case.

   procedure Set_Root_If_Empty
     (Self  : in out Client_Capability'Class;
      Value : VSS.Strings.Virtual_String);
   --  Is Self.Root is empty then replace it with given Value

   function Line_Folding_Only (Self : Client_Capability'Class) return Boolean;
   --  Returns capabilities.textDocument.foldingRange.lineFoldingOnly or False

   function Resolve_Lazily (Self : Client_Capability'Class) return Boolean;
   --  Returns True when resolve contains `documentation` and `details`

   function Token_Types (Self : Client_Capability'Class)
     return LSP.Structures.Virtual_String_Vector;

   function Token_Modifiers (Self : Client_Capability'Class)
     return LSP.Structures.Virtual_String_Vector;

   function Versioned_Documents
     (Self : Client_Capability'Class) return Boolean;
   --  Returns capabilities.workspace.workspaceEdit.documentChanges or False

   function Supports_Related_Diagnostics
     (Self : Client_Capability'Class) return Boolean;
   --  Returns capabilities.textDocument.publishDiagnostics.relatedInformation

   function Hierarchical_Symbol
     (Self : Client_Capability'Class) return Boolean;
   --  Returns textDocument.documentSymbol.hierarchicalDocumentSymbolSupport

   --  Resource 0perations --

   function Resource_Create_Supported
     (Self : Client_Capability'Class) return Boolean;

   function Resource_Delete_Supported
     (Self : Client_Capability'Class) return Boolean;

   function Resource_Rename_Supported
     (Self : Client_Capability'Class) return Boolean;

   --  Advanced refactoring settings --

   function Refactoring_Change_Parameters_Type
     (Self : Client_Capability'Class) return Boolean;

   function Refactoring_Add_Parameter
     (Self : Client_Capability'Class) return Boolean;

   function Refactoring_Change_Parameters_Default_Value
     (Self : Client_Capability'Class) return Boolean;

   function Refactoring_Replace_Type
     (Self : Client_Capability'Class) return Boolean;

   --  Code actions --

   function Code_Action
     (Self : Client_Capability'Class) return Boolean;

   function Code_ActionLiteralSupport
     (Self : Client_Capability'Class) return Boolean;

private

   type Advanced_Refactorings is
     (Add_Parameter,
      Change_Parameters_Type,
      Change_Parameters_Default_Value,
      Replace_Type);
   --  Enum with the advanced refactorings that clients might support

   type Advanced_Refactorings_Capabilities is
     array (Advanced_Refactorings) of Boolean;
   --  Array that determines what advanced refactorings clients support

   type Client_Capability is tagged limited record
      Value : LSP.Structures.InitializeParams;
      Root  : VSS.Strings.Virtual_String;

      Advanced_Refactorings : Advanced_Refactorings_Capabilities :=
        (others => False);
      --  Experimental client capabilities that are extensions of the ones
      --  defined in the LSP
   end record;

   function Root (Self : Client_Capability'Class)
     return VSS.Strings.Virtual_String is (Self.Root);

   procedure Parse_Experimental (Self : in out Client_Capability'Class);

end LSP.Ada_Client_Capabilities;

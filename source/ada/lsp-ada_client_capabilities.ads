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
   pragma Preelaborate;

   type Client_Capability is tagged limited private;
   --  This type holds client initialization response and provides handy
   --  queries on the client capabilities

   procedure Initialize
     (Self  : in out Client_Capability'Class;
      Value : LSP.Structures.InitializeParams);
   --  Save initialze parameters

   function To_Server_Capabilities
     (Self : Client_Capability'Class;
      Incremental_Text_Changes : Boolean)
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

private

   type Client_Capability is tagged limited record
      Value : LSP.Structures.InitializeParams;
      Root  : VSS.Strings.Virtual_String;
   end record;

   function Root (Self : Client_Capability'Class)
     return VSS.Strings.Virtual_String is (Self.Root);

end LSP.Ada_Client_Capabilities;

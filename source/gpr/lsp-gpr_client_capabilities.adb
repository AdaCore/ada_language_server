------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

with VSS.String_Vectors;

with LSP.Structures.Unwrap;

package body LSP.GPR_Client_Capabilities is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : in out Client_Capability'Class;
      Value : LSP.Structures.InitializeParams) is
   begin
      Self.Value := Value;
   end Initialize;

   --------------------
   -- Resolve_Lazily --
   --------------------

   function Resolve_Lazily (Self : Client_Capability'Class) return Boolean is
      use LSP.Structures.Unwrap;

      List : constant VSS.String_Vectors.Virtual_String_Vector :=
        properties
          (resolveSupport
             (completionItem
                (completion
                   (Self.Value.capabilities.textDocument))));

   begin
      return List.Contains ("detail") and then List.Contains ("documentation");
   end Resolve_Lazily;

end LSP.GPR_Client_Capabilities;

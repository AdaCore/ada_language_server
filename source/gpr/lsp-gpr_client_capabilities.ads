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

with LSP.Structures;

package LSP.GPR_Client_Capabilities is

   type Client_Capability is tagged limited private;
   --  This type holds client initialization response and provides handy
   --  queries on the client capabilities

   procedure Initialize
     (Self  : in out Client_Capability'Class;
      Value : LSP.Structures.InitializeParams);
   --  Save initialize parameters

   function Resolve_Lazily (Self : Client_Capability'Class) return Boolean;
   --  Returns True when resolve contains `documentation` and `details`

private

   type Client_Capability is tagged limited record
      Value : LSP.Structures.InitializeParams;
   end record;

end LSP.GPR_Client_Capabilities;

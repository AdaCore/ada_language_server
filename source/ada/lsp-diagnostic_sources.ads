------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with LSP.Messages;
limited with LSP.Ada_Contexts;

package LSP.Diagnostic_Sources is

   type Diagnostic_Source is limited interface;
   --  A source of diagnostic (errors/warnings) messages

   type Diagnostic_Source_Access is
     access LSP.Diagnostic_Sources.Diagnostic_Source'Class;

   procedure Get_Diagnostic
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context;
      Errors  : out LSP.Messages.Diagnostic_Vector) is abstract;
   --  Fill diagnostics for given document.

   function Has_New_Diagnostic
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context) return Boolean is abstract;
   --  Return True if diagnostic changed since last call to Get_Diagnostic or
   --  if Get_Diagnostic was never called and any diagnostic presents.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Diagnostic_Source'Class, Diagnostic_Source_Access);

end LSP.Diagnostic_Sources;

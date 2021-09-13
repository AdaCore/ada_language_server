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

with LSP.Diagnostic_Sources;

package LSP.Ada_Handlers.Project_Diagnostics is

   type Diagnostic_Source
     (Handler : not null access LSP.Ada_Handlers.Message_Handler)
   is limited new LSP.Diagnostic_Sources.Diagnostic_Source with private;

   overriding procedure Get_Diagnostic
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context;
      Errors  : out LSP.Messages.Diagnostic_Vector);
   --  Fill diagnostics for given document.

   overriding function Has_New_Diagnostic
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context) return Boolean;

private

   type Diagnostic_Source
     (Handler : not null access LSP.Ada_Handlers.Message_Handler)
   is limited new LSP.Diagnostic_Sources.Diagnostic_Source with record
      Last_Status : Load_Project_Status := Valid_Project_Configured;
   end record;

end LSP.Ada_Handlers.Project_Diagnostics;

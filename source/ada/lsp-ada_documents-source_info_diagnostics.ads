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
--
--  This package provides diagnostics displaying source information about files
--  being opened.

with LSP.Ada_Handlers;
with LSP.Diagnostic_Sources;

package LSP.Ada_Documents.Source_Info_Diagnostics is

   type Diagnostic_Source
     (Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Document : not null LSP.Ada_Documents.Document_Access)
   is limited new LSP.Diagnostic_Sources.Diagnostic_Source with private;

   overriding procedure Get_Diagnostic
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context;
      Errors  : out LSP.Structures.Diagnostic_Vector);

   overriding function Has_New_Diagnostic
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context) return Boolean;

   overriding function Is_Enabled
     (Self : Diagnostic_Source) return Boolean;

private

   type Diagnostic_Source
     (Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Document : not null LSP.Ada_Documents.Document_Access)
   is limited new LSP.Diagnostic_Sources.Diagnostic_Source with record
      Last_Project_Stamp : LSP.Ada_Handlers.Project_Stamp :=
        LSP.Ada_Handlers.No_Project_Stamp;
      --  Used to detect when the project has been reloaded.
      --  We want to update source information diagnostics only after
      --  project reloade.
   end record;

end LSP.Ada_Documents.Source_Info_Diagnostics;

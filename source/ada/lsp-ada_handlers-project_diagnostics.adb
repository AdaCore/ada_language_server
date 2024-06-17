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

package body LSP.Ada_Handlers.Project_Diagnostics is

   --------------------
   -- Get_Diagnostic --
   --------------------

   overriding procedure Get_Diagnostic
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context;
      Errors  : out LSP.Structures.Diagnostic_Vector) is
   begin
      Self.Last_Status := Self.Handler.Project_Status;

      Self.Handler.Tracer.Trace ("Diag: " & Self.Last_Status'Image);
      --  If we have a valid project return immediately: we want to display
      --  diagnostics only if there is an issue to solve or a potential
      --  enhancement.

      Errors.Append_Vector
        (LSP.Ada_Project_Loading.Get_Diagnostics (Self.Last_Status));
   end Get_Diagnostic;

   ------------------------
   -- Has_New_Diagnostic --
   ------------------------

   overriding function Has_New_Diagnostic
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context)
      return Boolean
   is
      pragma Unreferenced (Context);
   begin
      return LSP.Ada_Project_Loading.Has_New_Diagnostics
        (Self.Last_Status,
         Self.Handler.Project_Status);
   end Has_New_Diagnostic;

end LSP.Ada_Handlers.Project_Diagnostics;

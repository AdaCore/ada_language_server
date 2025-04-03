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

with LSP.Enumerations;
with VSS.Strings.Conversions;

package body LSP.Ada_Documents.Source_Info_Diagnostics is

   ------------------------
   -- Has_New_Diagnostic --
   ------------------------

   overriding function Has_New_Diagnostic
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context) return Boolean
     is
      use LSP.Ada_Handlers;

      Current_Project_Stamp : constant LSP.Ada_Handlers.Project_Stamp :=
        Self.Handler.Get_Project_Stamp;
      Is_Enabled : constant Boolean :=
        Self.Handler.Source_Info_Diagnostics_Enabled;
   begin
      --  The project has been reloaded: compute source information diagnostics
      --  again since the set of source files might have been changed.
      if Self.Last_Project_Stamp /= Current_Project_Stamp then
         Self.Last_Project_Stamp := Current_Project_Stamp;
         return True;
      end if;

      --  The 'sourceInfoDiagnostics' option has just changed: always return
      --  True in this case (e.g: to clear any existing diagnostic when the
      --  option gets disabled)
      if Self.Enabled /= Is_Enabled then
         Self.Enabled := Is_Enabled;
         return True;
      end if;

      return False;
   end Has_New_Diagnostic;

   ---------------------
   -- Get_Diagnostics --
   ---------------------

   overriding
   procedure Get_Diagnostics
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context;
      Errors  : out LSP.Structures.Diagnostic_Vector)
   is
   begin
      --  If the unit associated to the document belongs to the fallback context
      --  it means that the document's file does not belong the loaded project:
      --  emit a hint diagnostic in that case.
      if Self.Is_Enabled and then Context.Is_Fallback_Context then
         declare
            Diag_Msg : constant String :=
              (if Self.Handler.Get_Project_Status.Is_Project_Loaded
               then "This file does not belong to the loaded project tree"
               else "No project tree currently loaded");
         begin
            Errors.Append
              (LSP.Structures.Diagnostic'
                 (a_range  => ((0, 0), (0, 0)),
                  source   => "ada.sourceInformation",
                  severity => (True, LSP.Enumerations.Hint),
                  message  =>
                    VSS.Strings.Conversions.To_Virtual_String
                      (Diag_Msg
                       & ": some features might be imprecise "
                       & "(e.g: Find All References) or not available "
                       & "(e.g: Rename)."),
                  others   => <>));
         end;
      end if;
   end Get_Diagnostics;

   ----------------
   -- Is_Enabled --
   ----------------

   overriding function Is_Enabled
     (Self : Diagnostic_Source) return Boolean is
   begin
      return Self.Handler.Source_Info_Diagnostics_Enabled;
   end Is_Enabled;

end LSP.Ada_Documents.Source_Info_Diagnostics;

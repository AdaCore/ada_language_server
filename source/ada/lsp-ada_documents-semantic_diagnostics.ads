------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2026, AdaCore                     --
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
--  This package saves semantic errors found by Libadalang and makes them
--  available to the client. A background job finds the errors (see
--  LSP.Ada_Semantic_Diagnostics).

with LSP.Ada_Handlers;
with LSP.Diagnostic_Sources;

package LSP.Ada_Documents.Semantic_Diagnostics is

   type Diagnostic_Source
     (Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Document : not null LSP.Ada_Documents.Document_Access)
   is limited new LSP.Diagnostic_Sources.Diagnostic_Source with private;

   type Semantic_Diagnostic_Source_Access is
     access all Diagnostic_Source'Class;

   overriding
   procedure Get_Diagnostics
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context;
      Errors  : out LSP.Structures.Diagnostic_Vector);
   --  Return the saved semantic diagnostics.

   overriding
   function Has_New_Diagnostic
     (Self : in out Diagnostic_Source; Context : LSP.Ada_Contexts.Context)
      return Boolean;
   --  Return True if there are results the client has not received yet,
   --  or if the semantic diagnostics setting has changed.

   overriding
   function Is_Enabled (Self : Diagnostic_Source) return Boolean;
   --  Return True if the settings enable semantic diagnostics.

   procedure Rebase_Diagnostics
     (Self    : in out Diagnostic_Source;
      Changes : LSP.Structures.TextDocumentContentChangeEvent_Vector);
   --  Move saved diagnostics to match the edited text. For example, adding
   --  two lines above an error moves it down two lines. Remove errors that
   --  overlap an edit; the next check will compute them again. Replacing the
   --  whole text clears all saved errors. This does not publish the results;
   --  the next background check does that.

   procedure Update_Diagnostics
     (Self             : in out Diagnostic_Source;
      New_Diagnostics  : LSP.Structures.Diagnostic_Vector;
      Range_To_Replace : LSP.Structures.A_Range_Optional := (Is_Set => False));
   --  Replace saved diagnostics that overlap Range_To_Replace with
   --  New_Diagnostics. Keep the other saved diagnostics. If no range is
   --  given, replace them all with New_Diagnostics.

private

   type Diagnostic_Source
     (Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Document : not null LSP.Ada_Documents.Document_Access)
   is limited new LSP.Diagnostic_Sources.Diagnostic_Source with record
      Cached_Errors : LSP.Structures.Diagnostic_Vector;
      --  Saved semantic diagnostics, including errors kept from older checks.

      Has_Unpublished_Results : Boolean := False;
      --  True when results or settings have changed. Get_Diagnostics clears
      --  this flag when it returns the diagnostics to publish.

      Enabled : Boolean := True;
      --  The last value seen for the 'semanticDiagnostics' setting.
   end record;

end LSP.Ada_Documents.Semantic_Diagnostics;

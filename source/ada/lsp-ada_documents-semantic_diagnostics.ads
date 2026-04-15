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
--  This package provides a diagnostic source for Libadalang semantic
--  diagnostics (name resolution failures).  Diagnostics are computed
--  asynchronously by a background job (see LSP.Ada_Semantic_Diagnostics).

with LSP.Ada_Handlers;
with LSP.Diagnostic_Sources;

package LSP.Ada_Documents.Semantic_Diagnostics is

   type Diagnostic_Source
     (Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Document : not null LSP.Ada_Documents.Document_Access)
   is limited new LSP.Diagnostic_Sources.Diagnostic_Source with private;

   type Semantic_Diagnostic_Source_Access is access all Diagnostic_Source'Class;

   overriding procedure Get_Diagnostics
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context;
      Errors  : out LSP.Structures.Diagnostic_Vector);
   --  Return the cached set of semantic diagnostics.

   overriding function Has_New_Diagnostic
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context) return Boolean;
   --  Return True if a background job has deposited new results that have
   --  not yet been handed to the client.

   overriding function Is_Enabled
     (Self : Diagnostic_Source) return Boolean;
     --  Return True if semantic diagnostics are enabled
     --  in the handler's configuration.

   procedure Update_Diagnostics
     (Self           : in out Diagnostic_Source;
      Errors         : LSP.Structures.Diagnostic_Vector;
      Eviction_Range : LSP.Structures.A_Range_Optional);
   --  Update the cached diagnostics with Errors.
   --  When Eviction_Range is not set, Errors replaces the whole cache.
   --  Otherwise only cached diagnostics overlapping Eviction_Range are
   --  evicted, then Errors is appended.

private

   type Diagnostic_Source
     (Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Document : not null LSP.Ada_Documents.Document_Access)
   is limited new LSP.Diagnostic_Sources.Diagnostic_Source with record
      Cached_Errors : LSP.Structures.Diagnostic_Vector;
      --  The last set of semantic diagnostics deposited by a background job.

      Has_Unpublished_Results : Boolean := False;
      --  Used to know when to trigger a publish diagnostics notification.
      --  Set to True by background jobs when they deposit new results, and
      --  reset to False by when the client gets the diagnostics to publish
      --  them.
   end record;

end LSP.Ada_Documents.Semantic_Diagnostics;

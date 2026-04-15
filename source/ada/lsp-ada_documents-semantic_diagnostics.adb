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

with LSP.Utils;

package body LSP.Ada_Documents.Semantic_Diagnostics is

   ---------------------
   -- Get_Diagnostics --
   ---------------------

   overriding procedure Get_Diagnostics
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context;
      Errors  : out LSP.Structures.Diagnostic_Vector)
   is
      pragma Unreferenced (Context);
   begin
      Errors.Append_Vector (Self.Cached_Errors);
      Self.Has_Unpublished_Results := False;
   end Get_Diagnostics;

   ------------------------
   -- Has_New_Diagnostic --
   ------------------------

   overriding function Has_New_Diagnostic
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      return Self.Has_Unpublished_Results;
   end Has_New_Diagnostic;

   ----------------
   -- Is_Enabled --
   ----------------

   overriding function Is_Enabled
     (Self : Diagnostic_Source) return Boolean is
   begin
      return Self.Handler.Semantic_Diagnostics_Enabled;
   end Is_Enabled;

   -------------------------
   -- Update_Diagnostics --
   -------------------------

   procedure Update_Diagnostics
     (Self           : in out Diagnostic_Source;
      Errors         : LSP.Structures.Diagnostic_Vector;
      Eviction_Range : LSP.Structures.A_Range_Optional)
   is
   begin
      if not Eviction_Range.Is_Set then
         Self.Cached_Errors := Errors;
      else
         --  Evict cached diagnostics that overlap the traversal range,
         --  then append the newly computed ones.
         declare
            New_Cache : LSP.Structures.Diagnostic_Vector;
         begin
            for Diag of Self.Cached_Errors loop
               if not LSP.Utils.Overlaps (Diag.a_range, Eviction_Range.Value)
               then
                  New_Cache.Append (Diag);
               end if;
            end loop;
            New_Cache.Append_Vector (Errors);
            Self.Cached_Errors := New_Cache;
         end;
      end if;
      Self.Has_Unpublished_Results := True;
   end Update_Diagnostics;

end LSP.Ada_Documents.Semantic_Diagnostics;

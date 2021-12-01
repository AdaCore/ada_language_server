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

with Ada.Strings.Wide_Wide_Unbounded;

with LSP.Lal_Utils;

package body LSP.Ada_Documents.LAL_Diagnostics is

   function Get_Diagnostics
     (Self    : in out Diagnostic_Source'Class;
      Context : LSP.Ada_Contexts.Context) return Diagnostics;

   --------------------
   -- Get_Diagnostic --
   --------------------

   overriding procedure Get_Diagnostic
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context;
      Errors  : out LSP.Messages.Diagnostic_Vector)
   is
      Item : LSP.Messages.Diagnostic;
   begin
      Item.source := (True, "libadalang");
      Self.Errors := Self.Get_Diagnostics (Context);

      for J in Self.Errors.List'Range loop
         Item.span := LSP.Lal_Utils.To_Span (Self.Errors.List (J).Sloc_Range);
         Item.message := LSP.Lal_Utils.To_Virtual_String
           (Self.Errors.List (J).Message);

         Errors.Append (Item);
      end loop;
   end Get_Diagnostic;

   ------------------------
   -- Has_New_Diagnostic --
   ------------------------

   overriding function Has_New_Diagnostic
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context) return Boolean
   is
   begin
      return Self.Errors /= Self.Get_Diagnostics (Context);
   end Has_New_Diagnostic;

   --------------------
   -- Get_Diagnostics --
   --------------------

   function Get_Diagnostics
     (Self    : in out Diagnostic_Source'Class;
      Context : LSP.Ada_Contexts.Context) return Diagnostics
   is
      Last   : Natural := 0;
      Errors : Langkit_Support.Diagnostics.Diagnostics_Array
        (1 .. MAX_NB_DIAGNOSTICS);

      Unit : constant Libadalang.Analysis.Analysis_Unit :=
        Self.Document.Unit (Context);
   begin
      if Unit.Has_Diagnostics then
         for Error of Unit.Diagnostics loop

            --  Filter out diagnostics that simply report "Cannot parse <..>",
            --  as these are generally not useful to the end user.
            if Ada.Strings.Wide_Wide_Unbounded.Index
              (Error.Message, "Cannot parse <") /= 1
            then
               Last := Last + 1;
               Errors (Last) := Error;
               exit when Last >= MAX_NB_DIAGNOSTICS;
            end if;
         end loop;

         return (Last, Errors (1 .. Last));
      else
         return (Last => 0, List => (1 .. 0 => <>));
      end if;
   end Get_Diagnostics;

end LSP.Ada_Documents.LAL_Diagnostics;

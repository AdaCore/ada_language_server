------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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

package body LSP.Ada_Highlighters is

   ----------------
   -- Get_Tokens --
   ----------------

   function Get_Tokens
     (Self   : Ada_Highlighter'Class; Unit : Libadalang.Analysis.Analysis_Unit;
      Tracer : in out LSP.Tracers.Tracer'Class; Span : LSP.Structures.A_Range)
      return LSP.Structures.Natural_Vector
   is
   begin
      return LSP.Structures.Empty;
   end Get_Tokens;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : in out Ada_Highlighter'Class;
      Client :        LSP.Structures.SemanticTokensClientCapabilities;
      Legend :    out LSP.Structures.SemanticTokensLegend) is
   begin
      null;
   end Initialize;

end LSP.Ada_Highlighters;

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

with Libadalang.Analysis;

with LSP.Ada_Completions;
with LSP.Search;

package LSP.Ada_Handlers.Symbols is

   procedure Flat_Document_Symbols
     (Self    : in out Message_Handler'Class;
      Unit    : Libadalang.Analysis.Analysis_Unit;
      Pattern : LSP.Search.Search_Pattern'Class;
      Result  : in out LSP.Structures.DocumentSymbol_Result);

   procedure Hierarchical_Document_Symbols
     (Self    : in out Message_Handler'Class;
      Unit    : Libadalang.Analysis.Analysis_Unit;
      Pattern : LSP.Search.Search_Pattern'Class;
      Result  : in out LSP.Structures.DocumentSymbol_Vector);

   procedure Write_Symbols
     (Self   : in out Message_Handler'Class;
      Names  : LSP.Ada_Completions.Completion_Maps.Map;
      Result : in out LSP.Structures.SymbolInformation_Vector);

end LSP.Ada_Handlers.Symbols;

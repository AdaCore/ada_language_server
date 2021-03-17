------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

--  Provide utilities to preprocess buffers before passing them to Libadalang

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with VSS.Strings;           use VSS.Strings;

with Libadalang.Analysis;
with Libadalang.Common;     use Libadalang.Common;

package LSP.Preprocessor is

   function Preprocess_Buffer
     (Buffer : Virtual_String) return Unbounded_String;
   --  Preprocess a buffer for the needs of the Ada Language Server: take the
   --  whole buffer as parameter as read on disk or in open editors and
   --  return a modified version that we want Libadalang to see.
   --  Buffer must be encoded in UTF-8.
   --  The resulting string is also encoded in UTF-8.
   --  Note: at the moment this returns an Unbounded_String because this
   --  is what's most suitable for Libadalang.

   function Preprocess_File
     (Filename : String; Charset : String) return Unbounded_String;
   --  Load a file form disk and preprocess it with Preprocess_Buffer

   function Get_From_File
     (Context  : Libadalang.Analysis.Analysis_Context;
      Filename : String;
      Charset  : String;
      Reparse  : Boolean := False;
      Rule     : Grammar_Rule := Default_Grammar_Rule)
      return Libadalang.Analysis.Analysis_Unit;
   --  Behaves like Libadalang.Analysis.Get_From_File, but preprocesses
   --  the file using Preprocess_Buffer above.

end LSP.Preprocessor;

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

with VSS.Strings;           use VSS.Strings;

with Langkit_Support.File_Readers;

package LSP.Preprocessor is

   function Preprocess_Buffer
     (Buffer : Virtual_String)
      return Langkit_Support.File_Readers.Decoded_File_Contents;
   --  Preprocess a buffer for the needs of the Ada Language Server: take the
   --  whole buffer as parameter as read on disk or in open editors and
   --  return a modified version that we want Libadalang to see.
   --  Buffer must be encoded in UTF-8.
   --  The resulting string is also encoded in UTF-8.
   --  Note: at the moment this returns a Decoded_File_Contents because this
   --  is what's most suitable for Libadalang, and avoids one string copy.

   function Preprocess_File
     (Filename : String; Charset : String)
      return Langkit_Support.File_Readers.Decoded_File_Contents;
   --  Load a file form disk and preprocess it with Preprocess_Buffer
   --  The Filename has UTF-8 encoding.

end LSP.Preprocessor;

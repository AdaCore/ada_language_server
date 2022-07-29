------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2021, AdaCore                       --
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

--  This package provides a Langkit File_Reader which is able to
--     - read files from open documents
--     - preprocess code on the fly

with Langkit_Support.File_Readers;
with Langkit_Support.Diagnostics;
with Libadalang.Preprocessing;

package LSP.Ada_Handlers.File_Readers is

   type LSP_Reader_Interface (Handler : access Message_Handler) is new
     Langkit_Support.File_Readers.File_Reader_Interface with
   record
      Preprocessing_Data : Libadalang.Preprocessing.Preprocessor_Data :=
           Libadalang.Preprocessing.No_Preprocessor_Data;
   end record;

   overriding procedure Read
     (Self        : LSP_Reader_Interface;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Langkit_Support.File_Readers.Decoded_File_Contents;
      Diagnostics : in out
        Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector);

   overriding procedure Release
     (Self : in out LSP_Reader_Interface) is null;

end LSP.Ada_Handlers.File_Readers;

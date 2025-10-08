------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2021-2023, AdaCore                     --
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
--  - read files from open documents
--  - return empty text if file is excluded
--  - preprocess code on the fly

with GPR2.Project.View;

with Langkit_Support.File_Readers;
with Langkit_Support.Diagnostics;
with Libadalang.Preprocessing;

package LSP.Ada_Handlers.File_Readers is

   type LSP_File_Reader (Handler : access Message_Handler'Class) is
     new Langkit_Support.File_Readers.File_Reader_Interface with private;

   procedure Initialize
     (Self : in out LSP_File_Reader'Class;
      Tree : GPR2.Project.Tree.Object;
      View : GPR2.Project.View.Object);
   --  Extract the preprocessing options from the context's project
   --  and create the file reader which will preprocess the files
   --  accordingly.
   --  Extract IDE.Excluded_Source_Files list from the project and keep it
   --  to ignore such files when reading.

private

   type LSP_File_Reader (Handler : access Message_Handler'Class) is
     new Langkit_Support.File_Readers.File_Reader_Interface
   with record
      Preprocessing_Data : Libadalang.Preprocessing.Preprocessor_Data :=
        Libadalang.Preprocessing.No_Preprocessor_Data;

      Excluded_Files : LSP.Ada_File_Sets.File_Sets.Set;
      --  Set of files from IDE.Excluded_Source_Files project attribute
   end record;

   overriding
   procedure Read
     (Self        : LSP_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Langkit_Support.File_Readers.Decoded_File_Contents;
      Diagnostics :
        in out Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector);

   overriding
   procedure Release (Self : in out LSP_File_Reader) is null;

end LSP.Ada_Handlers.File_Readers;

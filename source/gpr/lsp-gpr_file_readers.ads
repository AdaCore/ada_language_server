------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
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

--  This package provides a GPR File_Reader which is able to
--     - read files from open documents

with GPR2.File_Readers;
with GPR2.Log;

with LSP.GPR_Handlers;

package LSP.GPR_File_Readers is

   type File_Reader is
     new GPR2.File_Readers.File_Reader_Interface with private;

   overriding procedure Read
     (Self        : File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out GPR2.File_Readers.Decoded_File_Contents;
      Diagnostics : in out GPR2.Log.Object);

   overriding procedure Release (Self : in out File_Reader) is null;

   function Create
     (Handler : access LSP.GPR_Handlers.Message_Handler)
      return GPR2.File_Readers.File_Reader_Reference;

private

   type File_Reader is new GPR2.File_Readers.File_Reader_Interface with
      record
         Handler : access LSP.GPR_Handlers.Message_Handler;
      end record;
end LSP.GPR_File_Readers;

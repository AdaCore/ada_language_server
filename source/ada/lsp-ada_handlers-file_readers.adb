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

with VSS.Strings;       use VSS.Strings;
with LSP.Ada_Documents; use LSP.Ada_Documents;
with LSP.Preprocessor;  use LSP.Preprocessor;

package body LSP.Ada_Handlers.File_Readers is

   procedure Internal_Read
     (Handler  : access Message_Handler;
      Filename : String;
      Charset  : String;
      Contents : out Langkit_Support.File_Readers.Decoded_File_Contents);

   -------------------
   -- Internal_Read --
   -------------------

   procedure Internal_Read
     (Handler  : access Message_Handler;
      Filename : String;
      Charset  : String;
      Contents : out Langkit_Support.File_Readers.Decoded_File_Contents) is
      Doc : Document_Access;
   begin
      --  First check if the file is an open document
      Doc := Handler.Get_Open_Document
        (URI   => LSP.Types.File_To_URI (Filename),
         Force => False);

      if Doc /= null then
         --  There is a document - we can get this and preprocess
         Contents := Preprocess_Buffer (Buffer => Doc.Text);

      else
         --  No open document: preprocess from the file
         Contents := Preprocess_File (Filename => Filename,
                                      Charset  => Charset);
      end if;
   end Internal_Read;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self        : LSP_Reader_Interface;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Langkit_Support.File_Readers.Decoded_File_Contents;
      Diagnostics : in out
        Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector)
   is
   begin
      Internal_Read (Self.Handler, Filename, Charset, Contents);
   end Read;

   overriding procedure Read
     (Self         : GPR2_Reader_Interface;
      Filename     : String;
      Charset      : String;
      Read_BOM     : Boolean;
      Contents     : out GPR2.File_Readers.Decoded_File_Contents;
      Diagnostics  : in out GPR2.Log.Object) is
      LKS_Contents    : Langkit_Support.File_Readers.Decoded_File_Contents;
   begin
      Internal_Read (Self.Handler, Filename, Charset, LKS_Contents);

      Contents.Buffer := GPR2.File_Readers.Text_Access (LKS_Contents.Buffer);
      Contents.First := LKS_Contents.First;
      Contents.Last := LKS_Contents.Last;
   end Read;

end LSP.Ada_Handlers.File_Readers;

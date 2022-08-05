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

with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Characters.Handling;
with GNAT.Strings;                 use GNAT.Strings;
with GNATCOLL.Traces;              use GNATCOLL.Traces;

with GNATCOLL.VFS;                 use GNATCOLL.VFS;
with GNATCOLL.Iconv;               use GNATCOLL.Iconv;
with VSS.Strings;                  use VSS.Strings;
with VSS.Strings.Conversions;
with LSP.Ada_Documents;            use LSP.Ada_Documents;
with Libadalang.Preprocessing;     use Libadalang.Preprocessing;
with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;

package body LSP.Ada_Handlers.File_Readers is

   Me : constant Trace_Handle := Create ("ALS.FILE_READERS");

   function Read_And_Convert_To_UTF8
     (Filename : String; Charset : String)
      return GNAT.Strings.String_Access;
   --  Read the file content from Filename and convert it from the original
   --  Charset to UTF-8.

   ------------------------------
   -- Read_And_Convert_To_UTF8 --
   ------------------------------

   function Read_And_Convert_To_UTF8
     (Filename : String; Charset : String)
      return GNAT.Strings.String_Access
   is
      Raw        : GNAT.Strings.String_Access;
      Decoded    : GNAT.Strings.String_Access;
   begin
      --  Read the file (this call uses MMAP)
      Raw := Create_From_UTF8 (Filename).Read_File;

      if Raw = null then
         return null;
      end if;

      --  Convert the file if it's not already encoded in utf-8

      if Ada.Characters.Handling.To_Lower (Charset) = "utf-8" then
         Decoded := Raw;
      else
         declare
            State        : constant Iconv_T := Iconv_Open (UTF8, Charset);
            Outbuf       : Byte_Sequence (1 .. 4096);
            Input_Index  : Positive := Raw'First;
            Conv_Result  : Iconv_Result := Full_Buffer;
            Output_Index : Positive;
         begin
            while Conv_Result = Full_Buffer loop
               Output_Index := 1;
               Iconv (State        => State,
                      Inbuf        => Raw.all,
                      Input_Index  => Input_Index,
                      Outbuf       => Outbuf,
                      Output_Index => Output_Index,
                      Result       => Conv_Result);

               --  Append the converted contents
               if Decoded /= null then
                  declare
                     Tmp : GNAT.Strings.String_Access := Decoded;
                  begin
                     Decoded := new String'
                       (Tmp.all & Outbuf (1 .. Output_Index - 1));
                     GNAT.Strings.Free (Tmp);
                  end;
               else
                  Decoded := new String'(Outbuf (1 .. Output_Index - 1));
               end if;
            end loop;

            GNAT.Strings.Free (Raw);
            Iconv_Close (State);

            case Conv_Result is
               when Success =>
                  --  The conversion was successful
                  null;
               when others =>
                  Me.Trace
                    ("Failed to convert '" & Filename & "' to UTF-8: "
                    & Conv_Result'Img);
                  return null;
            end case;
         exception
            when E : others =>

               Me.Trace
                 ("Exception caught when reading '" & Filename & "':"
                  & Exception_Message (E));
               return null;
         end;
      end if;

      --  Convert the string to a Virtual_String for easier handling

      return Decoded;
   exception
      when E : others =>
         if Decoded /= null then
            GNAT.Strings.Free (Decoded);
         end if;

         Me.Trace
           ("Exception caught when reading '" & Filename & "':"
            & Exception_Message (E));

         return null;
   end Read_And_Convert_To_UTF8;

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
      Doc    : Document_Access;
      Source : Preprocessed_Source := Preprocessed_Source'
        (Buffer => null, Last => 0);
      Buffer : GNAT.Strings.String_Access;
   begin
      --  First check if the file is an open document
      Doc := Self.Handler.Get_Open_Document
        (URI   => LSP.Types.File_To_URI (Filename),
         Force => False);

      --  Preprocess the document's contents if open, or the file contents if
      --  not.
      if Doc /= null then
         Buffer := new String'
           (VSS.Strings.Conversions.To_UTF_8_String (Doc.Text));
      else
         Buffer := Read_And_Convert_To_UTF8 (Filename, Charset);

         --  Return an empty sring when failing to read the file (i.e: when the
         --  file has been deleted).
         if Buffer = null then
            Buffer := new String'("");
         end if;
      end if;

      --  If we have preprocessing data, use LAL's API to preoprocess the file.
      --  Otherwise, just decode the contents of the document/file.

      if Self.Preprocessing_Data /= No_Preprocessor_Data then
         Libadalang.Preprocessing.Preprocess
           (Data        => Self.Preprocessing_Data,
            Filename    => Filename,
            Input       => Buffer.all,
            Contents    => Source,
            Diagnostics => Diagnostics);

         if Source.Buffer = null then
            --  Log the diagnostics when processing has failed
            for Diag of Diagnostics loop
               Me.Trace (Langkit_Support.Diagnostics.To_Pretty_String (Diag));
            end loop;
         end if;
      end if;

      --  Decode the preprocessed buffer (or the initial contents when there is
      --  no preprocessing needed) in utf-8.

      Decode_Buffer
        (Buffer      => (if Source.Buffer /= null then
                            Source.Buffer (1 .. Source.Last)
                         else
                            Buffer.all),
         Charset     => "utf-8",
         Read_BOM    => Read_BOM,
         Contents    => Contents,
         Diagnostics => Diagnostics);

      Free (Source);
      GNAT.Strings.Free (Buffer);
   end Read;

end LSP.Ada_Handlers.File_Readers;

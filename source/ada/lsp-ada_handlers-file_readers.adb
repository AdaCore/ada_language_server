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

with Ada.Streams;
with GNAT.Strings;                 use GNAT.Strings;

with GNATCOLL.Traces;              use GNATCOLL.Traces;
with GNATCOLL.VFS;                 use GNATCOLL.VFS;

with VSS.Strings;                  use VSS.Strings;
pragma Warnings
  (Off, "unit ""VSS.Strings.Character_Iterators"" is not referenced");
--  GNAT 20220919 report this package as unused, however it is necessary to
--  make visible full declaration of Character_Iterator.
with VSS.Strings.Character_Iterators;
with VSS.Strings.Converters.Decoders;
with VSS.Strings.Conversions;

with LSP.Ada_Documents;            use LSP.Ada_Documents;
with Libadalang.Preprocessing;     use Libadalang.Preprocessing;
with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;
with Langkit_Support.Slocs;
with Langkit_Support.Text;

with URIs;

package body LSP.Ada_Handlers.File_Readers is

   use all type VSS.Strings.Converters.Converter_Flag;

   Me : constant Trace_Handle := Create ("ALS.FILE_READERS");

   procedure Read_And_Decode
     (Filename : String;
      Charset  : VSS.Strings.Virtual_String;
      Decoded  : out VSS.Strings.Virtual_String;
      Error    : out VSS.Strings.Virtual_String);
   --  Read the file content from Filename and decode it from the original
   --  Charset.

   Decoder_Flags : constant VSS.Strings.Converters.Converter_Flags :=
     (Stateless     => True,
      --  Data is decoded as single chunk, don't save state but report error
      --  for incomplete byte sequences at the end of data
      Stop_On_Error => False,
      --  Errors should be reported but not to stop decoding of the following
      --  data
      Process_BOM   => True);
      --  Byte-Order-Mark at the beginning of the data should be ignored if
      --  present
   --  Default flags for the text decoder.

   ---------------------
   -- Read_And_Decode --
   ---------------------

   procedure Read_And_Decode
     (Filename : String;
      Charset  : VSS.Strings.Virtual_String;
      Decoded  : out VSS.Strings.Virtual_String;
      Error    : out VSS.Strings.Virtual_String)
   is
      Raw     : GNAT.Strings.String_Access;
      Decoder : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;

   begin
      --  Read the file (this call uses MMAP)

      Raw := Create_From_UTF8 (Filename).Read_File;

      if Raw = null then
         Decoded.Clear;
         Error := "Unable to read file";

         return;
      end if;

      Decoder.Initialize (Charset, Decoder_Flags);

      if not Decoder.Is_Valid then
         --  Charset is not supported, fallback to "utf-8".

         Me.Trace
           ("Encoding '"
            & VSS.Strings.Conversions.To_UTF_8_String (Charset)
            & "' is not supported by text decoder.");

         Decoder.Initialize ("utf-8", Decoder_Flags);
      end if;

      pragma Assert (Decoder.Is_Valid);
      --  At this point decoder is initialized to decode ether given encoding
      --  or fallback encoding "utf-8", which is known to be supported.

      declare
         Encoded : constant Ada.Streams.Stream_Element_Array (1 .. Raw'Length)
           with Import, Address => Raw.all'Address;

      begin
         Decoded := Decoder.Decode (Encoded);
         Error   := Decoder.Error_Message;
      end;

      GNAT.Strings.Free (Raw);
   end Read_And_Decode;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self        : LSP_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Langkit_Support.File_Readers.Decoded_File_Contents;
      Diagnostics : in out
        Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector)
   is
      URI : constant URIs.URI_String := URIs.Conversions.From_File (Filename);

      Doc   : Document_Access;
      Text  : VSS.Strings.Virtual_String;
      Error : VSS.Strings.Virtual_String;

   begin
      --  First check if the file is an open document

      Doc := Self.Handler.Get_Open_Document
        (URI   => (VSS.Strings.Conversions.To_Virtual_String (URI)
                     with null record),
         Force => False);

      --  Preprocess the document's contents if open, or the file contents if
      --  not.

      if Doc /= null then
         Text := Doc.Text;

      else
         Read_And_Decode
           (Filename => Filename,
            Charset  => VSS.Strings.Conversions.To_Virtual_String (Charset),
            Decoded  => Text,
            Error    => Error);

         if not Error.Is_Empty then
            Diagnostics.Append
              (Langkit_Support.Diagnostics.Diagnostic'
                 (Langkit_Support.Slocs.No_Source_Location_Range,
                  VSS.Strings.Conversions.To_Unbounded_Wide_Wide_String
                    (Error)));
         end if;
      end if;

      --  If we have preprocessing data, use LAL's API to preoprocess the file.
      --  Otherwise, just decode the contents of the document/file.

      if Self.Preprocessing_Data /= No_Preprocessor_Data then
         declare
            Buffer : GNAT.Strings.String_Access :=
              new String
                (1 .. Integer (Text.After_Last_Character.First_UTF8_Offset));
            --  Size of the "utf-8" encoded data for text is known, so
            --  allocate necessary space and fill it later. Allocation on the
            --  stack can't be use here due to potential stack overflow.
            Source : Preprocessed_Source := Preprocessed_Source'
              (Buffer => null, Last => 0);

         begin
            VSS.Strings.Conversions.Set_UTF_8_String (Text, Buffer.all);

            Libadalang.Preprocessing.Preprocess
              (Data        => Self.Preprocessing_Data,
               Filename    => Filename,
               Input       => Buffer.all,
               Contents    => Source,
               Diagnostics => Diagnostics);

            if Source.Buffer = null then
               --  Log the diagnostics when processing has failed

               for Diag of Diagnostics loop
                  Me.Trace
                    (Langkit_Support.Diagnostics.To_Pretty_String (Diag));
               end loop;
            end if;

            --  Decode the preprocessed buffer (or the initial contents when
            --  there is no preprocessing needed) in utf-8.

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
         end;

      else
         Contents :=
           (Buffer =>
               new Langkit_Support.Text.Text_Type
                     (1 .. Natural (Text.Character_Length)),
            First  => 1,
            Last   => Natural (Text.Character_Length));

         VSS.Strings.Conversions.Set_Wide_Wide_String
           (Text, Contents.Buffer.all);
      end if;
   end Read;

end LSP.Ada_Handlers.File_Readers;

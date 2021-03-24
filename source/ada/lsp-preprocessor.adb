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

with Ada.Characters.Handling;
with Ada.Characters.Wide_Wide_Latin_1;

with GNAT.Strings;

with GNATCOLL.VFS;   use GNATCOLL.VFS;
with GNATCOLL.Iconv; use GNATCOLL.Iconv;

with VSS.Characters;
with VSS.Strings.Conversions;
with VSS.String_Vectors;
with VSS.Strings.Cursors.Iterators.Characters;

package body LSP.Preprocessor is

   package LAL renames Libadalang.Analysis;

   function Preprocess_Buffer
     (Buffer : Virtual_String) return Unbounded_String
   is
      Vect       : VSS.String_Vectors.Virtual_String_Vector;
      Result     : Unbounded_String := Null_Unbounded_String;

      LT : GNAT.Strings.String_Access;
      --  The line terminator found in the non-processed buffer

      procedure Process_One_Line (Line : Virtual_String);
      --  Process one line of decoded output. This is the function that handles
      --  preprocessing support.

      --  Preprocessor hack: at the moment we consider that all the 'if'
      --  branches of preprocessing are active but remove all 'else' branches.
      --  We do this so that code of the form
      --
      --  #if something then
      --     procedure foo (
      --  #else
      --     procedure foo (
      --  #end if
      --
      --  ... still processable, even in degraded mode, by the language
      --  server.

      Currently_Preprocessing : Boolean := False;
      This_branch_Evaluates_To_True : Boolean := False;

      function Eval (Line : Virtual_String) return Boolean is
         (Line.Starts_With (To_Virtual_String ("#if")));
      --  Placeholder. This is where to insert "real" preprocessor logic.
      --  For now the first branch after #if is considered true, the #else
      --  branches are dropped - see above.

      procedure Process_One_Line (Line : Virtual_String) is
         Send_This_Line_To_Libadalang : Boolean := False;
         --  Whether to add the line to the buffer passed to Libadalang
      begin
         if Line.Starts_With (To_Virtual_String ("#if")) then
            Currently_Preprocessing := True;
            This_branch_Evaluates_To_True := Eval (Line);
         elsif Line.Starts_With (To_Virtual_String ("#el")) then
            This_branch_Evaluates_To_True := Eval (Line);
         elsif Line.Starts_With (To_Virtual_String (("#end"))) then
            Currently_Preprocessing := False;
         else
            Send_This_Line_To_Libadalang := (not Currently_Preprocessing)
              or else This_branch_Evaluates_To_True;
         end if;

         if Send_This_Line_To_Libadalang then
            Append (Result,
                    VSS.Strings.Conversions.To_UTF_8_String (Line) & LT.all);
         else
            --  If we're not sending the line to Libadalang, send an empty
            --  line to preserve line numbers.
            Append (Result, "" & LT.all);
         end if;
      end Process_One_Line;

   begin
      --  Easy handle of the empty string
      if Buffer.Is_Empty then
         return Result;
      end if;

      --  Figure out which is the line terminator in the original buffer
      declare
         use VSS.Strings.Cursors.Iterators.Characters;
         use VSS.Characters;
         Found_CR : Boolean := False;
         Found_LF : Boolean := False;
         It       : Character_Iterator := Buffer.First_Character;
      begin
         while It.Has_Element loop
            if It.Element =
              Virtual_Character (Ada.Characters.Wide_Wide_Latin_1.LF)
            then
               Found_LF := True;
            elsif It.Element =
              Virtual_Character (Ada.Characters.Wide_Wide_Latin_1.CR)
            then
               Found_CR := True;
            else
               if Found_CR or else Found_LF then
                  --  We have found a non-terminator character after
                  --  having found a terminator one: we can stop
                  --  iterating.
                  exit;
               end if;
            end if;

            exit when not It.Forward;
         end loop;

         if Found_LF then
            if Found_CR then
               LT := new String'(ASCII.CR & ASCII.LF);
            else
               LT := new String'((1 => ASCII.LF));
            end if;
         elsif Found_CR then
            LT := new String'((1 => ASCII.CR));
         else
            --  It can happen that we never found a line terminator
            --  (empty files or one-liners): default to LF
            LT := new String'((1 => ASCII.LF));
         end if;
      end;

      Vect := Buffer.Split_Lines;

      for Line of Vect loop
         Process_One_Line (Line);
      end loop;

      GNAT.Strings.Free (LT);

      return Result;
   end Preprocess_Buffer;

   ------------------
   -- Process_File --
   ------------------

   function Preprocess_File (Filename : String; Charset : String)
                          return Unbounded_String
   is
      use type GNAT.Strings.String_Access;
      Raw        : GNAT.Strings.String_Access;
      Decoded    : Virtual_String;
   begin
      --  Read the file (this call uses MMAP)
      Raw := Create_From_UTF8 (Filename).Read_File;

      if Raw = null then
         return Null_Unbounded_String;
      end if;

      --  Convert the file if it's not already encoded in utf-8

      if Ada.Characters.Handling.To_Lower (Charset) = "utf-8" then
         Decoded := VSS.Strings.Conversions.To_Virtual_String (Raw.all);
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
               Iconv (State => State,
                      Inbuf => Raw.all,
                      Input_Index => Input_Index,
                      Outbuf => Outbuf,
                      Output_Index => Output_Index,
                      Result => Conv_Result);
               Decoded.Append (VSS.Strings.Conversions.To_Virtual_String
                               (Outbuf (1 .. Output_Index - 1)));
            end loop;

            Iconv_Close (State);

            case Conv_Result is
               when Success =>
                  --  The conversion was successful
                  null;
               when others =>
                  --  TODO: transmit the result to the user
                  return Null_Unbounded_String;
            end case;
         exception
            when others =>
               --  TODO: transmit the result to the user
               return Null_Unbounded_String;
         end;
      end if;

      --  Convert the string to a Virtual_String for easier handling

      GNAT.Strings.Free (Raw);

      return Preprocess_Buffer (Decoded);
   exception
      when others =>
         if Raw /= null then
            GNAT.Strings.Free (Raw);
         end if;

         --  TODO: transmit this to the user
         return Null_Unbounded_String;
   end Preprocess_File;

   -------------------
   -- Get_From_File --
   -------------------

   function Get_From_File
     (Context  : Libadalang.Analysis.Analysis_Context;
      Filename : String;
      Charset  : String;
      Reparse  : Boolean := False;
      Rule     : Grammar_Rule := Default_Grammar_Rule)
      return Libadalang.Analysis.Analysis_Unit is
      Buffer   : Unbounded_String;
   begin
      --  Preprocessing guarantees that the buffers are known to LAL in
      --  UTF-8: we can use this safely rather than Charset below.
      if not Reparse
        and then Context.Has_Unit (Filename)
      then
         return LAL.Get_With_Error
           (Context, Filename, "", "utf-8");
      end if;
      Buffer := Preprocess_File (Filename, Charset);

      return LAL.Get_From_Buffer
        (Context, Filename, "utf-8", Buffer, Rule);
   end Get_From_File;

end LSP.Preprocessor;

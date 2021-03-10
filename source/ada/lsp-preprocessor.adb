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

with GNAT.Strings;

with GNATCOLL.VFS;   use GNATCOLL.VFS;
with GNATCOLL.Iconv; use GNATCOLL.Iconv;

with VSS.Strings.Conversions;
with VSS.String_Vectors;

package body LSP.Preprocessor is

   package LAL renames Libadalang.Analysis;

   function Preprocess_Buffer
     (Buffer : Virtual_String) return Unbounded_String
   is
      Vect       : VSS.String_Vectors.Virtual_String_Vector;
      Result     : Unbounded_String := Null_Unbounded_String;

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
      begin
         if Line.Starts_With (To_Virtual_String ("#if")) then
            Currently_Preprocessing := True;
            This_branch_Evaluates_To_True := Eval (Line);
         elsif Line.Starts_With (To_Virtual_String ("#el")) then
            This_branch_Evaluates_To_True := Eval (Line);
         elsif Line.Starts_With (To_Virtual_String (("#end"))) then
            Currently_Preprocessing := False;
         end if;

         if (not Currently_Preprocessing)
           or else This_branch_Evaluates_To_True
         then
            Append
              (Result,
               VSS.Strings.Conversions.To_UTF_8_String (Line) & ASCII.LF);
         else
            Append (Result, "" & ASCII.LF);
         end if;
      end Process_One_Line;

   begin
      Vect := Buffer.Split_Lines;

      for Line of Vect loop
         Process_One_Line (Line);
      end loop;

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
      Raw := Create (+Filename).Read_File;

      if Raw = null then
         return Null_Unbounded_String;
      end if;

      --  Convert the file if it's not already encoded in utf-8

      if Ada.Characters.Handling.To_Lower (Charset) /= "utf-8" then
         declare
            State        : constant Iconv_T := Iconv_Open (UTF8, Charset);
            Outbuf       : Byte_Sequence (1 .. Raw'Length * 4);
            Input_Index  : Positive := Raw'First;
            Conv_Result  : Iconv_Result;
            Output_Index : Positive := 1;
         begin
            Iconv (State => State,
                   Inbuf => Raw.all,
                   Input_Index => Input_Index,
                   Outbuf => Outbuf,
                   Output_Index => Output_Index,
                   Result => Conv_Result);

            GNAT.Strings.Free (Raw);

            case Conv_Result is
               when Success =>
                  --  The conversion was successful
                  Raw := new String'(Outbuf (1 .. Output_Index - 1));
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

      Decoded := VSS.Strings.Conversions.To_Virtual_String (Raw.all);
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
      if not Reparse
        and then Context.Has_Unit (Filename)
      then
         return LAL.Get_With_Error
           (Context, Filename, "", Charset);
      end if;
      Buffer := Preprocess_File (Filename, Charset);

      return LAL.Get_From_Buffer
        (Context, Filename, Charset, Buffer, Rule);
   end Get_From_File;

end LSP.Preprocessor;

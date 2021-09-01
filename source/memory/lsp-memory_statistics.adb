------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2020, AdaCore                       --
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

with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with GNAT.Regpat;             use GNAT.Regpat;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with System.Address_Image;
with System.Storage_Elements; use System.Storage_Elements;

package body LSP.Memory_Statistics is

   ----------------------------
   -- Dump_Memory_Statistics --
   ----------------------------

   function Dump_Memory_Statistics
     (Size   : Positive;
      Report : Report_Type := Memory_Usage)
      return String
   is
      Buffer           : Unbounded_String := To_Unbounded_String
        ("Dump_Memory_Statistics at 0x" &
           System.Address_Image (Dump_Memory_Statistics'Address) &
           Ada.Characters.Latin_1.LF);

      Traceback_Regexp : constant Pattern_Matcher :=
                           Compile ("\s0x0+([0-9a-zA-Z]+)");

      procedure Trace_Put (S : String);

      procedure Trace_Put_Line (S : String);

      ---------------
      -- Trace_Put --
      ---------------

      procedure Trace_Put (S : String) is
         Matched : Match_Array (0 .. 1);
      begin
         Match (Traceback_Regexp, S, Matched);

         --  If we are dealing with traceback addresses, resolve it to the
         --  actual source location using GNAT.Traceback.Symbolic.
         --  This is needed since these addresses can point to relocatable
         --  libraries, in which case addr2line won't be able to find the
         --  corresponding source locations.

         if Matched (0) = No_Match then
            Append (Buffer, S);
         else
            declare
               Traceback_Str : constant String :=
                                 S (Matched (1).First .. Matched (1).Last);
               Traceback_Long : constant Long_Integer :=
                                  Long_Integer'Value
                                    ("16#" & Traceback_Str & "#");
               Traceback_Addr : constant System.Address :=
                                  To_Address
                                    (Integer_Address (Traceback_Long));
               New_S          : constant String :=
                                  Symbolic_Traceback_No_Hex
                                    ((1 => Traceback_Addr));
            begin
               Append (Buffer, New_S);
            end;
         end if;
      end Trace_Put;

      --------------------
      -- Trace_Put_Line --
      --------------------

      procedure Trace_Put_Line (S : String) is
      begin
         Append (Buffer, S & Ada.Characters.Latin_1.LF);
      end Trace_Put_Line;

      procedure Internal is new GNATCOLL.Memory.Redirectable_Dump
        (Put_Line => Trace_Put_Line,
         Put      => Trace_Put);

   begin
      Internal (Size, Report);
      return To_String (Buffer);
   end Dump_Memory_Statistics;

end LSP.Memory_Statistics;

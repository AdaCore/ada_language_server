------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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

with VSS.Strings.Conversions;

package body LSP.GNATCOLL_Trace_Streams is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : in out Output_Text_Stream'Class;
      Trace : GNATCOLL.Traces.Trace_Handle) is
   begin
      Self.Trace := Trace;
   end Initialize;

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line
     (Self    : in out Output_Text_Stream;
      Success : in out Boolean) is
   begin
      if Success then
         Self.Trace.Trace
           (VSS.Strings.Conversions.To_UTF_8_String (Self.Incomplete));
         Self.Incomplete.Clear;
      end if;
   end New_Line;

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (Self    : in out Output_Text_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean) is
   begin
      if Success then
         Self.Incomplete.Append (Item);
      end if;
   end Put;

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (Self    : in out Output_Text_Stream;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean) is
   begin
      if Success then
         Self.Incomplete.Append (Item);
      end if;
   end Put;

   --------------
   -- Put_Line --
   --------------

   overriding procedure Put_Line
     (Self    : in out Output_Text_Stream;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean) is
   begin
      if Success then
         Self.Incomplete.Append (Item);
         Self.New_Line (Success);
      end if;
   end Put_Line;

end LSP.GNATCOLL_Trace_Streams;

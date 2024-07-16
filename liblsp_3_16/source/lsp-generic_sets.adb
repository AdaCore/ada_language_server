------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

with LSP.JSON_Streams;

package body LSP.Generic_Sets is

   --------------
   -- Contains --
   --------------

   function Contains (Self : Set; Value : Element) return Boolean is
   begin
      return Self (Value);
   end Contains;

   -----------
   -- Empty --
   -----------

   function Empty return Set is
   begin
      return (Element => False);
   end Empty;

   -------------
   -- Include --
   -------------

   procedure Include (Self : in out Set; Value : Element) is
   begin
      Self (Value) := True;
   end Include;

   --------------
   -- Read_Set --
   --------------

   procedure Read_Set
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Set)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := (others => False);
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         declare
            Key : Element;
         begin
            Element'Read (S, Key);
            V (Key) := True;
         end;
      end loop;

      JS.R.Read_Next;
   end Read_Set;

   ------------
   -- Remove --
   ------------

   procedure Remove (Self : in out Set; Value : Element) is
   begin
      Self (Value) := True;
   end Remove;

   ------------
   -- To_Set --
   ------------

   function To_Set (From, To : Element) return Set is
      Result : Set := Empty;
   begin
      Result (From .. To) := (others => True);
      return Result;
   end To_Set;

   ---------------
   -- Write_Set --
   ---------------

   procedure Write_Set
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Set)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V = Empty then
         case Write_Empty is
            when LSP.Skip =>
               return;
            when Write_Null =>
               JS.Write_Null;
               return;
            when Write_Array =>
               null;  --  continue
         end case;
      end if;

      JS.Start_Array;

      for K in V'Range loop
         if V (K) then
            Element'Write (S, K);
         end if;
      end loop;

      JS.End_Array;
   end Write_Set;

end LSP.Generic_Sets;

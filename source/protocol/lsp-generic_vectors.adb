------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with GNATCOLL.JSON;

with LSP.JSON_Streams;

package body LSP.Generic_Vectors is

   -----------------
   -- Read_Vector --
   -----------------

   procedure Read_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V.Clear;
      JS.Start_Array;

      while not JS.End_Of_Array loop
         declare
            Item : Element;
         begin
            Element'Read (S, Item);
            V.Append (Item);
         end;
      end loop;

      JS.End_Array;
   end Read_Vector;

   ------------------
   -- Write_Vector --
   ------------------

   procedure Write_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V.Is_Empty then
         JS.Write (GNATCOLL.JSON.Create (GNATCOLL.JSON.Empty_Array));
      else
         JS.Start_Array;

         for Item of V loop
            Element'Write (S, Item);
         end loop;

         JS.End_Array;
      end if;
   end Write_Vector;

end LSP.Generic_Vectors;

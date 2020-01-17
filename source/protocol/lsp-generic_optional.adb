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

package body LSP.Generic_Optional is

   ----------
   -- Read --
   ----------

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Optional_Type)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Value : constant GNATCOLL.JSON.JSON_Value := JS.Read;
   begin
      if Value.Kind in GNATCOLL.JSON.JSON_Null_Type then
         V := (Is_Set => False);
      elsif Value.Kind in GNATCOLL.JSON.JSON_Boolean_Type then
         --  During protocol extension some boolean settings become objects.
         --  To keep reading of optional settings compatible with earlier
         --  implementations we read `true` (while we expect an optional
         --  object) as a default value with Is_Set => True, but if we read
         --  `false`, then we return an empty value.
         if Value.Get then
            V := (Is_Set => True, Value => <>);
         else
            V := (Is_Set => False);
         end if;
      else
         V := (Is_Set => True, Value => <>);
         Element_Type'Read (S, V.Value);
      end if;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Optional_Type) is
   begin
      if V.Is_Set then
         Element_Type'Write (S, V.Value);
      end if;
   end Write;

end LSP.Generic_Optional;

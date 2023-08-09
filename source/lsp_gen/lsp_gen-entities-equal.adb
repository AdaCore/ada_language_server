------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
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

function LSP_Gen.Entities.Equal
  (Left, Right : LSP_Gen.Entities.AType) return Boolean
is
   use all type LSP_Gen.Entities.Enum.AType_Variant;
   use type VSS.Strings.Virtual_String;

   function "=" (L, R : LSP_Gen.Entities.AType_Vector) return Boolean;
   function "=" (L, R : LSP_Gen.Entities.Property_Vector) return Boolean;

   ---------
   -- "=" --
   ---------

   function "=" (L, R : LSP_Gen.Entities.AType_Vector) return Boolean is
   begin
      if L.Length /= R.Length then
         return False;
      end if;

      for J in 1 .. L.Length loop
         if not Equal (L (J), R (J)) then
            return False;
         end if;
      end loop;

      return True;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (L, R : LSP_Gen.Entities.Property_Vector) return Boolean is
   begin
      if L.Length /= R.Length then
         return False;
      end if;

      for J in 1 .. L.Length loop
         if L (J).name /= R (J).name or else
           L (J).optional /= R (J).optional or else
           not Equal (L (J).a_type, R (J).a_type)
         then
            return False;
         end if;
      end loop;

      return True;
   end "=";
begin
   if Left.Union.Kind /= Right.Union.Kind then
      return False;
   end if;

   case Left.Union.Kind is
      when base
         | reference
         | stringLiteral
         | integerLiteral
         | booleanLiteral =>

         return Left = Right;  --  predefined "="
      when an_array =>
         return Equal
           (Left.Union.an_array.element.Value,
            Right.Union.an_array.element.Value);
      when map =>
         return Left.Union.map.key = Right.Union.map.key and then
           Equal (Left.Union.map.value.Value, Right.Union.map.value.Value);
      when an_and =>
         return Left.Union.an_and.items = Right.Union.an_and.items;
      when a_or =>
         return Left.Union.a_or.items = Right.Union.a_or.items;
      when tuple =>
         return Left.Union.tuple.items = Right.Union.tuple.items;
      when literal =>
         return Left.Union.literal.value.properties =
           Right.Union.literal.value.properties;
   end case;
end LSP_Gen.Entities.Equal;

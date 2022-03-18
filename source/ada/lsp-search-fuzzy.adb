------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2021-2022, AdaCore                     --
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

with VSS.Characters;                           use VSS.Characters;
with VSS.Strings;                              use VSS.Strings;
with VSS.Strings.Cursors.Iterators.Characters;

package body LSP.Search.Fuzzy is

   -----------
   -- Build --
   -----------

   function Build
     (Pattern        : VSS.Strings.Virtual_String;
      Case_Sensitive : Boolean := False;
      Whole_Word     : Boolean := False;
      Negate         : Boolean := False)
      return Search_Pattern'Class is
   begin
      return Fuzzy_Search'
        (Ada.Finalization.Limited_Controlled with
         Text           => Pattern,
         Case_Sensitive => Case_Sensitive,
         Whole_Word     => Whole_Word,
         Negate         => Negate,
         Kind           => LSP.Messages.Fuzzy);
   end Build;

   -----------
   -- Match --
   -----------

   overriding function Match
     (Self : Fuzzy_Search;
      Text : VSS.Strings.Virtual_String)
      return Boolean
   is
      Pattern_Iterator : VSS.Strings.Cursors.Iterators.Characters.
        Character_Iterator := Self.Text.At_First_Character;

      Text_Iterator    : VSS.Strings.Cursors.Iterators.Characters.
        Character_Iterator := Text.At_First_Character;

      Pattern_Char, Text_Char : VSS.Characters.Virtual_Character;

      Dummy : Boolean;

   begin
      Pattern_Char := Pattern_Iterator.Element;
      Dummy := Pattern_Iterator.Forward;

      while Text_Iterator.Has_Element loop
         Text_Char := Text_Iterator.Element;
         Dummy := Text_Iterator.Forward;

         if Text_Char = Pattern_Char then
            if not Pattern_Iterator.Has_Element then
               return not Self.Negate;
            end if;

            Pattern_Char := Pattern_Iterator.Element;
            Dummy := Pattern_Iterator.Forward;
         end if;
      end loop;

      return Self.Negate;

   exception
      when others =>
         return False;
   end Match;

end LSP.Search.Fuzzy;

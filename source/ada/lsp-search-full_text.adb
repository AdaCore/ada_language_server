------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2021, AdaCore                          --
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
with Ada.Strings.UTF_Encoding;

with VSS.Strings;             use VSS.Strings;
with VSS.Strings.Conversions;

package body LSP.Search.Full_Text is

   -----------
   -- Build --
   -----------

   function Build
     (Pattern        : VSS.Strings.Virtual_String;
      Case_Sensitive : Boolean := False;
      Whole_Word     : Boolean := False;
      Negate         : Boolean := False)
      return Search_Pattern'Class
   is
      BM : GNATCOLL.Boyer_Moore.Pattern;
   begin
      Compile
        (BM,
         VSS.Strings.Conversions.To_UTF_8_String (Pattern),
         Case_Sensitive => Case_Sensitive);

      return Full_Text_Search'
        (Ada.Finalization.Limited_Controlled with
         Boyer          => BM,
         Text           => Pattern,
         Case_Sensitive => Case_Sensitive,
         Negate         => Negate,
         Whole_Word     => Whole_Word,
         Kind           => LSP.Messages.Full_Text);
   end Build;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Full_Text_Search) is
   begin
      GNATCOLL.Boyer_Moore.Free (Self.Boyer);
      Finalize (Search_Pattern (Self));
   end Finalize;

   -----------
   -- Match --
   -----------

   overriding function Match
     (Self : Full_Text_Search;
      Text : VSS.Strings.Virtual_String)
      return Boolean
   is
      Index : Integer;
      T     : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
        VSS.Strings.Conversions.To_UTF_8_String (Text);
      S     : Integer := T'First;

      function Is_Word_Delimiter (C : Character) return Boolean;
      function Is_Word_Delimiter (C : Character) return Boolean is
      begin
         return not (Ada.Characters.Handling.Is_Alphanumeric (C)
                     or else C = '_');
      end Is_Word_Delimiter;

   begin
      loop
         Index := GNATCOLL.Boyer_Moore.Search (Self.Boyer, T (S .. T'Last));

         exit when not Self.Whole_Word
           or else Index not in T'Range  --  Actually we check Index = -1 here
           or else
               --  Check we have word delimiters on either sides
           ((Index = T'First or else Is_Word_Delimiter (T (Index - 1)))
            and then
             (Index + T'Length > T'Last
                 or else Is_Word_Delimiter (T (Index + T'Length))));
         S := Index + 1;
      end loop;

      if Index = -1 then
         return Self.Negate;
      else
         return not Self.Negate;
      end if;
   end Match;

end LSP.Search.Full_Text;

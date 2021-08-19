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

with Ada.Strings.UTF_Encoding;    use Ada.Strings.UTF_Encoding;
with Ada.Unchecked_Deallocation;

with GNAT.Regpat;                 use GNAT.Regpat;

with VSS.Strings.Conversions;     use VSS.Strings.Conversions;

package body LSP.Search.Regexp is

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Regpat.Pattern_Matcher, GNAT.Expect.Pattern_Matcher_Access);

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
      UTF_8 : constant UTF_8_String := To_UTF_8_String (Pattern);
      Re    : GNAT.Expect.Pattern_Matcher_Access;
      Flags : constant Regexp_Flags := Multiple_Lines or Case_Insensitive;
      WD    : constant String := "\b";  --  word delimiter
   begin
      if Whole_Word then
         Re := new GNAT.Regpat.Pattern_Matcher'
           (Compile (WD & UTF_8 & WD, Flags));
      else
         Re := new GNAT.Regpat.Pattern_Matcher'(Compile (UTF_8, Flags));
      end if;

      return Regexp_Search'
        (Ada.Finalization.Limited_Controlled with
         Pattern        => Re,
         Text           => Pattern,
         Case_Sensitive => Case_Sensitive,
         Whole_Word     => Whole_Word,
         Kind           => LSP.Messages.Regexp,
         Negate         => Negate);
   end Build;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Regexp_Search) is
   begin
      Unchecked_Free (Self.Pattern);
      Finalize (Search_Pattern (Self));
   end Finalize;

   -----------
   -- Match --
   -----------

   overriding function Match
     (Self : Regexp_Search;
      Text : VSS.Strings.Virtual_String)
      return Boolean
   is
      Groups : GNAT.Regpat.Match_Array (0 .. 10);
      Name   : constant UTF_8_String := To_UTF_8_String (Text);
   begin
      Match (Self.Pattern.all, Name, Groups, Name'First, Name'Last);

      --  The second test below works around an apparent bug in GNAT.Regpat
      if Groups (0) = GNAT.Regpat.No_Match
        or else Groups (0).First > Name'Last
      then
         return Self.Negate;

      else
         return not Self.Negate;
      end if;
   end Match;

end LSP.Search.Regexp;

------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2021-2023, AdaCore                     --
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

with Ada.Finalization;

with VSS.Strings;

with LSP.Enumerations;

package LSP.Search is

   subtype Search_Kind is LSP.Enumerations.AlsSearchKind;
   --  A Full_Text match searches the pattern exactly in the contents.
   --
   --  A Start_Word_Text works like a Full_Text but tested word should mutch
   --  patters from the first letter
   --
   --  A regexp parses the pattern as a regular expression.
   --
   --  A fuzzy match will search for some contents that contains all the
   --  characters of the pattern, in the same order, but possibly with
   --  other characters in-between. The number of characters in-between is not
   --  limited, so this mode really only makes sense when matching short text
   --  (and not, for instance, in text editors).
   --
   --  Approximate allows one or two errors to appear in the match (character
   --  insertion, deletion or substitution). This is mostly suitable when
   --  matching in long texts. The implementation of this algorithm is
   --  optimized so that characters are matched only once, but the total length
   --  of the pattern is limited to 64 characters. The exact number of errors
   --  depends on the length of the pattern:
   --      patterns of length <= 4  => no error allowed
   --      patterns of length <= 10 => one error allowed
   --      long patterns            => up to two errors

   --------------------
   -- Search_Pattern --
   --------------------

   type Search_Pattern is abstract new
     Ada.Finalization.Limited_Controlled with private;
   --  A type used to describe the search context.
   --  It can also be used to do the actual matching using the appropriate
   --  algorithm, depending on the search kind.

   function Build
     (Pattern        : VSS.Strings.Virtual_String;
      Case_Sensitive : Boolean := False;
      Whole_Word     : Boolean := False;
      Negate         : Boolean := False;
      Kind           : Search_Kind := LSP.Enumerations.Full_Text)
      return Search_Pattern'Class;
   --  Create a new search matcher.
   --  It can be shared among multiple search providers, since it does not
   --  embed any context.
   --  If Case_Sensitive is True, the casing should have to match exactly.
   --  When False, the search will be done in a case-insensituve way but some
   --  malus will be applied for the fuzzy mode when the casing does not match
   --  exactly.
   --  If the kind is a regular expression, but the pattern is invalid, this
   --  function falls back to a Full_Text search.
   --  If Negate is true, the search_pattern will always return No_Match if
   --  the pattern in fact matches the text (and a match at position 1 if the
   --  pattern in fact does not match).

   function Match
     (Self : Search_Pattern;
      Text : VSS.Strings.Virtual_String)
      return Boolean is abstract;
   --  Returns True if the text is matched the search pattern. Text should
   --  be converted to lower case before call if Pattern is case insensitive.

   function Get_Kind (Self : Search_Pattern) return Search_Kind;

   function Get_Negate (Self : Search_Pattern) return Boolean;

   function Get_Canonical_Pattern
     (Self : Search_Pattern) return VSS.Strings.Virtual_String;
   --  Returns the pattern text in lowercase.

   function Get_Case_Sensitive (Self : Search_Pattern) return Boolean;

   function Get_Whole_Word (Self : Search_Pattern) return Boolean;

private

   type Search_Pattern is abstract new
     Ada.Finalization.Limited_Controlled with record
      Text           : VSS.Strings.Virtual_String;
      Case_Sensitive : Boolean     := False;
      Whole_Word     : Boolean     := False;
      Kind           : Search_Kind := LSP.Enumerations.Full_Text;
      Negate         : Boolean     := False;
   end record;

end LSP.Search;

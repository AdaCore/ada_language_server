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

with Ada.Finalization;

with VSS.Strings;  use VSS.Strings;
with LSP.Messages; use LSP.Messages;

package LSP.Search is

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
      Kind           : Search_Kind := Full_Text)
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
      Text           : VSS.Strings.Virtual_String := Empty_Virtual_String;
      Case_Sensitive : Boolean     := False;
      Whole_Word     : Boolean     := False;
      Kind           : Search_Kind := Full_Text;
      Negate         : Boolean     := False;
   end record;

end LSP.Search;

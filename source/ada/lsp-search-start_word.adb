------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2021-2024, AdaCore                     --
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

with VSS.Transformers.Caseless; use VSS.Transformers.Caseless;

package body LSP.Search.Start_Word is

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

      return Start_Word_Search'
        (Ada.Finalization.Limited_Controlled with
         Text           => Pattern,
         Case_Sensitive => Case_Sensitive,
         Negate         => Negate,
         Whole_Word     => Whole_Word,
         Kind           => LSP.Enumerations.Start_Word_Text);
   end Build;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Start_Word_Search) is
   begin
      Finalize (Search_Pattern (Self));
   end Finalize;

   -----------
   -- Match --
   -----------

   overriding function Match
     (Self : Start_Word_Search;
      Text : VSS.Strings.Virtual_String)
      return Boolean is
   begin
      case Self.Case_Sensitive is
         when False =>
            return Text.Starts_With (Self.Text, To_Identifier_Caseless)
              xor Self.Negate;
         when True =>
            return Text.Starts_With (Self.Text) xor Self.Negate;
      end case;
   end Match;

end LSP.Search.Start_Word;

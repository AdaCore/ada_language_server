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

with GNAT.Regpat;

with LSP.Search.Approximate;
with LSP.Search.Empty;
with LSP.Search.Full_Text;
with LSP.Search.Fuzzy;
with LSP.Search.Regexp;
with LSP.Search.Start_Word;
with LSP.Utils;

package body LSP.Search is

   -----------
   -- Build --
   -----------

   function Build
     (Pattern        : VSS.Strings.Virtual_String;
      Case_Sensitive : Boolean := False;
      Whole_Word     : Boolean := False;
      Negate         : Boolean := False;
      Kind           : Search_Kind := LSP.Enumerations.Full_Text)
      return Search_Pattern'Class
   is
      function Fixed_Case_Pattern return VSS.Strings.Virtual_String
        is (if Case_Sensitive then Pattern
            else LSP.Utils.Canonicalize (Pattern));

   begin
      if Pattern.Is_Empty then
         return LSP.Search.Empty.Build;
      end if;

      case Kind is
         when LSP.Enumerations.Full_Text =>
            return LSP.Search.Full_Text.Build
              (Fixed_Case_Pattern, Case_Sensitive, Whole_Word, Negate);

         when LSP.Enumerations.Start_Word_Text =>
            return LSP.Search.Start_Word.Build
              (Fixed_Case_Pattern, Case_Sensitive, Whole_Word, Negate);

         when LSP.Enumerations.Fuzzy =>
            return LSP.Search.Fuzzy.Build
              (Fixed_Case_Pattern, Case_Sensitive, Whole_Word, Negate);

         when LSP.Enumerations.Approximate =>
            if Pattern.Character_Length not in 4 .. 63 then
               --  Fallback to Full_Text, pattern is too long or too short
               return LSP.Search.Full_Text.Build
                 (Fixed_Case_Pattern, Case_Sensitive, Whole_Word, Negate);

            else
               return LSP.Search.Approximate.Build
                 (Fixed_Case_Pattern, Case_Sensitive, Whole_Word, Negate);
            end if;

         when LSP.Enumerations.Regexp =>
            begin
               return LSP.Search.Regexp.Build
                 (Fixed_Case_Pattern, Case_Sensitive, Whole_Word, Negate);
            exception
               when GNAT.Regpat.Expression_Error =>
                  return LSP.Search.Full_Text.Build
                    (Fixed_Case_Pattern, Case_Sensitive, Whole_Word, Negate);
            end;

      end case;
   end Build;

   --------------
   -- Get_Kind --
   --------------

   function Get_Kind (Self : Search_Pattern) return Search_Kind is
   begin
      return Self.Kind;
   end Get_Kind;

   ----------------
   -- Get_Negate --
   ----------------

   function Get_Negate (Self : Search_Pattern) return Boolean is
   begin
      return Self.Negate;
   end Get_Negate;

   ---------------------------
   -- Get_Canonical_Pattern --
   ---------------------------

   function Get_Canonical_Pattern
     (Self : Search_Pattern) return VSS.Strings.Virtual_String is
   begin
      if Self.Case_Sensitive then
         return Self.Text;
      else
         return LSP.Utils.Canonicalize (Self.Text);
      end if;
   end Get_Canonical_Pattern;

   ------------------------
   -- Get_Case_Sensitive --
   ------------------------

   function Get_Case_Sensitive (Self : Search_Pattern) return Boolean is
   begin
      return Self.Case_Sensitive;
   end Get_Case_Sensitive;

   --------------------
   -- Get_Whole_Word --
   --------------------

   function Get_Whole_Word (Self : Search_Pattern) return Boolean is
   begin
      return Self.Whole_Word;
   end Get_Whole_Word;

end LSP.Search;

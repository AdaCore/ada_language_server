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

with LSP.Lal_Utils;              use LSP.Lal_Utils;

with LSP.Search.Full_Text;       use LSP.Search.Full_Text;
with LSP.Search.Fuzzy;           use LSP.Search.Fuzzy;
with LSP.Search.Approximate;     use LSP.Search.Approximate;
with LSP.Search.Regexp;          use LSP.Search.Regexp;
with LSP.Search.Empty;           use LSP.Search.Empty;
with LSP.Search.Start_Word_Text; use LSP.Search.Start_Word_Text;

package body LSP.Search is

   -----------
   -- Build --
   -----------

   function Build
     (Pattern        : VSS.Strings.Virtual_String;
      Case_Sensitive : Boolean := False;
      Whole_Word     : Boolean := False;
      Negate         : Boolean := False;
      Kind           : Search_Kind := LSP.Messages.Full_Text)
      return Search_Pattern'Class
   is
      P : constant VSS.Strings.Virtual_String :=
        (if Case_Sensitive
         then Pattern
         else Canonicalize (Pattern));

   begin
      if P.Is_Empty then
         return Empty.Build;
      end if;

      case Kind is
         when LSP.Messages.Full_Text =>
            return Full_Text.Build (P, Case_Sensitive, Whole_Word, Negate);

         when LSP.Messages.Start_Word_Text =>
            return Start_Word_Text.Build
              (P, Case_Sensitive, Whole_Word, Negate);

         when LSP.Messages.Fuzzy =>
            return Fuzzy.Build (P, Case_Sensitive, Whole_Word, Negate);

         when LSP.Messages.Approximate =>
            if P.Character_Length <= 4
              or else P.Character_Length > 64
            then
               --  Fallback to Full_Text, pattern is too long or too short
               return Full_Text.Build (P, Case_Sensitive, Whole_Word, Negate);

            else
               return Approximate.Build
                 (P, Case_Sensitive, Whole_Word, Negate);
            end if;

         when LSP.Messages.Regexp =>
            begin
               return Regexp.Build (P, Case_Sensitive, Whole_Word, Negate);

            exception
               when GNAT.Regpat.Expression_Error =>
                  return Full_Text.Build
                    (P, Case_Sensitive, Whole_Word, Negate);
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
         return Canonicalize (Self.Text);
      else
         return Self.Text;
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

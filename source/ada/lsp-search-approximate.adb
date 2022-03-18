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

with Ada.Unchecked_Deallocation;

with VSS.Strings;   use VSS.Strings;

with VSS.Strings.Cursors.Iterators.Characters;

package body LSP.Search.Approximate is

   function Build
     (Pattern        : VSS.Strings.Virtual_String;
      Case_Sensitive : Boolean := False;
      Whole_Word     : Boolean := False;
      Negate         : Boolean := False)
      return Search_Pattern'Class
   is
      --  The maximum number of errors depends on the length of the
      --  patterns.
      --  For a very short pattern, allow no error, otherwise for
      --  instance "naa" would match "nmi", which is surprising to
      --  users). As the length of the pattern extends, allow more
      --  errors.

      Mask   : Character_Masks;
      Min    : Virtual_Character := Virtual_Character'Last;
      Max    : Virtual_Character := Virtual_Character'First;
      C      : Virtual_Character;
   begin
      declare
         P : VSS.Strings.Cursors.Iterators.Characters.Character_Iterator :=
           Pattern.Before_First_Character;

      begin
         while P.Forward loop
            C   := P.Element;
            Min := Virtual_Character'Min (Min, C);
            Max := Virtual_Character'Max (Max, C);
         end loop;
      end;

      Mask     := new Character_Mask_Array (Min .. Max);
      Mask.all := (others => 0);

      --  Compared to the paper, we revert the bit ordering in S

      declare
         P : VSS.Strings.Cursors.Iterators.Characters.Character_Iterator :=
           Pattern.Before_First_Character;

      begin
         while P.Forward loop
            C := P.Element;

            Mask (C) := Mask (C) or 2 ** Natural (P.Character_Index - 2);
         end loop;
      end;

      return Approximate_Search'
        (Ada.Finalization.Limited_Controlled with
         Text           => Pattern,
         Case_Sensitive => Case_Sensitive,
         Whole_Word     => Whole_Word,
         Kind           => LSP.Messages.Approximate,
         Negate         => Negate,
         Pattern        => Mask,
         Max_Errors     =>
           (if Pattern.Character_Length <= 4 then 0
            elsif Pattern.Character_Length <= 10 then 1
            else 2),
         Matched        => 2 ** (Natural (Pattern.Character_Length) - 1));
   end Build;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Approximate_Search) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Character_Mask_Array, Character_Masks);
   begin
      Unchecked_Free (Self.Pattern);
   end Finalize;

   -----------
   -- Match --
   -----------

   overriding function Match
     (Self : Approximate_Search;
      Text : VSS.Strings.Virtual_String)
      return Boolean
   is
      P : VSS.Strings.Cursors.Iterators.Characters.Character_Iterator :=
        Text.Before_First_Character;

      C      : VSS.Characters.Virtual_Character;
      Status : Approximate_Status := (others => 0);
      Tmp_R  : Approximate_Status;
      Offset : Mask;
      Result : Boolean := False;

   begin
      --  Initialize the pattern with K ones
      for K in 1 .. Self.Max_Errors loop
         Status (K) := Shift_Left (Status (K - 1), 1) or 1;
      end loop;

      Each_Symbol :
      while P.Forward loop
         C     := P.Element;
         Tmp_R := Status;

         if C in Self.Pattern'Range then
            Offset := Self.Pattern (C);
         else
            Offset := 0;
         end if;

         Status (0) := (Shift_Left (Tmp_R (0), 1) or 1) and Offset;

         for K in 1 .. Self.Max_Errors loop
            Status (K) :=
              ((Shift_Left (Tmp_R (K), 1) or 1) and Offset)
              or (Shift_Left (Tmp_R (K - Approximate_Substitution_Cost)
                              or Status
                                (K - Approximate_Deletion_Cost), 1) or 1)
              or Tmp_R (K - Approximate_Insertion_Cost);
         end loop;

         for K in Status'First .. Self.Max_Errors loop
            if P.Character_Index - Self.Text.Character_Length
                 - Character_Count (K) + 1 >= 1
              and then (Status (K) and Self.Matched) /= 0
            then
               Result := True;
               exit Each_Symbol;
            end if;
         end loop;
      end loop Each_Symbol;

      if Result then
         return Self.Negate;
      else
         return not Self.Negate;
      end if;
   end Match;

end LSP.Search.Approximate;

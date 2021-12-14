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

with Interfaces;      use Interfaces;
with VSS.Characters;  use VSS.Characters;

package LSP.Search.Approximate is

   function Build
     (Pattern        : VSS.Strings.Virtual_String;
      Case_Sensitive : Boolean := False;
      Whole_Word     : Boolean := False;
      Negate         : Boolean := False)
      return Search_Pattern'Class;

private

   subtype Mask is Interfaces.Unsigned_64;
   --  We only consider the 1..Pattern'Length

   type Character_Mask_Array is array (Virtual_Character range <>) of Mask;
   type Character_Masks is access all Character_Mask_Array;

   Approximate_Max_Errors        : constant := 2;
   Approximate_Insertion_Cost    : constant := 1;
   Approximate_Substitution_Cost : constant := 1;
   Approximate_Deletion_Cost     : constant := 1;
   --  The cost for character insertion, substitution or deletion. Set any of
   --  these to Integer'Last to disable this type of errors.

   Approximate_Max_Cost : constant :=
     Integer'Max
       (Integer'Max
            (Integer'Max
                 (Approximate_Insertion_Cost,
                  Approximate_Substitution_Cost),
             Approximate_Deletion_Cost),
        Approximate_Max_Errors);

   type Approximate_Status is
     array (-Approximate_Max_Cost .. Approximate_Max_Errors) of Mask;

   type Approximate_Search is new Search_Pattern with record
      Pattern : Character_Masks;
      --  Precomputed info about the pattern
      --  ??? We only need entries for the characters in the Pattern, so we
      --  are wasting space here. This would also allow working with UTF8
      --  characters.

      Max_Errors : Natural := Approximate_Max_Errors;

      Matched : Mask;
      --  Value in Result that indicates when the character matches
   end record;
   type Approximate_Search_Access is access all Approximate_Search'Class;
   --  An approximate matcher. The algorithm is from:
   --    http://en.wikipedia.org/wiki/Bitap_algorithm
   --    from Wu and Manber "Fast Text Searching With Errors"

   overriding procedure Finalize (Self : in out Approximate_Search);

   overriding function Match
     (Self : Approximate_Search;
      Text : VSS.Strings.Virtual_String)
      return Boolean;

end LSP.Search.Approximate;

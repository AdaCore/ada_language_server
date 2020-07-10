------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2020, AdaCore                     --
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
with Ada.Unchecked_Deallocation;

package body LSP.Ada_Id_Iterators is

   type Ref_Result_Array_Access is
     access all Libadalang.Analysis.Ref_Result_Array;

   type Ref_Result_Iterator is limited new Ada.Finalization.Limited_Controlled
     and Base_Id_Iterators.Forward_Iterator with
   record
      This  : access Ref_Result_Iterator;
      Data  : Ref_Result_Array_Access;
      Index : Natural;
   end record;

   overriding procedure Finalize (Self : in out Ref_Result_Iterator);

   overriding function First
     (Self : Ref_Result_Iterator) return Base_Id_Cursor;

   overriding function Next
     (Self : Ref_Result_Iterator;
      Prev : Base_Id_Cursor) return Base_Id_Cursor;

   type Empty_Iterator is
     limited new Base_Id_Iterators.Forward_Iterator with null record;

   overriding function First
     (Self : Empty_Iterator) return Base_Id_Cursor is
       (others => <>);

   overriding function Next
     (Self   : Empty_Iterator;
      Ignore : Base_Id_Cursor) return Base_Id_Cursor is
       (others => <>);

   ----------------------------
   -- Empty_Base_Id_Iterator --
   ----------------------------

   function Empty_Base_Id_Iterator
     return Base_Id_Iterators.Forward_Iterator'Class is
   begin
      return Result : Empty_Iterator;
   end Empty_Base_Id_Iterator;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Ref_Result_Iterator) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Libadalang.Analysis.Ref_Result_Array, Ref_Result_Array_Access);
   begin
      Free (Self.Data);
   end Finalize;

   -----------
   -- First --
   -----------

   overriding function First
     (Self : Ref_Result_Iterator) return Base_Id_Cursor
   is
      use Libadalang.Analysis;
   begin
      if Self.Data'Length > 0 then
         Self.This.Index := Self.Data'First;
         return (Element => Ref (Self.Data (Self.Index)).As_Base_Id,
                 Kind    => Kind (Self.Data (Self.Index)));
      else
         return (others => <>);
      end if;
   end First;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Self : Ref_Result_Iterator;
      Prev : Base_Id_Cursor) return Base_Id_Cursor
   is
      use Libadalang.Analysis;
   begin
      if Ref (Self.Data (Self.Index)).As_Base_Id = Prev.Element then
         Self.This.Index := Self.This.Index + 1;

         if Self.Index in Self.Data'Range then
            return (Element => Ref (Self.Data (Self.Index)).As_Base_Id,
                    Kind    => Kind (Self.Data (Self.Index)));
         else
            return (others => <>);
         end if;
      else
         raise Program_Error;
      end if;
   end Next;

   -------------------------------
   -- Ref_Result_Array_Iterator --
   -------------------------------

   function Ref_Result_Array_Iterator
     (Vector : Libadalang.Analysis.Ref_Result_Array)
      return Base_Id_Iterators.Forward_Iterator'Class is
   begin
      return Result : aliased Ref_Result_Iterator do
         Result.This := Result'Unchecked_Access;
         Result.Index := 0;
         Result.Data := new Libadalang.Analysis.Ref_Result_Array'(Vector);
      end return;
   end Ref_Result_Array_Iterator;

end LSP.Ada_Id_Iterators;

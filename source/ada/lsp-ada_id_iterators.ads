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
--
--  This package provides iterators over Libadalang Base_Id nodes.

with Ada.Iterator_Interfaces;

with Libadalang.Analysis;
with Libadalang.Common;

package LSP.Ada_Id_Iterators is

   use type Libadalang.Analysis.Base_Id;

   type Base_Id_Cursor is record
      Element : Libadalang.Analysis.Base_Id := Libadalang.Analysis.No_Base_Id;
      Kind    : Libadalang.Common.Ref_Result_Kind := Libadalang.Common.Noref;
   end record;

   function Has_Element (Self : Base_Id_Cursor) return Boolean
     is (Self.Element /= Libadalang.Analysis.No_Base_Id);

   package Base_Id_Iterators is new Ada.Iterator_Interfaces
     (Base_Id_Cursor, Has_Element);

   function Empty_Base_Id_Iterator
     return Base_Id_Iterators.Forward_Iterator'Class;
   --  Return an iterator without any elements inside

   function Ref_Result_Array_Iterator
     (Vector : Libadalang.Analysis.Ref_Result_Array)
       return Base_Id_Iterators.Forward_Iterator'Class;
   --  Return an iterator over elements inside given Ref_Result_Array

end LSP.Ada_Id_Iterators;

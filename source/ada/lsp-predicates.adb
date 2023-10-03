------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
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

with Libadalang.Analysis;
with Libadalang.Common;

package body LSP.Predicates is

   type Restricted_Kind_Predicate is
     new Libadalang.Iterators.Ada_Node_Predicate_Interface with null record;
   --  A custom node predicate to filter some declaration kinds.
   --  See Is_Restricted_Kind for details.

   overriding function Evaluate
     (Ignore : in out Restricted_Kind_Predicate;
      Node   : Libadalang.Analysis.Ada_Node) return Boolean;
   --  Evaluate the Restricted_Kind_Predicate filter
   --  See Is_Restricted_Kind for details.

   type Is_Global_Visible_Predicate is
     new Libadalang.Iterators.Ada_Node_Predicate_Interface with null record;
   --  A custom node predicate to filter some declaration kinds.
   --  See Is_Restricted_Kind for details.

   overriding function Evaluate
     (Ignore : in out Is_Global_Visible_Predicate;
      Node   : Libadalang.Analysis.Ada_Node) return Boolean;
   --  Evaluate the Restricted_Kind_Predicate filter
   --  See Is_Restricted_Kind for details.

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Ignore : in out Restricted_Kind_Predicate;
      Node   : Libadalang.Analysis.Ada_Node) return Boolean
   is
      use type Libadalang.Common.Ada_Node_Kind_Type;

      Decl : constant Libadalang.Analysis.Basic_Decl :=
        Node.As_Defining_Name.P_Basic_Decl;
      Next : Libadalang.Analysis.Ada_Node := Decl.Parent;

   begin
      if not Decl.Is_Null and then
        Decl.Kind in Libadalang.Common.Ada_For_Loop_Var_Decl
          | Libadalang.Common.Ada_Base_Formal_Param_Decl
          | Libadalang.Common.Ada_Extended_Return_Stmt_Object_Decl
          | Libadalang.Common.Ada_Anonymous_Expr_Decl
          | Libadalang.Common.Ada_Exception_Handler
          | Libadalang.Common.Ada_Label_Decl
          | Libadalang.Common.Ada_Named_Stmt_Decl
          | Libadalang.Common.Ada_Entry_Index_Spec
          | Libadalang.Common.Ada_Entry_Decl
      then
         return True;

      elsif not Decl.Is_Null and then
        Decl.Kind in Libadalang.Common.Ada_Object_Decl and then
        Decl.Parent.Kind = Libadalang.Common.Ada_Generic_Formal_Obj_Decl
      then
         --  This is a special case for the formal_object_declaration
         return True;
      end if;

      while not Next.Is_Null loop
         --  Any program unit body excluding library level package bodies
         if Next.Kind in Libadalang.Common.Ada_Body_Node and then
           (Next.Kind not in Libadalang.Common.Ada_Package_Body or else
              Next.Parent.Kind not in Libadalang.Common.Ada_Library_Item)
         then
            return True;
         end if;

         Next := Next.Parent;
      end loop;

      return False;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Ignore : in out Is_Global_Visible_Predicate;
      Node   : Libadalang.Analysis.Ada_Node) return Boolean
   is
      Decl : constant Libadalang.Analysis.Basic_Decl :=
        Node.As_Defining_Name.P_Basic_Decl;
      Next : Libadalang.Analysis.Ada_Node := Decl.Parent;
   begin
      while not Next.Is_Null loop
         if Next.Kind in
           Libadalang.Common.Ada_Body_Node
           | Libadalang.Common.Ada_Private_Part
           | Libadalang.Common.Ada_Protected_Type_Decl
           | Libadalang.Common.Ada_Single_Protected_Decl
         then
            return False;
         end if;

         Next := Next.Parent;
      end loop;

      return True;
   end Evaluate;

   ------------------------
   -- Is_Restricted_Kind --
   ------------------------

   function Is_Restricted_Kind
     return Libadalang.Iterators.Ada_Node_Predicate is
   begin
      return Result : Libadalang.Iterators.Ada_Node_Predicate do
         Result.Set (Restricted_Kind_Predicate'(null record));
      end return;
   end Is_Restricted_Kind;

   -----------------------
   -- Is_Global_Visible --
   -----------------------

   function Is_Global_Visible return Libadalang.Iterators.Ada_Node_Predicate is
   begin
      return Result : Libadalang.Iterators.Ada_Node_Predicate do
         Result.Set (Is_Global_Visible_Predicate'(null record));
      end return;
   end Is_Global_Visible;

end LSP.Predicates;

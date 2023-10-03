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
--
--  This package provides some Libadalang predicates.

with Libadalang.Iterators;

package LSP.Predicates is

   function Is_Restricted_Kind return Libadalang.Iterators.Ada_Node_Predicate;
   --  A node under query doesn't participate in global symbol index. It's a
   --  defining name of a declaration such as
   --  * a loop parameter specification
   --  * parameter declaration
   --  * discriminant/component declaration
   --  * return object declaration
   --  * entry/entry-index declaration
   --  * formal parameter declaration
   --  * etc
   --  It also includes any symbols local to some non-package body.
   --  These symbols isn't very useful in "invisible symbol completion" and
   --  in "Go to workspace symbol" requests.

   function Is_Global_Visible return Libadalang.Iterators.Ada_Node_Predicate;
   --  A node under query is a defined name visible at a library level, such
   --  as declaration in a public part of a library level project.

end LSP.Predicates;

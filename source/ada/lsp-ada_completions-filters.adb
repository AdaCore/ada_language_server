------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2021-2023, AdaCore                     --
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

package body LSP.Ada_Completions.Filters is
   pragma Warnings (Off);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Filter; Token : Libadalang.Common.Token_Reference;
      Node :        Libadalang.Analysis.Ada_Node)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Initialize unimplemented");
      raise Program_Error with "Unimplemented procedure Initialize";
   end Initialize;

   ------------------
   -- Is_End_Label --
   ------------------

   function Is_End_Label (Self : in out Filter'Class) return Boolean is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Is_End_Label unimplemented");
      return raise Program_Error with "Unimplemented function Is_End_Label";
   end Is_End_Label;

   ------------------------
   -- Is_Numeric_Literal --
   ------------------------

   function Is_Numeric_Literal (Self : in out Filter'Class) return Boolean is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Is_Numeric_Literal unimplemented");
      return
        raise Program_Error with "Unimplemented function Is_Numeric_Literal";
   end Is_Numeric_Literal;

   ----------------------
   -- Is_Attribute_Ref --
   ----------------------

   function Is_Attribute_Ref (Self : in out Filter'Class) return Boolean is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Is_Attribute_Ref unimplemented");
      return
        raise Program_Error with "Unimplemented function Is_Attribute_Ref";
   end Is_Attribute_Ref;

   ---------------
   -- Is_Aspect --
   ---------------

   function Is_Aspect (Self : in out Filter'Class) return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "Is_Aspect unimplemented");
      return raise Program_Error with "Unimplemented function Is_Aspect";
   end Is_Aspect;

   ------------------
   -- Is_Semicolon --
   ------------------

   function Is_Semicolon (Self : in out Filter'Class) return Boolean is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Is_Semicolon unimplemented");
      return raise Program_Error with "Unimplemented function Is_Semicolon";
   end Is_Semicolon;

   --------------
   -- Is_Comma --
   --------------

   function Is_Comma (Self : in out Filter'Class) return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "Is_Comma unimplemented");
      return raise Program_Error with "Unimplemented function Is_Comma";
   end Is_Comma;

end LSP.Ada_Completions.Filters;

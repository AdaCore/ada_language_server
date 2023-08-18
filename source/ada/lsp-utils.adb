------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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
--  This package provides some utility subprograms.

with GNATCOLL.VFS;
with GNATCOLL.Utils;

with VSS.Strings.Conversions;

with Libadalang.Sources;
with Langkit_Support.Symbols;

package body LSP.Utils is

   ------------------
   -- Canonicalize --
   ------------------

   function Canonicalize
     (Text : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String
   is
      use Langkit_Support.Symbols;

      UTF_32 : constant Wide_Wide_String :=
        VSS.Strings.Conversions.To_Wide_Wide_String (Text);
      Result : constant Symbolization_Result :=
        Libadalang.Sources.Canonicalize (UTF_32);

   begin
      if Result.Success then
         return VSS.Strings.To_Virtual_String (Result.Symbol);
      else
         return VSS.Strings.Empty_Virtual_String;
      end if;
   end Canonicalize;

   -------------------------
   -- Node_Location_Image --
   -------------------------

   function Node_Location_Image
     (Node : Libadalang.Analysis.Ada_Node'Class)
      return VSS.Strings.Virtual_String
   is
      Decl_Unit_File : constant GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.Create_From_UTF8 (Node.Unit.Get_Filename);

   begin
      return Result : VSS.Strings.Virtual_String do
         Result.Append ("at ");
         Result.Append
           (VSS.Strings.Conversions.To_Virtual_String
              (Decl_Unit_File.Display_Base_Name));
         Result.Append (" (");
         Result.Append
           (VSS.Strings.Conversions.To_Virtual_String
              (GNATCOLL.Utils.Image
                   (Integer (Node.Sloc_Range.Start_Line), Min_Width => 1)));
         Result.Append (':');
         Result.Append
           (VSS.Strings.Conversions.To_Virtual_String
              (GNATCOLL.Utils.Image
                   (Integer (Node.Sloc_Range.Start_Column), Min_Width => 1)));
         Result.Append (')');
      end return;
   end Node_Location_Image;

end LSP.Utils;

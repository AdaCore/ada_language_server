------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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

with Ada.Wide_Wide_Text_IO;
with Ada.Containers.Hashed_Sets;

with VSS.Characters;
with VSS.Strings.Conversions;
with VSS.Strings.Cursors.Iterators.Characters;
with VSS.Strings.Hash;
with VSS.Transformers.Casing;       use VSS.Transformers.Casing;

package body LSP_Gen.Puts is

   package String_Sets is new Ada.Containers.Hashed_Sets
     (VSS.Strings.Virtual_String,
      VSS.Strings.Hash,
      VSS.Strings."=",
      VSS.Strings."=");

   Keywords : constant String_Sets.Set :=
     ["abort", "abs", "abstract", "accept", "access", "aliased", "all", "and",
      "array", "at", "begin", "body", "case", "constant", "declare", "delay",
      "delta", "digits", "do", "else", "elsif", "end", "entry", "exception",
      "exit", "for", "function", "generic", "goto", "if", "in", "interface",
      "is", "limited", "loop", "mod", "new", "not", "null", "of", "or",
      "others", "out", "overriding", "package", "pragma", "private",
      "procedure", "protected", "raise", "range", "record", "rem", "renames",
      "requeue", "return", "reverse", "select", "separate", "some", "subtype",
      "synchronized", "tagged", "task", "terminate", "then", "type", "until",
      "use", "when", "while", "with", "xor"];

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Ada.Wide_Wide_Text_IO.New_Line;
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (Text : VSS.Strings.Virtual_String) is
   begin
      Ada.Wide_Wide_Text_IO.Put
        (VSS.Strings.Conversions.To_Wide_Wide_String (Text));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Number : Integer) is
      Image : constant Wide_Wide_String := Number'Wide_Wide_Image;
   begin
      if Number < 0 then
         Ada.Wide_Wide_Text_IO.Put (Image);
      else
         Ada.Wide_Wide_Text_IO.Put (Image (2 .. Image'Last));
      end if;
   end Put;

   ------------
   -- Put_Id --
   ------------

   procedure Put_Id (Id : VSS.Strings.Virtual_String) is
      use type VSS.Characters.Virtual_Character;

      Keyword : constant VSS.Strings.Virtual_String :=
        To_Lowercase.Transform (Id);
   begin
      if Keywords.Contains (Keyword) then
         if Id.At_First_Character.Element =
           Keyword.At_First_Character.Element
         then
            Put ("a");
         else
            Put ("A");
         end if;

         if Keyword.At_First_Character.Element in
           'a' | 'e' | 'i' | 'o' | '_' --  | 'u'
         then
            Put ("n");
         end if;

         Put ("_");
      elsif Id.Starts_With ("_") then
         Put ("An");
      end if;

      Put (Id);
   end Put_Id;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Text : VSS.Strings.Virtual_String) is
   begin
      Put (Text);
      New_Line;
   end Put_Line;

   ---------------
   -- Put_Lines --
   ---------------

   procedure Put_Lines
     (List   : VSS.String_Vectors.Virtual_String_Vector;
      Prefix : VSS.Strings.Virtual_String := VSS.Strings.Empty_Virtual_String)
   is
   begin
      for Item of List loop
         Put (Prefix);
         Put (Item);
         New_Line;
      end loop;
   end Put_Lines;

   --------------
   -- Put_Type --
   --------------

   procedure Put_Type (X : LSP_Gen.Entities.AType) is
      use all type LSP_Gen.Entities.Enum.AType_Variant;
      use all type LSP_Gen.Entities.Enum.BaseTypes;
   begin
      case X.Union.Kind is
         when base =>
            case X.Union.base.name is
               when LSP_Gen.Entities.Enum.Uri |
                    LSP_Gen.Entities.Enum.DocumentUri |
                    LSP_Gen.Entities.Enum.RegExp |
                    LSP_Gen.Entities.Enum.string
                  =>
                  Put ("VSS.Strings.Virtual_String");
               when LSP_Gen.Entities.Enum.integer =>
                  Put ("Integer");
               when LSP_Gen.Entities.Enum.uinteger =>
                  Put ("Natural");
               when LSP_Gen.Entities.Enum.decimal =>
                  Put ("Float");
               when LSP_Gen.Entities.Enum.a_boolean =>
                  Put ("Boolean");
               when LSP_Gen.Entities.Enum.a_null =>
                  Put ("null");
            end case;
         when reference =>
            Put_Id (X.Union.reference.name);
         when others =>
            raise Program_Error;
      end case;
   end Put_Type;

end LSP_Gen.Puts;

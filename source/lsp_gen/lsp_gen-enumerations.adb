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

with LSP_Gen.Entities;
with LSP_Gen.Puts; use LSP_Gen.Puts;

package body LSP_Gen.Enumerations is

   procedure Write_Type (Enum : LSP_Gen.Entities.Enumeration);
   procedure Write_Pseudo_Enum (Enum : LSP_Gen.Entities.Enumeration);
   --  When enumeration supports custom values we can't use enumeration type,
   --  so instead create new string or integer types and set of functions
   --  for corresponding literals.

   -----------------------
   -- Write_Pseudo_Enum --
   -----------------------

   procedure Write_Pseudo_Enum (Enum : LSP_Gen.Entities.Enumeration) is
      Last : constant Positive := Enum.values.Length;
   begin
      Put ("type ");
      Put_Id (Enum.name);
      Put_Line (" is ");

      case Enum.a_type.name is
         when LSP_Gen.Entities.Enum.string =>
            Put_Line ("new VSS.Strings.Virtual_String with null record");
         when LSP_Gen.Entities.Enum.integer =>
            Put_Line ("new Integer");
         when LSP_Gen.Entities.Enum.uinteger =>
            Put_Line ("mod 2 ** 16");
      end case;

      Put_Line (";");
      Put_Lines (Enum.documentation.Split_Lines, "   --  ");

      for J in 1 .. Last loop
         declare
            Item : constant LSP_Gen.Entities.EnumerationEntry :=
              Enum.values (J);
         begin
            New_Line;
            Put ("function ");
            Put_Id (Item.name);
            Put (" return ");
            Put_Id (Enum.name);
            Put (" is (");

            case Item.value.Is_String is
               when True =>
                  Put ("""");
                  Put (Item.value.String);
                  Put ("""");
               when False =>
                  Put (Item.value.Integer);
            end case;

            Put_Line (");");

            if not Item.documentation.Is_Empty then
               Put_Lines (Item.documentation.Split_Lines, "  --  ");
            end if;
         end;
      end loop;

      New_Line;
   end Write_Pseudo_Enum;

   ----------------
   -- Write_Type --
   ----------------

   procedure Write_Type (Enum : LSP_Gen.Entities.Enumeration) is
      Last : constant Positive := Enum.values.Length;
   begin
      Put ("type ");
      Put_Id (Enum.name);
      Put_Line (" is (");

      for J in 1 .. Last loop
         declare
            Item : constant LSP_Gen.Entities.EnumerationEntry :=
              Enum.values (J);
         begin
            Put_Id (Item.name);

            if J /= Last then
               Put (", ");
            end if;
         end;
      end loop;

      Put_Line (");");
      Put_Lines (Enum.documentation.Split_Lines, "   --  ");

      for J in 1 .. Last loop
         declare
            Item : constant LSP_Gen.Entities.EnumerationEntry :=
              Enum.values (J);
         begin
            if not Item.documentation.Is_Empty then
               Put_Line ("   --");
               Put ("   --  @value ");
               Put_Id (Item.name);
               New_Line;
               Put_Lines (Item.documentation.Split_Lines, "  --  ");
            end if;
         end;
      end loop;

      New_Line;
   end Write_Type;

   -----------------
   -- Write_Types --
   -----------------

   procedure Write_Types (Model : LSP_Gen.Meta_Models.Meta_Model) is
   begin
      Put_Lines (Model.License_Header, "--  ");
      New_Line;
      Put_Line ("with VSS.Strings;");
      New_Line;
      Put_Line ("package LSP.Enumerations is");
      Put_Line ("   pragma Preelaborate;"); New_Line;

      for Name of Model.Enumerations loop
         declare
            Enum : constant LSP_Gen.Entities.Enumeration :=
              Model.Enumeration (Name);
         begin
            if Enum.supportsCustomValues then
               Write_Pseudo_Enum (Enum);
            else
               Write_Type (Enum);
            end if;
         end;
      end loop;

      Put_Line ("end LSP.Enumerations;");
   end Write_Types;

end LSP_Gen.Enumerations;

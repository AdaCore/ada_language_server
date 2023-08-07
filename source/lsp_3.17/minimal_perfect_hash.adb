--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Hashed_Maps;

with VSS.Strings.Hash;

package body Minimal_Perfect_Hash is

   package String_Maps is new Ada.Containers.Hashed_Maps
     (VSS.Strings.Virtual_String,
      Positive,
      VSS.Strings.Hash,
      VSS.Strings."=");

   Map : String_Maps.Map;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index (Text : VSS.Strings.Virtual_String) return Natural is
      Cursor : String_Maps.Cursor;
   begin
      if Map.Is_Empty then
         Initialize;
      end if;

      Cursor := Map.Find (Text);

      if String_Maps.Has_Element (Cursor) then
         return String_Maps.Element (Cursor);
      else
         return 0;
      end if;
   end Get_Index;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Seed : Natural := 0) is
      pragma Unreferenced (Seed);
   begin
      for J in 1 .. Variants.Length loop
         Map.Insert (Variants (J), J);
      end loop;
   end Initialize;

end Minimal_Perfect_Hash;

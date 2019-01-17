------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
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

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Tester.Macros is

   function Expand
     (Value    : GNATCOLL.JSON.JSON_Value;
      Test_Dir : String) return GNATCOLL.JSON.JSON_Value;
   --  Expand recursively

   function Expand
     (Value    : GNATCOLL.JSON.JSON_Value;
      Test_Dir : String) return GNATCOLL.JSON.JSON_Value
   is
      procedure Each_Field
        (Object : in out GNATCOLL.JSON.JSON_Value;
         Name   : String;
         Value  : GNATCOLL.JSON.JSON_Value);
      --  Expand macros in given field

      function Expand_in_String (Text : String) return Unbounded_String;
      --  Expand macro in given string

      procedure Map is new GNATCOLL.JSON.Gen_Map_JSON_Object
        (GNATCOLL.JSON.JSON_Value);

      ----------------
      -- Each_Field --
      ----------------

      procedure Each_Field
        (Object : in out GNATCOLL.JSON.JSON_Value;
         Name   : String;
         Value  : GNATCOLL.JSON.JSON_Value) is
      begin
         Object.Set_Field (Name, Expand (Value, Test_Dir));
      end Each_Field;

      ----------------------
      -- Expand_in_String --
      ----------------------

      function Expand_in_String (Text : String) return Unbounded_String
      is
         Macro  : constant String := "${TD}";
         Result : Unbounded_String;
         Next   : Positive := 1;
      begin
         while Next < Text'Length loop
            declare
               Pos : constant Natural :=
                 Ada.Strings.Fixed.Index (Text, Macro, Next);
            begin
               exit when Pos = 0;

               Append (Result, Text (Next .. Pos - 1));
               Append (Result, Test_Dir);
               Next := Pos + Macro'Length;
            end;
         end loop;

         Append (Result, Text (Next .. Text'Last));

         return Result;
      end Expand_in_String;

   begin
      case Value.Kind is
         when GNATCOLL.JSON.JSON_Null_Type |
              GNATCOLL.JSON.JSON_Boolean_Type |
              GNATCOLL.JSON.JSON_Int_Type |
              GNATCOLL.JSON.JSON_Float_Type =>

            return Value;
         when GNATCOLL.JSON.JSON_String_Type =>

            return GNATCOLL.JSON.Create (Expand_in_String (Value.Get));
         when GNATCOLL.JSON.JSON_Array_Type =>
            declare
               Result : GNATCOLL.JSON.JSON_Array;
               Vector : constant GNATCOLL.JSON.JSON_Array := Value.Get;
            begin
               for J in 1 .. GNATCOLL.JSON.Length (Vector) loop
                  declare
                     Item : constant GNATCOLL.JSON.JSON_Value :=
                       GNATCOLL.JSON.Get (Vector, J);
                  begin
                     GNATCOLL.JSON.Append (Result, Expand (Item, Test_Dir));
                  end;
               end loop;

               return GNATCOLL.JSON.Create (Result);
            end;
         when GNATCOLL.JSON.JSON_Object_Type =>
            declare
               Result : GNATCOLL.JSON.JSON_Value :=
                 GNATCOLL.JSON.Create_Object;
            begin
               Map (Value, Each_Field'Access, Result);

               return Result;
            end;
      end case;
   end Expand;

   ------------
   -- Expand --
   ------------

   procedure Expand (Test : in out GNATCOLL.JSON.JSON_Value; Path : String) is
      Full_Name : constant String := Ada.Directories.Full_Name (Path);
      Directory : constant String :=
        Ada.Directories.Containing_Directory (Full_Name);
   begin
      Test := Expand (Test, Directory);
   end Expand;

end Tester.Macros;

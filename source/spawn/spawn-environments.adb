------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with Spawn.Environments.Internal;
pragma Elaborate (Spawn.Environments.Internal);

package body Spawn.Environments is

   Default : Process_Environment;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Process_Environment'Class) is
   begin
      Self.Map.Clear;
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains
     (Self : Process_Environment'Class;
      Name : UTF_8_String) return Boolean is
   begin
      return Self.Map.Contains (Name);
   end Contains;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self  : in out Process_Environment'Class;
      Name  : UTF_8_String;
      Value : UTF_8_String) is
   begin
      Self.Map.Insert (Name, Value);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self        : in out Process_Environment'Class;
      Environemnt : Process_Environment'Class)
   is
   begin
      for J in Environemnt.Map.Iterate loop
         Self.Map.Insert
           (UTF_8_String_Maps.Key (J),
            UTF_8_String_Maps.Element (J));
      end loop;
   end Insert;

   ----------
   -- Keys --
   ----------

   function Keys
     (Self : Process_Environment'Class)
      return Spawn.String_Vectors.UTF_8_String_Vector is
   begin
      return Result : Spawn.String_Vectors.UTF_8_String_Vector do
         for J in Self.Map.Iterate loop
            Result.Append (UTF_8_String_Maps.Key (J));
         end loop;
      end return;
   end Keys;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Self : in out Process_Environment'Class;
      Name : UTF_8_String) is
   begin
      Self.Map.Exclude (Name);
   end Remove;

   ------------------------
   -- System_Environment --
   ------------------------

   function System_Environment return Process_Environment is
   begin
      return Default;
   end System_Environment;

   -----------
   -- Value --
   -----------

   function Value
     (Self    : Process_Environment'Class;
      Name    : UTF_8_String;
      Default : UTF_8_String := "")
      return UTF_8_String
   is
      Cursor : constant UTF_8_String_Maps.Cursor := Self.Map.Find (Name);
   begin
      if UTF_8_String_Maps.Has_Element (Cursor) then
         return UTF_8_String_Maps.Element (Cursor);
      else
         return Default;
      end if;
   end Value;

begin
   Spawn.Environments.Internal.Initialize_Default (Default);
end Spawn.Environments;

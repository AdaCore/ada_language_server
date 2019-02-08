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

--  Notes about names of variables: its case sensitivity is platform dependent.
--  On case insensitive platforms original casing is returned by Keys function.

with Spawn.String_Vectors;

private with Ada.Containers.Indefinite_Ordered_Maps;
private with Spawn.Internal;

package Spawn.Environments is

   type Process_Environment is tagged private;

   procedure Clear (Self : in out Process_Environment'Class);

   function Contains
    (Self : Process_Environment'Class; Name : UTF_8_String) return Boolean;

   procedure Insert
    (Self  : in out Process_Environment'Class;
     Name  : UTF_8_String;
     Value : UTF_8_String);

   procedure Insert
    (Self        : in out Process_Environment'Class;
     Environemnt : Process_Environment'Class);

   function Keys (Self : Process_Environment'Class)
     return Spawn.String_Vectors.UTF_8_String_Vector;

   procedure Remove
    (Self : in out Process_Environment'Class;
     Name : UTF_8_String);

   function Value
    (Self    : Process_Environment'Class;
     Name    : UTF_8_String;
     Default : UTF_8_String := "") return UTF_8_String;

   function System_Environment return Process_Environment;

private

   package UTF_8_String_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => UTF_8_String,
      Element_Type => UTF_8_String,
      "<"          => Spawn.Internal.Environments."<",
      "="          => Spawn.Internal.Environments."=");

   type Process_Environment is tagged record
      Map : UTF_8_String_Maps.Map;
   end record;

end Spawn.Environments;

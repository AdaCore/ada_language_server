------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                      Copyright (C) 2022-2023, AdaCore                    --
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

with Ada.Containers.Hashed_Maps;

with VSS.Strings;
with VSS.Strings.Hash;
with VSS.String_Vectors;

package LSP_Gen.Configurations is

   type Configuration is tagged limited private;

   procedure Load
     (Self      : in out Configuration;
      File_Name : VSS.Strings.Virtual_String);

   type Message_Direction is
     (From_Client, From_Both, From_Server);

   function Direction
     (Self   : Configuration;
      Method : VSS.Strings.Virtual_String) return Message_Direction;

   function Name
     (Self   : Configuration;
      Method : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String;
   --  Notification name as an Ada identifier

   function License_Header
     (Self : Configuration) return VSS.String_Vectors.Virtual_String_Vector;
   --  Lines of license header to be included in each generated file

private

   type Message_Information is record
      Direction : Message_Direction;
      Name      : VSS.Strings.Virtual_String;
   end record;

   package Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => VSS.Strings.Virtual_String,
      Element_Type    => Message_Information,
      Hash            => VSS.Strings.Hash,
      Equivalent_Keys => VSS.Strings."=");

   type Configuration is tagged limited record
      License : VSS.String_Vectors.Virtual_String_Vector;
      Map : Maps.Map;
   end record;

   function License_Header
     (Self : Configuration) return VSS.String_Vectors.Virtual_String_Vector
       is (Self.License);

end LSP_Gen.Configurations;

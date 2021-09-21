------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                    Copyright (C) 2020-2021, AdaCore                      --
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

private with Ada.Containers.Vectors;

package VSS.JSON.Pull_Readers.Look_Ahead is

   type JSON_Look_Ahead_Reader
     (Parent : not null access JSON_Pull_Reader'Class)
       is limited new JSON_Pull_Reader with private;
   --  This type provides a limited capability to look ahead into some JSON
   --  stream by remembering observed events. After end of look ahead phase
   --  the user "rewind" the stream and is able to read all events againg.

   procedure Rewind (Self : in out JSON_Look_Ahead_Reader'Class);
   --  Restore JSON stream position to the very beginning, so it can be read
   --  again.

private

   type JSON_Event
     (Event_Kind : JSON_Event_Kind := JSON_Event_Kind'First) is
   record
      case Event_Kind is
         when No_Token | Invalid | Start_Document | End_Document |
              Start_Array | End_Array | Start_Object | End_Object |
              Null_Value =>
            null;
         when Key_Name =>
            Key_Name : VSS.Strings.Virtual_String;
         when String_Value =>
            String_Value : VSS.Strings.Virtual_String;
         when Number_Value =>
            Number_Value : VSS.JSON.JSON_Number;
         when Boolean_Value =>
            Boolean_Value : Boolean;
      end case;
   end record;

   package JSON_Event_Vectors is
     new Ada.Containers.Vectors (Positive, JSON_Event);

   type JSON_Look_Ahead_Reader
     (Parent : not null access JSON_Pull_Reader'Class)
   is limited new JSON_Pull_Reader with record
      Data      : JSON_Event_Vectors.Vector;
      Index     : Natural := 1;
      Save_Mode : Boolean := True;
   end record;

   overriding function At_End (Self : JSON_Look_Ahead_Reader) return Boolean;

   overriding function Read_Next
     (Self : in out JSON_Look_Ahead_Reader) return JSON_Event_Kind;

   overriding procedure Clear (Self : in out JSON_Look_Ahead_Reader);

   overriding function Error
     (Self : JSON_Look_Ahead_Reader) return JSON_Reader_Error;

   overriding function Error_Message
     (Self : JSON_Look_Ahead_Reader) return VSS.Strings.Virtual_String;

   overriding procedure Raise_Error
     (Self    : in out JSON_Look_Ahead_Reader;
      Message : VSS.Strings.Virtual_String);

   overriding function Event_Kind
     (Self : JSON_Look_Ahead_Reader) return JSON_Event_Kind;

   overriding function Key_Name
     (Self : JSON_Look_Ahead_Reader) return VSS.Strings.Virtual_String;

   overriding function String_Value
     (Self : JSON_Look_Ahead_Reader) return VSS.Strings.Virtual_String;

   overriding function Number_Value
     (Self : JSON_Look_Ahead_Reader) return VSS.JSON.JSON_Number;

   overriding function Boolean_Value
     (Self : JSON_Look_Ahead_Reader) return Boolean;

   overriding procedure Skip_Current_Array
     (Self : in out JSON_Look_Ahead_Reader);

   overriding procedure Skip_Current_Object
     (Self : in out JSON_Look_Ahead_Reader);

   overriding procedure Skip_Current_Value
     (Self : in out JSON_Look_Ahead_Reader);

end VSS.JSON.Pull_Readers.Look_Ahead;

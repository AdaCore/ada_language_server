------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                    Copyright (C) 2020-2023, AdaCore                      --
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

package body VSS.JSON.Pull_Readers.Look_Ahead is

   ------------
   -- At_End --
   ------------

   overriding function At_End (Self : JSON_Look_Ahead_Reader) return Boolean is
   begin
      if Self.Save_Mode or else Self.Index > Self.Data.Last_Index then
         return Self.Parent.At_End;
      else
         return False;
      end if;
   end At_End;

   -------------------
   -- Boolean_Value --
   -------------------

   overriding function Boolean_Value
     (Self : JSON_Look_Ahead_Reader) return Boolean is
   begin
      if Self.Save_Mode or else Self.Index > Self.Data.Last_Index then
         return Self.Parent.Boolean_Value;
      else
         return Self.Data (Self.Index).Boolean_Value;
      end if;
   end Boolean_Value;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Self : in out JSON_Look_Ahead_Reader) is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "Unimplemented procedure Clear";
   end Clear;

   ------------------
   -- Element_Kind --
   ------------------

   overriding function Element_Kind
     (Self : JSON_Look_Ahead_Reader)
      return VSS.JSON.Streams.JSON_Stream_Element_Kind is
   begin
      if Self.Save_Mode or else Self.Index > Self.Data.Last_Index then
         return Self.Parent.Element_Kind;
      else
         return Self.Data (Self.Index).Kind;
      end if;
   end Element_Kind;

   -----------
   -- Error --
   -----------

   overriding function Error
     (Self : JSON_Look_Ahead_Reader) return JSON_Reader_Error
   is
      pragma Unreferenced (Self);
   begin
      return raise Program_Error with "Unimplemented function Error";
   end Error;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : JSON_Look_Ahead_Reader) return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Self);
   begin
      return raise Program_Error with "Unimplemented function Error_Message";
   end Error_Message;

   --------------
   -- Key_Name --
   --------------

   overriding function Key_Name
     (Self : JSON_Look_Ahead_Reader) return VSS.Strings.Virtual_String is
   begin
      if Self.Save_Mode or else Self.Index > Self.Data.Last_Index then
         return Self.Parent.Key_Name;
      else
         return Self.Data (Self.Index).Key_Name;
      end if;
   end Key_Name;

   ------------------
   -- Number_Value --
   ------------------

   overriding function Number_Value
     (Self : JSON_Look_Ahead_Reader) return VSS.JSON.JSON_Number is
   begin
      if Self.Save_Mode or else Self.Index > Self.Data.Last_Index then
         return Self.Parent.Number_Value;
      else
         return Self.Data (Self.Index).Number_Value;
      end if;
   end Number_Value;

   -----------------
   -- Raise_Error --
   -----------------

   overriding procedure Raise_Error
     (Self    : in out JSON_Look_Ahead_Reader;
      Message : VSS.Strings.Virtual_String)
   is
      pragma Unreferenced (Self, Message);
   begin
      raise Program_Error with "Unimplemented procedure Raise_Error";
   end Raise_Error;

   ---------------
   -- Read_Next --
   ---------------

   overriding function Read_Next
     (Self : in out JSON_Look_Ahead_Reader)
      return VSS.JSON.Streams.JSON_Stream_Element_Kind is
   begin
      if Self.Save_Mode then
         Self.Data.Append (Self.Parent.Element);

         return Self.Parent.Read_Next;
      elsif Self.Index >= Self.Data.Last_Index then
         Self.Index := Self.Index + 1;

         return Self.Parent.Read_Next;
      else
         Self.Index := Self.Index + 1;

         return Self.Data (Self.Index).Kind;
      end if;
   end Read_Next;

   ------------
   -- Rewind --
   ------------

   procedure Rewind (Self : in out JSON_Look_Ahead_Reader'Class) is
   begin
      if Self.Save_Mode then
         Self.Data.Append (Self.Parent.Element);
         Self.Save_Mode := False;
      end if;

      Self.Index := 1;
   end Rewind;

   ------------------------
   -- Skip_Current_Array --
   ------------------------

   overriding procedure Skip_Current_Array
     (Self : in out JSON_Look_Ahead_Reader)
   is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "Unimplemented procedure Skip_Current_Array";
   end Skip_Current_Array;

   -------------------------
   -- Skip_Current_Object --
   -------------------------

   overriding procedure Skip_Current_Object
     (Self : in out JSON_Look_Ahead_Reader)
   is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "Unimplemented procedure Skip_Current_Object";
   end Skip_Current_Object;

   ------------------------
   -- Skip_Current_Value --
   ------------------------

   overriding procedure Skip_Current_Value
     (Self : in out JSON_Look_Ahead_Reader)
   is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "Unimplemented procedure Skip_Current_Value";
   end Skip_Current_Value;

   ------------------
   -- String_Value --
   ------------------

   overriding function String_Value
     (Self : JSON_Look_Ahead_Reader) return VSS.Strings.Virtual_String is
   begin
      if Self.Save_Mode or else Self.Index > Self.Data.Last_Index then
         return Self.Parent.String_Value;
      else
         return Self.Data (Self.Index).String_Value;
      end if;
   end String_Value;

end VSS.JSON.Pull_Readers.Look_Ahead;

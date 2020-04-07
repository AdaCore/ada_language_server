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
with Ada.Strings.UTF_Encoding.Wide_Strings;

with Interfaces;

with Magic.Strings.Conversions;

package body LSP.JSON_Streams is

   procedure Push
    (Self  : in out Read_Stream;
     Kind  : State_Kinds);
   procedure Push
    (Self : in out Read_Stream;
     Data : GNATCOLL.JSON.JSON_Array);
   procedure Push
    (Self : in out Read_Stream;
     Data : GNATCOLL.JSON.JSON_Value);
   --  Push current state into the stack and initialize new current state.

   procedure Pop (Self : in out Read_Stream);
   --  Unwind state stack and add constructed value to new state.

   function Read
    (Self : in out Read_Stream)
       return GNATCOLL.JSON.JSON_Value;

   function To_UTF_8_String
     (Value : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String)
      return Ada.Strings.UTF_Encoding.UTF_8_String;
   --  Unbounded_Wide_String (UTF-16) to String (UTF-8) conversion.

   procedure Write_Key (Self : in out Write_Stream);

   ---------------
   -- End_Array --
   ---------------

   procedure End_Array (Self : not null access JSON_Stream'Class) is
   begin
      case Self.Writable is
         when True =>
            End_Array (Self.W);
         when False =>
            End_Array (Self.R);
      end case;
   end End_Array;

   procedure End_Array (Self : in out Read_Stream) renames Pop;

   ---------------
   -- End_Array --
   ---------------

   procedure End_Array (Self : in out Write_Stream) is
   begin
      Self.Writer.End_Array;
      Self.Key := Magic.Strings.Empty_Magic_String;
   end End_Array;

   ------------------
   -- End_Document --
   ------------------

   procedure End_Document (Self : in out JSON_Stream'Class) is
   begin
      Self.W.Writer.End_Document;
   end End_Document;

   ----------------
   -- End_Object --
   ----------------

   procedure End_Object (Self : not null access JSON_Stream'Class) is
   begin
      case Self.Writable is
         when True =>
            End_Object (Self.W);
         when False =>
            End_Object (Self.R);
      end case;
   end End_Object;

   procedure End_Object (Self : in out Read_Stream) renames Pop;

   ----------------
   -- End_Object --
   ----------------

   procedure End_Object (Self : in out Write_Stream) is
   begin
      Self.Writer.End_Object;
      Self.Key := Magic.Strings.Empty_Magic_String;
   end End_Object;

   ------------------
   -- End_Of_Array --
   ------------------

   function End_Of_Array
    (Self : not null access JSON_Stream'Class) return Boolean is
   begin
      if Self.Writable then
         return True;
      else
         return
           (case Self.R.Current.Kind is
               when Array_State =>
                 Self.R.Current.Index > GNATCOLL.JSON.Length
                   (Self.R.Current.Current_Array),
               when Object_State => True);
      end if;
   end End_Of_Array;

   ---------
   -- Key --
   ---------

   procedure Key
     (Self : not null access JSON_Stream'Class;
      Key  : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String) is
   begin
      case Self.Writable is
         when True =>
            Self.W.Key := Magic.Strings.Conversions.To_Magic_String
              (To_UTF_8_String (Key));
         when False =>
            Self.R.Current.Key := Key;
      end case;
   end Key;

   ---------
   -- Key --
   ---------

   procedure Key
    (Self : not null access JSON_Stream'Class;
     Key  : Wide_String) is
   begin
      Self.Key (Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String (Key));
   end Key;

   ---------
   -- Pop --
   ---------

   procedure Pop (Self : in out Read_Stream) is
   begin
      Self.Current := Self.Stack.Last_Element;
      Self.Stack.Delete_Last;

      if Self.Current.Kind = Array_State then
         Self.Current.Index := Self.Current.Index + 1;
      end if;
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push
    (Self : in out Read_Stream;
     Kind : State_Kinds) is
   begin
      Self.Stack.Append (Self.Current);

      case Kind is
         when Array_State =>
            Self.Current :=
             (Array_State, GNATCOLL.JSON.Empty_Array, 1);
         when Object_State =>
            Self.Current :=
             (Kind           => Object_State,
              Current_Object => GNATCOLL.JSON.Create_Object,
              Key            => <>);
      end case;
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
    (Self : in out Read_Stream;
     Data : GNATCOLL.JSON.JSON_Array) is
   begin
      Self.Stack.Append (Self.Current);
      Self.Current := (Array_State, Data, 1);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
    (Self : in out Read_Stream;
     Data : GNATCOLL.JSON.JSON_Value) is
   begin
      Self.Stack.Append (Self.Current);
      Self.Current :=
       (Kind           => Object_State,
        Current_Object => Data,
        Key            => <>);
   end Push;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out JSON_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) is
   begin
      raise Program_Error;
   end Read;

   ----------
   -- Read --
   ----------

   function Read
    (Self : in out JSON_Stream'Class)
     return GNATCOLL.JSON.JSON_Value is (Read (Self.R));

   function Read
    (Self : in out Read_Stream)
       return GNATCOLL.JSON.JSON_Value is
   begin
      case Self.Current.Kind is
         when Array_State =>
            Self.Current.Index := Self.Current.Index + 1;

            return GNATCOLL.JSON.Get
              (Self.Current.Current_Array, Self.Current.Index - 1);

         when Object_State =>
            return GNATCOLL.JSON.Get
              (Self.Current.Current_Object,
               To_UTF_8_String (Self.Current.Key));
      end case;
   end Read;

   -----------------------
   -- Set_JSON_Document --
   -----------------------

   procedure Set_JSON_Document
     (Self : not null access JSON_Stream'Class;
      Data : GNATCOLL.JSON.JSON_Array) is
   begin
      Self.Writable := False;  --  Read-only stream
      Self.R.Current := (Array_State, Data, 1);
   end Set_JSON_Document;

   ----------------
   -- Set_Stream --
   ----------------

   procedure Set_Stream
     (Self   : in out JSON_Stream'Class;
      Stream : not null Magic.Text_Streams.Output_Text_Stream_Access) is
   begin
      Self.W.Writer.Set_Stream (Stream);
      Self.W.Writer.Start_Document;
   end Set_Stream;

   -----------------
   -- Start_Array --
   -----------------

   procedure Start_Array (Self : not null access JSON_Stream'Class) is
   begin
      case Self.Writable is
         when True =>
            Start_Array (Self.W);
         when False =>
            Start_Array (Self.R);
      end case;
   end Start_Array;

   -----------------
   -- Start_Array --
   -----------------

   procedure Start_Array (Self : in out Read_Stream) is
   begin
      case Self.Current.Kind is
         when Array_State =>
            if Self.Current.Index <=
              GNATCOLL.JSON.Length (Self.Current.Current_Array)
            then
               Push
                 (Self,
                  GNATCOLL.JSON.Get
                    (GNATCOLL.JSON.Get
                         (Self.Current.Current_Array, Self.Current.Index)));

            else
               Push (Self, Array_State);
            end if;

         when Object_State =>
            if Self.Current.Current_Object.Has_Field
              (To_UTF_8_String (Self.Current.Key))
            then
               Push
                 (Self,
                  GNATCOLL.JSON.Get
                    (GNATCOLL.JSON.Get
                         (Self.Current.Current_Object,
                          To_UTF_8_String (Self.Current.Key))));

            else
               Push (Self, Array_State);
            end if;
      end case;
   end Start_Array;

   -----------------
   -- Start_Array --
   -----------------

   procedure Start_Array (Self : in out Write_Stream) is
   begin
      Write_Key (Self);
      Self.Writer.Start_Array;
   end Start_Array;

   ------------------
   -- Start_Object --
   ------------------

   procedure Start_Object (Self : not null access JSON_Stream'Class) is
   begin
      case Self.Writable is
         when True =>
            Start_Object (Self.W);
         when False =>
            Start_Object (Self.R);
      end case;
   end Start_Object;

   ------------------
   -- Start_Object --
   ------------------

   procedure Start_Object (Self : in out Read_Stream) is
   begin
      case Self.Current.Kind is
         when Array_State =>
            if Self.Current.Index <=
              GNATCOLL.JSON.Length (Self.Current.Current_Array)
            then
               Push
                 (Self,
                  GNATCOLL.JSON.Get
                    (Self.Current.Current_Array, Self.Current.Index));

            else
               Push (Self, Object_State);
            end if;

         when Object_State =>
            if Self.Current.Current_Object.Has_Field
              (To_UTF_8_String (Self.Current.Key))
            then
               Push
                 (Self,
                  GNATCOLL.JSON.JSON_Value'
                    (GNATCOLL.JSON.Get
                       (Self.Current.Current_Object,
                        To_UTF_8_String (Self.Current.Key))));

            else
               Push (Self, Object_State);
            end if;
      end case;
   end Start_Object;

   ------------------
   -- Start_Object --
   ------------------

   procedure Start_Object (Self : in out Write_Stream) is
   begin
      Write_Key (Self);
      Self.Writer.Start_Object;
   end Start_Object;

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   function To_UTF_8_String
     (Value : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String)
      return Ada.Strings.UTF_Encoding.UTF_8_String is
   begin
      return
        Ada.Strings.UTF_Encoding.Wide_Strings.Encode
          (Ada.Strings.Wide_Unbounded.To_Wide_String (Value));
   end To_UTF_8_String;

   ---------------
   -- Write_Key --
   ---------------

   procedure Write_Key (Self : in out Write_Stream) is
   begin
      if not Self.Key.Is_Empty then
         Self.Writer.Key_Name (Self.Key);
         Self.Key := Magic.Strings.Empty_Magic_String;
      end if;
   end Write_Key;

   -----------
   -- Write --
   -----------

   procedure Write
    (Self : in out JSON_Stream'Class;
     Item : GNATCOLL.JSON.JSON_Value)
   is
      procedure Each_Field
        (Name  : String;
         Value : GNATCOLL.JSON.JSON_Value);
      --  Write "Name: Value" recursively

      ----------------
      -- Each_Field --
      ----------------

      procedure Each_Field
        (Name  : String;
         Value : GNATCOLL.JSON.JSON_Value) is
      begin
         Self.W.Writer.Key_Name
           (Magic.Strings.Conversions.To_Magic_String (Name));
         Self.Write (Value);
      end Each_Field;
   begin
      Write_Key (Self.W);

      case Item.Kind is
         when GNATCOLL.JSON.JSON_Null_Type =>
            Self.W.Writer.Null_Value;
         when GNATCOLL.JSON.JSON_Boolean_Type =>
            Self.W.Writer.Boolean_Value (Item.Get);
         when GNATCOLL.JSON.JSON_Int_Type =>
            Self.W.Writer.Integer_Value
              (Interfaces.Integer_64 (Long_Long_Integer'(Item.Get)));
         when GNATCOLL.JSON.JSON_Float_Type =>
            Self.W.Writer.Float_Value
              (Interfaces.IEEE_Float_64 (Float'(Item.Get)));
         when GNATCOLL.JSON.JSON_String_Type =>
            Self.W.Writer.String_Value
              (Magic.Strings.Conversions.To_Magic_String
                 (String'(Item.Get)));
         when GNATCOLL.JSON.JSON_Array_Type =>
            declare
               Vector : constant GNATCOLL.JSON.JSON_Array := Item.Get;
            begin
               Self.W.Writer.Start_Array;

               for J in 1 .. GNATCOLL.JSON.Length (Vector) loop
                  Self.Write (GNATCOLL.JSON.Get (Vector, J));
               end loop;

               Self.W.Writer.End_Array;
            end;
         when GNATCOLL.JSON.JSON_Object_Type =>
            Self.W.Writer.Start_Object;
            Item.Map_JSON_Object (Each_Field'Access);
            Self.W.Writer.End_Object;
      end case;
   end Write;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out JSON_Stream;
      Item   : Ada.Streams.Stream_Element_Array) is
   begin
      raise Program_Error;
   end Write;

end LSP.JSON_Streams;

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

package body LSP.JSON_Streams is

   procedure Update
    (Self  : not null access JSON_Stream'Class;
     Value : GNATCOLL.JSON.JSON_Value);
   --  Update (or add) value to result.

   procedure Push
    (Self  : not null access JSON_Stream'Class;
     Kind  : State_Kinds);
   procedure Push
    (Self : not null access JSON_Stream'Class;
     Data : GNATCOLL.JSON.JSON_Array);
   procedure Push
    (Self : not null access JSON_Stream'Class;
     Data : GNATCOLL.JSON.JSON_Value);
   --  Push current state into the stack and initialize new current state.

   procedure Pop (Self : not null access JSON_Stream'Class);
   --  Unwind state stack and add constructed value to new state.

   function To_UTF_8_String
     (Value : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String)
      return Ada.Strings.UTF_Encoding.UTF_8_String;
   --  Unbounded_Wide_String (UTF-16) to String (UTF-8) conversion.

   ---------------
   -- End_Array --
   ---------------

   procedure End_Array (Self : not null access JSON_Stream'Class) renames Pop;

   ----------------
   -- End_Object --
   ----------------

   procedure End_Object (Self : not null access JSON_Stream'Class) renames Pop;

   ------------------
   -- End_Of_Array --
   ------------------

   function End_Of_Array
    (Self : not null access JSON_Stream'Class) return Boolean is
   begin
      return
       (case Self.Current.Kind is
          when Array_State =>
             Self.Current.Index >
               GNATCOLL.JSON.Length (Self.Current.Current_Array),
          when Object_State => True);
   end End_Of_Array;

   -----------------------
   -- Get_JSON_Document --
   -----------------------

   function Get_JSON_Document
     (Self : not null access JSON_Stream'Class)
      return GNATCOLL.JSON.JSON_Array is
   begin
      return Self.Current.Current_Array;
   end Get_JSON_Document;

   ---------
   -- Key --
   ---------

   procedure Key
     (Self : not null access JSON_Stream'Class;
      Key  : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String) is
   begin
      Self.Current.Key := Key;
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

   procedure Pop (Self : not null access JSON_Stream'Class) is
      Modified : constant Boolean := Self.Current.Modified;
      Value    : constant GNATCOLL.JSON.JSON_Value
        := (case Self.Current.Kind is
               when Array_State => GNATCOLL.JSON.Create
                                     (Self.Current.Current_Array),
              when Object_State => Self.Current.Current_Object);

   begin
      Self.Current := Self.Stack.Last_Element;
      Self.Stack.Delete_Last;

      if Modified then
         Self.Update (Value);

      elsif Self.Current.Kind = Array_State then
         Self.Current.Index := Self.Current.Index + 1;
      end if;
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push
    (Self : not null access JSON_Stream'Class;
     Kind : State_Kinds) is
   begin
      Self.Stack.Append (Self.Current);

      case Kind is
         when Array_State =>
            Self.Current :=
             (Array_State, False, GNATCOLL.JSON.Empty_Array, 1);
         when Object_State =>
            Self.Current :=
             (Kind           => Object_State,
              Modified       => False,
              Current_Object => GNATCOLL.JSON.Create_Object,
              Key            => <>);
      end case;
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
    (Self : not null access JSON_Stream'Class;
     Data : GNATCOLL.JSON.JSON_Array) is
   begin
      Self.Stack.Append (Self.Current);
      Self.Current := (Array_State, False, Data, 1);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
    (Self : not null access JSON_Stream'Class;
     Data : GNATCOLL.JSON.JSON_Value) is
   begin
      Self.Stack.Append (Self.Current);
      Self.Current :=
       (Kind           => Object_State,
        Modified       => False,
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

      Self.Current := (Array_State, False, Data, 1);
   end Set_JSON_Document;

   -----------------
   -- Start_Array --
   -----------------

   procedure Start_Array (Self : not null access JSON_Stream'Class) is
   begin
      case Self.Current.Kind is
         when Array_State =>
            if Self.Current.Index <=
              GNATCOLL.JSON.Length (Self.Current.Current_Array)
            then
               Self.Push
                 (GNATCOLL.JSON.Get
                    (GNATCOLL.JSON.Get
                         (Self.Current.Current_Array, Self.Current.Index)));

            else
               Self.Push (Array_State);
            end if;

         when Object_State =>
            if Self.Current.Current_Object.Has_Field
              (To_UTF_8_String (Self.Current.Key))
            then
               Self.Push
                 (GNATCOLL.JSON.Get
                    (GNATCOLL.JSON.Get
                         (Self.Current.Current_Object,
                          To_UTF_8_String (Self.Current.Key))));

            else
               Self.Push (Array_State);
            end if;
      end case;
   end Start_Array;

   ------------------
   -- Start_Object --
   ------------------

   procedure Start_Object (Self : not null access JSON_Stream'Class) is
   begin
      case Self.Current.Kind is
         when Array_State =>
            if Self.Current.Index <=
              GNATCOLL.JSON.Length (Self.Current.Current_Array)
            then
               Self.Push
                 (GNATCOLL.JSON.Get
                    (Self.Current.Current_Array, Self.Current.Index));

            else
               Self.Push (Object_State);
            end if;

         when Object_State =>
            if Self.Current.Current_Object.Has_Field
              (To_UTF_8_String (Self.Current.Key))
            then
               Self.Push
                 (GNATCOLL.JSON.JSON_Value'
                    (GNATCOLL.JSON.Get
                       (Self.Current.Current_Object,
                        To_UTF_8_String (Self.Current.Key))));

            else
               Self.Push (Object_State);
            end if;
      end case;
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

   ------------
   -- Update --
   ------------

   procedure Update
    (Self  : not null access JSON_Stream'Class;
     Value : GNATCOLL.JSON.JSON_Value) is
   begin
      case Self.Current.Kind is
         when Array_State =>
            if Self.Current.Index <=
              GNATCOLL.JSON.Length (Self.Current.Current_Array)
            then
               GNATCOLL.JSON.Set_Element
                 (Self.Current.Current_Array,
                  Self.Current.Index,
                  Value);

            else
               GNATCOLL.JSON.Append (Self.Current.Current_Array, Value);
            end if;

            Self.Current.Index := Self.Current.Index + 1;

         when Object_State =>
            GNATCOLL.JSON.Set_Field
              (Self.Current.Current_Object,
               To_UTF_8_String (Self.Current.Key),
               Value);
      end case;

      Self.Current.Modified := True;
   end Update;

   -----------
   -- Write --
   -----------

   procedure Write
    (Self : in out JSON_Stream'Class;
     Item : GNATCOLL.JSON.JSON_Value) is
   begin
      Self.Update (Item);
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

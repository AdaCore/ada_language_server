--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Interfaces;
with LSP.Constants;

package body LSP.Structures.LSPAny_Vectors is

   use VSS.JSON;
   use VSS.JSON.Streams;
   use VSS.Strings;
   use LSP.Structures.JSON_Event_Vectors;

   -------------
   -- Add_Key --
   -------------

   procedure Add_Key
     (Key    : VSS.Strings.Virtual_String;
      Vector : in out LSPAny_Vector) is
   begin
      Vector.Append
        (JSON_Stream_Element'
           (Kind     => Key_Name,
            Key_Name => Key));
   end Add_Key;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Cursor : in out JSON_Event_Vectors.Cursor)
      return Integer is
   begin
      return Integer (Element (Cursor).Number_Value.Integer_Value);
   end From_Any;

   function From_Any
     (Cursor : in out JSON_Event_Vectors.Cursor)
      return Boolean is
   begin
      return Element (Cursor).Boolean_Value;
   end From_Any;

   function From_Any
     (Cursor : in out JSON_Event_Vectors.Cursor)
      return DocumentUri is
   begin
      if Has_Element (Cursor) then
         return (Element (Cursor).String_Value with null record);
      else
         return LSP.Constants.Empty;
      end if;
   end From_Any;

   function From_Any
     (Cursor : in out JSON_Event_Vectors.Cursor)
      return TextDocumentIdentifier
   is
      Result : TextDocumentIdentifier;
   begin
      pragma Assert (Element (Cursor).Kind = Start_Object);
      Next (Cursor);

      while Has_Element (Cursor)
        and then Element (Cursor).Kind /= End_Object
      loop
         pragma Assert (Element (Cursor).Kind = Key_Name);

         declare
            Key : constant Virtual_String := Element (Cursor).Key_Name;
         begin
            Next (Cursor);

            if Key = "uri" then
               Result.uri := From_Any (Cursor);

            else
               Skip_Value (Cursor);
            end if;
         end;
         Next (Cursor);
      end loop;

      return Result;
   end From_Any;

   function From_Any
     (Cursor : in out JSON_Event_Vectors.Cursor)
      return Position
   is
      Result : Position;
   begin
      pragma Assert (Element (Cursor).Kind = Start_Object);
      Next (Cursor);

      while Has_Element (Cursor)
        and then Element (Cursor).Kind /= End_Object
      loop
         pragma Assert (Element (Cursor).Kind = Key_Name);

         declare
            Key : constant Virtual_String := Element (Cursor).Key_Name;
         begin
            Next (Cursor);

            if Key = "line" then
               Result.line := From_Any (Cursor);

            elsif Key = "character" then
               Result.character := From_Any (Cursor);

            else
               Skip_Value (Cursor);
            end if;
         end;
         Next (Cursor);
      end loop;

      return Result;
   end From_Any;

   function From_Any
     (Cursor : in out LSP.Structures.JSON_Event_Vectors.Cursor)
      return TextDocumentPositionParams
   is
      Result : TextDocumentPositionParams;
   begin
      pragma Assert (Element (Cursor).Kind = Start_Object);
      Next (Cursor);

      while Has_Element (Cursor)
        and then Element (Cursor).Kind /= End_Object
      loop
         pragma Assert (Element (Cursor).Kind = Key_Name);

         declare
            Key : constant Virtual_String := Element (Cursor).Key_Name;
         begin
            Next (Cursor);

            if Key = "textDocument" then
               Result.textDocument := From_Any (Cursor);

            elsif Key = "position" then
               Result.position := From_Any (Cursor);

            else
               Skip_Value (Cursor);
            end if;
         end;
         Next (Cursor);
      end loop;

      return Result;
   end From_Any;

   function From_Any
     (Cursor : in out JSON_Event_Vectors.Cursor)
      return A_Range
   is
      Result : A_Range;
   begin
      pragma Assert (Element (Cursor).Kind = Start_Object);
      Next (Cursor);

      while Has_Element (Cursor)
        and then Element (Cursor).Kind /= End_Object
      loop
         pragma Assert (Element (Cursor).Kind = Key_Name);

         declare
            Key : constant Virtual_String := Element (Cursor).Key_Name;
         begin
            Next (Cursor);

            if Key = "start" then
               Result.start := From_Any (Cursor);

            elsif Key = "end" then
               Result.an_end := From_Any (Cursor);

            else
               Skip_Value (Cursor);
            end if;
         end;
         Next (Cursor);
      end loop;

      return Result;
   end From_Any;

   function From_Any
     (Cursor : in out JSON_Event_Vectors.Cursor)
      return Location
   is
      Result : Location;
   begin
      pragma Assert (Element (Cursor).Kind = Start_Object);
      Next (Cursor);

      while Has_Element (Cursor)
        and then Element (Cursor).Kind /= End_Object
      loop
         pragma Assert (Element (Cursor).Kind = Key_Name);

         declare
            Key : constant Virtual_String := Element (Cursor).Key_Name;
         begin
            Next (Cursor);

            if Key = "uri" then
               Result.uri := From_Any (Cursor);

            elsif Key = "range" then
               Result.a_range := From_Any (Cursor);

            else
               Skip_Value (Cursor);
            end if;
         end;

         Next (Cursor);
      end loop;

      return Result;
   end From_Any;

   function From_Any
     (Cursor : in out JSON_Event_Vectors.Cursor)
      return VSS.String_Vectors.Virtual_String_Vector
   is
      Result : VSS.String_Vectors.Virtual_String_Vector;
   begin
      pragma Assert (Element (Cursor).Kind = Start_Array);
      Next (Cursor);

      while Has_Element (Cursor)
        and then Element (Cursor).Kind /= End_Array
      loop
         Result.Append (Element (Cursor).String_Value);
         Next (Cursor);
      end loop;

      return Result;
   end From_Any;

   ----------------
   -- Skip_Value --
   ----------------

   procedure Skip_Value
     (Cursor : in out JSON_Event_Vectors.Cursor)
   is
      Count : Natural := 0;
   begin
      while Has_Element (Cursor)
        and then Count > 0
        and then Element (Cursor).Kind /= Key_Name
        and then Element (Cursor).Kind /= End_Object
      loop
         if Element (Cursor).Kind = Start_Object
           or else Element (Cursor).Kind = Start_Array
         then
            Count := Count + 1;

         elsif Element (Cursor).Kind = End_Object
           or else Element (Cursor).Kind = End_Array
         then
            Count := Count - 1;
         end if;

         Next (Cursor);
      end loop;

      if Element (Cursor).Kind = Key_Name then
         Previous (Cursor);
      end if;
   end Skip_Value;

   ------------
   -- To_Any --
   ------------

   procedure To_Any
     (Value  : Integer;
      Vector : in out LSPAny_Vector) is
   begin
      Vector.Append
        (JSON_Stream_Element'
           (Kind         => Number_Value,
            Number_Value => JSON_Number'
              (Kind          => JSON_Integer,
               String_Value  => <>,
               Integer_Value => Interfaces.Integer_64 (Value))));
   end To_Any;

   procedure To_Any
     (Value  : Boolean;
      Vector : in out LSPAny_Vector) is
   begin
      Vector.Append
        (JSON_Stream_Element'
           (Kind          => Boolean_Value,
            Boolean_Value => Value));
   end To_Any;

   procedure To_Any
     (String : VSS.Strings.Virtual_String;
      Vector : in out LSPAny_Vector) is
   begin
      Vector.Append
        (JSON_Stream_Element'
           (Kind         => String_Value,
            String_Value => String));
   end To_Any;

   procedure To_Any
     (Self   : DocumentUri;
      Vector : in out LSPAny_Vector) is
   begin
      To_Any (VSS.Strings.Virtual_String (Self), Vector);
   end To_Any;

   procedure To_Any
     (Self   : TextDocumentIdentifier;
      Vector : in out LSPAny_Vector) is
   begin
      Vector.Append (JSON_Stream_Element'(Kind => Start_Object));

      --  "uri"
      Add_Key ("uri", Vector);
      To_Any (Self.uri, Vector);

      Vector.Append (JSON_Stream_Element'(Kind => End_Object));
   end To_Any;

   procedure To_Any
     (Self   : Position;
      Vector : in out LSPAny_Vector) is
   begin
      Vector.Append (JSON_Stream_Element'(Kind => Start_Object));

      --  "line"
      Add_Key ("line", Vector);
      To_Any (Self.line, Vector);

      --  "character"
      Add_Key ("character", Vector);
      To_Any (Self.character, Vector);

      Vector.Append (JSON_Stream_Element'(Kind => End_Object));
   end To_Any;

   procedure To_Any
     (Self   : A_Range;
      Vector : in out LSPAny_Vector) is
   begin
      Vector.Append (JSON_Stream_Element'(Kind => Start_Object));

      --  "start"
      Add_Key ("start", Vector);
      To_Any (Self.start, Vector);

      --  "end"
      Add_Key ("end", Vector);
      To_Any (Self.an_end, Vector);

      Vector.Append (JSON_Stream_Element'(Kind => End_Object));
   end To_Any;

   procedure To_Any
     (Self   : Location;
      Vector : in out LSPAny_Vector) is
   begin
      Vector.Append (JSON_Stream_Element'(Kind => Start_Object));

      --  "uri"
      Add_Key ("uri", Vector);
      To_Any (Self.uri, Vector);

      --  "range"
      Add_Key ("range", Vector);
      To_Any (Self.a_range, Vector);

      Vector.Append (JSON_Stream_Element'(Kind => End_Object));
   end To_Any;

   procedure To_Any
     (Self   : TextDocumentPositionParams;
      Vector : in out LSPAny_Vector) is
   begin
      Vector.Append (JSON_Stream_Element'(Kind => Start_Object));
      Add_Key ("textDocument", Vector);
      To_Any (Self.textDocument, Vector);
      Add_Key ("position", Vector);
      To_Any (Self.position, Vector);
      Vector.Append (JSON_Stream_Element'(Kind => End_Object));
   end To_Any;

   procedure To_Any
     (Self   : VSS.String_Vectors.Virtual_String_Vector;
      Vector : in out LSPAny_Vector) is
   begin
      Vector.Append (JSON_Stream_Element'(Kind => Start_Array));

      for S of Self loop
         Vector.Append
           (JSON_Stream_Element'
              (Kind         => String_Value,
               String_Value => S));
      end loop;

      Vector.Append (JSON_Stream_Element'(Kind => End_Array));
   end To_Any;

end LSP.Structures.LSPAny_Vectors;

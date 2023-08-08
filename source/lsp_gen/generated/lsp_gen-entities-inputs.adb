--  Copyright <YEAR> <COPYRIGHT HOLDER>
--
--  Permission is hereby granted, free of charge, to any person obtaining a
--  copy of this software and associated documentation files (the "Software"),
--  to deal in the Software without restriction, including without limitation
--  the rights to use, copy, modify, merge, publish, distribute, sublicense,
--  and/or sell copies of the Software, and to permit persons to whom the
--  Software is furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
--  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
--  DEALINGS IN THE SOFTWARE.

pragma Ada_2022;
with Minimal_Perfect_Hash;
with VSS.JSON.Pull_Readers.Buffered;

package body LSP_Gen.Entities.Inputs is
   pragma Style_Checks (Off);
   use type VSS.JSON.JSON_Number_Kind;
   use type VSS.Strings.Virtual_String;

   procedure Input_Any_Value
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Any_Value'Class;
      Success : in out Boolean) is
      use type VSS.JSON.Streams.JSON_Stream_Element_Kind;
   begin
      case Reader.Element_Kind is
         when VSS.JSON.Streams.Start_Array =>
            Value.Append ((Kind => VSS.JSON.Streams.Start_Array));
            Reader.Read_Next;
            while Success and Reader.Element_Kind /= VSS.JSON.Streams.End_Array
            loop
               Input_Any_Value (Reader, Value, Success);
            end loop;
            Value.Append ((Kind => VSS.JSON.Streams.End_Array));
         when VSS.JSON.Streams.Start_Object =>
            Value.Append ((Kind => VSS.JSON.Streams.Start_Object));
            Reader.Read_Next;
            while Success and Reader.Element_Kind = VSS.JSON.Streams.Key_Name
            loop
               Value.Append (Reader.Element);
               Reader.Read_Next;
               Input_Any_Value (Reader, Value, Success);
            end loop;
            Value.Append ((Kind => VSS.JSON.Streams.End_Object));
         when VSS.JSON.Streams.String_Value | VSS.JSON.Streams.Number_Value
           | VSS.JSON.Streams.Boolean_Value | VSS.JSON.Streams.Null_Value =>
            Value.Append (Reader.Element);
         when others =>
            Success := False;
      end case;
      if Success then
         Reader.Read_Next;
      end if;
   end Input_Any_Value;

   package MapKeyType_name_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["Uri",
      "DocumentUri",
      "string",
      "integer"]);

   procedure Input_MapKeyType_name
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.MapKeyType_name;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           MapKeyType_name_Minimal_Perfect_Hash.Get_Index (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.MapKeyType_name'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_MapKeyType_name;

   package EnumerationType_name_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["string",
      "integer",
      "uinteger"]);

   procedure Input_EnumerationType_name
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.EnumerationType_name;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           EnumerationType_name_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.EnumerationType_name'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_EnumerationType_name;

   package BaseTypes_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["Uri",
      "DocumentUri",
      "integer",
      "uinteger",
      "decimal",
      "RegExp",
      "string",
      "boolean",
      "null"]);

   procedure Input_BaseTypes
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.BaseTypes;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           BaseTypes_Minimal_Perfect_Hash.Get_Index (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.BaseTypes'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_BaseTypes;

   package TypeKind_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["base",
      "reference",
      "array",
      "map",
      "and",
      "or",
      "tuple",
      "literal",
      "stringLiteral",
      "integerLiteral",
      "booleanLiteral"]);

   procedure Input_TypeKind
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.TypeKind;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           TypeKind_Minimal_Perfect_Hash.Get_Index (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.TypeKind'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_TypeKind;

   package MapKeyType_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["base",
      "reference"]);

   package AType_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["base",
      "reference",
      "array",
      "map",
      "and",
      "or",
      "tuple",
      "literal",
      "stringLiteral",
      "integerLiteral",
      "booleanLiteral"]);

   package Structure_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["documentation",
      "extends",
      "mixins",
      "name",
      "properties",
      "proposed",
      "since"]);

   procedure Input_Structure
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Structure;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 Structure_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  documentation
                     if Reader.Is_String_Value then
                        Value.documentation := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  extends
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : AType;
                           begin
                              Input_AType (Reader, Item, Success);
                              Value.extends.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  mixins
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : AType;
                           begin
                              Input_AType (Reader, Item, Success);
                              Value.mixins.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  name
                     if Reader.Is_String_Value then
                        Value.name := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  properties
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : Property;
                           begin
                              Input_Property (Reader, Item, Success);
                              Value.properties.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  proposed
                     if Reader.Is_Boolean_Value then
                        Value.proposed := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  since
                     if Reader.Is_String_Value then
                        Value.since := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_Structure;

   package Property_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["documentation",
      "name",
      "optional",
      "proposed",
      "since",
      "type"]);

   procedure Input_Property
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Property;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 Property_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  documentation
                     if Reader.Is_String_Value then
                        Value.documentation := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  name
                     if Reader.Is_String_Value then
                        Value.name := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  optional
                     if Reader.Is_Boolean_Value then
                        Value.optional := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  proposed
                     if Reader.Is_Boolean_Value then
                        Value.proposed := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  since
                     if Reader.Is_String_Value then
                        Value.since := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  type
                     Input_AType (Reader, Value.a_type, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_Property;

   package EnumerationEntry_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["documentation",
      "name",
      "proposed",
      "since",
      "value"]);

   procedure Input_EnumerationEntry
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out EnumerationEntry;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 EnumerationEntry_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  documentation
                     if Reader.Is_String_Value then
                        Value.documentation := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  name
                     if Reader.Is_String_Value then
                        Value.name := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  proposed
                     if Reader.Is_Boolean_Value then
                        Value.proposed := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  since
                     if Reader.Is_String_Value then
                        Value.since := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  value
                     Value.value :=
                       (False,
                        Integer => <>);
                     if Reader.Is_String_Value then
                        Value.value :=
                          (True,
                           Reader.String_Value);
                        Reader.Read_Next;
                     elsif Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.value.Integer :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_EnumerationEntry;

   package AndType_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["items",
      "kind"]);

   procedure Input_AndType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out AndType;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 AndType_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  items
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : AType;
                           begin
                              Input_AType (Reader, Item, Success);
                              Value.items.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  kind
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "and"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_AndType;

   package Notification_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["documentation",
      "method",
      "params",
      "proposed",
      "registrationOptions",
      "since"]);

   procedure Input_Notification
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Notification;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 Notification_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  documentation
                     if Reader.Is_String_Value then
                        Value.documentation := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  method
                     if Reader.Is_String_Value then
                        Value.method := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  params
                     Value.params :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_AType (Reader, Value.params.Value, Success);
                  when 4 =>  --  proposed
                     if Reader.Is_Boolean_Value then
                        Value.proposed := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  registrationOptions
                     Value.registrationOptions :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_AType
                       (Reader, Value.registrationOptions.Value, Success);
                  when 6 =>  --  since
                     if Reader.Is_String_Value then
                        Value.since := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_Notification;

   package MapKeyType_base_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["kind",
      "name"]);

   procedure Input_MapKeyType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out MapKeyType;
      Success : in out Boolean) is
      procedure Input_MapKeyType_base
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out MapKeyType_base;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    MapKeyType_base_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  kind
                        if Reader.Is_String_Value
                          and then Reader.String_Value = "base"
                        then
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 2 =>  --  name
                        Input_MapKeyType_name (Reader, Value.name, Success);
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_MapKeyType_base;

      use all type VSS.JSON.Streams.JSON_Stream_Element_Kind;

      Look_Ahead :
        VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
          (Reader'Access);

      Variant_Key : constant VSS.Strings.Virtual_String := "kind";
   begin
      Look_Ahead.Mark;
      if Success and Look_Ahead.Is_Start_Object then
         Look_Ahead.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Look_Ahead.Is_End_Object loop
         if not Look_Ahead.Is_Key_Name then
            Success := False;
         elsif Look_Ahead.Key_Name /= Variant_Key then
            Look_Ahead.Skip_Current_Value;
            Success := not Look_Ahead.Is_End_Object;
         elsif Look_Ahead.Read_Next = String_Value then
            declare
               Index : constant Natural :=
                 MapKeyType_Minimal_Perfect_Hash.Get_Index
                   (Look_Ahead.String_Value);
            begin
               Look_Ahead.Reset;
               Look_Ahead.Unmark;

               case Index is
                  when 1 =>  --  base
                     Value.Union :=
                       (Kind   => Enum.base,
                        others => <>);
                     Input_MapKeyType_base
                       (Look_Ahead, Value.Union.base, Success);
                  when 2 =>  --  reference
                     Value.Union :=
                       (Kind   => Enum.reference,
                        others => <>);
                     Input_ReferenceType
                       (Look_Ahead, Value.Union.reference, Success);
                  when others =>
                     Success := False;
               end case;

               return;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Look_Ahead.Read_Next;  --  skip End_Object
         Success := False;
      end if;
   end Input_MapKeyType;

   package Enumeration_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["documentation",
      "name",
      "proposed",
      "since",
      "supportsCustomValues",
      "type",
      "values"]);

   procedure Input_Enumeration
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enumeration;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 Enumeration_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  documentation
                     if Reader.Is_String_Value then
                        Value.documentation := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  name
                     if Reader.Is_String_Value then
                        Value.name := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  proposed
                     if Reader.Is_Boolean_Value then
                        Value.proposed := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  since
                     if Reader.Is_String_Value then
                        Value.since := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  supportsCustomValues
                     if Reader.Is_Boolean_Value then
                        Value.supportsCustomValues := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  type
                     Input_EnumerationType (Reader, Value.a_type, Success);
                  when 7 =>  --  values
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : EnumerationEntry;
                           begin
                              Input_EnumerationEntry (Reader, Item, Success);
                              Value.values.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_Enumeration;

   package OrType_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["items",
      "kind"]);

   procedure Input_OrType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out OrType;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 OrType_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  items
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : AType;
                           begin
                              Input_AType (Reader, Item, Success);
                              Value.items.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  kind
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "or"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_OrType;

   package MetaModel_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["enumerations",
      "notifications",
      "requests",
      "structures",
      "typeAliases"]);

   procedure Input_MetaModel
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out MetaModel;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 MetaModel_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  enumerations
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : Enumeration;
                           begin
                              Input_Enumeration (Reader, Item, Success);
                              Value.enumerations.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  notifications
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : Notification;
                           begin
                              Input_Notification (Reader, Item, Success);
                              Value.notifications.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  requests
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : Request;
                           begin
                              Input_Request (Reader, Item, Success);
                              Value.requests.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  structures
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : Structure;
                           begin
                              Input_Structure (Reader, Item, Success);
                              Value.structures.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  typeAliases
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : TypeAlias;
                           begin
                              Input_TypeAlias (Reader, Item, Success);
                              Value.typeAliases.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_MetaModel;

   package EnumerationType_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["kind",
      "name"]);

   procedure Input_EnumerationType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out EnumerationType;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 EnumerationType_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  kind
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "base"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  name
                     Input_EnumerationType_name (Reader, Value.name, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_EnumerationType;

   package StructureLiteral_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["documentation",
      "properties",
      "proposed",
      "since"]);

   procedure Input_StructureLiteral
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StructureLiteral;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StructureLiteral_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  documentation
                     if Reader.Is_String_Value then
                        Value.documentation := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  properties
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : Property;
                           begin
                              Input_Property (Reader, Item, Success);
                              Value.properties.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  proposed
                     if Reader.Is_Boolean_Value then
                        Value.proposed := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  since
                     if Reader.Is_String_Value then
                        Value.since := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StructureLiteral;

   package TupleType_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["items",
      "kind"]);

   procedure Input_TupleType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out TupleType;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 TupleType_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  items
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : AType;
                           begin
                              Input_AType (Reader, Item, Success);
                              Value.items.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  kind
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "tuple"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_TupleType;

   package TypeAlias_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["documentation",
      "name",
      "proposed",
      "since",
      "type"]);

   procedure Input_TypeAlias
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out TypeAlias;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 TypeAlias_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  documentation
                     if Reader.Is_String_Value then
                        Value.documentation := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  name
                     if Reader.Is_String_Value then
                        Value.name := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  proposed
                     if Reader.Is_Boolean_Value then
                        Value.proposed := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  since
                     if Reader.Is_String_Value then
                        Value.since := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  type
                     Input_AType (Reader, Value.a_type, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_TypeAlias;

   package IntegerLiteralType_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["kind",
      "value"]);

   procedure Input_IntegerLiteralType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out IntegerLiteralType;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 IntegerLiteralType_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  kind
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "integerLiteral"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  value
                     if Reader.Is_Number_Value then
                        if Reader.Number_Value.Kind = VSS.JSON.JSON_Integer
                        then
                           Value.value :=
                             Float (Reader.Number_Value.Integer_Value);
                        elsif Reader.Number_Value.Kind = VSS.JSON.JSON_Float
                        then
                           Value.value :=
                             Float (Reader.Number_Value.Float_Value);
                        else
                           Success := False;
                        end if;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_IntegerLiteralType;

   package Request_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["documentation",
      "errorData",
      "method",
      "params",
      "partialResult",
      "proposed",
      "registrationOptions",
      "result",
      "since"]);

   procedure Input_Request
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Request;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 Request_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  documentation
                     if Reader.Is_String_Value then
                        Value.documentation := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  errorData
                     Value.errorData :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_AType (Reader, Value.errorData.Value, Success);
                  when 3 =>  --  method
                     if Reader.Is_String_Value then
                        Value.method := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  params
                     Value.params :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_AType (Reader, Value.params.Value, Success);
                  when 5 =>  --  partialResult
                     Value.partialResult :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_AType (Reader, Value.partialResult.Value, Success);
                  when 6 =>  --  proposed
                     if Reader.Is_Boolean_Value then
                        Value.proposed := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  registrationOptions
                     Value.registrationOptions :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_AType
                       (Reader, Value.registrationOptions.Value, Success);
                  when 8 =>  --  result
                     Input_AType (Reader, Value.result, Success);
                  when 9 =>  --  since
                     if Reader.Is_String_Value then
                        Value.since := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_Request;

   package ArrayType_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["element",
      "kind"]);

   procedure Input_ArrayType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ArrayType;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ArrayType_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  element
                     Input_AType (Reader, Value.element.Element, Success);
                  when 2 =>  --  kind
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "array"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ArrayType;

   package ReferenceType_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["kind",
      "name"]);

   procedure Input_ReferenceType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ReferenceType;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ReferenceType_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  kind
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "reference"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  name
                     if Reader.Is_String_Value then
                        Value.name := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ReferenceType;

   package BooleanLiteralType_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["kind",
      "value"]);

   procedure Input_BooleanLiteralType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out BooleanLiteralType;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 BooleanLiteralType_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  kind
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "booleanLiteral"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  value
                     if Reader.Is_Boolean_Value then
                        Value.value := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_BooleanLiteralType;

   package BaseType_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["kind",
      "name"]);

   procedure Input_BaseType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out BaseType;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 BaseType_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  kind
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "base"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  name
                     Input_BaseTypes (Reader, Value.name, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_BaseType;

   package StructureLiteralType_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["kind",
      "value"]);

   procedure Input_StructureLiteralType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StructureLiteralType;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StructureLiteralType_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  kind
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "literal"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  value
                     Input_StructureLiteral (Reader, Value.value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StructureLiteralType;

   package StringLiteralType_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["kind",
      "value"]);

   procedure Input_StringLiteralType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StringLiteralType;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StringLiteralType_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  kind
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "stringLiteral"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  value
                     if Reader.Is_String_Value then
                        Value.value := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StringLiteralType;

   package MapType_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["key",
      "kind",
      "value"]);

   procedure Input_MapType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out MapType;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 MapType_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  key
                     Input_MapKeyType (Reader, Value.key, Success);
                  when 2 =>  --  kind
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "map"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  value
                     Input_AType (Reader, Value.value.Element, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_MapType;

   procedure Input_AType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out AType;
      Success : in out Boolean) is
      use all type VSS.JSON.Streams.JSON_Stream_Element_Kind;

      Look_Ahead :
        VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
          (Reader'Access);

      Variant_Key : constant VSS.Strings.Virtual_String := "kind";
   begin
      Look_Ahead.Mark;
      if Success and Look_Ahead.Is_Start_Object then
         Look_Ahead.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Look_Ahead.Is_End_Object loop
         if not Look_Ahead.Is_Key_Name then
            Success := False;
         elsif Look_Ahead.Key_Name /= Variant_Key then
            Look_Ahead.Skip_Current_Value;
            Success := not Look_Ahead.Is_End_Object;
         elsif Look_Ahead.Read_Next = String_Value then
            declare
               Index : constant Natural :=
                 AType_Minimal_Perfect_Hash.Get_Index
                   (Look_Ahead.String_Value);
            begin
               Look_Ahead.Reset;
               Look_Ahead.Unmark;

               case Index is
                  when 1 =>  --  base
                     Value.Union :=
                       (Kind   => Enum.base,
                        others => <>);
                     Input_BaseType (Look_Ahead, Value.Union.base, Success);
                  when 2 =>  --  reference
                     Value.Union :=
                       (Kind   => Enum.reference,
                        others => <>);
                     Input_ReferenceType
                       (Look_Ahead, Value.Union.reference, Success);
                  when 3 =>  --  an_array
                     Value.Union :=
                       (Kind   => Enum.an_array,
                        others => <>);
                     Input_ArrayType
                       (Look_Ahead, Value.Union.an_array, Success);
                  when 4 =>  --  map
                     Value.Union :=
                       (Kind   => Enum.map,
                        others => <>);
                     Input_MapType (Look_Ahead, Value.Union.map, Success);
                  when 5 =>  --  an_and
                     Value.Union :=
                       (Kind   => Enum.an_and,
                        others => <>);
                     Input_AndType (Look_Ahead, Value.Union.an_and, Success);
                  when 6 =>  --  a_or
                     Value.Union :=
                       (Kind   => Enum.a_or,
                        others => <>);
                     Input_OrType (Look_Ahead, Value.Union.a_or, Success);
                  when 7 =>  --  tuple
                     Value.Union :=
                       (Kind   => Enum.tuple,
                        others => <>);
                     Input_TupleType (Look_Ahead, Value.Union.tuple, Success);
                  when 8 =>  --  literal
                     Value.Union :=
                       (Kind   => Enum.literal,
                        others => <>);
                     Input_StructureLiteralType
                       (Look_Ahead, Value.Union.literal, Success);
                  when 9 =>  --  stringLiteral
                     Value.Union :=
                       (Kind   => Enum.stringLiteral,
                        others => <>);
                     Input_StringLiteralType
                       (Look_Ahead, Value.Union.stringLiteral, Success);
                  when 10 =>  --  integerLiteral
                     Value.Union :=
                       (Kind   => Enum.integerLiteral,
                        others => <>);
                     Input_IntegerLiteralType
                       (Look_Ahead, Value.Union.integerLiteral, Success);
                  when 11 =>  --  booleanLiteral
                     Value.Union :=
                       (Kind   => Enum.booleanLiteral,
                        others => <>);
                     Input_BooleanLiteralType
                       (Look_Ahead, Value.Union.booleanLiteral, Success);
                  when others =>
                     Success := False;
               end case;

               return;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Look_Ahead.Read_Next;  --  skip End_Object
         Success := False;
      end if;
   end Input_AType;

end LSP_Gen.Entities.Inputs;

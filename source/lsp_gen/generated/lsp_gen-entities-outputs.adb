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
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.schema.json.

with Interfaces;
package body LSP_Gen.Entities.Outputs is
   pragma Style_Checks (Off);
   procedure Output_Any_Value
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Any_Value'Class) is
   begin
      for Item of Value loop
         case Item.Kind is
            when VSS.JSON.Streams.Start_Array =>
               Handler.Start_Array;
            when VSS.JSON.Streams.End_Array =>
               Handler.End_Array;
            when VSS.JSON.Streams.Start_Object =>
               Handler.Start_Object;
            when VSS.JSON.Streams.End_Object =>
               Handler.End_Object;
            when VSS.JSON.Streams.Key_Name =>
               Handler.Key_Name (Item.Key_Name);
            when VSS.JSON.Streams.String_Value =>
               Handler.String_Value (Item.String_Value);
            when VSS.JSON.Streams.Number_Value =>
               Handler.Number_Value (Item.Number_Value);
            when VSS.JSON.Streams.Boolean_Value =>
               Handler.Boolean_Value (Item.Boolean_Value);
            when VSS.JSON.Streams.Null_Value =>
               Handler.Null_Value;
            when VSS.JSON.Streams.None =>
               null;
            when VSS.JSON.Streams.Invalid =>
               raise Program_Error;
            when VSS.JSON.Streams.Start_Document =>
               raise Program_Error;
            when VSS.JSON.Streams.End_Document =>
               raise Program_Error;
            when VSS.JSON.Streams.Comment =>
               raise Program_Error;
         end case;
      end loop;
   end Output_Any_Value;

   procedure Output_MapKeyType_name
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.MapKeyType_name) is
   begin
      case Value is
         when Enum.Uri =>
            Handler.String_Value ("Uri");
         when Enum.DocumentUri =>
            Handler.String_Value ("DocumentUri");
         when Enum.string =>
            Handler.String_Value ("string");
         when Enum.integer =>
            Handler.String_Value ("integer");
      end case;
   end Output_MapKeyType_name;

   procedure Output_EnumerationType_name
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.EnumerationType_name) is
   begin
      case Value is
         when Enum.string =>
            Handler.String_Value ("string");
         when Enum.integer =>
            Handler.String_Value ("integer");
         when Enum.uinteger =>
            Handler.String_Value ("uinteger");
      end case;
   end Output_EnumerationType_name;

   procedure Output_BaseTypes
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.BaseTypes) is
   begin
      case Value is
         when Enum.Uri =>
            Handler.String_Value ("Uri");
         when Enum.DocumentUri =>
            Handler.String_Value ("DocumentUri");
         when Enum.integer =>
            Handler.String_Value ("integer");
         when Enum.uinteger =>
            Handler.String_Value ("uinteger");
         when Enum.decimal =>
            Handler.String_Value ("decimal");
         when Enum.RegExp =>
            Handler.String_Value ("RegExp");
         when Enum.string =>
            Handler.String_Value ("string");
         when Enum.a_boolean =>
            Handler.String_Value ("boolean");
         when Enum.a_null =>
            Handler.String_Value ("null");
      end case;
   end Output_BaseTypes;

   procedure Output_TypeKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.TypeKind) is
   begin
      case Value is
         when Enum.base =>
            Handler.String_Value ("base");
         when Enum.reference =>
            Handler.String_Value ("reference");
         when Enum.an_array =>
            Handler.String_Value ("array");
         when Enum.map =>
            Handler.String_Value ("map");
         when Enum.an_and =>
            Handler.String_Value ("and");
         when Enum.a_or =>
            Handler.String_Value ("or");
         when Enum.tuple =>
            Handler.String_Value ("tuple");
         when Enum.literal =>
            Handler.String_Value ("literal");
         when Enum.stringLiteral =>
            Handler.String_Value ("stringLiteral");
         when Enum.integerLiteral =>
            Handler.String_Value ("integerLiteral");
         when Enum.booleanLiteral =>
            Handler.String_Value ("booleanLiteral");
      end case;
   end Output_TypeKind;

   procedure Output_Structure
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Structure) is
   begin
      Handler.Start_Object;
      if not Value.documentation.Is_Null then
         Handler.Key_Name ("documentation");
         Handler.String_Value (Value.documentation);
      end if;
      if Value.extends.Length > 0 then
         Handler.Key_Name ("extends");
         Handler.Start_Array;
         for J in 1 .. Value.extends.Length loop
            Output_AType (Handler, Value.extends (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.mixins.Length > 0 then
         Handler.Key_Name ("mixins");
         Handler.Start_Array;
         for J in 1 .. Value.mixins.Length loop
            Output_AType (Handler, Value.mixins (J));
         end loop;
         Handler.End_Array;
      end if;
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      Handler.Key_Name ("properties");
      Handler.Start_Array;
      for J in 1 .. Value.properties.Length loop
         Output_Property (Handler, Value.properties (J));
      end loop;
      Handler.End_Array;
      if Value.proposed then
         Handler.Key_Name ("proposed");
         Handler.Boolean_Value (Value.proposed);
      end if;
      if not Value.since.Is_Null then
         Handler.Key_Name ("since");
         Handler.String_Value (Value.since);
      end if;
      Handler.End_Object;
   end Output_Structure;

   procedure Output_Property
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Property) is
   begin
      Handler.Start_Object;
      if not Value.documentation.Is_Null then
         Handler.Key_Name ("documentation");
         Handler.String_Value (Value.documentation);
      end if;
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      if Value.optional then
         Handler.Key_Name ("optional");
         Handler.Boolean_Value (Value.optional);
      end if;
      if Value.proposed then
         Handler.Key_Name ("proposed");
         Handler.Boolean_Value (Value.proposed);
      end if;
      if not Value.since.Is_Null then
         Handler.Key_Name ("since");
         Handler.String_Value (Value.since);
      end if;
      Handler.Key_Name ("type");
      Output_AType (Handler, Value.a_type);
      Handler.End_Object;
   end Output_Property;

   procedure Output_EnumerationEntry
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : EnumerationEntry) is
   begin
      Handler.Start_Object;
      if not Value.documentation.Is_Null then
         Handler.Key_Name ("documentation");
         Handler.String_Value (Value.documentation);
      end if;
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      if Value.proposed then
         Handler.Key_Name ("proposed");
         Handler.Boolean_Value (Value.proposed);
      end if;
      if not Value.since.Is_Null then
         Handler.Key_Name ("since");
         Handler.String_Value (Value.since);
      end if;
      Handler.Key_Name ("value");
      if Value.value.Is_String then
         Handler.String_Value (Value.value.String);
      else
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.value.Integer)));
      end if;
      Handler.End_Object;
   end Output_EnumerationEntry;

   procedure Output_AndType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : AndType) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("items");
      Handler.Start_Array;
      for J in 1 .. Value.items.Length loop
         Output_AType (Handler, Value.items (J));
      end loop;
      Handler.End_Array;
      Handler.Key_Name ("kind");
      Handler.String_Value ("and");
      Handler.End_Object;
   end Output_AndType;

   procedure Output_Notification
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Notification) is
   begin
      Handler.Start_Object;
      if not Value.documentation.Is_Null then
         Handler.Key_Name ("documentation");
         Handler.String_Value (Value.documentation);
      end if;
      Handler.Key_Name ("method");
      Handler.String_Value (Value.method);
      if Value.params.Is_Set then
         Handler.Key_Name ("params");
         Output_AType (Handler, Value.params.Value);
      end if;
      if Value.proposed then
         Handler.Key_Name ("proposed");
         Handler.Boolean_Value (Value.proposed);
      end if;
      if Value.registrationOptions.Is_Set then
         Handler.Key_Name ("registrationOptions");
         Output_AType (Handler, Value.registrationOptions.Value);
      end if;
      if not Value.since.Is_Null then
         Handler.Key_Name ("since");
         Handler.String_Value (Value.since);
      end if;
      Handler.End_Object;
   end Output_Notification;

   procedure Output_MapKeyType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : MapKeyType) is
      procedure Output_MapKeyType_base
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : MapKeyType_base) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("kind");
         Handler.String_Value ("base");
         Handler.Key_Name ("name");
         Output_MapKeyType_name (Handler, Value.name);
         Handler.End_Object;
      end Output_MapKeyType_base;

   begin
      case Value.Union.Kind is
         when Enum.base =>
            Output_MapKeyType_base (Handler, Value.Union.base);
         when Enum.reference =>
            Output_ReferenceType (Handler, Value.Union.reference);
      end case;
   end Output_MapKeyType;

   procedure Output_Enumeration
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enumeration) is
   begin
      Handler.Start_Object;
      if not Value.documentation.Is_Null then
         Handler.Key_Name ("documentation");
         Handler.String_Value (Value.documentation);
      end if;
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      if Value.proposed then
         Handler.Key_Name ("proposed");
         Handler.Boolean_Value (Value.proposed);
      end if;
      if not Value.since.Is_Null then
         Handler.Key_Name ("since");
         Handler.String_Value (Value.since);
      end if;
      if Value.supportsCustomValues then
         Handler.Key_Name ("supportsCustomValues");
         Handler.Boolean_Value (Value.supportsCustomValues);
      end if;
      Handler.Key_Name ("type");
      Output_EnumerationType (Handler, Value.a_type);
      Handler.Key_Name ("values");
      Handler.Start_Array;
      for J in 1 .. Value.values.Length loop
         Output_EnumerationEntry (Handler, Value.values (J));
      end loop;
      Handler.End_Array;
      Handler.End_Object;
   end Output_Enumeration;

   procedure Output_OrType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : OrType) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("items");
      Handler.Start_Array;
      for J in 1 .. Value.items.Length loop
         Output_AType (Handler, Value.items (J));
      end loop;
      Handler.End_Array;
      Handler.Key_Name ("kind");
      Handler.String_Value ("or");
      Handler.End_Object;
   end Output_OrType;

   procedure Output_MetaModel
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : MetaModel) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("enumerations");
      Handler.Start_Array;
      for J in 1 .. Value.enumerations.Length loop
         Output_Enumeration (Handler, Value.enumerations (J));
      end loop;
      Handler.End_Array;
      Handler.Key_Name ("notifications");
      Handler.Start_Array;
      for J in 1 .. Value.notifications.Length loop
         Output_Notification (Handler, Value.notifications (J));
      end loop;
      Handler.End_Array;
      Handler.Key_Name ("requests");
      Handler.Start_Array;
      for J in 1 .. Value.requests.Length loop
         Output_Request (Handler, Value.requests (J));
      end loop;
      Handler.End_Array;
      Handler.Key_Name ("structures");
      Handler.Start_Array;
      for J in 1 .. Value.structures.Length loop
         Output_Structure (Handler, Value.structures (J));
      end loop;
      Handler.End_Array;
      Handler.Key_Name ("typeAliases");
      Handler.Start_Array;
      for J in 1 .. Value.typeAliases.Length loop
         Output_TypeAlias (Handler, Value.typeAliases (J));
      end loop;
      Handler.End_Array;
      Handler.End_Object;
   end Output_MetaModel;

   procedure Output_EnumerationType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : EnumerationType) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Handler.String_Value ("base");
      Handler.Key_Name ("name");
      Output_EnumerationType_name (Handler, Value.name);
      Handler.End_Object;
   end Output_EnumerationType;

   procedure Output_StructureLiteral
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StructureLiteral) is
   begin
      Handler.Start_Object;
      if not Value.documentation.Is_Null then
         Handler.Key_Name ("documentation");
         Handler.String_Value (Value.documentation);
      end if;
      Handler.Key_Name ("properties");
      Handler.Start_Array;
      for J in 1 .. Value.properties.Length loop
         Output_Property (Handler, Value.properties (J));
      end loop;
      Handler.End_Array;
      if Value.proposed then
         Handler.Key_Name ("proposed");
         Handler.Boolean_Value (Value.proposed);
      end if;
      if not Value.since.Is_Null then
         Handler.Key_Name ("since");
         Handler.String_Value (Value.since);
      end if;
      Handler.End_Object;
   end Output_StructureLiteral;

   procedure Output_TupleType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : TupleType) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("items");
      Handler.Start_Array;
      for J in 1 .. Value.items.Length loop
         Output_AType (Handler, Value.items (J));
      end loop;
      Handler.End_Array;
      Handler.Key_Name ("kind");
      Handler.String_Value ("tuple");
      Handler.End_Object;
   end Output_TupleType;

   procedure Output_TypeAlias
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : TypeAlias) is
   begin
      Handler.Start_Object;
      if not Value.documentation.Is_Null then
         Handler.Key_Name ("documentation");
         Handler.String_Value (Value.documentation);
      end if;
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      if Value.proposed then
         Handler.Key_Name ("proposed");
         Handler.Boolean_Value (Value.proposed);
      end if;
      if not Value.since.Is_Null then
         Handler.Key_Name ("since");
         Handler.String_Value (Value.since);
      end if;
      Handler.Key_Name ("type");
      Output_AType (Handler, Value.a_type);
      Handler.End_Object;
   end Output_TypeAlias;

   procedure Output_IntegerLiteralType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : IntegerLiteralType) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Handler.String_Value ("integerLiteral");
      Handler.Key_Name ("value");
      Handler.Float_Value (Interfaces.IEEE_Float_64 (Value.value));
      Handler.End_Object;
   end Output_IntegerLiteralType;

   procedure Output_Request
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Request) is
   begin
      Handler.Start_Object;
      if not Value.documentation.Is_Null then
         Handler.Key_Name ("documentation");
         Handler.String_Value (Value.documentation);
      end if;
      if Value.errorData.Is_Set then
         Handler.Key_Name ("errorData");
         Output_AType (Handler, Value.errorData.Value);
      end if;
      Handler.Key_Name ("method");
      Handler.String_Value (Value.method);
      if Value.params.Is_Set then
         Handler.Key_Name ("params");
         Output_AType (Handler, Value.params.Value);
      end if;
      if Value.partialResult.Is_Set then
         Handler.Key_Name ("partialResult");
         Output_AType (Handler, Value.partialResult.Value);
      end if;
      if Value.proposed then
         Handler.Key_Name ("proposed");
         Handler.Boolean_Value (Value.proposed);
      end if;
      if Value.registrationOptions.Is_Set then
         Handler.Key_Name ("registrationOptions");
         Output_AType (Handler, Value.registrationOptions.Value);
      end if;
      Handler.Key_Name ("result");
      Output_AType (Handler, Value.result);
      if not Value.since.Is_Null then
         Handler.Key_Name ("since");
         Handler.String_Value (Value.since);
      end if;
      Handler.End_Object;
   end Output_Request;

   procedure Output_ArrayType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ArrayType) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("element");
      Output_AType (Handler, Value.element.Value);
      Handler.Key_Name ("kind");
      Handler.String_Value ("array");
      Handler.End_Object;
   end Output_ArrayType;

   procedure Output_ReferenceType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ReferenceType) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Handler.String_Value ("reference");
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      Handler.End_Object;
   end Output_ReferenceType;

   procedure Output_BooleanLiteralType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : BooleanLiteralType) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Handler.String_Value ("booleanLiteral");
      Handler.Key_Name ("value");
      Handler.Boolean_Value (Value.value);
      Handler.End_Object;
   end Output_BooleanLiteralType;

   procedure Output_BaseType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : BaseType) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Handler.String_Value ("base");
      Handler.Key_Name ("name");
      Output_BaseTypes (Handler, Value.name);
      Handler.End_Object;
   end Output_BaseType;

   procedure Output_StructureLiteralType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StructureLiteralType) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Handler.String_Value ("literal");
      Handler.Key_Name ("value");
      Output_StructureLiteral (Handler, Value.value);
      Handler.End_Object;
   end Output_StructureLiteralType;

   procedure Output_StringLiteralType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StringLiteralType) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Handler.String_Value ("stringLiteral");
      Handler.Key_Name ("value");
      Handler.String_Value (Value.value);
      Handler.End_Object;
   end Output_StringLiteralType;

   procedure Output_MapType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : MapType) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("key");
      Output_MapKeyType (Handler, Value.key);
      Handler.Key_Name ("kind");
      Handler.String_Value ("map");
      Handler.Key_Name ("value");
      Output_AType (Handler, Value.value.Value);
      Handler.End_Object;
   end Output_MapType;

   procedure Output_AType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : AType) is
   begin
      case Value.Union.Kind is
         when Enum.base =>
            Output_BaseType (Handler, Value.Union.base);
         when Enum.reference =>
            Output_ReferenceType (Handler, Value.Union.reference);
         when Enum.an_array =>
            Output_ArrayType (Handler, Value.Union.an_array);
         when Enum.map =>
            Output_MapType (Handler, Value.Union.map);
         when Enum.an_and =>
            Output_AndType (Handler, Value.Union.an_and);
         when Enum.a_or =>
            Output_OrType (Handler, Value.Union.a_or);
         when Enum.tuple =>
            Output_TupleType (Handler, Value.Union.tuple);
         when Enum.literal =>
            Output_StructureLiteralType (Handler, Value.Union.literal);
         when Enum.stringLiteral =>
            Output_StringLiteralType (Handler, Value.Union.stringLiteral);
         when Enum.integerLiteral =>
            Output_IntegerLiteralType (Handler, Value.Union.integerLiteral);
         when Enum.booleanLiteral =>
            Output_BooleanLiteralType (Handler, Value.Union.booleanLiteral);
      end case;
   end Output_AType;

end LSP_Gen.Entities.Outputs;

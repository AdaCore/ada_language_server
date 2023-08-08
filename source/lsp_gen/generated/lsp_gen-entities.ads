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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;
with VSS.JSON.Streams;
with VSS.Strings;
with VSS.String_Vectors;

package LSP_Gen.Entities is
   package JSON_Event_Lists is new Ada.Containers.Doubly_Linked_Lists
     (VSS.JSON.Streams.JSON_Stream_Element, VSS.JSON.Streams."=");

   type Any_Value is new JSON_Event_Lists.List with null record;
   type Any_Object is new Any_Value with null record;

   type Optional_Integer (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : Integer;
         when False =>
            null;
      end case;
   end record;

   type Optional_Float (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : Float;
         when False =>
            null;
      end case;
   end record;

   type Integer_Or_String (Is_String : Boolean := False) is record
      case Is_String is
         when False =>
            Integer : Standard.Integer;
         when True =>
            String : VSS.Strings.Virtual_String;
      end case;
   end record;

   type Optional_Integer_Or_String (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : Integer_Or_String;
         when False =>
            null;
      end case;
   end record;

   type Property_Vector is tagged private with
     Variable_Indexing => Get_Property_Variable_Reference,
     Constant_Indexing => Get_Property_Constant_Reference;

   type Request_Vector is tagged private with
     Variable_Indexing => Get_Request_Variable_Reference,
     Constant_Indexing => Get_Request_Constant_Reference;

   type Integer_Vector is tagged private with
     Variable_Indexing => Get_Integer_Variable_Reference,
     Constant_Indexing => Get_Integer_Constant_Reference;

   type TypeAlias_Vector is tagged private with
     Variable_Indexing => Get_TypeAlias_Variable_Reference,
     Constant_Indexing => Get_TypeAlias_Constant_Reference;

   type Structure_Vector is tagged private with
     Variable_Indexing => Get_Structure_Variable_Reference,
     Constant_Indexing => Get_Structure_Constant_Reference;

   type Notification_Vector is tagged private with
     Variable_Indexing => Get_Notification_Variable_Reference,
     Constant_Indexing => Get_Notification_Constant_Reference;

   type EnumerationEntry_Vector is tagged private with
     Variable_Indexing => Get_EnumerationEntry_Variable_Reference,
     Constant_Indexing => Get_EnumerationEntry_Constant_Reference;

   type Enumeration_Vector is tagged private with
     Variable_Indexing => Get_Enumeration_Variable_Reference,
     Constant_Indexing => Get_Enumeration_Constant_Reference;

   type AType_Vector is tagged private with
     Variable_Indexing => Get_AType_Variable_Reference,
     Constant_Indexing => Get_AType_Constant_Reference;

   type AType_Holder is tagged private;

   package Enum is

      type MapKeyType_name is (Uri, DocumentUri, string, integer);

      type EnumerationType_name is (string, integer, uinteger);

      type BaseTypes is
        (Uri, DocumentUri, integer, uinteger, decimal, RegExp, string,
         a_boolean, a_null);

      type TypeKind is
        (base, reference, an_array, map, an_and, a_or, tuple, literal,
         stringLiteral, integerLiteral, booleanLiteral);

      type MapKeyType_Variant is (base, reference);

      type AType_Variant is
        (base, reference, an_array, map, an_and, a_or, tuple, literal,
         stringLiteral, integerLiteral, booleanLiteral);

   end Enum;

   type Structure is record
      documentation : VSS.Strings.Virtual_String;
      --  An optional documentation;
      extends       : AType_Vector;
      --  Structures extended from. This structures form a polymorphic type
      --  hierarchy.
      mixins        : AType_Vector;
      --  Structures to mix in. The properties of these structures are `copied`
      --  into this structure. Mixins don't form a polymorphic type hierarchy
      --  in LSP.
      name          : VSS.Strings.Virtual_String;
      --  The name of the structure.
      properties    : Property_Vector;
      --  The properties.
      proposed      : Boolean := Boolean'First;
      --  Whether this is a proposed structure. If omitted, the structure is
      --  final.
      since         : VSS.Strings.Virtual_String;
      --  Since when (release number) this structure is available. Is undefined
      --  if not known.
   end record;

   type BaseType is record
      name : Enum.BaseTypes;
   end record;

   type ReferenceType is record
      name : VSS.Strings.Virtual_String;
   end record;

   type ArrayType is record
      element : AType_Holder;
   end record;

   type MapKeyType_base is record
      name : Enum.MapKeyType_name;
   end record;

   type Optional_MapKeyType_base (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : MapKeyType_base;
         when False =>
            null;
      end case;
   end record;

   type MapKeyType_Union
     (Kind : Enum.MapKeyType_Variant := Enum.MapKeyType_Variant'First) is
   record
      case Kind is
         when Enum.base =>
            base : MapKeyType_base;
         when Enum.reference =>
            reference : ReferenceType;
      end case;
   end record;

   type MapKeyType is record
      Union : MapKeyType_Union;
   end record;

   type MapType is record
      key   : MapKeyType;
      value : AType_Holder;
   end record;

   type AndType is record
      items : AType_Vector;
   end record;

   type OrType is record
      items : AType_Vector;
   end record;

   type TupleType is record
      items : AType_Vector;
   end record;

   type StructureLiteral is record
      documentation : VSS.Strings.Virtual_String;
      --  An optional documentation.
      properties    : Property_Vector;
      --  The properties.
      proposed      : Boolean := Boolean'First;
      --  Whether this is a proposed structure. If omitted, the structure is
      --  final.
      since         : VSS.Strings.Virtual_String;
      --  Since when (release number) this structure is available. Is undefined
      --  if not known.
   end record;

   type StructureLiteralType is record
      value : StructureLiteral;
   end record;

   type StringLiteralType is record
      value : VSS.Strings.Virtual_String;
   end record;

   type IntegerLiteralType is record
      value : Float;
   end record;

   type BooleanLiteralType is record
      value : Boolean;
   end record;

   type AType_Union (Kind : Enum.AType_Variant := Enum.AType_Variant'First) is
   record
      case Kind is
         when Enum.base =>
            base : BaseType;
         when Enum.reference =>
            reference : ReferenceType;
         when Enum.an_array =>
            an_array : ArrayType;
         when Enum.map =>
            map : MapType;
         when Enum.an_and =>
            an_and : AndType;
         when Enum.a_or =>
            a_or : OrType;
         when Enum.tuple =>
            tuple : TupleType;
         when Enum.literal =>
            literal : StructureLiteralType;
         when Enum.stringLiteral =>
            stringLiteral : StringLiteralType;
         when Enum.integerLiteral =>
            integerLiteral : IntegerLiteralType;
         when Enum.booleanLiteral =>
            booleanLiteral : BooleanLiteralType;
      end case;
   end record;

   type AType is record
      Union : AType_Union;
   end record;

   type Optional_AType (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : AType;
         when False =>
            null;
      end case;
   end record;

   type Property is record
      documentation : VSS.Strings.Virtual_String;
      --  An optional documentation.
      name          : VSS.Strings.Virtual_String;
      --  The property name;
      optional      : Boolean := Boolean'First;
      --  Whether the property is optional. If omitted, the property is
      --  mandatory.
      proposed      : Boolean := Boolean'First;
      --  Whether this is a proposed property. If omitted, the structure is
      --  final.
      since         : VSS.Strings.Virtual_String;
      --  Since when (release number) this property is available. Is undefined
      --  if not known.
      a_type        : AType;
      --  The type of the property
   end record;

   type EnumerationEntry is record
      documentation : VSS.Strings.Virtual_String;
      --  An optional documentation.
      name          : VSS.Strings.Virtual_String;
      --  The name of the enum item.
      proposed      : Boolean := Boolean'First;
      --  Whether this is a proposed enumeration entry. If omitted, the
      --  enumeration entry is final.
      since         : VSS.Strings.Virtual_String;
      --  Since when (release number) this enumeration entry is available. Is
      --  undefined if not known.
      value         : Integer_Or_String;
      --  The value.
   end record;

   type Notification is record
      documentation       : VSS.Strings.Virtual_String;
      --  An optional documentation;
      method              : VSS.Strings.Virtual_String;
      --  The request's method name.
      params              : Optional_AType;
      --  The parameter type(s) if any.
      proposed            : Boolean := Boolean'First;
      --  Whether this is a proposed notification. If omitted the notification
      --  is final.
      registrationOptions : Optional_AType;
      --  Optional registration options if the notification supports dynamic
      --  registration.
      since               : VSS.Strings.Virtual_String;
      --  Since when (release number) this notification is available. Is
      --  undefined if not known.
   end record;

   type EnumerationType is record
      name : Enum.EnumerationType_name;
   end record;

   type Enumeration is record
      documentation        : VSS.Strings.Virtual_String;
      --  An optional documentation.
      name                 : VSS.Strings.Virtual_String;
      --  The name of the enumeration.
      proposed             : Boolean := Boolean'First;
      --  Whether this is a proposed enumeration. If omitted, the enumeration
      --  is final.
      since                : VSS.Strings.Virtual_String;
      --  Since when (release number) this enumeration is available. Is
      --  undefined if not known.
      supportsCustomValues : Boolean := Boolean'First;
      --  Whether the enumeration supports custom values (e.g. values which are
      --  not part of the set defined in `values`). If omitted no custom values
      --  are supported.
      a_type               : EnumerationType;
      --  The type of the elements.
      values               : EnumerationEntry_Vector;
      --  The enum values.
   end record;

   type MetaModel is record
      enumerations  : Enumeration_Vector;
      --  The enumerations.
      notifications : Notification_Vector;
      --  The notifications.
      requests      : Request_Vector;
      --  The requests.
      structures    : Structure_Vector;
      --  The structures.
      typeAliases   : TypeAlias_Vector;
      --  The type aliases.
   end record;

   type TypeAlias is record
      documentation : VSS.Strings.Virtual_String;
      --  An optional documentation.
      name          : VSS.Strings.Virtual_String;
      --  The name of the type alias.
      proposed      : Boolean := Boolean'First;
      --  Whether this is a proposed type alias. If omitted, the type alias is
      --  final.
      since         : VSS.Strings.Virtual_String;
      --  Since when (release number) this structure is available. Is undefined
      --  if not known.
      a_type        : AType;
      --  The aliased type.
   end record;

   type Request is record
      documentation       : VSS.Strings.Virtual_String;
      --  An optional documentation;
      errorData           : Optional_AType;
      --  An optional error data type.
      method              : VSS.Strings.Virtual_String;
      --  The request's method name.
      params              : Optional_AType;
      --  The parameter type(s) if any.
      partialResult       : Optional_AType;
      --  Optional partial result type if the request supports partial result
      --  reporting.
      proposed            : Boolean := Boolean'First;
      --  Whether this is a proposed feature. If omitted the feature is final.
      registrationOptions : Optional_AType;
      --  Optional registration options if the request supports dynamic
      --  registration.
      result              : AType;
      --  The result type.
      since               : VSS.Strings.Virtual_String;
      --  Since when (release number) this request is available. Is undefined
      --  if not known.
   end record;

   function Length (Self : Property_Vector) return Natural;

   procedure Clear (Self : in out Property_Vector);

   procedure Append (Self : in out Property_Vector; Value : Property);

   type Property_Variable_Reference (Element : not null access Property) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Property_Variable_Reference
     (Self  : aliased in out Property_Vector;
      Index : Positive)
      return Property_Variable_Reference with
     Inline;

   type Property_Constant_Reference
     (Element : not null access constant Property) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Property_Constant_Reference
     (Self  : aliased Property_Vector;
      Index : Positive)
      return Property_Constant_Reference with
     Inline;

   function Length (Self : Request_Vector) return Natural;

   procedure Clear (Self : in out Request_Vector);

   procedure Append (Self : in out Request_Vector; Value : Request);

   type Request_Variable_Reference (Element : not null access Request) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Request_Variable_Reference
     (Self  : aliased in out Request_Vector;
      Index : Positive)
      return Request_Variable_Reference with
     Inline;

   type Request_Constant_Reference (Element : not null access constant Request)
   is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Request_Constant_Reference
     (Self  : aliased Request_Vector;
      Index : Positive)
      return Request_Constant_Reference with
     Inline;

   function Length (Self : Integer_Vector) return Natural;

   procedure Clear (Self : in out Integer_Vector);

   procedure Append (Self : in out Integer_Vector; Value : Integer);

   type Integer_Variable_Reference (Element : not null access Integer) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Integer_Variable_Reference
     (Self  : aliased in out Integer_Vector;
      Index : Positive)
      return Integer_Variable_Reference with
     Inline;

   type Integer_Constant_Reference (Element : not null access constant Integer)
   is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Integer_Constant_Reference
     (Self  : aliased Integer_Vector;
      Index : Positive)
      return Integer_Constant_Reference with
     Inline;

   function Length (Self : TypeAlias_Vector) return Natural;

   procedure Clear (Self : in out TypeAlias_Vector);

   procedure Append (Self : in out TypeAlias_Vector; Value : TypeAlias);

   type TypeAlias_Variable_Reference (Element : not null access TypeAlias) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_TypeAlias_Variable_Reference
     (Self  : aliased in out TypeAlias_Vector;
      Index : Positive)
      return TypeAlias_Variable_Reference with
     Inline;

   type TypeAlias_Constant_Reference
     (Element : not null access constant TypeAlias) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_TypeAlias_Constant_Reference
     (Self  : aliased TypeAlias_Vector;
      Index : Positive)
      return TypeAlias_Constant_Reference with
     Inline;

   function Length (Self : Structure_Vector) return Natural;

   procedure Clear (Self : in out Structure_Vector);

   procedure Append (Self : in out Structure_Vector; Value : Structure);

   type Structure_Variable_Reference (Element : not null access Structure) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Structure_Variable_Reference
     (Self  : aliased in out Structure_Vector;
      Index : Positive)
      return Structure_Variable_Reference with
     Inline;

   type Structure_Constant_Reference
     (Element : not null access constant Structure) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Structure_Constant_Reference
     (Self  : aliased Structure_Vector;
      Index : Positive)
      return Structure_Constant_Reference with
     Inline;

   function Length (Self : Notification_Vector) return Natural;

   procedure Clear (Self : in out Notification_Vector);

   procedure Append (Self : in out Notification_Vector; Value : Notification);

   type Notification_Variable_Reference
     (Element : not null access Notification) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Notification_Variable_Reference
     (Self  : aliased in out Notification_Vector;
      Index : Positive)
      return Notification_Variable_Reference with
     Inline;

   type Notification_Constant_Reference
     (Element : not null access constant Notification) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Notification_Constant_Reference
     (Self  : aliased Notification_Vector;
      Index : Positive)
      return Notification_Constant_Reference with
     Inline;

   function Length (Self : EnumerationEntry_Vector) return Natural;

   procedure Clear (Self : in out EnumerationEntry_Vector);

   procedure Append
     (Self : in out EnumerationEntry_Vector; Value : EnumerationEntry);

   type EnumerationEntry_Variable_Reference
     (Element : not null access EnumerationEntry) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_EnumerationEntry_Variable_Reference
     (Self  : aliased in out EnumerationEntry_Vector;
      Index : Positive)
      return EnumerationEntry_Variable_Reference with
     Inline;

   type EnumerationEntry_Constant_Reference
     (Element : not null access constant EnumerationEntry) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_EnumerationEntry_Constant_Reference
     (Self  : aliased EnumerationEntry_Vector;
      Index : Positive)
      return EnumerationEntry_Constant_Reference with
     Inline;

   function Length (Self : Enumeration_Vector) return Natural;

   procedure Clear (Self : in out Enumeration_Vector);

   procedure Append (Self : in out Enumeration_Vector; Value : Enumeration);

   type Enumeration_Variable_Reference (Element : not null access Enumeration)
   is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Enumeration_Variable_Reference
     (Self  : aliased in out Enumeration_Vector;
      Index : Positive)
      return Enumeration_Variable_Reference with
     Inline;

   type Enumeration_Constant_Reference
     (Element : not null access constant Enumeration) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Enumeration_Constant_Reference
     (Self  : aliased Enumeration_Vector;
      Index : Positive)
      return Enumeration_Constant_Reference with
     Inline;

   function Length (Self : AType_Vector) return Natural;

   procedure Clear (Self : in out AType_Vector);

   procedure Append (Self : in out AType_Vector; Value : AType);

   type AType_Variable_Reference (Element : not null access AType) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_AType_Variable_Reference
     (Self  : aliased in out AType_Vector;
      Index : Positive)
      return AType_Variable_Reference with
     Inline;

   type AType_Constant_Reference (Element : not null access constant AType) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_AType_Constant_Reference
     (Self  : aliased AType_Vector;
      Index : Positive)
      return AType_Constant_Reference with
     Inline;

   not overriding function Element
     (Self : aliased in out AType_Holder) return AType_Variable_Reference with
     Inline;

   not overriding function Value
     (Self : aliased AType_Holder) return AType_Constant_Reference with
     Inline;

private
   type Property_Array is array (Positive range <>) of aliased Property;
   type Property_Array_Access is access Property_Array;
   type Property_Vector is new Ada.Finalization.Controlled with record
      Data   : Property_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out Property_Vector);

   overriding procedure Finalize (Self : in out Property_Vector);

   type Request_Array is array (Positive range <>) of aliased Request;
   type Request_Array_Access is access Request_Array;
   type Request_Vector is new Ada.Finalization.Controlled with record
      Data   : Request_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out Request_Vector);

   overriding procedure Finalize (Self : in out Request_Vector);

   type Integer_Array is array (Positive range <>) of aliased Integer;
   type Integer_Array_Access is access Integer_Array;
   type Integer_Vector is new Ada.Finalization.Controlled with record
      Data   : Integer_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out Integer_Vector);

   overriding procedure Finalize (Self : in out Integer_Vector);

   type TypeAlias_Array is array (Positive range <>) of aliased TypeAlias;
   type TypeAlias_Array_Access is access TypeAlias_Array;
   type TypeAlias_Vector is new Ada.Finalization.Controlled with record
      Data   : TypeAlias_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out TypeAlias_Vector);

   overriding procedure Finalize (Self : in out TypeAlias_Vector);

   type Structure_Array is array (Positive range <>) of aliased Structure;
   type Structure_Array_Access is access Structure_Array;
   type Structure_Vector is new Ada.Finalization.Controlled with record
      Data   : Structure_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out Structure_Vector);

   overriding procedure Finalize (Self : in out Structure_Vector);

   type Notification_Array is
     array (Positive range <>) of aliased Notification;
   type Notification_Array_Access is access Notification_Array;
   type Notification_Vector is new Ada.Finalization.Controlled with record
      Data   : Notification_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out Notification_Vector);

   overriding procedure Finalize (Self : in out Notification_Vector);

   type EnumerationEntry_Array is
     array (Positive range <>) of aliased EnumerationEntry;
   type EnumerationEntry_Array_Access is access EnumerationEntry_Array;
   type EnumerationEntry_Vector is new Ada.Finalization.Controlled with record
      Data   : EnumerationEntry_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out EnumerationEntry_Vector);

   overriding procedure Finalize (Self : in out EnumerationEntry_Vector);

   type Enumeration_Array is array (Positive range <>) of aliased Enumeration;
   type Enumeration_Array_Access is access Enumeration_Array;
   type Enumeration_Vector is new Ada.Finalization.Controlled with record
      Data   : Enumeration_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out Enumeration_Vector);

   overriding procedure Finalize (Self : in out Enumeration_Vector);

   type AType_Array is array (Positive range <>) of aliased AType;
   type AType_Array_Access is access AType_Array;
   type AType_Vector is new Ada.Finalization.Controlled with record
      Data   : AType_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out AType_Vector);

   overriding procedure Finalize (Self : in out AType_Vector);

   type AType_Holder is new AType_Vector with null record;

end LSP_Gen.Entities;

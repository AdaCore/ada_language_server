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

with VSS.JSON.Pull_Readers;

package LSP_Gen.Entities.Inputs is

   procedure Input_MapKeyType_name
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.MapKeyType_name;
      Success : in out Boolean);

   procedure Input_EnumerationType_name
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.EnumerationType_name;
      Success : in out Boolean);

   procedure Input_BaseTypes
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.BaseTypes;
      Success : in out Boolean);

   procedure Input_TypeKind
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.TypeKind;
      Success : in out Boolean);

   procedure Input_Structure
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Structure;
      Success : in out Boolean);

   procedure Input_Property
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Property;
      Success : in out Boolean);

   procedure Input_EnumerationEntry
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out EnumerationEntry;
      Success : in out Boolean);

   procedure Input_AndType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out AndType;
      Success : in out Boolean);

   procedure Input_Notification
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Notification;
      Success : in out Boolean);

   procedure Input_MapKeyType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out MapKeyType;
      Success : in out Boolean);

   procedure Input_Enumeration
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enumeration;
      Success : in out Boolean);

   procedure Input_OrType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out OrType;
      Success : in out Boolean);

   procedure Input_MetaModel
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out MetaModel;
      Success : in out Boolean);

   procedure Input_EnumerationType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out EnumerationType;
      Success : in out Boolean);

   procedure Input_StructureLiteral
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StructureLiteral;
      Success : in out Boolean);

   procedure Input_TupleType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out TupleType;
      Success : in out Boolean);

   procedure Input_TypeAlias
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out TypeAlias;
      Success : in out Boolean);

   procedure Input_IntegerLiteralType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out IntegerLiteralType;
      Success : in out Boolean);

   procedure Input_Request
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Request;
      Success : in out Boolean);

   procedure Input_ArrayType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ArrayType;
      Success : in out Boolean);

   procedure Input_ReferenceType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ReferenceType;
      Success : in out Boolean);

   procedure Input_BooleanLiteralType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out BooleanLiteralType;
      Success : in out Boolean);

   procedure Input_BaseType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out BaseType;
      Success : in out Boolean);

   procedure Input_StructureLiteralType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StructureLiteralType;
      Success : in out Boolean);

   procedure Input_StringLiteralType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StringLiteralType;
      Success : in out Boolean);

   procedure Input_MapType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out MapType;
      Success : in out Boolean);

   procedure Input_AType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out AType;
      Success : in out Boolean);

end LSP_Gen.Entities.Inputs;

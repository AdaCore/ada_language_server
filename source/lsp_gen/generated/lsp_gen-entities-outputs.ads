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

with VSS.JSON.Content_Handlers;

package LSP_Gen.Entities.Outputs is

   procedure Output_MapKeyType_name
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.MapKeyType_name);

   procedure Output_EnumerationType_name
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.EnumerationType_name);

   procedure Output_BaseTypes
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.BaseTypes);

   procedure Output_TypeKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.TypeKind);

   procedure Output_Structure
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Structure);

   procedure Output_Property
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Property);

   procedure Output_EnumerationEntry
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : EnumerationEntry);

   procedure Output_AndType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : AndType);

   procedure Output_Notification
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Notification);

   procedure Output_MapKeyType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : MapKeyType);

   procedure Output_Enumeration
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enumeration);

   procedure Output_OrType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : OrType);

   procedure Output_MetaModel
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : MetaModel);

   procedure Output_EnumerationType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : EnumerationType);

   procedure Output_StructureLiteral
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StructureLiteral);

   procedure Output_TupleType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : TupleType);

   procedure Output_TypeAlias
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : TypeAlias);

   procedure Output_IntegerLiteralType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : IntegerLiteralType);

   procedure Output_Request
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Request);

   procedure Output_ArrayType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ArrayType);

   procedure Output_ReferenceType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ReferenceType);

   procedure Output_BooleanLiteralType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : BooleanLiteralType);

   procedure Output_BaseType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : BaseType);

   procedure Output_StructureLiteralType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StructureLiteralType);

   procedure Output_StringLiteralType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StringLiteralType);

   procedure Output_MapType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : MapType);

   procedure Output_AType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : AType);

end LSP_Gen.Entities.Outputs;

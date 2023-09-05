--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.String_Vectors;

package LSP.Structures.LSPAny_Vectors is
   pragma Preelaborate;

   procedure To_Any
     (Value  : Integer;
      Vector : in out LSPAny_Vector);

   function From_Any
     (Cursor : in out JSON_Event_Vectors.Cursor)
      return Integer;

   procedure To_Any
     (Value  : Boolean;
      Vector : in out LSPAny_Vector);

   function From_Any
     (Cursor : in out JSON_Event_Vectors.Cursor)
      return Boolean;

   procedure To_Any
     (String : VSS.Strings.Virtual_String;
      Vector : in out LSPAny_Vector);

   procedure To_Any
     (Self   : DocumentUri;
      Vector : in out LSPAny_Vector);

   function From_Any
     (Cursor : in out JSON_Event_Vectors.Cursor)
      return DocumentUri;

   procedure To_Any
     (Self   : Position;
      Vector : in out LSPAny_Vector);

   function From_Any
     (Cursor : in out JSON_Event_Vectors.Cursor)
      return Position;

   procedure To_Any
     (Self   : A_Range;
      Vector : in out LSPAny_Vector);

   function From_Any
     (Cursor : in out JSON_Event_Vectors.Cursor) return A_Range;

   procedure To_Any
     (Self   : Location;
      Vector : in out LSPAny_Vector);

   function From_Any
     (Cursor : in out JSON_Event_Vectors.Cursor)
      return Location;

   procedure To_Any
     (Self   : VSS.String_Vectors.Virtual_String_Vector;
      Vector : in out LSPAny_Vector);

   function From_Any
     (Cursor : in out JSON_Event_Vectors.Cursor)
      return VSS.String_Vectors.Virtual_String_Vector;

   procedure To_Any
     (Self   : TextDocumentPositionParams;
      Vector : in out LSPAny_Vector);

   function From_Any
     (Cursor : in out LSP.Structures.JSON_Event_Vectors.Cursor)
      return TextDocumentPositionParams;

   procedure To_Any
     (Self   : TextDocumentIdentifier;
      Vector : in out LSPAny_Vector);

   function From_Any
     (Cursor : in out JSON_Event_Vectors.Cursor)
      return TextDocumentIdentifier;

   procedure Add_Key
     (Key    : VSS.Strings.Virtual_String;
      Vector : in out LSPAny_Vector);

   procedure Skip_Value
     (Cursor : in out JSON_Event_Vectors.Cursor);

end LSP.Structures.LSPAny_Vectors;

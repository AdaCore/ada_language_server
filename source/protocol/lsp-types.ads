------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

--  This package provides basic types to implement Language Server Protocol.

with Ada.Containers.Vectors;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.Wide_Unbounded.Wide_Hash;
with Ada.Strings.Wide_Wide_Unbounded;
with GNATCOLL.JSON;

with VSS.String_Vectors;
with VSS.Strings;
with VSS.Unicode;

with LSP.Generic_Optional;

limited with LSP.JSON_Streams;

package LSP.Types is

   type LSP_Any is new GNATCOLL.JSON.JSON_Value with null record;
   procedure Read_Any
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP_Any);
   --  Read a value from JSON stream

   procedure Write_Any
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP_Any);
   --  Write a value to JSON stream

   for LSP_Any'Read use Read_Any;
   for LSP_Any'Write use Write_Any;

   function No_Any return LSP_Any;
   --  A null value of No_Any type
   function Empty return LSP_Any;
   --  An empty object value of No_Any type

   type LSP_Number is new Integer;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP_Number);
   --  Read a value from JSON stream

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP_Number);
   --  Write a value to JSON stream

   for LSP_Number'Read use Read;
   for LSP_Number'Write use Write;

   type LSP_String is new Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Types.LSP_String);
   --  Read string from the stream

   procedure Read_String
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out VSS.Strings.Virtual_String);
   --  Read string from the stream

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Types.LSP_String);
   --  Write string to the stream

   procedure Write_String
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : VSS.Strings.Virtual_String);
   --  Write string to the stream

   function Hash
     (Item : VSS.Strings.Virtual_String) return Ada.Containers.Hash_Type;

   --  Now we don't put 'Read/'Write clauses on LSP_String to be able to
   --  switch the LSP_String to the Virtual_String in the future.
   --   for LSP_String'Read use Read;
   --   for LSP_String'Write use Write;

   Empty_LSP_String : constant LSP_String :=
     LSP_String (Ada.Strings.Wide_Unbounded.Null_Unbounded_Wide_String);

   package LSP_String_Vectors is
     new Ada.Containers.Vectors (Positive, LSP_String, "=");

   type LSP_String_Vector is new LSP_String_Vectors.Vector with null record;
   --  Vector of strings

   procedure Read_LSP_String_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP_String_Vector);

   procedure Write_LSP_String_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP_String_Vector);

   for LSP_String_Vector'Read use Read_LSP_String_Vector;
   for LSP_String_Vector'Write use Write_LSP_String_Vector;

   Empty_Vector : constant LSP_String_Vector :=
     (LSP_String_Vectors.Vector with null record);

   procedure Read_String_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out VSS.String_Vectors.Virtual_String_Vector);

   procedure Write_String_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : VSS.String_Vectors.Virtual_String_Vector);

   function To_LSP_String (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
     return LSP_String;
   function To_LSP_String (Text : GNATCOLL.JSON.UTF8_Unbounded_String)
     return LSP_String;
   --  Convert given UTF-8 string into LSP_String
   function To_LSP_String (Text : Wide_Wide_String)
     return LSP_String;
   function To_LSP_String
     (Text : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String)
      return LSP_String
   is (To_LSP_String
       (Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String (Text)));
   function To_LSP_String
     (Item : VSS.Strings.Virtual_String) return LSP_String;

   function To_UTF_8_String (Value : LSP_String)
     return Ada.Strings.UTF_Encoding.UTF_8_String;
   --  Convert given LSP_String into UTF-8 string. Note that this
   --  allocates strings on the stack: if the string is potentially
   --  large (such as the content of a file), prefer calling
   --  To_UTF_8_Unbounded_String below.

   function To_UTF_8_Unbounded_String (Value : LSP_String)
     return GNATCOLL.JSON.UTF8_Unbounded_String;
   --  Same as To_UTF_8_String above, but returns an Unbounded_String.

   function To_Virtual_String
     (Item : LSP_String) return VSS.Strings.Virtual_String;
   --  Converts LSP_String to Virtual_String.

   function Is_Empty (Text : LSP_String) return Boolean;
   --  Check if given Text is an empty string

   function Starts_With
     (Text           : LSP_String;
      Prefix         : Ada.Strings.UTF_Encoding.UTF_8_String;
      Case_Sensitive : Boolean := True) return Boolean;
   --  Check if Text starts with given prefix

   function Ends_With
     (Text           : LSP_String;
      Suffix         : Ada.Strings.UTF_Encoding.UTF_8_String;
      Case_Sensitive : Boolean := True) return Boolean;
   --  Check if Text starts with given suffix

   function Hash (Text : LSP_String) return Ada.Containers.Hash_Type is
     (Ada.Strings.Wide_Unbounded.Wide_Hash
        (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Text)));
   --  Compute hash of the Text

   type LSP_Number_Or_String (Is_Number : Boolean := False) is record
      case Is_Number is
         when True =>
            Number : LSP_Number;
         when False =>
            String : VSS.Strings.Virtual_String;
      end case;
   end record;

   procedure Read_LSP_Number_Or_String
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP_Number_Or_String);
   --  Read a value from JSON stream

   procedure Write_LSP_Number_Or_String
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP_Number_Or_String);
   --  Write a value to JSON stream

   for LSP_Number_Or_String'Read use Read_LSP_Number_Or_String;
   for LSP_Number_Or_String'Write use Write_LSP_Number_Or_String;

   function Assigned (Id : LSP_Number_Or_String) return Boolean;
   --  Check if Id has an empty value

   function Hash
     (Item : LSP.Types.LSP_Number_Or_String) return Ada.Containers.Hash_Type;

   function To_Virtual_String (Item : LSP.Types.LSP_Number_Or_String)
      return VSS.Strings.Virtual_String;

   type LSP_Boolean_Or_String (Is_Boolean : Boolean := False) is record
      case Is_Boolean is
         when True =>
            Boolean : Standard.Boolean;
         when False =>
            String : LSP_String;
      end case;
   end record;

   procedure Read_LSP_Boolean_Or_String
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP_Boolean_Or_String);

   procedure Write_LSP_Boolean_Or_String
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP_Boolean_Or_String);
   for LSP_Boolean_Or_String'Read use Read_LSP_Boolean_Or_String;
   for LSP_Boolean_Or_String'Write use Write_LSP_Boolean_Or_String;

   type Line_Number is new LSP_Number range 0 .. LSP_Number'Last;
   --  Line number. In LSP first line has zero number
   subtype UTF_16_Index is VSS.Unicode.UTF16_Code_Unit_Index;
   --  LSP measures character position in UTF_16 code units starting from zero
   type Version_Id is new LSP_Number;
   --  Document version

   procedure Read_UTF16_Code_Unit_Count
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Item   : out VSS.Unicode.UTF16_Code_Unit_Count);
   procedure Write_UTF16_Code_Unit_Count
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Item   : VSS.Unicode.UTF16_Code_Unit_Count);

   ----------------------
   -- Optional_LSP_Any --
   ----------------------

   package Optional_LSP_Anys is new LSP.Generic_Optional (LSP_Any);
   type Optional_LSP_Any is new Optional_LSP_Anys.Optional_Type;

   ---------------------
   -- Optional_Number --
   ---------------------

   package Optional_Numbers is new LSP.Generic_Optional (LSP_Number);
   type Optional_Number is new Optional_Numbers.Optional_Type;

   ----------------------
   -- Optional_Boolean --
   ----------------------

   type Optional_Boolean (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : Boolean;
         when False =>
            null;
      end case;
   end record;

   procedure Read_Optional_Boolean
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Optional_Boolean);
   --  Read a value from JSON stream

   procedure Write_Optional_Boolean
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Optional_Boolean);
   --  Write a value to JSON stream

   for Optional_Boolean'Read use Read_Optional_Boolean;
   for Optional_Boolean'Write use Write_Optional_Boolean;

   function False return Optional_Boolean
     is ((Is_Set => Standard.True, Value => Standard.False));
   function True return Optional_Boolean
     is ((Is_Set => Standard.True, Value => Standard.True));
   function None return Optional_Boolean
     is ((Is_Set => Standard.False));

   --  Shortcut utilities

   procedure Read_Boolean
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Item   : out Boolean);

   procedure Write_Boolean
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : VSS.Strings.Virtual_String;
     Item   : Boolean);

   procedure Write_Optional_Boolean
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : VSS.Strings.Virtual_String;
     Item   : LSP.Types.Optional_Boolean);

   ---------------------
   -- Optional_String --
   ---------------------

   type Optional_String (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : LSP_String;
         when False =>
            null;
      end case;
   end record;

   procedure Read_Optional_String
     (S    : access Ada.Streams.Root_Stream_Type'Class;
      Item : out LSP.Types.Optional_String);
   --  Read optional string from the JSON stream

   procedure Write_Optional_String
     (S    : access Ada.Streams.Root_Stream_Type'Class;
      Item : LSP.Types.Optional_String);

   for Optional_String'Read use Read_Optional_String;
   for Optional_String'Write use Write_Optional_String;

   type Nullable_String (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : LSP_String;
         when False =>
            null;
      end case;
   end record;
   --  A type corresponding to `string | null` in TypeScript

   procedure Read_Nullable_String
     (S    : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Nullable_String);
   --  Read Nullable string from the JSON stream

   procedure Write_Nullable_String
     (S    : access Ada.Streams.Root_Stream_Type'Class;
      Item : Nullable_String);

   for Nullable_String'Read use Read_Nullable_String;
   for Nullable_String'Write use Write_Nullable_String;

   subtype MessageActionItem_Vector is LSP_String_Vector;

   type Registration_Option_Kinds is
     (Absent,
                    Text_Document_Registration_Option,
             Text_Document_Change_Registration_Option,
               Text_Document_Save_Registration_Option,
                       Completion_Registration_Option,
                   Signature_Help_Registration_Option,
                        Code_Lens_Registration_Option,
                    Document_Link_Registration_Option,
      Document_On_Type_Formatting_Registration_Option,
                  Execute_Command_Registration_Option,
         Did_Change_Watched_Files_Registration_Option,
                      Code_Action_Registration_Option,
                           Rename_Registration_Option);

   procedure Write_String
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : VSS.Strings.Virtual_String;
     Item   : LSP.Types.LSP_String);

   procedure Write_String
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : VSS.Strings.Virtual_String;
     Item   : VSS.Strings.Virtual_String);

   procedure Write_Number
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : VSS.Strings.Virtual_String;
     Item   : LSP.Types.LSP_Number);

   --  procedure Read_Number_Or_String
   --   (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
   --    Item   : out LSP.Types.LSP_Number_Or_String);
   --  --  Read number or string from the JSON stream

   procedure Write_Number_Or_String
     (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
      Key    : VSS.Strings.Virtual_String;
      Item   : LSP.Types.LSP_Number_Or_String);
   --  Write number or string from the JSON stream

   -------------------
   -- ProgressToken --
   -------------------

   subtype ProgressToken is LSP_Number_Or_String;

   type LSP_URI is new LSP_String;

   function Equal (Left, Right : LSP_URI) return Boolean renames "=";
   --  Let's try to avoid URI comparison.

   overriding function "=" (Left, Right : LSP_URI) return Boolean is abstract;
   --  Disable URI comparison with "=". Two URIs can point the same file
   --  even when they are distinct. In most case we shouldn't compare URIs,
   --  but find corresponding files and compare their normalized names.

   function File_To_URI (File : String) return LSP.Types.LSP_URI;
   --  Turn the File name into an URI

   function File_To_URI
     (File : Ada.Strings.Unbounded.Unbounded_String) return LSP.Types.LSP_URI;

   -----------------------------
   -- Optional_Virtual_String --
   -----------------------------

   type Optional_Virtual_String (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : VSS.Strings.Virtual_String;
         when False =>
            null;
      end case;
   end record;

   procedure Read_Optional_Virtual_String
     (S    : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Optional_Virtual_String);
   --  Read an optional VSS.Strings.Virtual_String from the JSON stream

   procedure Write_Optional_Virtual_String
     (S    : access Ada.Streams.Root_Stream_Type'Class;
      Item : Optional_Virtual_String);
   --  Read an optional VSS.Strings.Virtual_String to the JSON stream

   for Optional_Virtual_String'Read use Read_Optional_Virtual_String;
   for Optional_Virtual_String'Write use Write_Optional_Virtual_String;

end LSP.Types;

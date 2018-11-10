------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
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

with Ada.Containers.Vectors;
with Ada.Streams;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.Wide_Unbounded.Wide_Hash;
with GNATCOLL.JSON;
with LSP.Generic_Optional;

limited with LSP.JSON_Streams;

package LSP.Types is
--   pragma Preelaborate;

   subtype LSP_Any is GNATCOLL.JSON.JSON_Value;
   subtype LSP_Number is Natural;
   type LSP_String is new Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   not overriding procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Types.LSP_String);

   for LSP_String'Read use Read;

   not overriding procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Types.LSP_String);

   for LSP_String'Write use Write;
   package LSP_String_Vectors is new Ada.Containers.Vectors
     (Positive, LSP_String, "=");

   type LSP_String_Vector is new LSP_String_Vectors.Vector with null record;

   Empty_Vector : constant LSP_String_Vector :=
     (LSP_String_Vectors.Vector with null record);

   Empty_LSP_String : constant LSP_String :=
     LSP_String (Ada.Strings.Wide_Unbounded.Null_Unbounded_Wide_String);

   function To_LSP_String (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
     return LSP_String;

   function To_UTF_8_String (Value : LSP_String)
     return Ada.Strings.UTF_Encoding.UTF_8_String;

   function Is_Empty (Text : LSP_String) return Boolean;

   function Starts_With
     (Text   : LSP_String;
      Prefix : Ada.Strings.UTF_Encoding.UTF_8_String) return Boolean;

   function Hash (Text : LSP_String) return Ada.Containers.Hash_Type is
     (Ada.Strings.Wide_Unbounded.Wide_Hash
        (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Text)));

   type LSP_Number_Or_String (Is_Number : Boolean := False) is record
      case Is_Number is
         when True =>
            Number : LSP_Number;
         when False =>
            String : LSP_String;
      end case;
   end record;

   function Assigned (Id : LSP_Number_Or_String) return Boolean;
   --  Check if Id has an empty value

   type Line_Number is new Natural;
   type UTF_16_Index is new Natural;
   type Version_Id is new Natural;

   type Trace_Kinds is (Unspecified, Off, Messages, Verbose);

   package Optional_Numbers is new LSP.Generic_Optional (LSP_Number);
   type Optional_Number is new Optional_Numbers.Optional_Type;

   package Optional_Booleans is new LSP.Generic_Optional (Boolean);
   type Optional_Boolean is new Optional_Booleans.Optional_Type;

   package Optional_Strings is new LSP.Generic_Optional (LSP_String);
   type Optional_String is new Optional_Strings.Optional_Type;

   Optional_False : constant Optional_Boolean := (True, False);
   Optional_True  : constant Optional_Boolean := (True, True);

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
                  Execute_Command_Registration_Option);

   procedure Read_String
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out LSP.Types.LSP_String);

   procedure Read_Optional_String
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out LSP.Types.Optional_String);

   procedure Read_Number_Or_String
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out LSP.Types.LSP_Number_Or_String);

end LSP.Types;

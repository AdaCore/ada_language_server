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

with LSP.Messages;
with LSP.Messages.Client_Responses;
with LSP.Messages.Server_Responses;
with LSP.JSON_Streams;

with Ada.Command_Line;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Text_IO;
with Ada.Streams.Stream_IO;

with GNATCOLL.JSON;

with VSS.Stream_Element_Vectors.Conversions;
with VSS.Text_Streams.Memory_UTF8_Input;
with VSS.Text_Streams.Memory_UTF8_Output;
with VSS.JSON.Pull_Readers.Simple;

procedure Codec_Test is

   type Test_Access is access function
     (Input : VSS.Stream_Element_Vectors.Stream_Element_Vector)
      return VSS.Stream_Element_Vectors.Stream_Element_Vector;
   --  A function that converts Input to an Ada type and then converts it back
   --  to Stream_Element_Buffer.

   procedure Process_File
     (File_Name : String;
      Type_Name : String);
   --  Process one test file. Read JSON from File_Name and deserialize it into
   --  an object of given type, then serialize it tab and compare with origin.
   --  It prints found differences and set failure exit status if test fails.

   function Read_File (File_Name : String)
      return VSS.Stream_Element_Vectors.Stream_Element_Vector;
   --  Read content of the file and return it as a string

   procedure Register_Tests;
   --  Register all known codec test in Test_Map

   function Compare (Input, Output : GNATCOLL.JSON.JSON_Value) return Boolean;
   --  Compare two JSONs and raise error is they differ

   generic
      type Response is new LSP.Messages.ResponseMessage with private;
   function Generic_Response_Test
     (Input : VSS.Stream_Element_Vectors.Stream_Element_Vector)
      return VSS.Stream_Element_Vectors.Stream_Element_Vector;
   --  Generic codec for a response.

   package Test_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Test_Access,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   Test_Map : Test_Maps.Map;
   --  A map from Ada type name to corresponding codec test

   ---------------------------
   -- Generic_Response_Test --
   ---------------------------

   function Generic_Response_Test
     (Input : VSS.Stream_Element_Vectors.Stream_Element_Vector)
      return VSS.Stream_Element_Vectors.Stream_Element_Vector
   is
      Text_Input : aliased
        VSS.Text_Streams.Memory_UTF8_Input.Memory_UTF8_Input_Stream;
      Reader     : aliased
        VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
      In_JS      : aliased LSP.JSON_Streams.JSON_Stream (False, Reader'Access);
   begin
      Text_Input.Set_Data (Input);
      Reader.Set_Stream (Text_Input'Unchecked_Access);
      Reader.Read_Next;
      pragma Assert (Reader.Is_Start_Document);
      Reader.Read_Next;
      pragma Assert (Reader.Is_Start_Object);

      declare
         Out_JS : aliased LSP.JSON_Streams.JSON_Stream;
         Output : aliased
           VSS.Text_Streams.Memory_UTF8_Output.Memory_UTF8_Output_Stream;
         Object : Response (Is_Error => False);

      begin
         Out_JS.Set_Stream (Output'Unchecked_Access);
         Response'Read (In_JS'Access, Object);
         Response'Write (Out_JS'Access, Object);

         return Output.Buffer;
      end;
   end Generic_Response_Test;

   -------------
   -- Compare --
   -------------

   function Compare
     (Input, Output : GNATCOLL.JSON.JSON_Value) return Boolean
   is
      use type GNATCOLL.JSON.JSON_Value_Type;

      Fields_Match : Boolean := True;

      procedure Check_Input_Field
        (Field : String;
         Value : GNATCOLL.JSON.JSON_Value);

      procedure Check_Output_Field
        (Field : String;
         Value : GNATCOLL.JSON.JSON_Value);

      -----------------------
      -- Check_Input_Field --
      -----------------------

      procedure Check_Input_Field
        (Field : String;
         Value : GNATCOLL.JSON.JSON_Value) is
      begin
         if Output.Has_Field (Field) then
            if not Compare (Value, Output.Get (Field)) then
               Fields_Match := False;
               Ada.Text_IO.Put_Line ("Unmatched JSON field: " & Field);
            end if;
         else
            Ada.Text_IO.Put_Line ("No JSON field: " & Field);
         end if;
      end Check_Input_Field;

      ------------------------
      -- Check_Output_Field --
      ------------------------

      procedure Check_Output_Field
        (Field : String;
         Value : GNATCOLL.JSON.JSON_Value)
      is
         pragma Unreferenced (Value);
      begin
         if not Output.Has_Field (Field) then
            Ada.Text_IO.Put_Line ("Extra JSON field: " & Field);
         end if;
      end Check_Output_Field;

   begin
      if Input.Kind /= Output.Kind then
         Ada.Text_IO.Put_Line ("Unmatched JSON value kind");
         return False;
      end if;

      case Input.Kind is
         when GNATCOLL.JSON.JSON_Null_Type =>
            null;

         when GNATCOLL.JSON.JSON_Boolean_Type =>
            if Boolean'(Input.Get) /= Output.Get then
               Ada.Text_IO.Put_Line ("Unmatched JSON boolean value");
               return False;
            end if;

         when GNATCOLL.JSON.JSON_Int_Type =>
            if Integer'(Input.Get) /= Output.Get then
               Ada.Text_IO.Put_Line ("Unmatched JSON Integer value");
               return False;
            end if;

         when GNATCOLL.JSON.JSON_Float_Type =>
            if Float'(Input.Get) /= Output.Get then
               Ada.Text_IO.Put_Line ("Unmatched JSON Float value");
               return False;
            end if;

         when GNATCOLL.JSON.JSON_String_Type =>
            if String'(Input.Get) /= String'(Output.Get) then
               Ada.Text_IO.Put_Line ("Unmatched JSON String value");
               return False;
            end if;

         when GNATCOLL.JSON.JSON_Array_Type =>
            declare
               Left  : constant GNATCOLL.JSON.JSON_Array := Input.Get;
               Right : constant GNATCOLL.JSON.JSON_Array := Output.Get;
            begin
               if GNATCOLL.JSON.Length (Left) /=
                 GNATCOLL.JSON.Length (Right)
               then
                  Ada.Text_IO.Put_Line ("Unmatched JSON Array length");
                  return False;
               end if;

               for J in 1 .. GNATCOLL.JSON.Length (Left) loop
                  if not Compare
                    (GNATCOLL.JSON.Get (Left, J),
                     GNATCOLL.JSON.Get (Right, J))
                  then
                     Ada.Text_IO.Put_Line
                       ("Unmatched JSON Array element:" & J'Img);
                     return False;
                  end if;
               end loop;
            end;

         when GNATCOLL.JSON.JSON_Object_Type =>
            Input.Map_JSON_Object (Check_Input_Field'Access);
            Output.Map_JSON_Object (Check_Output_Field'Access);

            if not Fields_Match then
               Ada.Text_IO.Put_Line ("Unmatched JSON Object");
            end if;

            return Fields_Match;
      end case;

      return True;
   end Compare;

   ------------------
   -- Process_File --
   ------------------

   procedure Process_File
     (File_Name : String;
      Type_Name : String)
   is
      In_Buffer : constant VSS.Stream_Element_Vectors.Stream_Element_Vector
        := Read_File (File_Name);

      Out_Buffer : VSS.Stream_Element_Vectors.Stream_Element_Vector;
   begin
      Out_Buffer := Test_Map (Type_Name).all (In_Buffer);

      declare
         Input : constant GNATCOLL.JSON.JSON_Value :=
           GNATCOLL.JSON.Read
             (VSS.Stream_Element_Vectors.Conversions.Unchecked_To_String
                (In_Buffer),  File_Name);

         Output : constant GNATCOLL.JSON.JSON_Value :=
           GNATCOLL.JSON.Read
             (VSS.Stream_Element_Vectors.Conversions.Unchecked_To_String
                (Out_Buffer));
      begin
         if not Compare (Input, Output) then
            Ada.Text_IO.Put_Line
              ("Test FAILED: " & File_Name & " " & Type_Name);
            Ada.Text_IO.Put_Line (Output.Write);
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         end if;
      end;
   end Process_File;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (File_Name : String)
      return VSS.Stream_Element_Vectors.Stream_Element_Vector
   is
      use type Ada.Streams.Stream_Element_Count;

      Input  : Ada.Streams.Stream_IO.File_Type;
      Result : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Data   : Ada.Streams.Stream_Element_Array (1 .. 256);
      Last   : Ada.Streams.Stream_Element_Count;
   begin
      Ada.Streams.Stream_IO.Open
        (Input, Ada.Streams.Stream_IO.In_File, File_Name);

      loop
         Ada.Streams.Stream_IO.Read (Input, Data, Last);
         exit when Last = 0;

         for J of Data (1 .. Last) loop
            Result.Append (J);
         end loop;
      end loop;

      Ada.Streams.Stream_IO.Close (Input);

      return Result;
   end Read_File;

   function ApplyWorkspaceEdit_Response_Test is new Generic_Response_Test
       (LSP.Messages.Client_Responses.ApplyWorkspaceEdit_Response);

   function CodeAction_Response_Test is new Generic_Response_Test
     (LSP.Messages.Server_Responses.CodeAction_Response);

   function Completion_Response_Test is new Generic_Response_Test
     (LSP.Messages.Server_Responses.Completion_Response);

   function Highlight_Response_Test is new Generic_Response_Test
     (LSP.Messages.Server_Responses.Highlight_Response);

   function Hover_Response_Test is
     new Generic_Response_Test (LSP.Messages.Server_Responses.Hover_Response);

   function Initialize_Response_Test is new Generic_Response_Test
     (LSP.Messages.Server_Responses.Initialize_Response);

   function Location_Response_Test is new Generic_Response_Test
     (LSP.Messages.Server_Responses.Location_Response);

   function SignatureHelp_Response_Test is new Generic_Response_Test
     (LSP.Messages.Server_Responses.SignatureHelp_Response);

   function Symbol_Response_Test is new Generic_Response_Test
     (LSP.Messages.Server_Responses.Symbol_Response);

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests is
   begin
      Test_Map.Insert
        ("ApplyWorkspaceEdit_Response",
         ApplyWorkspaceEdit_Response_Test'Access);
      Test_Map.Insert ("CodeAction_Response", CodeAction_Response_Test'Access);
      Test_Map.Insert ("Completion_Response", Completion_Response_Test'Access);
      Test_Map.Insert ("Highlight_Response", Highlight_Response_Test'Access);
      Test_Map.Insert ("Hover_Response", Hover_Response_Test'Access);
      Test_Map.Insert ("Initialize_Response", Initialize_Response_Test'Access);
      Test_Map.Insert ("Location_Response", Location_Response_Test'Access);
      Test_Map.Insert
        ("SignatureHelp_Response", SignatureHelp_Response_Test'Access);
      Test_Map.Insert ("Symbol_Response", Symbol_Response_Test'Access);
   end Register_Tests;

begin
   Register_Tests;

   while not Ada.Text_IO.End_Of_File loop
      declare
         Line  : constant String := Ada.Text_IO.Get_Line;
         Space : constant Natural := Ada.Strings.Fixed.Index (Line, " ");
      begin
         if Line'Length > 0 and then Line (1) /= '#' then
            Process_File
              (File_Name => Line (1 .. Space - 1),
               Type_Name => Line (Space + 1 .. Line'Last));
         end if;
      end;
   end loop;
end Codec_Test;

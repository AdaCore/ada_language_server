--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings;

procedure LSP.Inputs.Read_ResponseError
  (Stream : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
   Value  : out LSP.Errors.ResponseError)
   is
   use type VSS.Strings.Virtual_String;

begin
   pragma Assert (Stream.Is_Start_Object);
   Stream.Read_Next;

   while not Stream.Is_End_Object loop
      pragma Assert (Stream.Is_Key_Name);

      declare
         Key : constant VSS.Strings.Virtual_String := Stream.Key_Name;

      begin
         Stream.Read_Next;

         if Key = "code" then
            LSP.Inputs.Read_ErrorCodes (Stream, Value.code);

         elsif Key = "message" then
            Value.message := Stream.String_Value;
            Stream.Read_Next;

         else
            Stream.Skip_Current_Value;
         end if;
      end;
   end loop;

   Stream.Read_Next;
end LSP.Inputs.Read_ResponseError;

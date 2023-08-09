------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                      Copyright (C) 2022-2023, AdaCore                    --
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

with VSS.JSON.Pull_Readers.JSON5;
with VSS.Strings.Character_Iterators;
with VSS.Text_Streams.File_Input;

package body LSP_Gen.Configurations is

   ---------------
   -- Direction --
   ---------------

   function Direction
     (Self   : Configuration;
      Method : VSS.Strings.Virtual_String)
      return Message_Direction is
        (if not Self.Map.Contains (Method) then From_Client
         else Self.Map (Method).Direction);

   ----------
   -- Load --
   ----------

   procedure Load
     (Self      : in out Configuration;
      File_Name : VSS.Strings.Virtual_String)
   is
      use type VSS.Strings.Virtual_String;

      procedure Read_Method
        (Reader  : in out VSS.JSON.Pull_Readers.JSON5.JSON5_Pull_Reader;
         Method  : VSS.Strings.Virtual_String);

      procedure Read_License_Header
        (Reader : in out VSS.JSON.Pull_Readers.JSON5.JSON5_Pull_Reader);

      -------------------------
      -- Read_License_Header --
      -------------------------

      procedure Read_License_Header
        (Reader : in out VSS.JSON.Pull_Readers.JSON5.JSON5_Pull_Reader) is
      begin
         pragma Assert (Reader.Is_Start_Array);
         Reader.Read_Next;

         while Reader.Is_String_Value loop
            Self.License.Append (Reader.String_Value);
            Reader.Read_Next;
         end loop;

         pragma Assert (Reader.Is_End_Array);
         Reader.Read_Next;
      end Read_License_Header;

      -----------------
      -- Read_Method --
      -----------------

      procedure Read_Method
        (Reader  : in out VSS.JSON.Pull_Readers.JSON5.JSON5_Pull_Reader;
         Method  : VSS.Strings.Virtual_String)
      is
         Info   : Message_Information;
      begin
         pragma Assert (Reader.Is_Start_Object);
         Reader.Read_Next;

         while Reader.Is_Key_Name loop

            if Reader.Key_Name = "from" then
               Reader.Read_Next;
               pragma Assert (Reader.Is_String_Value);

               Info.Direction :=
                 (if Reader.String_Value    = "client" then From_Client
                  elsif Reader.String_Value = "server" then From_Server
                  elsif Reader.String_Value = "both"   then From_Both
                  else raise Constraint_Error);

            elsif Reader.Key_Name = "name" then
               Reader.Read_Next;
               pragma Assert (Reader.Is_String_Value);

               Info.Name := Reader.String_Value;
            else
               raise Program_Error with "unexpected key";
            end if;

            Reader.Read_Next;
         end loop;

         pragma Assert (Reader.Is_End_Object);
         Reader.Read_Next;

         Self.Map.Insert (Method, Info);
      end Read_Method;

      Input   : aliased VSS.Text_Streams.File_Input.File_Input_Text_Stream;
      Reader  : VSS.JSON.Pull_Readers.JSON5.JSON5_Pull_Reader;
      Method  : VSS.Strings.Virtual_String;
   begin
      Input.Open (File_Name, "utf-8");
      Reader.Set_Stream (Input'Unchecked_Access);
      Reader.Read_Next;  --  Start_Document
      Reader.Read_Next;
      pragma Assert (Reader.Is_Start_Object);
      Reader.Read_Next;

      while Reader.Is_Key_Name loop
         Method := Reader.Key_Name;
         Reader.Read_Next;

         if Method = "license header" then
            Read_License_Header (Reader);
         else
            Read_Method (Reader, Method);
         end if;

      end loop;
   end Load;

   ----------
   -- Name --
   ----------

   function Name
     (Self   : Configuration;
      Method : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String
   is
      use type VSS.Strings.Virtual_String;

      Result : VSS.Strings.Virtual_String :=
        (if not Self.Map.Contains (Method)
         then VSS.Strings.Empty_Virtual_String
         else Self.Map (Method).Name);
   begin
      if Result.Is_Empty then
         Result := Method.Split ('/').Last_Element;

         --  Turn the first character too uppercase
         declare
            Upper : constant VSS.Strings.Virtual_String :=
              Result.To_Simple_Uppercase;
            Left  : VSS.Strings.Character_Iterators.Character_Iterator :=
              Upper.At_First_Character;
         begin
            pragma Assert (Left.Forward);

            Result := Upper.Head_Before (Left)
              & Result.Tail_After (Result.At_First_Character);
         end;
      end if;

      return Result;
   end Name;

end LSP_Gen.Configurations;

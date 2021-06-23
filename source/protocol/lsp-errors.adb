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

with Interfaces;

with LSP.JSON_Streams;

package body LSP.Errors is
   use type Interfaces.Integer_64;

   Error_Map : constant array (ErrorCodes) of Interfaces.Integer_64
     :=
     (ParseError           => -32700,
      InvalidRequest       => -32600,
      MethodNotFound       => -32601,
      InvalidParams        => -32602,
      InternalError        => -32603,
      serverErrorStart     => -32099,
      serverErrorEnd       => -32000,
      ServerNotInitialized => -32002,
      UnknownErrorCode     => -32001,
      RequestCancelled     => -32800,
      ContentModified      => -32801);

   ------------------------
   -- Read_ResponseError --
   ------------------------

   procedure Read_ResponseError
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ResponseError)
   is
      Code : Interfaces.Integer_64;
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         declare
            use type VSS.Strings.Virtual_String;

            Key : constant VSS.Strings.Virtual_String :=
              JS.R.Key_Name;

         begin
            JS.R.Read_Next;

            if Key = "code" then
               pragma Assert (JS.R.Is_Number_Value);
               Code := JS.R.Number_Value.Integer_Value;
               JS.R.Read_Next;

               for J in Error_Map'Range loop
                  if Error_Map (J) = Code then
                     V.code := J;
                     exit;
                  end if;
               end loop;

            elsif Key = "message" then
               LSP.Types.Read_String (S, V.message);

            elsif Key = "data" then
               LSP.Types.LSP_Any'Read (S, V.data);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;

      JS.R.Read_Next;
   end Read_ResponseError;

   -------------------------
   -- Write_ResponseError --
   -------------------------

   procedure Write_ResponseError
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ResponseError)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("code");
      JS.Write_Integer (Error_Map (V.code));
      Write_String (JS, "message", V.message);

      if not V.data.Is_Empty then
         JS.Key ("data");
         LSP.Types.LSP_Any'Write (S, V.data);
      end if;

      JS.End_Object;
   end Write_ResponseError;

end LSP.Errors;

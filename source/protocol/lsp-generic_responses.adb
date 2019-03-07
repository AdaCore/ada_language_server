------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with Ada.Strings.UTF_Encoding;

with LSP.JSON_Streams;
with LSP.Types;

package body LSP.Generic_Responses is

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
      return LSP.Types.LSP_String renames
       LSP.Types.To_LSP_String;

   -----------
   -- Input --
   -----------

   function Input
     (S : not null access Ada.Streams.Root_Stream_Type'Class)
      return Response
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      declare
         Parent : constant ResponseMessage := Read_Response_Prexif (S);
      begin
         if Parent.Is_Error then
            JS.End_Object;
            return (Parent with Is_Error => True);
         else
            return V : Response :=
              (Parent with Is_Error => False, result => <>)
            do
               T'Read (S, V.result);
               JS.End_Object;
            end return;
         end if;
      end;
   end Input;

   -----------
   -- Write --
   -----------

   procedure Write
     (S : not null access Ada.Streams.Root_Stream_Type'Class;
      V : Response)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Response_Prexif (S, V);
      if not V.Is_Error then
         JS.Key (+"result");
         T'Write (S, V.result);
      end if;
      JS.End_Object;
   end Write;

end LSP.Generic_Responses;

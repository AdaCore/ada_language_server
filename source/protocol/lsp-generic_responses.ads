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
--
--  This package provides a template to create LSP Responses based on
--  response result type.

with Ada.Streams;

generic
   type ResponseMessage (Is_Error : Boolean) is tagged private;
   --  Base response class (will be LSP. Pass LSP.Messages.ResponseMessage

   type T is private;
   --  Result type

   with function Read_Response_Prexif
     (S : not null access Ada.Streams.Root_Stream_Type'Class)
      return ResponseMessage is <>;
   --  Function that reads common attributes of ResponseMessage

   with procedure Write_Response_Prexif
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ResponseMessage'Class) is <>;
   --  Procedure that writes common attributes of ResponseMessage

package LSP.Generic_Responses is

   type Response (Is_Error : Boolean) is new ResponseMessage (Is_Error) with
   record
      case Is_Error is
         when False =>
            result : T;
         when True =>
            null;
      end case;
   end record;

   function Input
     (S : not null access Ada.Streams.Root_Stream_Type'Class)
      return Response;

   procedure Write
     (S : not null access Ada.Streams.Root_Stream_Type'Class;
      V : Response);

   for Response'Input use Input;
   for Response'Write use Write;
end LSP.Generic_Responses;

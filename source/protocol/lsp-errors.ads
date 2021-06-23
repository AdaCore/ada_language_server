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

with Ada.Streams;

with VSS.Strings;

with LSP.Generic_Optional;
with LSP.Types; use LSP.Types;

package LSP.Errors is

   type ErrorCodes is
     (ParseError,
      InvalidRequest,
      MethodNotFound,
      InvalidParams,
      InternalError,
      serverErrorStart,
      serverErrorEnd,
      ServerNotInitialized,
      UnknownErrorCode,
      RequestCancelled,
      ContentModified);

   type ResponseError is record
      code    : ErrorCodes;
      message : VSS.Strings.Virtual_String;
      data    : LSP.Types.LSP_Any;
   end record;

   procedure Read_ResponseError
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ResponseError);
   for ResponseError'Read use Read_ResponseError;

   procedure Write_ResponseError
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ResponseError);
   for ResponseError'Write use Write_ResponseError;

   package Optional_ResponseErrors is new LSP.Generic_Optional (ResponseError);
   type Optional_ResponseError is new Optional_ResponseErrors.Optional_Type;

end LSP.Errors;

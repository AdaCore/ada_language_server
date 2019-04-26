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
--  This package provides a template to create LSP Request based on
--  request parameter type.
--
--  We don't use request type for reading, so there are no corresponding read
--  stuff.

with Ada.Streams;

generic
   type RequestMessage is tagged private;
   --  Base response class. Pass LSP.Messages.RequestMessage here

   type T is private;
   --  Type of request parameter

   with procedure Write_Prefix
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RequestMessage'Class);
   --  Procedure that writes common attributes of RequestMessage

package LSP.Generic_Requests is
   type Request is new RequestMessage with record
      params : T;
   end record;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Request);

   for Request'Write use Write;
end LSP.Generic_Requests;

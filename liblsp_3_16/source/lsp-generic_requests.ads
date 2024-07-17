------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2020, AdaCore                     --
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

with Ada.Streams;

with LSP.JSON_Streams;
with LSP.Messages;

generic
   type Base_Message is abstract new LSP.Messages.RequestMessage
     with private;

   type T is private;
   --  Type of request parameter

   type Visitor (<>) is limited private;

package LSP.Generic_Requests is
   type Request is new Base_Message with record
      params : T;
   end record;

   function Decode
     (JS : not null access LSP.JSON_Streams.JSON_Stream)
        return Request;

   procedure Visit
     (Self    : Request;
      Handler : access Visitor);

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Request);

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Request);

   for Request'Read use Read;
   for Request'Write use Write;

end LSP.Generic_Requests;

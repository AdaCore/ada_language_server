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
--  This package provides (server side) requests handler abstraction.
--  Requests are processed in a synchronous way.

with LSP.Messages;

package LSP.Message_Handlers is

   type Request_Handler is limited interface;
   type Request_Handler_Access is access all Request_Handler'Class;

   function Handle_Request
     (Self    : access Request_Handler;
      Request : LSP.Messages.RequestMessage'Class)
      return LSP.Messages.ResponseMessage'Class is abstract;
   --  Process the given request and return the response.

end LSP.Message_Handlers;

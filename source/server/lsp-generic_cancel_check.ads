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
--
--  Each server request has an atomic flag to inform the handler that
--  the request has been canceled. But, under a havy load, a frequent atomic
--  checking cound generate overhead. This package provides a function to
--  avoid an extra overhread by reducing atomic flag check frequency.

with LSP.Messages.Server_Requests;

generic
   Request : access constant LSP.Messages.Server_Requests.Server_Request'Class;
   --  A request to check cancelation

   Max_Skip_Count : Natural;
   --  How much checks to skip before make a real atomic flag check

package LSP.Generic_Cancel_Check is

   function Has_Been_Canceled return Boolean with Inline;

end LSP.Generic_Cancel_Check;

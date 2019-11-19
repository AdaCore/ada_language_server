------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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

--  Define an interface for server backends

with LSP.Messages;

package LSP.Server_Backends is

   type Server_Backend is limited interface;
   type Server_Backend_Access is access all Server_Backend'Class;

   procedure Before_Work
     (Self    : access Server_Backend;
      Message : LSP.Messages.Message'Class) is abstract;
   --  Called before working on Message

   procedure After_Work
     (Self    : access Server_Backend;
      Message : LSP.Messages.Message'Class) is abstract;
   --  Called after working on Message

end LSP.Server_Backends;

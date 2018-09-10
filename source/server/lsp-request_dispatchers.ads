------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
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

with Ada.Containers.Hashed_Maps;
with Ada.Streams;

with LSP.Messages;
with LSP.Message_Handlers;
with LSP.Types;

package LSP.Request_Dispatchers is
--   pragma Preelaborate;

   type Request_Dispatcher is tagged limited private;

   type Parameter_Handler_Access is access function
    (Stream     : access Ada.Streams.Root_Stream_Type'Class;
     Handler    : not null LSP.Message_Handlers.Request_Handler_Access)
       return LSP.Messages.ResponseMessage'Class;

   not overriding procedure Register
    (Self   : in out Request_Dispatcher;
     Method : LSP.Types.LSP_String;
     Value  : Parameter_Handler_Access);

   not overriding function Dispatch
     (Self    : in out Request_Dispatcher;
      Method  : LSP.Types.LSP_String;
      Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null LSP.Message_Handlers.Request_Handler_Access)
      return LSP.Messages.ResponseMessage'Class;

private

   package Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => LSP.Types.LSP_String,
      Element_Type    => Parameter_Handler_Access,
      Hash            => LSP.Types.Hash,
      Equivalent_Keys => LSP.Types."=",
      "="             => "=");

   type Request_Dispatcher is tagged limited record
      Map   : Maps.Map;
      Value : LSP.Message_Handlers.Request_Handler_Access;
   end record;

end LSP.Request_Dispatchers;

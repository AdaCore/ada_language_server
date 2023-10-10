------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2020-2023, AdaCore                     --
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
--  This package provides a base class to implement custom commands.
--  The concrete LSP server provides the set of commands by inheriting from
--  this base class. Read_XXX procedures declared in LSP.Structures construct
--  corresponding object using generic dispatching constructor on the server
--  side. On the client side any command is an opaque object and will always be
--  represented as a JSON object.

with Ada.Tags;

with VSS.String_Vectors;

with LSP.Structures;
with LSP.Errors;

limited with LSP.Ada_Handlers;

package LSP.Ada_Commands is

   type Command is abstract tagged null record;
   type Command_Access is access all Command'Class with Storage_Size => 0;

   function Create
     (Any : not null access LSP.Structures.LSPAny_Vector)
      return Command is abstract;

   procedure Execute
     (Self     : Command;
      Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Response : in out LSP.Structures.LSPAny_Or_Null;
      Error    : in out LSP.Errors.ResponseError_Optional) is abstract;
   --  Execute given command and return Error is something went wrong.
   --  Commands are executed on the server side only.
   --  The Handler is the access to the message handler executing the command.

   procedure Register (Value : Ada.Tags.Tag);
   --  Register a new command type. The type should be in Command'Class

   function All_Commands return VSS.String_Vectors.Virtual_String_Vector;
   --  Return all registered command names.

end LSP.Ada_Commands;

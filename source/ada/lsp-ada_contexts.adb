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

package body LSP.Ada_Contexts is

   ------------------
   -- Get_Document --
   ------------------

   not overriding function Get_Document
     (Self : Context;
      URI  : LSP.Messages.DocumentUri)
        return LSP.Ada_Documents.Document_Access is
   begin
      return Self.Documents (URI);
   end Get_Document;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (Self : in out Context;
      Root : LSP.Types.LSP_String) is
   begin
      Self.Root := Root;
   end Initialize;

   -------------------
   -- Load_Document --
   -------------------

   not overriding procedure Load_Document
     (Self  : in out Context;
      Item  : LSP.Messages.TextDocumentItem)
   is
      Object : constant LSP.Ada_Documents.Document_Access :=
        new LSP.Ada_Documents.Document;
   begin
      Object.Initialize (Item);
      Object.Update;
      Self.Documents.Insert (Item.uri, Object);
   end Load_Document;

   ---------------------
   -- Update_Document --
   ---------------------

   not overriding procedure Update_Document
     (Self : in out Context;
      Item : not null LSP.Ada_Documents.Document_Access)
   is
      pragma Unreferenced (Self);
   begin
      Item.Update;
   end Update_Document;

end LSP.Ada_Contexts;

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

package body LSP.Ada_Documents is

   -------------------
   -- Apply_Changes --
   -------------------

   not overriding procedure Apply_Changes
     (Self   : aliased in out Document;
      Vector : LSP.Messages.TextDocumentContentChangeEvent_Vector)
   is
      pragma Unreferenced (Self);
   begin
      --  FIX ME Sort Vector before applying?
      for Change of reverse Vector loop
         null;
      end loop;
   end Apply_Changes;

   ----------------
   -- Get_Errors --
   ----------------

   not overriding procedure Get_Errors
     (Self   : Document;
      Errors : out LSP.Messages.Diagnostic_Vector)
   is
      pragma Unreferenced (Self);
   begin
      Errors.Clear;
   end Get_Errors;

   -----------------
   -- Get_Symbols --
   -----------------

   not overriding procedure Get_Symbols
     (Self   : Document;
      Result : out LSP.Messages.SymbolInformation_Vector)
   is
      pragma Unreferenced (Self);
   begin
      Result.Clear;
   end Get_Symbols;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (Self : in out Document;
      Item : LSP.Messages.TextDocumentItem)
   is
   begin
      Self.URI := Item.uri;
   end Initialize;

   ------------
   -- Update --
   ------------

   not overriding procedure Update
     (Self     : aliased in out Document) is
   begin
      null;
   end Update;

end LSP.Ada_Documents;

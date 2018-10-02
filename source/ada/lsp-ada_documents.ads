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

with LSP.Messages;
with Libadalang.Analysis;

package LSP.Ada_Documents is

   type Document is tagged limited private;
   type Document_Access is access all LSP.Ada_Documents.Document;
   type Constant_Document_Access is access constant LSP.Ada_Documents.Document;

   not overriding procedure Initialize
     (Self : in out Document;
      LAL  : Libadalang.Analysis.Analysis_Context;
      Item : LSP.Messages.TextDocumentItem);

   not overriding procedure Apply_Changes
     (Self   : aliased in out Document;
      Vector : LSP.Messages.TextDocumentContentChangeEvent_Vector);

   not overriding procedure Get_Errors
     (Self   : Document;
      Errors : out LSP.Messages.Diagnostic_Vector);

   not overriding procedure Get_Symbols
     (Self   : Document;
      Result : out LSP.Messages.SymbolInformation_Vector);

   No_Defining_Name_At_Position : exception;

   not overriding function Get_Definition_At
     (Self     : Document;
      Position : LSP.Messages.Position)
      return Libadalang.Analysis.Defining_Name;

private

   type Document is tagged limited record
      URI  : LSP.Messages.DocumentUri;
      LAL  : Libadalang.Analysis.Analysis_Context;
      Unit : Libadalang.Analysis.Analysis_Unit;
   end record;

end LSP.Ada_Documents;

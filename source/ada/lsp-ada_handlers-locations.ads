------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
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

--  This package provides routines to convert Libadalang locations to LSP
--  locations. If there is the corresponding open document then it is used
--  to do conversion. Otherwise we use Libadalang to get corresponding lines
--  and compute character offsets.

with Langkit_Support.Slocs;
with Libadalang.Analysis;
with Libadalang.Common;

with LSP.Ada_Contexts;
with LSP.Constants;
with LSP.Locations;

package LSP.Ada_Handlers.Locations is

   function To_LSP_Location
     (Self : in out Message_Handler'Class;
      Node : Libadalang.Analysis.Ada_Node'Class;
      Kind : LSP.Structures.AlsReferenceKind_Set := LSP.Constants.Empty)
      return LSP.Structures.Location;
   --  Convert LAL's Node to a LSP location

   function To_LSP_Location
     (Self    : in out Message_Handler'Class;
      Context : LSP.Ada_Contexts.Context;
      File    : String;
      Sloc    : Langkit_Support.Slocs.Source_Location_Range;
      Kinds   : LSP.Structures.AlsReferenceKind_Set := LSP.Constants.Empty)
      return LSP.Structures.Location;

   function To_LSP_Range
     (Self  : in out Message_Handler'Class;
      Unit  : Libadalang.Analysis.Analysis_Unit;
      Token : Libadalang.Common.Token_Reference)
      return LSP.Structures.A_Range;

   function Get_Node_At
     (Self     : in out Message_Handler'Class;
      Context  : LSP.Ada_Contexts.Context;
      Value    : LSP.Structures.TextDocumentPositionParams'Class)
      return Libadalang.Analysis.Ada_Node;

   function Start_Position
     (Token : Libadalang.Common.Token_Reference)
      return LSP.Structures.Position;

   procedure Append_Location
     (Self   : in out Message_Handler;
      Result : in out LSP.Structures.Location_Vector;
      Filter : in out LSP.Locations.File_Span_Sets.Set;
      Node   : Libadalang.Analysis.Ada_Node'Class;
      Kinds  : LSP.Structures.AlsReferenceKind_Set := LSP.Constants.Empty);
   --  Append given Node location to the Result.
   --  Do nothing if the item inside of an synthetic file (like __standard).

   procedure Append_Location
     (Result   : in out LSP.Structures.DocumentHighlight_Vector;
      Document : not null access LSP.Ada_Documents.Document'Class;
      File     : GNATCOLL.VFS.Virtual_File;
      Node     : Libadalang.Analysis.Ada_Node'Class;
      Kind     : LSP.Structures.DocumentHighlightKind_Optional);
   --  The same for DocumentHighlight_Vector.
   --  File is used to filter out the location not in the Document.
   --  Note, File and Document must be same document.
   --  XXX File parameter can be removed when whether Document can return
   --  associated Virtual_File or Node can able to return URI of the enclosing
   --  file.

   procedure Sort (Result : in out LSP.Structures.Location_Vector);
   --  Sort Result using next rules:
   --  We're being a bit clever when comparing two URIs:
   --    * for a same file, return ".ads" before ".adb"
   --    * return "pack.adb" before "pack-child.adb"

end LSP.Ada_Handlers.Locations;

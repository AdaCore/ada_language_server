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

with Libadalang.Analysis;
with Libadalang.Common;

package LSP.Ada_Handlers.Locations is

   function Get_Node_At
     (Self    : in out Message_Handler'Class;
      Context : LSP.Ada_Contexts.Context;
      Value   : LSP.Structures.TextDocumentPositionParams'Class)
      return Libadalang.Analysis.Ada_Node;

   function Start_Position
     (Token : Libadalang.Common.Token_Reference) return LSP.Structures.Position;

   procedure Sort (Result : in out LSP.Structures.Location_Vector);
   --  Sort Result using next rules:
   --  We're being a bit clever when comparing two URIs:
   --    * for a same file, return ".ads" before ".adb"
   --    * return "pack.adb" before "pack-child.adb"

end LSP.Ada_Handlers.Locations;

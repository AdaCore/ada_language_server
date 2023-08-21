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
--
--  This package provides routines to convert Libadalang locations to LSP
--  locations. If there is the corresponding open document then it is used
--  to do conversion. Otherwise we use Libadalang to get corresponding lines
--  and compute character offsets.

with Libadalang.Analysis;

with LSP.Ada_Contexts;

package LSP.Ada_Handlers.Locations is

   function To_LSP_Location
     (Self : in out Message_Handler'Class;
      Node : Libadalang.Analysis.Ada_Node'Class)
      return LSP.Structures.Location;
   --  Convert LAL's Node to a LSP location

   function Get_Node_At
     (Self     : in out Message_Handler'Class;
      Context  : LSP.Ada_Contexts.Context;
      Value    : LSP.Structures.TextDocumentPositionParams'Class)
      return Libadalang.Analysis.Ada_Node;

   type AlsReferenceKind is
     (Simple,
      Access_Ref,
      Write,
      Static_Call,
      Dispatching_Call,
      Parent,
      Child,
      Overriding_Decl);
   --  This should be part of the protocol

   type AlsReferenceKind_Array is array (AlsReferenceKind) of Boolean;

   function Empty return AlsReferenceKind_Array is ([others => False]);

   procedure Append_Location
     (Self   : in out Message_Handler;
      Result : in out LSP.Structures.Location_Vector;
      Node   : Libadalang.Analysis.Ada_Node'Class;
      Ignore : AlsReferenceKind_Array := Empty);
   --  Append given Node location to the Result.
   --  Do nothing if the item inside of an synthetic file (like __standard).

end LSP.Ada_Handlers.Locations;

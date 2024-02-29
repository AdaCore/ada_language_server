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

with Libadalang.Analysis;

with LSP.Ada_Contexts;
with LSP.Locations;

package LSP.Ada_Handlers.Call_Hierarchy is

   procedure Find_Incoming_Calls
     (Self        : in out Message_Handler;
      Response    : in out LSP.Structures.CallHierarchyIncomingCall_Vector;
      Filter      : in out LSP.Locations.File_Span_Sets.Set;
      Context     : LSP.Ada_Contexts.Context;
      Definition  : Libadalang.Analysis.Defining_Name)
          with Pre => Definition.P_Basic_Decl.P_Is_Subprogram;
   --  Return the list of all the calls made to the subprogram pointed at by
   --  the node given by Definition, organized by the subprograms in which
   --  these calls are listed, ordered by the name of these subprograms.

   procedure Find_Outgoing_Calls
     (Self        : in out Message_Handler;
      Response    : in out LSP.Structures.CallHierarchyOutgoingCall_Vector;
      Filter      : in out LSP.Locations.File_Span_Sets.Set;
      Definition  : Libadalang.Analysis.Defining_Name)
          with Pre => Definition.P_Basic_Decl.P_Is_Subprogram;
   --  Return the list of all the calls made in the subprogram pointed at by
   --  the node given by Definition, organized by the subprograms in which
   --  these calls are listed, ordered by the name of these subprograms.

end LSP.Ada_Handlers.Call_Hierarchy;

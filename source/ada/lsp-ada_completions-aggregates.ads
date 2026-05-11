------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2025-2026, AdaCore                     --
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
--  A completion provider for aggregates

with LSP.Ada_Completions.Filters;

package LSP.Ada_Completions.Aggregates is

   type Aggregate_Completion_Provider
      (Handler      : not null access LSP.Ada_Handlers.Message_Handler;
       Use_Snippets : Boolean) is new Completion_Provider with null record;

   overriding procedure Propose_Completion
     (Self   : Aggregate_Completion_Provider;
      Sloc   : Langkit_Support.Slocs.Source_Location;
      Token  : Libadalang.Common.Token_Reference;
      Node   : Libadalang.Analysis.Ada_Node;
      Filter : in out LSP.Ada_Completions.Filters.Filter;
      Result : out Ada_Completions.Completion_Result);
   --  Return completion for aggregates if we are within an empty aggregate.

end LSP.Ada_Completions.Aggregates;

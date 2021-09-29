------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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
--  A completion provider for invisible names.

with LSP.Ada_Completions;
with LSP.Ada_Completions.Filters;
with Langkit_Support.Slocs;
with Libadalang.Analysis;
with Libadalang.Common;

package LSP.Ada_Handlers.Invisibles is

   type Invisible_Completion_Provider
     (Handler : not null access LSP.Ada_Handlers.Message_Handler;
      Context : not null LSP.Ada_Handlers.Context_Access)
     is new LSP.Ada_Completions.Completion_Provider with null record;

   overriding procedure Propose_Completion
     (Self   : Invisible_Completion_Provider;
      Sloc   : Langkit_Support.Slocs.Source_Location;
      Token  : Libadalang.Common.Token_Reference;
      Node   : Libadalang.Analysis.Ada_Node;
      Filter : in out LSP.Ada_Completions.Filters.Filter;
      Names  : in out Ada_Completions.Completion_Maps.Map;
      Result : in out LSP.Messages.CompletionList);

end LSP.Ada_Handlers.Invisibles;

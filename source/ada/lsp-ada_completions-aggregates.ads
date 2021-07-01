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
--  A completion provider for aggregates

package LSP.Ada_Completions.Aggregates is

   type Aggregate_Completion_Provider
     (Named_Notation_Threshold : Natural) is new Completion_Provider with
        null record;
   --  This providder returns the completion list for the aggregate node.
   --  The returned completion list may contain several items if the aggregate
   --  is used to assign a value to a variant record: in that case, a snippet
   --  per shape (i.e: a shape corresponds to one of the various forms that a
   --  discriminated variant record can take) will be proposed to the user.
   --
   --  Named_Notation_Threshold defines the number of parameters/components at
   --  which point named notation is used for subprogram/aggregate completion
   --  snippets.

   overriding procedure Propose_Completion
     (Self   : Aggregate_Completion_Provider;
      Sloc   : Langkit_Support.Slocs.Source_Location;
      Token  : Libadalang.Common.Token_Reference;
      Node   : Libadalang.Analysis.Ada_Node;
      Names  : out Ada_Completions.Completion_Maps.Map;
      Result : out LSP.Messages.CompletionList);
   --  Check if we are dealing with an aggregate node. If yes, handle it
   --  separately to propose snipppets to the user, allowing him to fill
   --  all the needed discriminants/components easily.

end LSP.Ada_Completions.Aggregates;

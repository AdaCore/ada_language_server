------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2021, AdaCore                          --
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
--  A completion provider for parameters after '(' or ','

with LSP.Ada_Context_Sets;
with LSP.Ada_Documents;

package LSP.Ada_Completions.Parameters is

   type Parameter_Completion_Provider
     (Context                  : not null LSP.Ada_Context_Sets.Context_Access;
      Document                 : LSP.Ada_Documents.Document_Access;
      Named_Notation_Threshold : Natural;
      Compute_Doc_And_Details  : Boolean)
   is new Completion_Provider with null record;

   overriding procedure Propose_Completion
     (Self   : Parameter_Completion_Provider;
      Sloc   : Langkit_Support.Slocs.Source_Location;
      Token  : Libadalang.Common.Token_Reference;
      Node   : Libadalang.Analysis.Ada_Node;
      Filter : in out LSP.Ada_Completions.Filters.Filter;
      Names  : in out Ada_Completions.Completion_Maps.Map;
      Result : in out LSP.Structures.CompletionList);
   --  Using the context, return the list of parameters after '(' or ','

   procedure Propose_Signatures
     (Context         : not null LSP.Ada_Context_Sets.Context_Access;
      Node            : Libadalang.Analysis.Ada_Node;
      Cursor          : Langkit_Support.Slocs.Source_Location;
      Prev_Signatures : LSP.Structures.SignatureHelpContext_Optional;
      Res             : in out LSP.Structures.SignatureHelp);
   --  Using the context, return the possible signatures
end LSP.Ada_Completions.Parameters;

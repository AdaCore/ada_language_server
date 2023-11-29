------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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

with GNATCOLL.Traces; use GNATCOLL.Traces;

with VSS.Strings;

with LSP.Structures;  use LSP.Structures;
with Libadalang.Analysis;

package LSP.Predefined_Completion is

   procedure Load_Predefined_Completion_Db (Trace : Trace_Handle);
   --  Load all the predefined completiom items
   --  (aspects, pragmas and attributes) from the database associated with
   --  the user's compiler version.

   procedure Get_Aspects
     (Prefix : VSS.Strings.Virtual_String;
      Result : in out CompletionItem_Vector);
   --  Return completion for aspects, filtering the results using Prefix.

   procedure Get_Attributes
     (Prefix : VSS.Strings.Virtual_String;
      Result : in out CompletionItem_Vector);
   --  Return completion for attributes, filtering the results using Prefix.

   procedure Get_Pragmas
     (Prefix : VSS.Strings.Virtual_String;
      Result : in out CompletionItem_Vector);
   --  Return completion for pragmas, filtering the results using Prefix.

   procedure Get_Tooltip_Text
     (Node               : Libadalang.Analysis.Identifier;
      Declaration_Text   : out VSS.Strings.Virtual_String;
      Documentation_Text : out VSS.Strings.Virtual_String);
   --  Get all the information needed to produce tooltips (hover and completion
   --  requests) for the given node, if it's a predefined entity (e.g: aspect,
   --  pragma or attribute).
   --
   --  @param Node
   --    The predefined entity's node.
   --  @param Declaration_Text
   --    Contains the code corresponding to the predefined entity's declaration.
   --  @param Documentation_Text
   --    Contains the documentation associated to the predefined entity.

end LSP.Predefined_Completion;

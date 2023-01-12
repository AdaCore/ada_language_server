------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
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

with Laltools.Common;
with Langkit_Support.Slocs;
with Libadalang.Analysis;
with LSP.Ada_Completions.Filters;
with LSP.Ada_Completions.Generic_Assoc_Utils;
with LSP.Ada_Completions.Parameters;
with LSP.Ada_Handlers;

generic
   type Element is private;
   --  From Libabalang.Analysis.Ada_Node'Class

   Null_Element : Element;
   --  For example No_Ada_Node

   with function Search_Element
     (Node : Libadalang.Analysis.Ada_Node'Class) return Element;
   --  Using Node tries to find an Element in the Tree by doing basic
   --  error recovery.
   --  Return Null_Element if there is no such Element.

   with function Get_Designators
     (E              : Element;
      Sloc           : Langkit_Support.Slocs.Source_Location;
      Prefixed       : out Boolean;
      Unnamed_Params : out Natural)
      return Laltools.Common.Node_Vectors.Vector;
   --  Compute the list of parameters in E, they are separated in 4 categories:
   --  - Prefixed indicates if we are prefixed by a parameter
   --  - Unnamed_Params corresponds to all the params without designators
   --  at the start of the function
   --  - The Result starts with the list of all the designators
   --  - The Result finishes with X No_Ada_Node, one for each params without
   --  designator after the first named param.
   --
   --  Example: Obj.Foo (1, 2, 3, X => 4, Y => 5, 7, 8
   --  Prefixed       = Is_Param (Obj)
   --  Unnamed_Params = 3
   --  Result         = [X, Y, No_Ada_Node, No_Ada_Node]

   with function Get_Spec_Designators
     (E             : Element;
      Context       : not null LSP.Ada_Handlers.Context_Access;
      For_Signature : Boolean)
      return LSP.Ada_Completions.Generic_Assoc_Utils.Assoc_Data_Lists.List;
   --  Return all the specs matching E
   --  When For_Signature, modify the data for signature
   --  For example: completion requires the missing parameters but signature
   --  requires all parameters

   Pretty_Print_Rule : Libadalang.Common.Grammar_Rule;
   --  Rule used to pretty print the completion item

   with function Get_Prefix_Node
     (E      : Element;
      Column : out Langkit_Support.Slocs.Column_Number)
      return Libadalang.Analysis.Ada_Node'Class;
   --  Get the node needed to match Pretty_Print_Rule
   --  Column represent the block indentation level

   with function To_Node
     (E : Element) return Libadalang.Analysis.Ada_Node'Class;

package LSP.Ada_Completions.Generic_Assoc is
   procedure Propose_Completion
     (Self         :
        LSP.Ada_Completions.Parameters.Parameter_Completion_Provider;
      Sloc         : Langkit_Support.Slocs.Source_Location;
      Token        : Libadalang.Common.Token_Reference;
      Node         : Libadalang.Analysis.Ada_Node;
      Limit        : Natural;
      Filter       : in out LSP.Ada_Completions.Filters.Filter;
      Names        : in out Ada_Completions.Completion_Maps.Map;
      Unsorted_Res : in out LSP.Messages.CompletionItem_Vector);

   procedure Propose_Signatures
     (Context         : not null LSP.Ada_Handlers.Context_Access;
      Node            : Libadalang.Analysis.Ada_Node;
      Cursor          : Langkit_Support.Slocs.Source_Location;
      Prev_Signatures : LSP.Messages.Optional_SignatureHelpContext;
      Res             : in out LSP.Messages.SignatureHelp;
      Lazy            : Boolean := False);
   --  If Lazy, give up on trying to highlight the parameters when multiple
   --  signature are possible.
end LSP.Ada_Completions.Generic_Assoc;

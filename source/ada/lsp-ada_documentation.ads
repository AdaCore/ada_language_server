------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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

--  Subprograms to obtain documentation for declarations.

with VSS.Strings;

with Libadalang.Analysis;

with GNATdoc.Comments.Options;

package LSP.Ada_Documentation is

   procedure Get_Tooltip_Text
     (BD                 : Libadalang.Analysis.Basic_Decl;
      Style              : GNATdoc.Comments.Options.Documentation_Style;
      Declaration_Text   : out VSS.Strings.Virtual_String;
      Qualifier_Text     : out VSS.Strings.Virtual_String;
      Location_Text      : out VSS.Strings.Virtual_String;
      Documentation_Text : out VSS.Strings.Virtual_String;
      Aspects_Text       : out VSS.Strings.Virtual_String);
   --  Get all the information needed to produce tooltips (hover and completion
   --  requests) for the given declaration.
   --
   --  @param BD        Declaration's node.
   --  @param Style
   --    Is used by GNATdoc for extracting the associated comments.
   --  @param Qualifier_Text
   --    Auxiliary information about properties of the delaration.
   --  @param Location_Text
   --    Contains the declaration's location text (e.g: a.adb (11:1)).
   --  @param Documentation_Text
   --    Contains the comments associated with the declaration.
   --  @param Declaration_Text
   --    Contains the code corresponding to the declaration.
   --  @param Aspects_Text
   --    Contains the code of aspects of the declaration.

end LSP.Ada_Documentation;

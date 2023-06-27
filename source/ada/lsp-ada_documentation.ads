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

with GNATCOLL.Traces;

with VSS.Strings;

with Libadalang.Analysis;

with GNATdoc.Comments.Options;

package LSP.Ada_Documentation is

   procedure Get_Tooltip_Text
     (BD        : Libadalang.Analysis.Basic_Decl;
      Trace     : GNATCOLL.Traces.Trace_Handle;
      Style     : GNATdoc.Comments.Options.Documentation_Style;
      Loc_Text  : out VSS.Strings.Virtual_String;
      Doc_Text  : out VSS.Strings.Virtual_String;
      Decl_Text : out VSS.Strings.Virtual_String);
   --  Get all the information needed to produce tooltips (hover and completion
   --  requests) for the given declaration.
   --  Style is used by GNATdoc for extracting the associated comments.
   --  Loc_Text contains the declaration's location text (e.g: a.adb (11:1)).
   --  Doc_Text contains the comments associated with the declaration.
   --  Decl_Text contains the code corresponding to the declaration, formatted
   --  by GNATdoc when possible.

end LSP.Ada_Documentation;

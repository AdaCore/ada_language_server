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

--  Subprogram to obtain documentation for packages & attributes.

with GNATdoc.Comments.Options;

with LSP.GPR_Documents;
with LSP.GPR_Files;
with LSP.Structures;

with VSS.Strings;

package LSP.GPR_Documentation is

   procedure Get_Tooltip_Text
     (Self               : LSP.GPR_Files.File_Access;
      URI                : LSP.Structures.DocumentUri;
      Document_Provider  : LSP.GPR_Documents.Document_Provider_Access;
      Position           : LSP.Structures.Position;
      Style              : GNATdoc.Comments.Options.Documentation_Style;
      Declaration_Text   : out VSS.Strings.Virtual_String;
      Documentation_Text : out VSS.Strings.Virtual_String;
      Location_Text      : out VSS.Strings.Virtual_String);
   --  Get all the information needed to produce tooltips (hover and completion
   --  requests)

end LSP.GPR_Documentation;

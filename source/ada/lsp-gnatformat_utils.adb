------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2024, AdaCore                          --
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

package body LSP.GNATFormat_Utils is

   -----------------------
   -- Get_Format_Option --
   -----------------------

   function Get_Format_Option
     (Option : LSP.Structures.FormattingOptions)
      return Standard.Gnatformat.Configuration.Format_Options_Type
   is
      use Gnatformat;
      use Gnatformat.Configuration;
      Format_Options_Builder : Format_Options_Builder_Type :=
        Create_Format_Options_Builder;
   begin

      if Option.tabSize /= 0 then
         --  FormattingOptions is not optional, however in case a client
         --  forgot to set it try to be resilient.
         Format_Options_Builder.With_Indentation
           (Option.tabSize, Ada_Language);
      end if;
      Format_Options_Builder.With_Indentation_Kind
        ((if Option.insertSpaces then Spaces else Tabs), Ada_Language);

      if Option.gnatFormatMaxSize.Is_Set then
         Format_Options_Builder.With_Width
           (Option.gnatFormatMaxSize.Value, Ada_Language);
      end if;

      if Option.gnatFormatContinuationLineIndent.Is_Set then
         Format_Options_Builder.With_Indentation_Continuation
           (Option.gnatFormatContinuationLineIndent.Value, Ada_Language);
      end if;

      return Format_Options_Builder.Build;
   end Get_Format_Option;

end LSP.GNATFormat_Utils;

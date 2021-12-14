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

with GNATCOLL.Boyer_Moore;  use GNATCOLL.Boyer_Moore;

package LSP.Search.Start_Word_Text is

   function Build
     (Pattern        : VSS.Strings.Virtual_String;
      Case_Sensitive : Boolean := False;
      Whole_Word     : Boolean := False;
      Negate         : Boolean := False)
      return Search_Pattern'Class;

private

   type Start_Word_Text_Search is new Search_Pattern with record
      Boyer : GNATCOLL.Boyer_Moore.Pattern;
   end record;

   overriding procedure Finalize (Self : in out Start_Word_Text_Search);

   overriding function Match
     (Self : Start_Word_Text_Search;
      Text : VSS.Strings.Virtual_String)
      return Boolean;

end LSP.Search.Start_Word_Text;

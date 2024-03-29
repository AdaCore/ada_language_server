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

package body LSP.Search.Empty is

   -----------
   -- Build --
   -----------

   function Build return Search_Pattern'Class is
   begin
      return Empty_Text_Search'
        (Ada.Finalization.Limited_Controlled with others => <>);
   end Build;

   -----------
   -- Match --
   -----------

   overriding function Match
     (Self : Empty_Text_Search;
      Text : VSS.Strings.Virtual_String)
      return Boolean is
   begin
      return True;
   end Match;

end LSP.Search.Empty;

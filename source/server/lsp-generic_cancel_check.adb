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

package body LSP.Generic_Cancel_Check is

   Count : Natural := Max_Skip_Count;
   --  Counter to restrict frequency of Request.Canceled checks

   Is_Canceled : Boolean := False;

   -----------------------
   -- Has_Been_Canceled --
   -----------------------

   function Has_Been_Canceled return Boolean is
   begin
      if Is_Canceled then
         return True;
      end if;

      Count := Count - 1;

      if Count = 0 then
         Count       := Max_Skip_Count;
         Is_Canceled := Request.Canceled;

         return Is_Canceled;

      else
         return False;
      end if;
   end Has_Been_Canceled;

end LSP.Generic_Cancel_Check;

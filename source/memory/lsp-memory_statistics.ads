------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2020, AdaCore                       --
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

--  This package is used to dump the memory statistics gathered by
--  GNATCOLL.Memory.

with GNATCOLL.Memory; use GNATCOLL.Memory;

package LSP.Memory_Statistics is

   function Dump_Memory_Statistics
     (Size   : Positive;
      Report : Report_Type := Memory_Usage)
      return String;
   --  Dump information about memory usage to configurable output
   --  Size is the number of the biggest memory users we want to show. Report
   --  indicates which sorting order is used in the report.

end LSP.Memory_Statistics;

------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017-2018, AdaCore                     --
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

with Ada.Streams;
generic
   type Element_Type is private;
package LSP.Generic_Optional is
--   pragma Preelaborate;

   type Optional_Type (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : Element_Type;
         when False =>
            null;
      end case;
   end record;

   not overriding procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Optional_Type);

   for Optional_Type'Read use Read;

   not overriding procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Optional_Type);

   for Optional_Type'Write use Write;

end LSP.Generic_Optional;

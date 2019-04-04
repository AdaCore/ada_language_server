------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with Ada.Strings.UTF_Encoding.Wide_Strings;
with Interfaces.C;
with Spawn.Windows_API;

separate (Spawn.Environments)
function Search_In_Path
  (File : UTF_8_String;
   Path : UTF_8_String) return UTF_8_String
is
   use type Spawn.Windows_API.DWORD;

   Raw_Path : Interfaces.C.wchar_array :=
     Interfaces.C.To_C
       (Ada.Strings.UTF_Encoding.Wide_Strings.Decode (Path));

   Raw_File : Interfaces.C.wchar_array :=
     Interfaces.C.To_C
       (Ada.Strings.UTF_Encoding.Wide_Strings.Decode (File));

   Raw_Exe : Interfaces.C.wchar_array := Interfaces.C.To_C (".exe");

   Buffer : Interfaces.C.wchar_array (1 .. Spawn.Windows_API.MAX_PATH);

   Length : constant Spawn.Windows_API.DWORD :=
     Spawn.Windows_API.SearchPath
       (lpPath        => Raw_Path (Raw_Path'First)'Unchecked_Access,
        lpFileName    => Raw_File (Raw_File'First)'Unchecked_Access,
        lpExtension   => Raw_Exe (Raw_Exe'First)'Unchecked_Access,
        nBufferLength => Buffer'Length,
        lpBuffer      => Buffer (Buffer'First)'Unchecked_Access,
        lpFilePart    => null);
begin
   if Length = 0 or Length > Buffer'Length then

      return "";
   else

      return Ada.Strings.UTF_Encoding.Wide_Strings.Encode
        (Interfaces.C.To_Ada
           (Buffer (1 .. Interfaces.C.size_t (Length)),
            Trim_Nul => False));
   end if;
end Search_In_Path;

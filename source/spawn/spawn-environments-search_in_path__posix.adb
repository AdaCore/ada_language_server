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

with Ada.Strings.Fixed;
with Ada.Directories;

separate (Spawn.Environments)
function Search_In_Path
  (File : UTF_8_String;
   Path : UTF_8_String) return UTF_8_String
is
   From : Natural := Path'First;
begin
   loop
      declare
         use all type Ada.Directories.File_Kind;

         To : constant Natural := Ada.Strings.Fixed.Index (Path, ":", From);

         Directory : constant UTF_8_String :=
           (if To = 0 then
               Path (From .. Path'Last)
            else
               Path (From .. To - 1));

         Candidate : constant UTF_8_String :=
           Ada.Directories.Compose (Directory, File);
      begin
         if Ada.Directories.Exists (Candidate)
           and then Ada.Directories.Kind (Candidate) = Ordinary_File
         then
            return Ada.Directories.Full_Name (Candidate);
         end if;

         exit when To = 0;

         From := To + 1;
      end;
   end loop;

   return "";
end Search_In_Path;

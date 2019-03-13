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

pragma Warnings (Off);
with System.Win32;
pragma Warnings (On);

with Spawn.Windows_API;
with Spawn.Internal;

separate (Spawn.Environments)
procedure Initialize_Default (Default : out Process_Environment) is
   use type Interfaces.C.size_t;
   use type Interfaces.C.wchar_t;
   use type Windows_API.Environment_Block_Access;
   use type Windows_API.BOOL;

   procedure Append (Name, Value : Interfaces.C.wchar_array);

   ------------
   -- Append --
   ------------

   procedure Append (Name, Value : Interfaces.C.wchar_array) is
   begin
      Default.Map.Include
        (Ada.Strings.UTF_Encoding.Wide_Strings.Encode
           (Interfaces.C.To_Ada (Name, False)),
         Ada.Strings.UTF_Encoding.Wide_Strings.Encode
           (Interfaces.C.To_Ada (Value, False)));
   end Append;

   Env   : constant Windows_API.Environment_Block_Access :=
     Windows_API.GetEnvironmentStringsW;
   Equal : Interfaces.C.size_t := 1;
   From  : Interfaces.C.size_t := 1;
   Index : Interfaces.C.size_t := 1;

begin
   if Env /= null then
      loop
         if Env (Index) = Interfaces.C.wide_nul then
            exit when Index = From;
            Append (Env (From .. Equal - 1), Env (Equal + 1 .. Index - 1));
            From := Index + 1;
         elsif Index /= From and Env (Index) = '=' then
            Equal := Index;
         end if;

         Index := Index + 1;
      end loop;

      if Windows_API.FreeEnvironmentStringsW (Env) = System.Win32.FALSE then
         raise Program_Error;
      end if;
   end if;
end Initialize_Default;

------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
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

with LSP.Structures;
with LSP.Enumerations;

package body LSP.Ada_Handlers.Alire_Diagnostics is

   ---------------------
   -- Get_Diagnostics --
   ---------------------

   overriding
   procedure Get_Diagnostics
     (Self          : in out Diagnostic_Source;
      Diagnostics   : out LSP.Structures.Diagnostic_Vector;
      Target_File   : out GNATCOLL.VFS.Virtual_File)
   is
      Diag : LSP.Structures.Diagnostic;
   begin
      for Msg of Self.Handler.Project_Status.Get_Alire_Messages loop
         Diag.message := Msg;
         Diag.a_range := LSP.Structures.A_Range'((0, 0), (0, 0));
         Diag.severity := (True, LSP.Enumerations.Warning);
         Diag.source := Alire_Diagnostics_Source_ID;
         Diagnostics.Append (Diag);
      end loop;

      --  Emit Alire-related diagnostics directly on the 'alire.toml' file
      Target_File :=
        GNATCOLL.VFS.Create_From_Base
          (Base_Name => "alire.toml",
           Base_Dir  => Self.Handler.Client.Root_Directory.Full_Name);
   end Get_Diagnostics;

   ------------------------
   -- Has_New_Diagnostic --
   ------------------------

   overriding
   function Has_New_Diagnostic (Self : in out Diagnostic_Source) return Boolean
   is
   begin
      return not Self.Handler.Project_Status.Get_Alire_Messages.Is_Empty;
   end Has_New_Diagnostic;

end LSP.Ada_Handlers.Alire_Diagnostics;

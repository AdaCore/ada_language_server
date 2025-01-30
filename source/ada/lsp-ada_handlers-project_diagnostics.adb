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

with LSP.GNATCOLL_Tracers;
package body LSP.Ada_Handlers.Project_Diagnostics is

   Tracer : constant LSP.GNATCOLL_Tracers.Tracer :=
      LSP.GNATCOLL_Tracers.Create ("ALS.PROJECT.DIAGNOSTICS");
   --  We do not activate this trace by default because it is too noisy to
   --  repeat the information every time diagnostics are published for every
   --  file.

   ---------------------
   -- Get_Diagnostics --
   ---------------------

   overriding
   procedure Get_Diagnostics
     (Self          : in out Diagnostic_Source;
      Diagnostics   : out LSP.Structures.Diagnostic_Vector;
      Target_File   : out GNATCOLL.VFS.Virtual_File)
   is
      use GNATCOLL.VFS;
   begin
      if Self.Handler.Configuration.Project_Diagnostics_Enabled then
         declare
            Project_File : constant Virtual_File :=
              Self.Handler.Project_Status.Get_Project_File;
            Root_Dir     : constant Virtual_File :=
              Self.Handler.Client.Root_Directory;
         begin
            --  Set the target file according to the source's initial target
            --  (root directory or project file itself)
            Target_File :=
              (if Project_File.Is_Regular_File then Project_File
               else Root_Dir);

            Self.Last_Status := Self.Handler.Project_Status;

            Tracer.Trace ("Project loading status: " & Self.Last_Status'Image);

            --  If we have a valid project return immediately: we want to display
            --  diagnostics only if there is an issue to solve or a potential
            --  enhancement.

            Diagnostics.Append_Vector
              (LSP.Ada_Project_Loading.Get_Diagnostics (Self.Last_Status));
         end;

      end if;
   end Get_Diagnostics;

   ------------------------
   -- Has_New_Diagnostic --
   ------------------------

   overriding function Has_New_Diagnostic
     (Self    : in out Diagnostic_Source)
      return Boolean is
   begin
      if Self.Handler.Configuration.Project_Diagnostics_Enabled then
         return LSP.Ada_Project_Loading.Has_New_Diagnostics
           (Self.Last_Status,
            Self.Handler.Project_Status);
      else
         return False;
      end if;
   end Has_New_Diagnostic;

end LSP.Ada_Handlers.Project_Diagnostics;

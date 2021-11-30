------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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

with VSS.Strings.Conversions;

with LSP.Messages.Client_Requests;
with LSP.Types;

package body LSP.Client_Side_File_Monitors is

   method : constant VSS.Strings.Virtual_String :=
     "workspace/didChangeWatchedFiles";

   -------------------------
   -- Monitor_Directories --
   -------------------------

   overriding procedure Monitor_Directories
     (Self        : access File_Monitor;
      Directories : GNATCOLL.VFS.File_Array)
   is
      Request      : LSP.Messages.Client_Requests.RegisterCapability_Request;
      Registration : LSP.Messages.Registration :=
        ((id     => <>,
          method => method,
          registerOptions =>
            (Kind => LSP.Types.Did_Change_Watched_Files_Registration_Option,
             others => <>)));

   begin
      Self.Stop_Monitoring_Directories;
      --  Construct a registration id
      Self.Registration_Id :=
        VSS.Strings.To_Virtual_String
          ("fm" & Integer'Wide_Wide_Image (-Self.Last_Id));
      Self.Last_Id := Self.Last_Id + 1;
      Registration.id := Self.Registration_Id;

      for Dir of Directories loop
         declare
            use type VSS.Strings.Virtual_String;

            Full_Name : constant GNATCOLL.VFS.Filesystem_String :=
              Dir.Full_Name;

            Glob : constant VSS.Strings.Virtual_String :=
              VSS.Strings.Conversions.To_Virtual_String (String (Full_Name))
              & "*";

            Watcher   : constant LSP.Messages.FileSystemWatcher :=
              (kind => (LSP.Messages.WatchKind => True),
               globPattern => Glob);

         begin
            Registration.registerOptions.DidChangeWatchedFiles.watchers.Append
              (Watcher);
         end;
      end loop;

      Request.params.registrations.Append (Registration);
      Self.Client.On_RegisterCapability_Request (Request);
   end Monitor_Directories;

   ---------------------------------
   -- Stop_Monitoring_Directories --
   ---------------------------------

   overriding procedure Stop_Monitoring_Directories
     (Self : access File_Monitor)
   is
      Request : LSP.Messages.Client_Requests.UnregisterCapability_Request;
      Unregistration : constant LSP.Messages.Unregistration :=
        ((id     => Self.Registration_Id,
          method => method));

   begin
      if not Self.Registration_Id.Is_Empty then
         Request.params.unregisterations.Append (Unregistration);
         Self.Client.On_UnregisterCapability_Request (Request);
         Self.Registration_Id := VSS.Strings.Empty_Virtual_String;
      end if;
   end Stop_Monitoring_Directories;

end LSP.Client_Side_File_Monitors;

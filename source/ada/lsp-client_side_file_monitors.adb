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

with VSS.JSON.Streams;
with VSS.Strings.Conversions;

with LSP.Enumerations;
with LSP.Servers;
with LSP.Structures.LSPAny_Vectors;
with LSP.Utils;

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
      use type LSP.Enumerations.WatchKind;

      Create_Change_Delete : constant LSP.Enumerations.WatchKind :=
        LSP.Enumerations.Create +
        LSP.Enumerations.Change +
        LSP.Enumerations.Delete;

      Request_Id   : constant LSP.Structures.Integer_Or_Virtual_String :=
        Self.Handler.Server.Allocate_Request_Id;

      Request      : LSP.Structures.RegistrationParams;

      Registration : LSP.Structures.Registration :=
        (id     => <>,
         method => method,
         registerOptions => <>);

      Options : LSP.Structures.LSPAny_Optional renames
        Registration.registerOptions;
      --  JSON: { watchers: FileSystemWatcher[]; }
   begin
      Self.Stop_Monitoring_Directories;
      --  Construct a registration id
      Self.Registration_Id := LSP.Utils.Image (Request_Id);
      Registration.id := Self.Registration_Id;

      Options.Append
        (VSS.JSON.Streams.JSON_Stream_Element'
           (Kind => VSS.JSON.Streams.Start_Object));

      Options.Append
        (VSS.JSON.Streams.JSON_Stream_Element'
           (Kind => VSS.JSON.Streams.Key_Name,
            Key_Name => "watchers"));

      Options.Append
        (VSS.JSON.Streams.JSON_Stream_Element'
           (Kind => VSS.JSON.Streams.Start_Array));

      for Dir of Directories loop
         declare
            Full_Name : constant String := Dir.Display_Full_Name;

            Pattern : constant LSP.Structures.Pattern :=
              (VSS.Strings.Conversions.To_Virtual_String (Full_Name & '*')
                 with null record);

            Glob : constant LSP.Structures.GlobPattern :=
              (Is_Pattern => True, Pattern => Pattern);

            Watcher : constant LSP.Structures.FileSystemWatcher :=
              (kind => (Is_Set => True, Value => Create_Change_Delete),
               globPattern => Glob);

         begin
            LSP.Structures.LSPAny_Vectors.To_Any (Watcher, Options);
         end;
      end loop;

      Options.Append
        (VSS.JSON.Streams.JSON_Stream_Element'
           (Kind => VSS.JSON.Streams.End_Array));

      Options.Append
        (VSS.JSON.Streams.JSON_Stream_Element'
           (Kind => VSS.JSON.Streams.End_Object));

      Request.registrations.Append (Registration);
      Self.Handler.Server.On_RegisterCapability_Request (Request_Id, Request);
   end Monitor_Directories;

   ---------------------------------
   -- Stop_Monitoring_Directories --
   ---------------------------------

   overriding procedure Stop_Monitoring_Directories
     (Self : access File_Monitor)
   is
      Request_Id   : constant LSP.Structures.Integer_Or_Virtual_String :=
        Self.Handler.Server.Allocate_Request_Id;

      Request : LSP.Structures.UnregistrationParams;

      Unregistration : constant LSP.Structures.Unregistration :=
        ((id     => Self.Registration_Id,
          method => method));

   begin
      if not Self.Registration_Id.Is_Empty then
         Request.unregisterations.Append (Unregistration);
         Self.Handler.Server.On_UnregisterCapability_Request
           (Request_Id, Request);
         Self.Registration_Id := VSS.Strings.Empty_Virtual_String;
      end if;
   end Stop_Monitoring_Directories;

end LSP.Client_Side_File_Monitors;

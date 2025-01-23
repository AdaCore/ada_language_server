------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2024, AdaCore                       --
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

with GNATCOLL.VFS;
with LSP.Ada_Configurations;
with LSP.Client_Message_Receivers;
with LSP.Enumerations;
with LSP.Server_Notifications.DidChangeConfiguration;
with VSS.String_Vectors;

package body LSP.Ada_Did_Change_Configurations is

   type Apply_Config_Job
     (Parent : not null access constant Ada_Did_Change_Handler)
   is limited new LSP.Server_Jobs.Server_Job with record
      Message       : LSP.Server_Messages.Server_Message_Access;
      Configuration : LSP.Ada_Configurations.Configuration;
      Reload        : Boolean := False;
   end record;

   type Apply_Config_Job_Access is access all Apply_Config_Job;

   overriding function Priority
     (Self : Apply_Config_Job) return LSP.Server_Jobs.Job_Priority is
       (LSP.Server_Jobs.Fence);

   overriding procedure Execute
     (Self   : in out Apply_Config_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   overriding function Message (Self : Apply_Config_Job)
     return LSP.Server_Messages.Server_Message_Access is (Self.Message);

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : in out Apply_Config_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status) is
   begin
      Status := LSP.Server_Jobs.Done;
      Self.Parent.Context.Set_Configuration (Self.Configuration);

      if Self.Reload then
         Self.Parent.Context.Reload_Project;
      end if;

   exception
      when E : others =>
         Self.Parent.Context.Trace_Exception (E);
   end Execute;

   ----------------
   -- Create_Job --
   ----------------

   overriding function Create_Job
     (Self    : Ada_Did_Change_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
      return LSP.Server_Jobs.Server_Job_Access
   is
      Value : LSP.Server_Notifications.DidChangeConfiguration.Notification
        renames LSP.Server_Notifications.DidChangeConfiguration.Notification
          (Message.all);

      Result : constant Apply_Config_Job_Access :=
        new Apply_Config_Job (Self'Unchecked_Access);

      Base_Config : constant LSP.Ada_Configurations.Configuration :=
        LSP.Ada_Configurations.Configuration
          (Self.Context.Get_Base_Configuration.all);

      Current_Config : constant LSP.Ada_Configurations.Configuration :=
        LSP.Ada_Configurations.Configuration
          (Self.Context.Get_Configuration.all);

      New_Config : LSP.Ada_Configurations.Configuration
        renames Result.Configuration;
      Reload : Boolean renames Result.Reload;

      Messages : VSS.String_Vectors.Virtual_String_Vector;
   begin
      Self.Context.Get_Trace_Handle.Trace
        ("Processing received workspace/didChangeConfiguration notification");

      --  The configuration loading logic offers a Reload output boolean that
      --  indicates if values were changed that require reloading the project.
      --  However this doesn't work when a 'null' value is received. 'null'
      --  values are ignored by the configuration parser and thus don't signal
      --  the Reload flag.
      --
      --  To address that we have to compare the relevant configuration
      --  settings after they are parsed and cannot rely on the configuration
      --  parsing logic.

      --  Start with the base configuration.
      New_Config := Base_Config;

      --  Read the new received configuration. 'null' values will be ignored
      --  and thus keep the value from the base config.
      New_Config.Read_JSON (Value.Params.settings, Messages);

      Reload := False;

      Self.Context.Send_Messages
        (Show     => True,
         Messages => Messages,
         Severity => LSP.Enumerations.Warning,
         File     => GNATCOLL.VFS.No_File);

      --  Always reload project if Project_Tree isn't ready
      if not Self.Context.Project_Tree_Is_Defined then
         Self.Context.Get_Trace_Handle.Trace
           ("Scheduling a reload because the project tree is not defined");
         Reload := True;
      end if;

      --  Then compare the new configuration with the current one
      Reload := Reload or else Current_Config.Needs_Reload (New_Config);

      if Reload then
         --  Stop indexing by changing project stamp
         Self.Context.Increment_Project_Timestamp;
      end if;

      return LSP.Server_Jobs.Server_Job_Access (Result);
   end Create_Job;

end LSP.Ada_Did_Change_Configurations;

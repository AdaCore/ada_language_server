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

with GPR2.Context;
with LSP.Ada_Configurations;
with LSP.Client_Message_Receivers;
with LSP.Server_Notifications.DidChangeConfiguration;
with VSS.Strings;

package body LSP.Ada_Did_Change_Configurations is

   type Apply_Config_Job
     (Parent : not null access constant Ada_Did_Change_Handler)
   is limited new LSP.Server_Jobs.Server_Job with record
      Message       : LSP.Server_Messages.Server_Message_Access;
      Configuration : LSP.Ada_Configurations.Configuration;
      Reload        : Boolean;
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
      use type VSS.Strings.Virtual_String;
      use type GPR2.Context.Object;

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
   begin

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
      New_Config.Read_JSON (Value.Params.settings, Reload);

      --  Always reload project if Project_Tree isn't ready
      Reload := Reload or not Self.Context.Project_Tree_Is_Defined;

      --  Compare with the current configuration to see if a reload is
      --  necessary
      Reload :=
        Reload
        or else New_Config.Relocate_Build_Tree
                /= Current_Config.Relocate_Build_Tree
        or else New_Config.Relocate_Root /= Current_Config.Relocate_Root
        or else New_Config.Project_File /= Current_Config.Project_File
        or else New_Config.Context /= Current_Config.Context
        or else New_Config.Charset /= Current_Config.Charset
        or else New_Config.Follow_Symlinks /= Current_Config.Follow_Symlinks;

      if Reload then
         --  Stop indexing by changing project stamp
         Self.Context.Increment_Project_Timestamp;
      end if;

      return LSP.Server_Jobs.Server_Job_Access (Result);
   end Create_Job;

end LSP.Ada_Did_Change_Configurations;

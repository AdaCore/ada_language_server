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

with LSP.Ada_Configurations;
with LSP.Client_Message_Receivers;
with LSP.Server_Notifications.DidChangeConfiguration;

package body LSP.Ada_Did_Change_Configurations is

   type Apply_Config_Job
     (Parent : not null access constant Ada_Did_Change_Handler)
   is limited new LSP.Server_Jobs.Server_Job with record
      Message       : LSP.Server_Messages.Server_Message_Access;
      Configuration : LSP.Ada_Configurations.Configuration;
      Reload        : Boolean;
      Is_Done       : Boolean := False;
   end record;

   type Apply_Config_Job_Access is access all Apply_Config_Job;

   overriding function Priority
     (Self : Apply_Config_Job) return LSP.Server_Jobs.Job_Priority is
       (LSP.Server_Jobs.Fence);

   overriding function Is_Done (Self : Apply_Config_Job) return Boolean is
     (Self.Is_Done);

   overriding procedure Execute
     (Self   : in out Apply_Config_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class);

   overriding function Message (Self : Apply_Config_Job)
     return LSP.Server_Messages.Server_Message_Access is (Self.Message);

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : in out Apply_Config_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class) is
   begin
      Self.Parent.Context.Set_Configuration (Self.Configuration);

      if Self.Reload then
         Self.Parent.Context.Reload_Project;
      end if;

      Self.Is_Done := True;
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

      Reload : Boolean renames Result.Reload;
   begin
      Result.Configuration := Self.Context.Get_Configuration.all;
      Result.Configuration.Read_JSON (Value.Params.settings, Reload);

      --  Always reload project if Project_Tree isn't ready
      Reload := Reload or not Self.Context.Project_Tree_Is_Defined;

      if Reload then
         --  Stop indexing by changing project stamp
         Self.Context.Increment_Project_Timestamp;
      end if;

      return LSP.Server_Jobs.Server_Job_Access (Result);
   end Create_Job;

end LSP.Ada_Did_Change_Configurations;

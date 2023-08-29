------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
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

with LSP.Servers;

with VSS.Strings.Formatters.Integers;
with VSS.Strings.Templates;

package body LSP.Ada_Indexing is

   -----------------
   -- Index_Files --
   -----------------

   procedure Index_Files (Self : in out Indexing_Job'Class) is

      use type LSP.Ada_Handlers.Project_Stamp;

      procedure Emit_Progress_Report (Files_Indexed, Total_Files : Natural);

      --------------------------
      -- Emit_Progress_Report --
      --------------------------

      procedure Emit_Progress_Report (Files_Indexed, Total_Files : Natural) is
         use type Ada.Calendar.Time;

         Current          : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         Message_Template : VSS.Strings.Templates.Virtual_String_Template :=
           "{}/{} files";

      begin
         if Current - Self.Progress_Report_Sent < 0.5 then
            --  Send only 2 notifications per second
            return;
         end if;

         Self.Progress_Report_Sent := Current;

         Self.Server.On_ProgressReport_Work_Done
           (Self.Indexing_Token,
            (percentage => (True, (Files_Indexed * 100) / Total_Files),
             message    =>
               Message_Template.Format
                 (VSS.Strings.Formatters.Integers.Image (Files_Indexed),
                  VSS.Strings.Formatters.Integers.Image (Total_Files)),
             others     => <>));
      end Emit_Progress_Report;

   begin
      if Self.Total_Files_Indexed = 0 then
         Self.Server.On_ProgressBegin_Work_Done
           (Self.Indexing_Token,
            (title => "Indexing", percentage => (True, 0), others => <>));
      end if;

      if Self.Handler.Get_Project_Stamp /= Self.Project_Stamp
        or Self.Handler.Is_Shutdown
      then
         --  Project was updated, nothing to index; or server is under shutdown

         Self.Files_To_Index.Clear;
      end if;

      while not Self.Files_To_Index.Is_Empty loop
         declare
            Cursor : File_Sets.Cursor := Self.Files_To_Index.First;
            File   : constant GNATCOLL.VFS.Virtual_File :=
              File_Sets.Element (Cursor);

         begin
            Self.Files_To_Index.Delete (Cursor);
            Self.Total_Files_Indexed := Self.Total_Files_Indexed + 1;

            if not Self.Handler.Is_Open_Document (File) then
               for Context of Self.Handler.Contexts_For_File (File) loop
                  --  Set Reparse to False to avoid issues with LAL envs
                  --  for now (see T226-048 for more info).
                  Context.Index_File (File, Reparse => False);
               end loop;

               Emit_Progress_Report
                 (Self.Total_Files_Indexed, Self.Total_Files_To_Index);

               --  Check whether another request is pending. If so, pause
               --  the indexing; it will be resumed later as part of
               --  After_Request.

               exit when Self.Server.Has_Pending_Work;
            end if;

         end;
      end loop;

      if Self.Files_To_Index.Is_Empty then
         --  Indexing done.

         Self.Server.On_ProgressEnd_Work_Done
           (Self.Indexing_Token, (message => <>));

         return;
      end if;

      declare
         Job : LSP.Server_Jobs.Server_Jobs_Access :=
           new Indexing_Job'
             (Server               => Self.Server.all'Unchecked_Access,
              Handler              => Self.Handler.all'Unchecked_Access,
              Files_To_Index       => Self.Files_To_Index,
              Indexing_Token       => Self.Indexing_Token,
              Total_Files_Indexed  => Self.Total_Files_Indexed,
              Total_Files_To_Index => Self.Total_Files_To_Index,
              Progress_Report_Sent => Self.Progress_Report_Sent,
              Project_Stamp        => Self.Project_Stamp);

      begin
         Self.Server.Enqueue (Job);
      end;
   end Index_Files;

   -----------------------
   -- Schedule_Indexing --
   -----------------------

   procedure Schedule_Indexing
     (Server        : not null access LSP.Servers.Server'Class;
      Handler       : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Configuration : LSP.Ada_Configurations.Configuration'Class;
      Project_Stamp : LSP.Ada_Handlers.Project_Stamp;
      Files         : File_Sets.Set) is
   begin
      if Files.Is_Empty
        or not Configuration.Indexing_Enabled
        or Handler.Is_Shutdown
      then
         return;
      end if;

      declare
         Id    : constant LSP.Structures.Integer_Or_Virtual_String :=
           Server.Allocate_Request_Id;
         Token : constant LSP.Structures.ProgressToken :=
           Handler.Allocate_Progress_Token ("indexing");
         Job   : LSP.Server_Jobs.Server_Jobs_Access :=
           new Indexing_Job'
             (Server               => Server,
              Handler              => Handler,
              Files_To_Index       => Files,
              Indexing_Token       => Token,
              Total_Files_Indexed  => 0,
              Total_Files_To_Index => Natural (Files.Length),
              Progress_Report_Sent => Ada.Calendar.Clock,
              Project_Stamp        => Project_Stamp);

      begin
         Job.Server.On_Progress_Create_Request (Id, (token => Token));
         --  FIXME: wait response before sending progress notifications.
         --  Currenctly, we just send a `window/workDoneProgress/create`
         --  request and immediately after this start sending notifications.
         --  We could do better, send request, wait for client response and
         --  start progress-report sending only after response.

         Server.Enqueue (Job);
      end;
   end Schedule_Indexing;

   ----------------------------------
   -- Visit_Server_Message_Visitor --
   ----------------------------------

   overriding procedure Visit_Server_Message_Visitor
     (Self  : Indexing_Job;
      Value : in out
        LSP.Server_Message_Visitors.Server_Message_Visitor'Class) is
   begin
      Self'Unrestricted_Access.Index_Files;
   end Visit_Server_Message_Visitor;

end LSP.Ada_Indexing;

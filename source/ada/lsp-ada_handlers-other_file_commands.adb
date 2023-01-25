------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2021, AdaCore                       --
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

with Ada.Strings.UTF_Encoding;

with LSP.Messages.Client_Requests;

with VSS.Strings.Conversions;

package body LSP.Ada_Handlers.Other_File_Commands is

   ------------
   -- Create --
   ------------

   overriding function Create
     (JS : not null access LSP.JSON_Streams.JSON_Stream'Class) return Command
   is
   begin
      return V : Command do
         pragma Assert (JS.R.Is_Start_Object);
         JS.R.Read_Next;

         while not JS.R.Is_End_Object loop
            pragma Assert (JS.R.Is_Key_Name);
            declare
               Key : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
                 VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
            begin
               JS.R.Read_Next;

               if Key = "uri" then
                  LSP.Types.Read_LSP_URI (JS, V.URI);
               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end return;
   end Create;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self    : Command;
      Handler : not null access LSP.Server_Notification_Receivers
        .Server_Notification_Receiver'
        Class;
      Client : not null access LSP.Client_Message_Receivers
        .Client_Message_Receiver'
        Class;
      Error : in out LSP.Errors.Optional_ResponseError)
   is
      Message_Handler : LSP.Ada_Handlers.Message_Handler renames
        LSP.Ada_Handlers.Message_Handler (Handler.all);

      File : constant GNATCOLL.VFS.Virtual_File :=
        Message_Handler.To_File (Self.URI);

      Other_File : constant GNATCOLL.VFS.Virtual_File :=
        Message_Handler.Project_Tree.Other_File (File);

      URI : constant LSP.Messages.DocumentUri :=
        Message_Handler.From_File (Other_File);

      Message : constant LSP.Messages.Client_Requests.ShowDocument_Request :=
        (params =>
           (uri       => URI,
            takeFocus => LSP.Types.True,
            others    => <>),
         others => <>);
   begin
      Client.On_ShowDocument_Request (Message);
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Command'Class; URI : LSP.Messages.DocumentUri)
   is
   begin
      Self.URI := URI;
   end Initialize;

   -------------------
   -- Write_Command --
   -------------------

   procedure Write_Command
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Command)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write_LSP_URI (S, V.URI);
      JS.End_Object;
   end Write_Command;

end LSP.Ada_Handlers.Other_File_Commands;

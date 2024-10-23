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

with GNATCOLL.VFS; use GNATCOLL.VFS;

with LSP.Constants;
with LSP.Enumerations;
with LSP.Servers;

package body LSP.Ada_Handlers.Open_Project_File_Commands is

   ------------
   -- Create --
   ------------

   overriding function Create
     (Any : not null access LSP.Structures.LSPAny_Vector)
      return Command
   is
   begin
      return (LSP.Ada_Commands.Command with null record);
   end Create;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self     : Command;
      Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Response : in out LSP.Structures.LSPAny_Or_Null;
      Error    : in out LSP.Errors.ResponseError_Optional)
   is
      pragma Unreferenced (Response);

      Project_File : constant Virtual_File :=
        LSP.Ada_Project_Loading.Get_Project_File (Handler.Project_Status);

      URI : constant LSP.Structures.DocumentUri :=
        (if Project_File.Is_Regular_File
         then Handler.To_URI (Project_File.Display_Full_Name)
         else "");

      Message : constant LSP.Structures.ShowDocumentParams :=
        (uri       => (VSS.Strings.Virtual_String (URI) with null record),
         takeFocus => LSP.Constants.True,
         others    => <>);

   begin
      if Project_File.Is_Regular_File then
         Handler.Sender.On_ShowDocument_Request
           (Handler.Server.Allocate_Request_Id, Message);
      else
         Handler.Sender.On_ShowMessage_Notification (
            (LSP.Enumerations.Error,
             "Ada: No GPR project file is loaded.")
         );
      end if;
   end Execute;

end LSP.Ada_Handlers.Open_Project_File_Commands;

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

with VSS.JSON.Streams;

with GNATCOLL.Tribooleans;

with GPR2.Project.Source;
with GPR2.Path_Name;

with LSP.Constants;
with LSP.Servers;

package body LSP.Ada_Handlers.Other_File_Commands is

   ------------
   -- Create --
   ------------

   overriding function Create
     (Any : not null access LSP.Structures.LSPAny_Vector)
      return Command
   is
      use type VSS.Strings.Virtual_String;
      use all type VSS.JSON.Streams.JSON_Stream_Element_Kind;

      Index : Natural := Any.First_Index;
   begin
      return V : Command do

         if Index < Any.Last_Index
           and then Any (Index).Kind = Start_Array
         then
            Index := Index + 1;

            if Index < Any.Last_Index
              and then Any (Index).Kind = Start_Object
            then
               Index := Index + 1;

               if Index < Any.Last_Index
                 and then Any (Index).Kind = Key_Name
                 and then Any (Index).Key_Name = "uri"
               then
                  Index := Index + 1;

                  if Index < Any.Last_Index
                    and then Any (Index).Kind = String_Value
                  then
                     V.URI := (Any (Index).String_Value with null record);
                  end if;
               end if;
            end if;
         end if;
      end return;
   end Create;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self    : Command;
      Handler : not null access
        LSP.Server_Notification_Receivers.Server_Notification_Receiver'Class;
      Sender  : not null access LSP.Client_Message_Receivers.
        Client_Message_Receiver'Class;
      Id      : LSP.Structures.Integer_Or_Virtual_String;
      Error   : in out LSP.Errors.ResponseError_Optional)
   is
      Message_Handler : LSP.Ada_Handlers.Message_Handler renames
        LSP.Ada_Handlers.Message_Handler (Handler.all);

      File : constant GNATCOLL.VFS.Virtual_File :=
        Message_Handler.To_File (Self.URI);

      function Other_File return GNATCOLL.VFS.Virtual_File;

      ----------------
      -- Other_File --
      ----------------

      function Other_File return GNATCOLL.VFS.Virtual_File is
         F : constant GPR2.Path_Name.Object := GPR2.Path_Name.Create (File);
      begin
         for V in Message_Handler.Project_Tree.Iterate
           (Status => (GPR2.Project.S_Externally_Built =>
                           GNATCOLL.Tribooleans.Indeterminate))
         loop
            declare
               Source     : constant GPR2.Project.Source.Object :=
                               GPR2.Project.Tree.Element (V).Source (F);
               Other_Part : GPR2.Project.Source.Source_Part;
            begin
               if Source.Is_Defined then
                  Other_Part := Source.Other_Part_Unchecked (GPR2.No_Index);
                  if Other_Part.Source.Is_Defined then
                     return Other_Part.Source.Path_Name.Virtual_File;
                  end if;
               end if;
            end;
         end loop;

         if Message_Handler.Project_Tree.Has_Runtime_Project then
            declare
               Source     : constant GPR2.Project.Source.Object :=
                               Message_Handler.Project_Tree.Runtime_Project.
                                 Source (F);
               Other_Part : GPR2.Project.Source.Source_Part;
            begin
               if Source.Is_Defined then
                  Other_Part := Source.Other_Part_Unchecked (GPR2.No_Index);
                  if Other_Part.Source.Is_Defined then
                     return Other_Part.Source.Path_Name.Virtual_File;
                  end if;
               end if;
            end;
         end if;
         return File;
      end Other_File;

      URI : constant LSP.Structures.DocumentUri :=
        Message_Handler.To_URI (Other_File.Display_Full_Name);

      Message : constant LSP.Structures.ShowDocumentParams :=
        (uri       => (VSS.Strings.Virtual_String (URI) with null record),
         takeFocus => LSP.Constants.True,
         others    => <>);

      New_Id : constant LSP.Structures.Integer_Or_Virtual_String :=
        Message_Handler.Server.Allocate_Request_Id;
   begin
      Sender.On_ShowDocument_Request (New_Id, Message);
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Command'Class;
      URI  : LSP.Structures.DocumentUri)
   is
   begin
      Self.URI := URI;
   end Initialize;

end LSP.Ada_Handlers.Other_File_Commands;

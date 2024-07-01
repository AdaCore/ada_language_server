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

with GNATCOLL.VFS; use GNATCOLL.VFS;

with GPR2.Build.Compilation_Unit;
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
     (Self     : Command;
      Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Response : in out LSP.Structures.LSPAny_Or_Null;
      Error    : in out LSP.Errors.ResponseError_Optional)
   is
      pragma Unreferenced (Response);
      File : constant GNATCOLL.VFS.Virtual_File :=
        Handler.To_File (Self.URI);

      function Other_File return GNATCOLL.VFS.Virtual_File;

      ----------------
      -- Other_File --
      ----------------

      function Other_File return GNATCOLL.VFS.Virtual_File is

         F : constant GPR2.Path_Name.Object := GPR2.Path_Name.Create (File);

         function Other_File_From_Unit
           (Unit : GPR2.Build.Compilation_Unit.Object)
           return GNATCOLL.VFS.Virtual_File;
         --  Return the other file, knowing that the original file was
         --  related to Unit.

         function Unit_For_File return GPR2.Build.Compilation_Unit.Object;
         --   File the Unit object corresponding to File.

         --------------------------
         -- Other_File_From_Unit --
         --------------------------

         function Other_File_From_Unit
           (Unit : GPR2.Build.Compilation_Unit.Object)
           return GNATCOLL.VFS.Virtual_File
         is
            Spec_File : Virtual_File;
            Body_File : Virtual_File;
         begin
            Spec_File := Unit.Spec.Source.Virtual_File;
            Body_File := Unit.Main_Body.Source.Virtual_File;
            if File = Spec_File then
               return Body_File;
            else
               return Spec_File;
            end if;
         end Other_File_From_Unit;

         -------------------
         -- Unit_For_File --
         -------------------

         function Unit_For_File return GPR2.Build.Compilation_Unit.Object
         is
            Unit : GPR2.Build.Compilation_Unit.Object;
         begin
            --  First look in the closure of sources, then in the
            --  runtime project.
            Unit := Handler.Project_Tree.Root_Project.Unit (F.Base_Name);

            if not Unit.Is_Defined then
               Unit := Handler.Project_Tree.Runtime_Project.Unit
                 (F.Base_Name);
            end if;
            return Unit;
         end Unit_For_File;

      begin
         return Other_File_From_Unit (Unit_For_File);
      end Other_File;

      URI : constant LSP.Structures.DocumentUri :=
        Handler.To_URI (Other_File.Display_Full_Name);

      Message : constant LSP.Structures.ShowDocumentParams :=
        (uri       => (VSS.Strings.Virtual_String (URI) with null record),
         takeFocus => LSP.Constants.True,
         others    => <>);

      New_Id : constant LSP.Structures.Integer_Or_Virtual_String :=
        Handler.Server.Allocate_Request_Id;
   begin
      Handler.Sender.On_ShowDocument_Request (New_Id, Message);
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

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

with GPR2;                 use GPR2;
with GPR2.Build.Source;
with GPR2.Build.Unit_Info;
with GPR2.Project.View;
with VSS.JSON.Streams;

with GNATCOLL.VFS;         use GNATCOLL.VFS;

with GPR2.Build.Compilation_Unit;
with GPR2.Path_Name;

with LSP.Constants;
with LSP.Enumerations;
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

      function Get_Other_File
        (Success   : out Boolean;
         Error_Msg : out VSS.Strings.Virtual_String)
         return GNATCOLL.VFS.Virtual_File;
      --  Return File's other file. If it does not exist,
      --  Success is set to False and it will return No_File,
      --  with an Error_Msg explaining why the other file could not be found.

      --------------------
      -- Get_Other_File --
      --------------------

      function Get_Other_File
        (Success   : out Boolean;
         Error_Msg : out VSS.Strings.Virtual_String)
         return GNATCOLL.VFS.Virtual_File
      is

         F : constant GPR2.Path_Name.Object := GPR2.Path_Name.Create (File);

         function Other_File_From_Unit
           (Unit : GPR2.Build.Compilation_Unit.Object)
           return GNATCOLL.VFS.Virtual_File;
         --  Return the other file, knowing that the original file was
         --  related to Unit.
         --  Return No_File if no other file has been found
         --  (e.g: when querying the other file of a package that has only a
         --  a specification file).

         function Unit_For_File return GPR2.Build.Compilation_Unit.Object;
         --  Return the Unit object corresponding to File

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
            Spec_File := (if Unit.Has_Part (S_Spec) then
               Unit.Spec.Source.Virtual_File else No_File);
            Body_File := (if Unit.Has_Part (S_Body) then
               Unit.Main_Body.Source.Virtual_File else No_File);

            if File = Spec_File then
               return Body_File;
            else
               return Spec_File;
            end if;
         end Other_File_From_Unit;

         -------------------
         -- Unit_For_File --
         -------------------

         function Unit_For_File
           return GPR2.Build.Compilation_Unit.Object is
         begin
            --  Check in the root project's closure for a visible source
            --  corresponding to this file.
            --  Not that the root's project closure includes the runtime.
            if Handler.Project_Tree.Is_Defined then
               declare
                  View           : constant GPR2.Project.View.Object :=
                    Handler.Project_Tree.Root_Project;
                  Visible_Source : constant GPR2.Build.Source.Object :=
                    View.Visible_Source (F.Simple_Name);
                  Unit           : GPR2.Build.Compilation_Unit.Object :=
                    GPR2.Build.Compilation_Unit.Undefined;
               begin
                  --  The source is not visible from the root project (e.g:
                  --  when  querying the other file of an Ada file that
                  --  does not belong to the loaded project).
                  if not Visible_Source.Is_Defined then
                     return GPR2.Build.Compilation_Unit.Undefined;
                  end if;

                  declare
                     Unit_Info : constant GPR2.Build.Unit_Info.Object :=
                       Visible_Source.Unit;
                  begin
                     if Unit_Info.Is_Defined then
                        Unit :=
                          View.Namespace_Roots.First_Element.Unit
                            (Unit_Info.Name);
                     end if;

                     return Unit;
                  end;
               end;
            else
               return GPR2.Build.Compilation_Unit.Undefined;
            end if;
         end Unit_For_File;

         Unit       : constant GPR2.Build.Compilation_Unit.Object := Unit_For_File;
         Other_File : Virtual_File;
      begin
         if not Unit.Is_Defined then
            Success := False;
            Error_Msg :=
              VSS.Strings.Conversions.To_Virtual_String
                ("Could not find other file for '"
                 & File.Display_Base_Name
                 & "': this file is not visible from the current project.");

            return No_File;
         else
            Other_File := Other_File_From_Unit (Unit => Unit);

            if Other_File = No_File then
               Success := False;
               Error_Msg :=
                 VSS.Strings.Conversions.To_Virtual_String
                   ("Could not find other file for '"
                    & File.Display_Base_Name
                    & "': the unit has no other part.");
            else
               Success := True;
            end if;

            return Other_File;
         end if;
      end Get_Other_File;

      Success    : Boolean;
      Error_Msg  : VSS.Strings.Virtual_String;
      Other_File : constant Virtual_File := Get_Other_File (Success, Error_Msg);
   begin
      if not Success then
         Error :=
           (Is_Set => True,
            Value  =>
              (code => LSP.Enumerations.InternalError, message => Error_Msg));
         return;
      end if;

      declare
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
      end;
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

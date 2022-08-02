------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;

with Laltools.Refactor; use Laltools.Refactor;
with Laltools.Refactor.Extract_Subprogram;
use Laltools.Refactor.Extract_Subprogram;

with LSP.Common;
with LSP.Messages.Client_Requests;
with LSP.Lal_Utils;

with VSS.Strings.Conversions;

package body LSP.Ada_Handlers.Refactor_Extract_Subprogram is

   ------------------------
   -- Append_Code_Action --
   ------------------------

   procedure Append_Code_Action
     (Self            : in out Command;
      Context         : Context_Access;
      Commands_Vector : in out LSP.Messages.CodeAction_Vector;
      Where           : LSP.Messages.Location;
      Subprogram_Kind : Libadalang.Common.Ada_Subp_Kind)
   is
      Pointer     : LSP.Commands.Command_Pointer;
      Code_Action : LSP.Messages.CodeAction;

      Code_Action_Title  : constant String :=
        (if Subprogram_Kind in Ada_Subp_Kind_Procedure_Range then
           "Extract Procedure"
         else
           "Extract Function");

   begin
      Self.Initialize
        (Context         => Context.all,
         Where           => Where,
         Subprogram_Kind => Subprogram_Kind);

      Pointer.Set (Self);

      Code_Action :=
        (title       =>
           VSS.Strings.Conversions.To_Virtual_String (Code_Action_Title),
         kind        =>
           (Is_Set => True,
            Value  => LSP.Messages.RefactorExtract),
         diagnostics => (Is_Set => False),
         edit        => (Is_Set => False),
         isPreferred => (Is_Set => False),
         disabled    => (Is_Set => False),
         command     =>
           (Is_Set => True,
            Value  =>
              (Is_Unknown => False,
               title      => "",
               Custom     => Pointer)));

      Commands_Vector.Append (Code_Action);
   end Append_Code_Action;

   ------------
   -- Create --
   ------------

   overriding function Create
     (JS : not null access LSP.JSON_Streams.JSON_Stream'Class)
      return Command is
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

               if Key = "context_id" then
                  LSP.Types.Read_String (JS, V.Context_Id);

               elsif Key = "section_to_extract_sloc" then
                  LSP.Messages.Location'Read
                    (JS, V.Section_To_Extract_SLOC);

               elsif Key = "subprogram_kind" then
                  declare
                     Subprogram_Kind : VSS.Strings.Virtual_String;

                     use VSS.Strings.Conversions;

                  begin
                     LSP.Types.Read_String (JS, Subprogram_Kind);
                     V.Subprogram_Kind :=
                       Libadalang.Common.Ada_Subp_Kind'Value
                         (To_UTF_8_String (Subprogram_Kind));
                  end;

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
      Handler : not null access LSP.Server_Notification_Receivers.
        Server_Notification_Receiver'Class;
      Client : not null access LSP.Client_Message_Receivers.
        Client_Message_Receiver'Class;
      Error : in out LSP.Errors.Optional_ResponseError)
   is
      use LSP.Messages;
      use LSP.Types;
      use VSS.Strings.Conversions;

      Message_Handler : LSP.Ada_Handlers.Message_Handler renames
        LSP.Ada_Handlers.Message_Handler (Handler.all);
      Context         : LSP.Ada_Contexts.Context renames
        Message_Handler.Contexts.Get (Self.Context_Id).all;

      Apply           : Client_Requests.Workspace_Apply_Edit_Request;
      Workspace_Edits : WorkspaceEdit renames Apply.params.edit;
      Label           : Optional_Virtual_String renames Apply.params.label;

      function Analysis_Units return Analysis_Unit_Array is
        (Context.Analysis_Units);
      --  Provides the Context Analysis_Unit_Array to the Mode_Changer

      Unit               : constant Analysis_Unit :=
        Context.LAL_Context.Get_From_File
          (Context.URI_To_File (Self.Section_To_Extract_SLOC.uri));
      Section_To_Extract : constant Source_Location_Range :=
        (Langkit_Support.Slocs.Line_Number
           (Self.Section_To_Extract_SLOC.span.first.line) + 1,
         Langkit_Support.Slocs.Line_Number
           (Self.Section_To_Extract_SLOC.span.last.line) + 1,
         Column_Number (Self.Section_To_Extract_SLOC.span.first.character) + 1,
         Column_Number (Self.Section_To_Extract_SLOC.span.last.character) + 1);

      Extractor : constant Subprogram_Extractor :=
        Create_Subprogram_Extractor
          (Unit               => Unit,
           Section_To_Extract => Section_To_Extract,
           Subprogram_Kind    => Self.Subprogram_Kind,
           Subprogram_Name    =>
             Default_Extracted_Subprogram_Name
               (Unit            => Unit,
                Location        =>
                  (Section_To_Extract.Start_Line,
                   Section_To_Extract.Start_Column)));
      Edits     : constant Refactoring_Edits :=
        Extractor.Refactor (Analysis_Units'Access);

   begin
      if Edits = No_Refactoring_Edits then
         Error :=
           (Is_Set => True,
            Value  =>
              (code    => LSP.Errors.UnknownErrorCode,
               message => VSS.Strings.Conversions.To_Virtual_String
                 ("Failed to execute the Extract Subprogram refactoring."),
               data    => <>));

      else
         Workspace_Edits :=
           LSP.Lal_Utils.To_Workspace_Edit
             (Edits               => Edits,
              Resource_Operations => Message_Handler.Resource_Operations,
              Versioned_Documents => Message_Handler.Versioned_Documents,
              Document_Provider   => Message_Handler'Access);
         Label :=
           (Is_Set => True,
            Value  => To_Virtual_String (Command'External_Tag));

         Client.On_Workspace_Apply_Edit_Request (Apply);
      end if;

   exception
      when E : others =>
         LSP.Common.Log (Message_Handler.Trace, E);
         Error :=
           (Is_Set => True,
            Value  =>
              (code => LSP.Errors.UnknownErrorCode,
               message => VSS.Strings.Conversions.To_Virtual_String
                 ("Failed to execute the Extract Subprogram refactoring."),
               data => <>));
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self             : in out Command'Class;
      Context          : LSP.Ada_Contexts.Context;
      Where            : LSP.Messages.Location;
      Subprogram_Kind  : Libadalang.Common.Ada_Subp_Kind) is
   begin
      Self.Context_Id := Context.Id;
      Self.Section_To_Extract_SLOC := Where;
      Self.Subprogram_Kind := Subprogram_Kind;
   end Initialize;

   -------------------
   -- Write_Command --
   -------------------

   procedure Write_Command
     (S : access Ada.Streams.Root_Stream_Type'Class;
      C : Command)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

   begin
      JS.Start_Object;
      JS.Key ("context_id");
      LSP.Types.Write_String (S, C.Context_Id);
      JS.Key ("section_to_extract_sloc");
      LSP.Messages.Location'Write (S, C.Section_To_Extract_SLOC);
      JS.Key ("subprogram_kind");
      LSP.Types.Write_String
        (S,
         VSS.Strings.Conversions.To_Virtual_String (C.Subprogram_Kind'Image));
      JS.End_Object;
   end Write_Command;

end LSP.Ada_Handlers.Refactor_Extract_Subprogram;

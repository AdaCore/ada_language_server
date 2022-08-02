------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                    Copyright (C) 2021-2022, AdaCore                      --
--                                                                          --
-- Libadalang Tools  is free software; you can redistribute it and/or modi- --
-- fy  it  under  terms of the  GNU General Public License  as published by --
-- the Free Software Foundation;  either version 3, or (at your option) any --
-- later version. This software  is distributed in the hope that it will be --
-- useful but  WITHOUT  ANY  WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                  --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Strings.UTF_Encoding;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Analysis; use  Libadalang.Analysis;

with Laltools.Refactor.Subprogram_Signature.Change_Parameters_Default_Value;

with LSP.Common;
with LSP.Messages.Client_Requests;
with LSP.Lal_Utils;

with VSS.Strings.Conversions;

package body LSP.Ada_Handlers.Refactor_Change_Parameters_Default_Value is

   ------------------------
   -- Append_Code_Action --
   ------------------------

   procedure Append_Code_Action
     (Self            : in out Command;
      Context         : Context_Access;
      Commands_Vector : in out LSP.Messages.CodeAction_Vector;
      Where           : LSP.Messages.Location)
   is
      Pointer     : LSP.Commands.Command_Pointer;
      Code_Action : LSP.Messages.CodeAction;

   begin
      Self.Initialize
        (Context                      => Context.all,
         Where                        => Where,
         New_Parameters_Default_Value => "");

      Pointer.Set (Self);

      Code_Action :=
        (title       => "Change Parameter Default Value",
         kind        =>
           (Is_Set => True,
            Value  => LSP.Messages.RefactorRewrite),
         diagnostics => (Is_Set => False),
         edit        => (Is_Set => False),
         isPreferred => (Is_Set => False),
         disabled    => (Is_Set => False),
         command     =>
           (Is_Set => True,
            Value  =>
              (Is_Unknown => False,
               title      => <>,
               Custom     => Pointer)));

      Commands_Vector.Append (Code_Action);
   end Append_Code_Action;

   ------------
   -- Create --
   ------------

   overriding
   function Create
     (JS : not null access LSP.JSON_Streams.JSON_Stream'Class)
      return Command
   is
      use Ada.Strings.UTF_Encoding;
      use LSP.Messages;
      use LSP.Types;
      use VSS.Strings.Conversions;

   begin
      return C : Command do
         pragma Assert (JS.R.Is_Start_Object);

         JS.R.Read_Next;

         while not JS.R.Is_End_Object loop
            pragma Assert (JS.R.Is_Key_Name);

            declare
               Key : constant UTF_8_String := To_UTF_8_String (JS.R.Key_Name);

            begin
               JS.R.Read_Next;

               if Key = "context" then
                  Read_String (JS, C.Context);

               elsif Key = "where" then
                  Location'Read (JS, C.Where);

               elsif Key = "newParametersDefaultValue" then
                  Read_String (JS, C.New_Parameters_Default_Value);

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

   overriding
   procedure Execute
     (Self    : Command;
      Handler : not null access LSP.Server_Notification_Receivers.
        Server_Notification_Receiver'Class;
      Client  : not null access LSP.Client_Message_Receivers.
        Client_Message_Receiver'Class;
      Error   : in out LSP.Errors.Optional_ResponseError)
   is
      use Laltools.Refactor;
      use Laltools.Refactor.Subprogram_Signature.
            Change_Parameters_Default_Value;
      use LSP.Messages;
      use LSP.Types;
      use VSS.Strings.Conversions;

      Message_Handler : LSP.Ada_Handlers.Message_Handler renames
        LSP.Ada_Handlers.Message_Handler (Handler.all);
      Context         : LSP.Ada_Contexts.Context renames
        Message_Handler.Contexts.Get (Self.Context).all;

      Apply           : Client_Requests.Workspace_Apply_Edit_Request;
      Workspace_Edits : WorkspaceEdit renames Apply.params.edit;
      Label           : Optional_Virtual_String renames Apply.params.label;

      Unit : constant Analysis_Unit :=
        Context.LAL_Context.Get_From_File
          (Context.URI_To_File (Self.Where.uri));

      Parameters_SLOC_Range : constant Source_Location_Range :=
        (Langkit_Support.Slocs.Line_Number (Self.Where.span.first.line) + 1,
         Langkit_Support.Slocs.Line_Number (Self.Where.span.last.line) + 1,
         Column_Number (Self.Where.span.first.character) + 1,
         Column_Number (Self.Where.span.last.character) + 1);

      Changer : constant Parameters_Default_Value_Changer :=
        Create_Parameters_Default_Value_Changer
          (Unit                             => Unit,
           Parameters_Source_Location_Range =>
             Parameters_SLOC_Range,
           New_Parameters_Default_Value     =>
             To_Unbounded_UTF_8_String (Self.New_Parameters_Default_Value));

      function Analysis_Units return Analysis_Unit_Array is
        (Context.Analysis_Units);
      --  Provides the Context Analysis_Unit_Array to the Pull_Upper

      Edits : constant Laltools.Refactor.Refactoring_Edits :=
        Changer.Refactor (Analysis_Units'Access);

   begin
      if Edits = No_Refactoring_Edits then
         Error :=
           (Is_Set => True,
            Value  =>
              (code    => LSP.Errors.UnknownErrorCode,
               message => VSS.Strings.Conversions.To_Virtual_String
                 ("Failed to execute the Change Parameter Default Value "
                  & "refactoring."),
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
                 ("Failed to execute the Change Parameter Default Value "
                  & "refactoring."),
               data => <>));
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self                         : in out Command'Class;
      Context                      : LSP.Ada_Contexts.Context;
      Where                        : LSP.Messages.Location;
      New_Parameters_Default_Value : VSS.Strings.Virtual_String) is
   begin
      Self.Context := Context.Id;
      Self.Where := Where;
      Self.New_Parameters_Default_Value := New_Parameters_Default_Value;
   end Initialize;

   -------------------
   -- Write_Command --
   -------------------

   procedure Write_Command
     (S : access Ada.Streams.Root_Stream_Type'Class;
      C : Command)
   is
      use LSP.Messages;
      use LSP.Types;

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

   begin
      JS.Start_Object;
      JS.Key ("context");
      Write_String (S, C.Context);
      JS.Key ("where");
      Location'Write (S, C.Where);
      JS.Key ("newParametersDefaultValue");
      Write_String (S, C.New_Parameters_Default_Value);
      JS.End_Object;
   end Write_Command;

end LSP.Ada_Handlers.Refactor_Change_Parameters_Default_Value;

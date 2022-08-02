------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2021-2022, AdaCore                     --
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

with Langkit_Support.Slocs;

with Libadalang.Analysis; use Libadalang.Analysis;

with Laltools.Refactor.Subprogram_Signature;
use Laltools.Refactor.Subprogram_Signature;

with LSP.Common;
with LSP.Messages.Client_Requests;
with LSP.Lal_Utils;

with VSS.Strings.Conversions;

package body LSP.Ada_Handlers.Refactor_Add_Parameter is

   ------------------------
   -- Append_Code_Action --
   ------------------------

   procedure Append_Code_Action
     (Self                        : in out Command;
      Context                     : Context_Access;
      Commands_Vector             : in out LSP.Messages.CodeAction_Vector;
      Where                       : LSP.Messages.Location;
      Requires_Full_Specification : Boolean)
   is
      Pointer     : LSP.Commands.Command_Pointer;
      Code_Action : LSP.Messages.CodeAction;

   begin
      Self.Initialize
        (Context                     => Context.all,
         Where                       => Where,
         Requires_Full_Specification => Requires_Full_Specification);

      Pointer.Set (Self);

      Code_Action :=
        (title       => "Add Parameter",
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

               elsif Key = "where" then
                  LSP.Messages.Location'Read (JS, V.Where);

               elsif Key = "newParameter" then
                  LSP.Types.Read_String (JS, V.New_Parameter);

               elsif Key = "requiresFullSpecification" then
                  LSP.Types.Read_Boolean
                    (JS.all, V.Requires_Full_Specification);

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
      use Langkit_Support.Slocs;
      use Laltools.Refactor;
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

      Adder : constant Parameter_Adder :=
        Create
          (Unit          => Context.Get_AU
             (Context.URI_To_File (Self.Where.uri)),
           Location      =>
             (Langkit_Support.Slocs.Line_Number
                (Self.Where.span.first.line) + 1,
              Langkit_Support.Slocs.Column_Number
                (Self.Where.span.first.character) + 1),
           New_Parameter =>
             VSS.Strings.Conversions.To_Unbounded_UTF_8_String
               (Self.New_Parameter));
      Edits : constant Laltools.Refactor.Refactoring_Edits :=
        Adder.Refactor (Analysis_Units'Access);

   begin
      if Edits = No_Refactoring_Edits then
         Error :=
           (Is_Set => True,
            Value  =>
              (code    => LSP.Errors.UnknownErrorCode,
               message => VSS.Strings.Conversions.To_Virtual_String
                 ("Failed to execute the Add Parameter refactoring."),
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
                 ("Failed to execute the Add Parameter refactoring."),
               data => <>));
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self                        : in out Command'Class;
      Context                     : LSP.Ada_Contexts.Context;
      Where                       : LSP.Messages.Location;
      Requires_Full_Specification : Boolean) is
   begin
      Self.Context_Id := Context.Id;
      Self.Where := Where;
      Self.New_Parameter := VSS.Strings.Empty_Virtual_String;
      Self.Requires_Full_Specification := Requires_Full_Specification;
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

      function "+"
        (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
         return VSS.Strings.Virtual_String
         renames VSS.Strings.Conversions.To_Virtual_String;

   begin
      JS.Start_Object;
      JS.Key ("context_id");
      LSP.Types.Write_String (S, C.Context_Id);
      JS.Key ("where");
      LSP.Messages.Location'Write (S, C.Where);
      JS.Key ("newParameter");
      LSP.Types.Write_String (S, C.New_Parameter);
      LSP.Types.Write_Boolean
        (JS, +"requiresFullSpecification", C.Requires_Full_Specification);
      JS.End_Object;
   end Write_Command;

end LSP.Ada_Handlers.Refactor_Add_Parameter;

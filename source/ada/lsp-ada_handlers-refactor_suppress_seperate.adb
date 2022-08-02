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

with Libadalang.Analysis; use Libadalang.Analysis;

with Laltools.Common; use Laltools.Common;
with Laltools.Refactor.Suppress_Separate;
use Laltools.Refactor.Suppress_Separate;

with LSP.Common;
with LSP.Messages;
with LSP.Messages.Client_Requests;
with LSP.Lal_Utils;

with VSS.Strings.Conversions;

package body LSP.Ada_Handlers.Refactor_Suppress_Seperate is

   ------------------------
   -- Append_Code_Action --
   ------------------------

   procedure Append_Code_Action
     (Self            : in out Command;
      Context         : Context_Access;
      Commands_Vector : in out LSP.Messages.CodeAction_Vector;
      Target_Separate : Basic_Decl)
   is
      Pointer      : LSP.Commands.Command_Pointer;
      Code_Action  : LSP.Messages.CodeAction;
      Subp_Name    : constant Libadalang.Analysis.Name :=
        Target_Separate.P_Defining_Name.F_Name;
      Where        : constant LSP.Messages.Location :=
        LSP.Lal_Utils.Get_Node_Location (Subp_Name);
      Action_Title : constant VSS.Strings.Virtual_String :=
        VSS.Strings.To_Virtual_String
          ("Suppress separate subprogram " & Subp_Name.Text);

   begin
      Self.Initialize
        (Context => Context.all,
         Where   => ((uri => Where.uri), Where.span.first));

      Pointer.Set (Self);

      Code_Action :=
        (title       => Action_Title,
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
      return Command
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

               if Key = "context" then
                  LSP.Types.Read_String (JS, V.Context);

               elsif Key = "where" then
                  LSP.Messages.TextDocumentPositionParams'Read (JS, V.Where);

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
      use Laltools.Refactor;
      use LSP.Messages;
      use LSP.Types;
      use VSS.Strings.Conversions;

      Message_Handler : LSP.Ada_Handlers.Message_Handler renames
        LSP.Ada_Handlers.Message_Handler (Handler.all);
      Context         : LSP.Ada_Contexts.Context renames
        Message_Handler.Contexts.Get (Self.Context).all;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Message_Handler.Get_Open_Document (Self.Where.textDocument.uri);

      Apply           : Client_Requests.Workspace_Apply_Edit_Request;
      Workspace_Edits : WorkspaceEdit renames Apply.params.edit;
      Label           : Optional_Virtual_String renames Apply.params.label;

      Node : constant Ada_Node :=
        Document.Get_Node_At (Context, Self.Where.position);

      Target_Separate : constant Basic_Decl :=
        Get_Node_As_Name (Node).Parent.Parent.Parent.As_Basic_Decl;

      Suppressor : Separate_Suppressor;
      Edits      : Refactoring_Edits;

      function Analysis_Units return Analysis_Unit_Array is
        (Context.Analysis_Units);
      --  Provides the Context Analysis_Unit_Array to the Mode_Changer

   begin
      if Target_Separate.Is_Null then
         Error :=
           (Is_Set => True,
            Value  =>
              (code    => LSP.Errors.InvalidRequest,
               message => VSS.Strings.To_Virtual_String
                 ("Failed to execute the Suppress Separate refactoring. "
                  & "The target subprogram could not be resolved precisely."),
               data    => <>));
         return;
      end if;

      Suppressor := Create (Target_Separate);

      Edits := Suppressor.Refactor (Analysis_Units'Access);

      if Edits = No_Refactoring_Edits then
         Error :=
           (Is_Set => True,
            Value  =>
              (code    => LSP.Errors.UnknownErrorCode,
               message => VSS.Strings.Conversions.To_Virtual_String
                 ("Failed to execute the Suppress Separate refactoring."),
               data    => <>));

      else
         Workspace_Edits :=
           LSP.Lal_Utils.To_Workspace_Edit
             (Edits               => Edits,
              Resource_Operations => Message_Handler.Resource_Operations,
              Versioned_Documents => Message_Handler.Versioned_Documents,
              Document_Provider   => Message_Handler'Access,
              Rename              => True);
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
              (code    => LSP.Errors.UnknownErrorCode,
               message => VSS.Strings.Conversions.To_Virtual_String
                 ("Failed to execute the Suppress Separate refactoring."),
               data    => <>));
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self             : in out Command'Class;
      Context          : LSP.Ada_Contexts.Context;
      Where            : LSP.Messages.TextDocumentPositionParams) is
   begin
      Self.Context := Context.Id;
      Self.Where := Where;
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
      JS.Key ("context");
      LSP.Types.Write_String (S, C.Context);
      JS.Key ("where");
      LSP.Messages.TextDocumentPositionParams'Write (S, C.Where);
      JS.End_Object;
   end Write_Command;

end LSP.Ada_Handlers.Refactor_Suppress_Seperate;

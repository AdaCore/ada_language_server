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

with Ada.Exceptions;

with Ada.Strings.UTF_Encoding;

with Langkit_Support.Text;

with Libadalang.Analysis; use Libadalang.Analysis;

with Laltools.Common; use Laltools.Common;

with LSP.Messages;
with LSP.Messages.Client_Requests;
with LSP.Lal_Utils;

with VSS.Strings.Conversions;

package body LSP.Ada_Handlers.Refactor_Move_Parameter is

   ------------------------
   -- Append_Code_Action --
   ------------------------

   procedure Append_Code_Action
     (Self              : in out Command;
      Context           : Context_Access;
      Commands_Vector   : in out LSP.Messages.CodeAction_Vector;
      Target_Subp       : Libadalang.Analysis.Basic_Decl;
      Parameter_Index   : Positive;
      Move_Direction    : Move_Direction_Type)
   is
      Pointer : LSP.Commands.Command_Pointer;
      Code_Action : LSP.Messages.CodeAction;
      Where       : constant LSP.Messages.Location :=
        LSP.Lal_Utils.Get_Node_Location (Target_Subp.P_Defining_Name.F_Name);

      function Image
        (D : Move_Direction_Type)
         return Langkit_Support.Text.Text_Type;
      --  Returns 'forward' if D = Forward and 'backward' if D = Backward

      function Create_Code_Action_Title return VSS.Strings.Virtual_String;
      --  Creates the code action text that will be shown by the client to
      --  to the developer. The text is costumized based on the name and number
      --  of parameters that will be removed.

      ------------------------------
      -- Create_Code_Action_Title --
      ------------------------------

      function Create_Code_Action_Title return VSS.Strings.Virtual_String is
         Parameter_Name : constant Langkit_Support.Text.Text_Type :=
           Get_Parameter_Name (Target_Subp, Parameter_Index);

      begin
         return VSS.Strings.To_Virtual_String
           ("Move "
            & Parameter_Name
            & " "
            & Image (Move_Direction));
      end Create_Code_Action_Title;

      -----------
      -- Image --
      -----------

      function Image
        (D : Move_Direction_Type)
         return Langkit_Support.Text.Text_Type is
      begin
         case D is
            when Backward => return "backward";
            when Forward  => return "forward";
         end case;
      end Image;

   begin

      Self.Initialize
        (Context         => Context.all,
         Where           =>
           ((uri => Where.uri),
            Where.span.first),
         Parameter_Index => LSP.Types.LSP_Number (Parameter_Index),
         Direction       => LSP.Types.To_LSP_String (Image (Move_Direction)));

      Pointer.Set (Self);

      Code_Action :=
        (title       => Create_Code_Action_Title,
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self             : in out Command'Class;
      Context          : LSP.Ada_Contexts.Context;
      Where            : LSP.Messages.TextDocumentPositionParams;
      Parameter_Index  : LSP.Types.LSP_Number;
      Direction        : LSP.Types.LSP_String) is
   begin
      Self.Context := Context.Id;
      Self.Where := Where;
      Self.Parameter_Index := Parameter_Index;
      Self.Direction := Direction;
   end Initialize;

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

               if Key = "context" then
                  LSP.Types.Read_String (JS, V.Context);

               elsif Key = "where" then
                  LSP.Messages.TextDocumentPositionParams'Read (JS, V.Where);

               elsif Key = "parameter_index" then
                  LSP.Types.Read (JS, V.Parameter_Index);

               elsif Key = "direction" then
                  LSP.Types.Read (JS, V.Direction);

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
      Message_Handler : LSP.Ada_Handlers.Message_Handler renames
        LSP.Ada_Handlers.Message_Handler (Handler.all);

      Context         : LSP.Ada_Contexts.Context renames
        Message_Handler.Contexts.Get (Self.Context).all;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Message_Handler.Get_Open_Document (Self.Where.textDocument.uri);
      Apply    : LSP.Messages.Client_Requests.Workspace_Apply_Edit_Request;

      Workspace_Edits : LSP.Messages.WorkspaceEdit renames Apply.params.edit;

      Node : constant Ada_Node :=
        Document.Get_Node_At (Context, Self.Where.position);

      Target_Subp            : Defining_Name := No_Defining_Name;
      Target_Parameter_Index : constant Positive :=
        Positive (Self.Parameter_Index);

      Edits : Laltools.Refactor.Refactoring_Edits;

      function Analysis_Units return Analysis_Unit_Array is
        (Context.Analysis_Units);
      --  Provides the Context Analysis_Unit_Array to the Mode_Changer

      use type LSP.Types.LSP_String;

   begin
      Apply.params.label :=
        (Is_Set => True,
         Value  =>
           VSS.Strings.Conversions.To_Virtual_String (Command'External_Tag));
      Target_Subp := Resolve_Name_Precisely (Get_Node_As_Name (Node));

      if Target_Subp.Is_Null then
         Error :=
           (Is_Set => True,
            Value  =>
              (code    => LSP.Errors.InvalidRequest,
               message => VSS.Strings.To_Virtual_String
                 ("Could not execute Move Parameter command. "
                  & "The target subprogram could not be resolved precisely."),
               data    => <>));
         return;
      end if;

      if Self.Direction = "backward" then
         declare
            Mover : constant Backward_Mover :=
              Create (Target_Subp.P_Basic_Decl, Target_Parameter_Index);
         begin
            Edits := Mover.Refactor (Analysis_Units'Access);
         end;
      else
         declare
            Mover : constant Forward_Mover :=
              Create (Target_Subp.P_Basic_Decl, Target_Parameter_Index);
         begin
            Edits := Mover.Refactor (Analysis_Units'Access);
         end;
      end if;

      Workspace_Edits := LSP.Lal_Utils.To_Workspace_Edit
        (Edits,
         Message_Handler.Resource_Operations,
         Message_Handler.Versioned_Documents,
         Message_Handler'Access);

      Client.On_Workspace_Apply_Edit_Request (Apply);

   exception
      when E : others =>
         Error :=
           (Is_Set => True,
            Value  =>
              (code    => LSP.Errors.UnknownErrorCode,
               message => VSS.Strings.Conversions.To_Virtual_String
                 (Ada.Exceptions.Exception_Information (E)),
               data    => <>));
   end Execute;

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
      JS.Key ("parameter_index");
      LSP.Types.Write (S, C.Parameter_Index);
      JS.Key ("direction");
      LSP.Types.Write (S, C.Direction);
      JS.End_Object;
   end Write_Command;

end LSP.Ada_Handlers.Refactor_Move_Parameter;

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
with Laltools.Refactor.Subprogram_Signature.Remove_Parameter;
use Laltools.Refactor.Subprogram_Signature.Remove_Parameter;

with LSP.Common;
with LSP.Messages;
with LSP.Messages.Client_Requests;
with LSP.Lal_Utils;

with VSS.Strings.Conversions;

package body LSP.Ada_Handlers.Refactor_Remove_Parameter is

   ------------------------
   -- Append_Code_Action --
   ------------------------

   procedure Append_Code_Action
     (Self               : in out Command;
      Context            : Context_Access;
      Commands_Vector    : in out LSP.Messages.CodeAction_Vector;
      Target_Subp        : Basic_Decl;
      Parameters_Indices : Parameter_Indices_Range_Type)
   is
      Pointer : LSP.Commands.Command_Pointer;
      Code_Action : LSP.Messages.CodeAction;
      Where       : constant LSP.Messages.Location :=
        LSP.Lal_Utils.Get_Node_Location (Target_Subp.P_Defining_Name.F_Name);

      function Create_Code_Action_Title return VSS.Strings.Virtual_String;
      --  Creates the code action text that will be shown by the client to
      --  to the developer. The text is costumized based on the name and number
      --  of parameters that will be removed.
      --  There are three handlers based on the number of parameters: 1, 2, or
      --  more than 2.

      ------------------------------
      -- Create_Code_Action_Title --
      ------------------------------

      function Create_Code_Action_Title return VSS.Strings.Virtual_String is
         use type VSS.Strings.Virtual_String;

         First_Parameter_Name : constant VSS.Strings.Virtual_String :=
           VSS.Strings.To_Virtual_String
             (Get_Parameter_Name (Target_Subp, Parameters_Indices.First));
         Last_Parameter_Name  : constant VSS.Strings.Virtual_String :=
           VSS.Strings.To_Virtual_String
             (Get_Parameter_Name (Target_Subp, Parameters_Indices.Last));

         Action_Title : VSS.Strings.Virtual_String;

      begin
         if Parameters_Indices.First = Parameters_Indices.Last then
            --  One parameter
            Action_Title := "Remove parameter " & First_Parameter_Name;

         elsif Parameters_Indices.Last - Parameters_Indices.First = 1 then
            --  Two parameters
            Action_Title :=
              "Remove parameters "
              & First_Parameter_Name
              & " and "
              & Last_Parameter_Name;

         else
            --  Three or more parameters
            Action_Title :=
              "Remove parameters "
              & First_Parameter_Name
              & " to "
              & Last_Parameter_Name;
         end if;

         return Action_Title;
      end Create_Code_Action_Title;

   begin
      Self.Initialize
        (Context           => Context.all,
         Where             => ((uri => Where.uri), Where.span.first),
         First_Parameter => LSP.Types.LSP_Number (Parameters_Indices.First),
         Last_Parameter  => LSP.Types.LSP_Number (Parameters_Indices.Last));

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

               elsif Key = "first_parameter" then
                  LSP.Types.Read (JS, V.First_Parameter);

               elsif Key = "last_parameter" then
                  LSP.Types.Read (JS, V.Last_Parameter);

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

      Target_Subp               : constant Defining_Name :=
        Resolve_Name_Precisely (Get_Node_As_Name (Node));
      Target_Parameters_Indices : constant Parameter_Indices_Range_Type :=
        (First => Positive (Self.First_Parameter),
         Last  => Positive (Self.Last_Parameter));

      Remover : Parameter_Remover;
      Edits   : Refactoring_Edits;

      function Analysis_Units return Analysis_Unit_Array is
        (Context.Analysis_Units);
      --  Provides the Context Analysis_Unit_Array to the Mode_Changer

   begin
      if Target_Subp.Is_Null then
         Error :=
           (Is_Set => True,
            Value  =>
              (code    => LSP.Errors.InvalidRequest,
               message => VSS.Strings.To_Virtual_String
                 ("Failed to execute the Remove Parameter refactoring. "
                  & "The target subprogram could not be resolved precisely."),
               data    => <>));
         return;
      end if;

      Remover := Create (Target_Subp.P_Basic_Decl, Target_Parameters_Indices);

      Edits := Remover.Refactor (Analysis_Units'Access);

      if Edits = Laltools.Refactor.No_Refactoring_Edits then
         Error :=
           (Is_Set => True,
            Value  =>
              (code    => LSP.Errors.UnknownErrorCode,
               message => VSS.Strings.Conversions.To_Virtual_String
                 ("Failed to execute the Remove Parameter refactoring."),
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
              (code    => LSP.Errors.UnknownErrorCode,
               message => VSS.Strings.Conversions.To_Virtual_String
                 ("Failed to execute the Remove Parameter refactoring."),
               data    => <>));
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self             : in out Command'Class;
      Context          : LSP.Ada_Contexts.Context;
      Where            : LSP.Messages.TextDocumentPositionParams;
      First_Parameter  : LSP.Types.LSP_Number;
      Last_Parameter   : LSP.Types.LSP_Number) is
   begin
      Self.Context := Context.Id;
      Self.Where := Where;
      Self.First_Parameter := First_Parameter;
      Self.Last_Parameter := Last_Parameter;
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
      JS.Key ("first_parameter");
      LSP.Types.Write (S, C.First_Parameter);
      JS.Key ("last_parameter");
      LSP.Types.Write (S, C.Last_Parameter);
      JS.End_Object;
   end Write_Command;

end LSP.Ada_Handlers.Refactor_Remove_Parameter;

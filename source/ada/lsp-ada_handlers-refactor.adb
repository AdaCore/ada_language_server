------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

with VSS.Strings.Conversions;
with LSP.Ada_Documents; use LSP.Ada_Documents;
with LSP.Common;
with LSP.Messages.Client_Requests; use LSP.Messages.Client_Requests;
with LSP.Types; use LSP.Types;
with LSP.Messages; use LSP.Messages;
with LSP.Lal_Utils;

package body LSP.Ada_Handlers.Refactor is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self    : Command;
      Handler : not null access
        LSP.Server_Notification_Receivers.Server_Notification_Receiver'Class;
      Client  : not null access
        LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Error   : in out LSP.Errors.Optional_ResponseError)
   is
      use LAL_Refactor;

      function To_LSP_Diagnostic
        (Problem   : LAL_Refactor.Refactoring_Diagnostic'Class;
         Error_Msg : String)
         return LSP.Messages.Diagnostic;

      -----------------------
      -- To_LSP_Diagnostic --
      -----------------------

      function To_LSP_Diagnostic
        (Problem   : LAL_Refactor.Refactoring_Diagnostic'Class;
         Error_Msg : String)
         return LSP.Messages.Diagnostic
      is
         Diagnostic : LSP.Messages.Diagnostic;
      begin
         Diagnostic := LSP.Messages.Diagnostic'
           (span               => LSP.Lal_Utils.To_Span (Problem.Location),
            severity           => (True, LSP.Messages.Error),
            code               => <>,
            codeDescription    => <>,
            source             =>
              (True, VSS.Strings.Conversions.To_Virtual_String ("Ada")),
            message            => VSS.Strings.Conversions.To_Virtual_String
              (Error_Msg),
            tags               => <>,
            relatedInformation => <>);

         Diagnostic.relatedInformation.Append
              (LSP.Messages.DiagnosticRelatedInformation'(
               location => LSP.Messages.Location'
                 (uri     => File_To_URI (Problem.Filename),
                  span    => Lal_Utils.To_Span (Problem.Location),
                  alsKind => <>),
               message  => VSS.Strings.Conversions.To_Virtual_String
                 (Problem.Info)));

         return Diagnostic;
      end To_LSP_Diagnostic;

      Name            : constant String :=
        LSP.Ada_Handlers.Refactor.Command'Class (Self).Name;
      Error_Msg       : constant String := "Failed to execute the "
        & Name
        & " refactoring.";
      Message_Handler : LSP.Ada_Handlers.Message_Handler renames
        LSP.Ada_Handlers.Message_Handler (Handler.all);
      Apply           : Client_Requests.Workspace_Apply_Edit_Request;
      Workspace_Edits : WorkspaceEdit renames Apply.params.edit;
      Label           : Optional_Virtual_String renames Apply.params.label;
      Edits           : LAL_Refactor.Refactoring_Edits;
   begin
      LSP.Ada_Handlers.Refactor.Command'Class (Self).Refactor
        (Handler => Handler,
         Client  => Client,
         Edits   => Edits);

      --  The refactoring failed to compute edits: send an error response
      --  and publish the diagnostics, if any
      if LAL_Refactor.Has_Failed (Edits) then
         Error :=
           (Is_Set => True,
            Value  =>
              (code    => LSP.Errors.UnknownErrorCode,
               message => VSS.Strings.Conversions.To_Virtual_String
                 (Error_Msg),
               data    => <>));

         --  Publish the diagnostics when we have some
         if not Edits.Diagnostics.Is_Empty then
            declare
               Diagnostic  : LSP.Messages.Diagnostic;
               Diagnostics : LSP.Messages.Diagnostic_Vector;
               URI         : LSP.Types.LSP_URI := To_LSP_URI ("");
               Document    : Document_Access;
               Idx         : Integer := 1;
            begin
               for Problem of Edits.Diagnostics loop
                  Document := Get_Open_Document
                    (Self  => Message_Handler'Access,
                     URI   => File_To_URI (Problem.Filename),
                     Force => False);

                  --  Publish any processed diagnostic when switching to a
                  --  different file.
                  if not Diagnostics.Is_Empty
                    and then To_UTF_8_String (URI) /= Problem.Filename
                  then
                     Publish_Diagnostics
                       (Self              => Message_Handler'Access,
                        Document          => Document,
                        Other_Diagnostics => Diagnostics,
                        Force             => True);
                  end if;

                  URI := File_To_URI (Problem.Filename);
                  Diagnostic := To_LSP_Diagnostic (Problem, Error_Msg);
                  Diagnostics.Append (Diagnostic);

                  --  We have processed the last refactoring diagnostic:
                  --  publish all the LSP diagnostics we have.
                  if Idx = Integer (Edits.Diagnostics.Length) then
                     Publish_Diagnostics
                       (Self              => Message_Handler'Access,
                        Document          => Document,
                        Other_Diagnostics => Diagnostics,
                        Force             => True);
                  end if;

                  Idx := Idx + 1;
               end loop;
            end;
         end if;
      else
         --  Apply the computed refactoring edits
         Workspace_Edits :=
           LSP.Lal_Utils.To_Workspace_Edit
             (Edits               => Edits,
              Resource_Operations => Message_Handler.Resource_Operations,
              Versioned_Documents => Message_Handler.Versioned_Documents,
              Document_Provider   => Message_Handler'Access,
              Rename              => True);
         Label :=
           (Is_Set => True,
            Value  => VSS.Strings.Conversions.To_Virtual_String (Name));

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
                 (Error_Msg),
               data    => <>));
   end Execute;

end LSP.Ada_Handlers.Refactor;

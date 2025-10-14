------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2025, AdaCore                          --
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

with Gnatformat.Configuration;
with Laltools.Partial_GNATPP;
with Libadalang.Common;
with LSP.Ada_Context_Sets;
with LSP.Ada_Configurations;
with LSP.Ada_Documents;
with LSP.Ada_Request_Jobs;
with LSP.Client_Message_Receivers;
with LSP.Enumerations;
with LSP.Errors;
with LSP.Ada_Handlers;
with LSP.Ada_Handlers.Formatting;
with LSP.Formatters.Fallback_Indenter;
with LSP.Server_Requests.OnTypeFormatting;
with LSP.Server_Requests.RangeFormatting;
with LSP.Structures;
with VSS.Strings;
with VSS.String_Vectors;

package body LSP.Ada_Formatter is

   type Ada_Range_Formatter_Job
     (Parent : not null access constant Ada_Range_Formatter_Handler)
   is limited
     new LSP.Ada_Request_Jobs.Ada_Request_Job
          (Priority => LSP.Server_Jobs.Fence)
   with null record;

   type Ada_Range_Formatter_Job_Access is access all Ada_Range_Formatter_Job;

   overriding
   procedure Execute_Ada_Request
     (Self   : in out Ada_Range_Formatter_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   type Ada_On_Type_Formatter_Job
     (Parent : not null access constant Ada_On_Type_Formatter_Handler)
   is limited
     new LSP.Ada_Request_Jobs.Ada_Request_Job
          (Priority => LSP.Server_Jobs.Fence)
   with null record;

   type Ada_On_Type_Formatter_Job_Access is
     access all Ada_On_Type_Formatter_Job;

   overriding
   procedure Execute_Ada_Request
     (Self   : in out Ada_On_Type_Formatter_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   ----------------
   -- Create_Job --
   ----------------

   overriding function Create_Job
     (Self    : Ada_Range_Formatter_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
      return LSP.Server_Jobs.Server_Job_Access
   is
      Result : constant Ada_Range_Formatter_Job_Access :=
        new Ada_Range_Formatter_Job'
          (Parent  => Self'Unchecked_Access,
           Request => LSP.Ada_Request_Jobs.Request_Access (Message));
   begin
      return LSP.Server_Jobs.Server_Job_Access (Result);
   end Create_Job;

   ----------------
   -- Create_Job --
   ----------------

   overriding function Create_Job
     (Self    : Ada_On_Type_Formatter_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
      return LSP.Server_Jobs.Server_Job_Access
   is
      Result : constant Ada_On_Type_Formatter_Job_Access :=
        new Ada_On_Type_Formatter_Job'
          (Parent  => Self'Unchecked_Access,
           Request => LSP.Ada_Request_Jobs.Request_Access (Message));
   begin
      return LSP.Server_Jobs.Server_Job_Access (Result);
   end Create_Job;

   -------------------------
   -- Execute_Ada_Request --
   -------------------------

   overriding procedure Execute_Ada_Request
     (Self   : in out Ada_Range_Formatter_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is
      Message : LSP.Server_Requests.RangeFormatting.Request renames
        LSP.Server_Requests.RangeFormatting.Request (Self.Message.all);

      Value : LSP.Structures.DocumentRangeFormattingParams renames
        Message.Params;

      Context  : constant LSP.Ada_Context_Sets.Context_Access :=
        Self.Parent.Context.Get_Best_Context (Value.textDocument.uri);
      Document : constant LSP.Ada_Documents.Document_Access :=
        Self.Parent.Context.Get_Open_Document (Value.textDocument.uri);

      Response : LSP.Structures.TextEdit_Vector_Or_Null;
      Error    : LSP.Errors.ResponseError;
      Success  : Boolean := True;
      Messages : VSS.String_Vectors.Virtual_String_Vector;

   begin
      Status := LSP.Server_Jobs.Done;

      if Document.Has_Diagnostics (Context.all)
        and then
          Self.Parent.Context.Get_Configuration.Range_Formatting_Fallback
      then
         LSP.Ada_Handlers.Formatting.Indent_Lines
           (Context  => Context.all,
            Document => Document,
            Span     => Value.a_range,
            Options  =>
              LSP.Ada_Handlers.Formatting.Get_Formatting_Options
                (Context.all, Value.options),
            Success  => Success,
            Response => Response,
            Messages => Messages,
            Error    => Error);
      else
         LSP.Ada_Handlers.Formatting.Format
           (Context  => Context.all,
            Document => Document,
            Span     => Value.a_range,
            Options  =>
              LSP.Ada_Handlers.Formatting.Get_Formatting_Options
                (Context.all, Value.options),
            Success  => Success,
            Response => Response,
            Messages => Messages,
            Error    => Error);
      end if;

      if Success then
         Client.On_RangeFormatting_Response (Message.Id, Response);

         for Message of Messages loop
            Client.On_ShowMessage_Notification
              ((LSP.Enumerations.Info, Message));
         end loop;

      else
         Client.On_Error_Response (Message.Id, Error);
      end if;

   end Execute_Ada_Request;

   -------------------------
   -- Execute_Ada_Request --
   -------------------------

   overriding procedure Execute_Ada_Request
     (Self   : in out Ada_On_Type_Formatter_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is
      use type VSS.Strings.Character_Count;
      use type VSS.Strings.Virtual_String;

      procedure Compute_Response;
      --  Determines if how this document needs to be handled based on its
      --  diagnostics and ALS settings. Dispatches to
      --  Handle_Document_With_Diagnostics and
      --  Handle_Document_Without_Diagnostics accordingly.

      procedure Handle_Document_With_Diagnostics;
      --  Simply adds indentation to the new line

      procedure Handle_Document_Without_Diagnostics;
      --  Adds indentation to the new line and formats the previous node
      --  if configured to do so and taking into account where the cursor
      --  is.

      Message : LSP.Server_Requests.OnTypeFormatting.Request renames
        LSP.Server_Requests.OnTypeFormatting.Request (Self.Message.all);

      Value : LSP.Structures.DocumentOnTypeFormattingParams renames
        Message.Params;

      Context  : constant LSP.Ada_Context_Sets.Context_Access :=
        Self.Parent.Context.Get_Best_Context (Value.textDocument.uri);
      Document : constant LSP.Ada_Documents.Document_Access :=
        Self.Parent.Context.Get_Open_Document (Value.textDocument.uri);
      Response : LSP.Structures.TextEdit_Vector_Or_Null;

      Full_Options : constant Gnatformat.Configuration.Format_Options_Type :=
        LSP.Ada_Handlers.Formatting.Get_Formatting_Options
          (Context.all, Value.options);

      Indent_Array :
        constant LSP.Formatters.Fallback_Indenter.Indentation_Array :=
          LSP.Ada_Handlers.Formatting.Get_Indentation
            (Context  => Context.all,
             Document => Document,
             Span     =>
               ((Value.position.line, 0), (Value.position.line + 1, 0)),
             Options  => Full_Options);
      Indentation  : constant VSS.Strings.Character_Count :=
        (declare
           Indentation_First_Guess : constant VSS.Strings.Character_Count :=
             ((if Indent_Array (Value.position.line + 1) = -1
               then 0
               else
                 VSS.Strings.Character_Count
                   (Indent_Array (Value.position.line + 1))));
         begin
           (if Indentation_First_Guess
              >= VSS.Strings.Character_Count (Value.position.character)
            then
              Indentation_First_Guess
              - VSS.Strings.Character_Count (Value.position.character)
            else 0));
      --  Do not add any indentation if the current cursor position is greater
      --  than the calculated one.

      ----------------------
      -- Compute_Response --
      ----------------------

      procedure Compute_Response is
      begin
         if not LSP.Ada_Configurations.On_Type_Formatting then
            Self.Parent.Context.Get_Trace_Handle.Trace
              ("'onTypeFormatting' is not active, yet, ALS received a request "
               & " - exiting earlier");

            return;
         end if;

         if Value.ch
           /= LSP
                .Ada_Configurations
                .On_Type_Formatting_Settings
                .firstTriggerCharacter
         then
            Self.Parent.Context.Get_Trace_Handle.Trace
              ("Trigger character ch is not a new line - exiting earlier");

            return;
         end if;

         if Document.Has_Diagnostics (Context.all) then
            --  This is the unhappy path: when this Document has diagnostics.
            --  Get the previous node (based on the previous non whitespace
            --  token) and compute the indentation.

            Self.Parent.Context.Get_Trace_Handle.Trace
              ("Document has diagnostics");
            Handle_Document_With_Diagnostics;

         else
            --  This is the happy path: when this Document does not have any
            --  diagnostics.
            --  Get the previous node (based on the previous non whitespace
            --  token), compute the indentation and format it (if configured to
            --  to so).

            Self.Parent.Context.Get_Trace_Handle.Trace
               ("Document does not have any diagnostics");
            Handle_Document_Without_Diagnostics;
         end if;
      end Compute_Response;

      --------------------------------------
      -- Handle_Document_With_Diagnostics --
      --------------------------------------

      procedure Handle_Document_With_Diagnostics is
      begin
         Response.Append
           (LSP.Structures.TextEdit'
              (a_range => (start => Value.position, an_end => Value.position),
               newText => LSP.Ada_Handlers.Formatting.Handle_Tabs
                      (Context.all,
                       Document,
                       Full_Options,
                       Indentation * ' ')));
      end Handle_Document_With_Diagnostics;

      -----------------------------------------
      -- Handle_Document_Without_Diagnostics --
      -----------------------------------------

      procedure Handle_Document_Without_Diagnostics is

         function Is_Between
           (Position : LSP.Structures.Position; Span : LSP.Structures.A_Range)
            return Boolean;
         --  Checks if Position is between Span

         ----------------
         -- Is_Between --
         ----------------

         function Is_Between
           (Position : LSP.Structures.Position; Span : LSP.Structures.A_Range)
            return Boolean
         is ((Position.line = Span.start.line
              and then Position.character >= Span.start.character)
             or else (Position.line = Span.an_end.line
                      and then Position.character <= Span.an_end.character)
             or else (Position.line > Span.start.line
                      and then Position.line < Span.an_end.line));

         Token                    :
           constant Libadalang.Common.Token_Reference :=
             Document.Get_Token_At (Context.all, Value.position);
         Previous_NWNC_Token      :
           constant Libadalang.Common.Token_Reference :=
             Laltools.Partial_GNATPP.Previous_Non_Whitespace_Non_Comment_Token
               (Token);
         Previous_NWNC_Token_Span : constant LSP.Structures.A_Range :=
           Document.To_A_Range
             (Libadalang.Common.Sloc_Range
                (Libadalang.Common.Data (Previous_NWNC_Token)));

         Formatting_Region :
           constant Laltools.Partial_GNATPP.Formatting_Region_Type :=
             Document.Get_Formatting_Region
               (Context.all, Previous_NWNC_Token_Span.start);
         Formatting_Span   : constant LSP.Structures.A_Range :=
           Document.To_A_Range
             (Libadalang.Slocs.Make_Range
                (Libadalang.Slocs.Start_Sloc
                   (Libadalang.Common.Sloc_Range
                      (Libadalang.Common.Data
                         (Formatting_Region.Start_Token))),
                 Libadalang.Slocs.Start_Sloc
                   (Libadalang.Common.Sloc_Range
                      (Libadalang.Common.Data
                         (Formatting_Region.End_Token)))));
         --  This is the span that would be formatted based on the cursor
         --  position.

      begin
         if Self.Parent.Context.Get_Configuration.Indent_Only then
            Self.Parent.Context.Get_Trace_Handle.Trace
              ("'onTypeFormatting' request configured to indent only");

            Response.Append
              (LSP.Structures.TextEdit'
                 (a_range =>
                    (start => Value.position, an_end => Value.position),
                  newText =>
                    LSP.Ada_Handlers.Formatting.Handle_Tabs
                      (Context.all,
                       Document,
                       Full_Options,
                       Indentation * ' ')));

            return;
         end if;

         --  onTypeFormatting is configured to also format the previous node,
         --  however, we can only do this if the cursor is not between the
         --  Formatting_Span.

         if Is_Between (Value.position, Formatting_Span) then
            Self.Parent.Context.Get_Trace_Handle.Trace
              ("Current position is within the Formatting_Span");
            Self.Parent.Context.Get_Trace_Handle.Trace
              ("Adding indentation only");

            Response.Append
              (LSP.Structures.TextEdit'
                 (a_range =>
                    (start => Value.position, an_end => Value.position),
                  newText =>
                    LSP.Ada_Handlers.Formatting.Handle_Tabs
                      (Context.all,
                       Document,
                       Full_Options,
                       Indentation * ' ')));

            return;
         end if;

         Self.Parent.Context.Get_Trace_Handle.Trace
           ("Formatting previous node and adding indentation");

         declare
            Success : Boolean;
            Error   : LSP.Errors.ResponseError;

         begin
            LSP.Ada_Handlers.Formatting.Range_Format
              (Context  => Context.all,
               Document => Document,
               Span     => Previous_NWNC_Token_Span,
               Options  => Full_Options,
               Success  => Success,
               Response => Response,
               Error    => Error);

            if not Success then
               Self.Parent.Context.Get_Trace_Handle.Trace
                 ("The 'onTypeFormatting' has failed because of a "
                  & "Range_Format error");
            end if;

            Response.Append
              (LSP.Structures.TextEdit'
                 (a_range =>
                    (start => Value.position, an_end => Value.position),
                  newText =>
                    LSP.Ada_Handlers.Formatting.Handle_Tabs
                      (Context.all,
                       Document,
                       Full_Options,
                       Indentation * ' ')));
         end;
      end Handle_Document_Without_Diagnostics;

   begin
      Status := LSP.Server_Jobs.Done;

      Self.Parent.Context.Get_Trace_Handle.Trace
        ("On 'onTypeFormatting' Request");
      Compute_Response;
      Self.Parent.Context.Get_Trace_Handle.Trace
        ("Exiting 'onTypeFormatting' Request");

      Client.On_OnTypeFormatting_Response (Message.Id, Response);
   end Execute_Ada_Request;

end LSP.Ada_Formatter;

------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
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

with LSP.Types; use LSP.Types;

with LSP.Ada_Documents;

with LSP.Ada_Cross_Reference_Services;
--  Temporary dependency, see note in package's spec file

with Langkit_Support.Slocs;

with Libadalang.Analysis;
with Libadalang.Common;

package body LSP.Ada_Handlers is

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
                 return LSP.Types.LSP_String renames
     LSP.Types.To_LSP_String;

   ------------------------
   -- Initialize_Request --
   ------------------------

   overriding procedure Initialize_Request
     (Self     : access Message_Handler;
      Value    : LSP.Messages.InitializeParams;
      Response : in out LSP.Messages.Initialize_Response)
   is
      Root : LSP.Types.LSP_String;
   begin
      Response.result.capabilities.textDocumentSync :=
        (Is_Set => True, Is_Number => True, Value => LSP.Messages.Full);
      Response.result.capabilities.referencesProvider :=
        LSP.Types.Optional_True;
      Response.result.capabilities.documentSymbolProvider :=
        LSP.Types.Optional_True;

      if not LSP.Types.Is_Empty (Value.rootUri) then
         Root := Delete (Value.rootUri, 1, 7);
      elsif not LSP.Types.Is_Empty (Value.rootPath) then
         Root := "file://" & Value.rootPath;
      end if;

      Self.Context.Initialize (Root);
   end Initialize_Request;

   --------------------------------------
   -- Text_Document_References_Request --
   --------------------------------------

   procedure Text_Document_References_Request
     (Self     : access Message_Handler;
      Value    : LSP.Messages.ReferenceParams;
      Response : in out LSP.Messages.Location_Response)
   is

      Document   : constant LSP.Ada_Documents.Document_Access :=
        Self.Context.Get_Document (Value.textDocument.uri);

      Definition : constant Libadalang.Analysis.Defining_Name :=
        Document.Get_Definition_At (Value.position);

      References : LSP.Ada_Cross_Reference_Services.Ref_Vector;

      use Libadalang.Analysis;

   begin

      if Definition = No_Defining_Name then
         return;
      end if;

      References := LSP.Ada_Cross_Reference_Services.Find_All_References
        (Definition         => Definition,
         Sources            => Self.Context.Get_Source_Files,
         Include_Definition => Value.context.includeDeclaration);
      --  TODO: This call to `Find_All_References` should later be replaced by
      --  a call to a subprogram in Libadalang with the same functionality

      for N in 1 .. Integer (References.Length) loop

         declare

            Node : constant Libadalang.Analysis.Ada_Node :=
              References.Element (N);

            use Libadalang.Common;

            Start_Sloc_Range :
              constant Langkit_Support.Slocs.Source_Location_Range :=
                Sloc_Range (Data (Node.Token_Start));
            End_Sloc_Range   :
              constant Langkit_Support.Slocs.Source_Location_Range :=
                Sloc_Range (Data (Node.Token_End));

            First_Position : constant LSP.Messages.Position :=
              (Line_Number (Start_Sloc_Range.Start_Line) - 1,
               UTF_16_Index (Start_Sloc_Range.Start_Column) - 1);
            Last_Position  : constant LSP.Messages.Position :=
              (Line_Number (End_Sloc_Range.End_Line) - 1,
               UTF_16_Index (End_Sloc_Range.End_Column) - 1);

            Location : constant LSP.Messages.Location :=
              (uri  => +("file://" & Node.Unit.Get_Filename),
               span => LSP.Messages.Span'(First_Position, Last_Position));

         begin
            Response.result.Append (Location);
         end;

      end loop;

   end Text_Document_References_Request;

   ----------------------------------
   -- Text_Document_Symbol_Request --
   ----------------------------------

   overriding procedure Text_Document_Symbol_Request
     (Self     : access Message_Handler;
      Value    : LSP.Messages.DocumentSymbolParams;
      Response : in out LSP.Messages.Symbol_Response)
   is
      Document : constant LSP.Ada_Documents.Document_Access :=
        Self.Context.Get_Document (Value.textDocument.uri);
   begin
      Document.Get_Symbols (Response.result);
   end Text_Document_Symbol_Request;

   ----------------------------
   -- Text_Document_Did_Open --
   ----------------------------

   overriding procedure Text_Document_Did_Open
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidOpenTextDocumentParams)
   is
   begin
      Self.Context.Load_Document (Value.textDocument);
   end Text_Document_Did_Open;

   ------------------------------
   -- Text_Document_Did_Change --
   ------------------------------

   overriding procedure Text_Document_Did_Change
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidChangeTextDocumentParams)
   is
      Document : constant LSP.Ada_Documents.Document_Access :=
        Self.Context.Get_Document (Value.textDocument.uri);
      Note     : LSP.Messages.PublishDiagnostics_Notification;
   begin
      Document.Apply_Changes (Value.contentChanges);
      Document.Get_Errors (Note.params.diagnostics);

      Note.method := +"textDocument/publishDiagnostics";
      Note.params.uri := Value.textDocument.uri;
      Self.Server.Send_Notification (Note);
   end Text_Document_Did_Change;

   -----------------------------
   -- Text_Document_Did_Close --
   -----------------------------

   overriding procedure Text_Document_Did_Close
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidCloseTextDocumentParams)
   is
   begin
      Self.Context.Unload_Document (Value.textDocument);
   end Text_Document_Did_Close;

   -----------------------
   -- Exit_Notification --
   -----------------------

   overriding procedure Exit_Notification (Self : access Message_Handler) is
   begin
      Self.Server.Stop;
   end Exit_Notification;

end LSP.Ada_Handlers;

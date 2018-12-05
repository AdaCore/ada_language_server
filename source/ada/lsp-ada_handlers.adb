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

with GNATCOLL.JSON;

with LSP.Types; use LSP.Types;

with LSP.Ada_Documents;

with Langkit_Support.Slocs;

with Libadalang.Analysis;
with Libadalang.Common;

with GNATCOLL.VFS;

package body LSP.Ada_Handlers is

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
                 return LSP.Types.LSP_String renames
     LSP.Types.To_LSP_String;

   -----------------------
   -- Exit_Notification --
   -----------------------

   overriding procedure Exit_Notification (Self : access Message_Handler) is
   begin
      Self.Server.Stop;
   end Exit_Notification;

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
      Response.result.capabilities.definitionProvider :=
        LSP.Types.Optional_True;
      Response.result.capabilities.referencesProvider :=
        LSP.Types.Optional_True;
      Response.result.capabilities.documentSymbolProvider :=
        LSP.Types.Optional_True;
      Response.result.capabilities.textDocumentSync :=
        (Is_Set => True, Is_Number => True, Value => LSP.Messages.Full);
      Response.result.capabilities.completionProvider :=
        (True,
         (resolveProvider => (True, False),
          triggerCharacters => Empty_Vector & To_LSP_String (".")));

      if not LSP.Types.Is_Empty (Value.rootUri) then
         Root := Self.Context.URI_To_File (Value.rootUri);
      else
         --  URI isn't provided, rollback to depricated rootPath
         Root := Value.rootPath;
      end if;

      Self.Context.Initialize (Root);
   end Initialize_Request;

   --------------------------------------
   -- Text_Document_Definition_Request --
   --------------------------------------

   procedure Text_Document_Definition_Request
     (Self     : access Message_Handler;
      Value    : LSP.Messages.TextDocumentPositionParams;
      Response : in out LSP.Messages.Location_Response)
   is

      Document   : constant LSP.Ada_Documents.Document_Access :=
        Self.Context.Get_Document (Value.textDocument.uri);

      Node       : constant Libadalang.Analysis.Ada_Node :=
        Document.Get_Node_At (Value.position);

      Definition : Libadalang.Analysis.Defining_Name;

      use Libadalang.Analysis;

   begin

      if Node = No_Ada_Node then
         return;
      end if;

      Definition := Node.P_Xref;

      if Definition = No_Defining_Name then
         return;
      end if;

      declare

         use Libadalang.Common;

         Start_Sloc_Range :
         constant Langkit_Support.Slocs.Source_Location_Range :=
           Sloc_Range (Data (Definition.Token_Start));
         End_Sloc_Range   :
         constant Langkit_Support.Slocs.Source_Location_Range :=
           Sloc_Range (Data (Definition.Token_End));

         First_Position : constant LSP.Messages.Position :=
           (Line_Number (Start_Sloc_Range.Start_Line) - 1,
            UTF_16_Index (Start_Sloc_Range.Start_Column) - 1);
         Last_Position  : constant LSP.Messages.Position :=
           (Line_Number (End_Sloc_Range.End_Line) - 1,
            UTF_16_Index (End_Sloc_Range.End_Column) - 1);

         Location : constant LSP.Messages.Location :=
           (uri  => Self.Context.File_To_URI (+Definition.Unit.Get_Filename),
            span => LSP.Messages.Span'(First_Position, Last_Position));

      begin
         Response.result.Append (Location);
      end;

   end Text_Document_Definition_Request;

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

   --------------------------------------
   -- Text_Document_References_Request --
   --------------------------------------

   procedure Text_Document_References_Request
     (Self     : access Message_Handler;
      Value    : LSP.Messages.ReferenceParams;
      Response : in out LSP.Messages.Location_Response)
   is
      use Libadalang.Analysis;

      function Find_All_References
        (Definition         : Defining_Name;
         Sources            : GNATCOLL.VFS.File_Array_Access;
         Include_Definition : Boolean := False)
      return Ada_Node_Array;
      --  Helper function, finds all references of a given defining name in a
      --  given list of units.

      function Find_All_References
        (Definition         : Defining_Name;
         Sources            : GNATCOLL.VFS.File_Array_Access;
         Include_Definition : Boolean := False)
      return Ada_Node_Array
      is
         Context : constant Analysis_Context := Definition.Unit.Context;
         Source_Units : Analysis_Unit_Array (Sources'Range);
      begin
         for N in Sources'Range loop
            Source_Units (N) := Context.Get_From_File
              (Sources (N).Display_Full_Name);
         end loop;

         declare
            References : constant Ada_Node_Array :=
              Definition.P_Find_All_References (Source_Units);
         begin
            if Include_Definition then
               return References & (1 => Definition.As_Ada_Node);
            else
               return References;
            end if;
         end;
      end Find_All_References;

      Document   : constant LSP.Ada_Documents.Document_Access :=
        Self.Context.Get_Document (Value.textDocument.uri);

      Definition : constant Defining_Name :=
        Document.Get_Definition_At (Value.position);

   begin
      if Definition.Is_Null then
         return;
      end if;

      declare
         References : constant Ada_Node_Array := Find_All_References
             (Definition         => Definition,
              Sources            => Self.Context.Get_Source_Files,
              Include_Definition => Value.context.includeDeclaration);
      begin
         for Node of References loop
            declare
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
                 (uri  => Self.Context.File_To_URI (+Node.Unit.Get_Filename),
                  span => LSP.Messages.Span'(First_Position, Last_Position));
            begin
               Response.result.Append (Location);
            end;
         end loop;
      end;
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

   ----------------------------------------
   -- Workspace_Did_Change_Configuration --
   ----------------------------------------

   overriding procedure Workspace_Did_Change_Configuration
     (Self     : access Message_Handler;
      Value    : LSP.Messages.DidChangeConfigurationParams)
   is
      use type GNATCOLL.JSON.JSON_Value_Type;

      projectFile       : constant String := "projectFile";
      scenarioVariables : constant String := "scenarioVariables";

      Ada       : constant LSP.Types.LSP_Any := Value.settings.Get ("ada");
      File      : LSP.Types.LSP_String;
      Variables : LSP.Types.LSP_Any;
   begin
      if Ada.Kind = GNATCOLL.JSON.JSON_Object_Type then
         if Ada.Has_Field (projectFile) then
            File := +Ada.Get (projectFile).Get;

            --  Drop uri scheme if present
            if LSP.Types.Starts_With (File, "file:") then
               File := Self.Context.URI_To_File (File);
            end if;
         end if;

         if Ada.Has_Field (scenarioVariables) and then
           Ada.Get (scenarioVariables).Kind  = GNATCOLL.JSON.JSON_Object_Type
         then
            Variables := Ada.Get (scenarioVariables);
         end if;
      end if;

      Self.Context.Load_Project (File, Variables);
   end Workspace_Did_Change_Configuration;

   --------------------------------------
   -- Text_Document_Completion_Request --
   --------------------------------------

   overriding procedure Text_Document_Completion_Request
    (Self     : access Message_Handler;
     Value    : LSP.Messages.TextDocumentPositionParams;
     Response : in out LSP.Messages.Completion_Response)
   is
      Document   : constant LSP.Ada_Documents.Document_Access :=
        Self.Context.Get_Document (Value.textDocument.uri);
   begin
      Document.Get_Completions_At (Value.position, Response.result);
   end Text_Document_Completion_Request;

end LSP.Ada_Handlers;

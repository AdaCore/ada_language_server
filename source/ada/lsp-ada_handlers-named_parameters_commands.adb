------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2020, AdaCore                       --
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

with VSS.Strings.Conversions;

with LSP.Lal_Utils;
with Libadalang.Analysis;
with Libadalang.Common;

with LSP.Messages.Client_Requests;

package body LSP.Ada_Handlers.Named_Parameters_Commands is

   function Get_Parameters
     (Args : Libadalang.Analysis.Basic_Assoc_List)
      return LSP.Types.LSP_String_Vector;
   --  Find list of parameter names from given AST node.

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
                  LSP.Types.Read (JS, V.Context);
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
      Handler : not null access LSP.Server_Notification_Receivers
        .Server_Notification_Receiver'
        Class;
      Client : not null access LSP.Client_Message_Receivers
        .Client_Message_Receiver'
        Class;
      Error : in out LSP.Errors.Optional_ResponseError)
   is
      procedure Append
        (Node : Libadalang.Analysis.Ada_Node;
         Name : LSP.Types.LSP_String);
      --  Create and append a TextEdit to insert Name & " => " before Node.

      Apply  : LSP.Messages.Client_Requests.Workspace_Apply_Edit_Request;
      Edits  : LSP.Messages.WorkspaceEdit renames Apply.params.edit;

      Client_Supports_documentChanges : constant Boolean := True;

      ------------
      -- Append --
      ------------

      procedure Append
        (Node : Libadalang.Analysis.Ada_Node;
         Name : LSP.Types.LSP_String)
      is
         use type LSP.Types.LSP_String;
         Loc    : constant LSP.Messages.Location :=
           LSP.Lal_Utils.Get_Node_Location (Node);
         Edit   : LSP.Messages.TextEdit;
      begin
         Edit.span := (Loc.span.first, Loc.span.first);
         Edit.newText := Name & " => ";

         if Client_Supports_documentChanges then
            Edits.documentChanges (1).Text_Document_Edit.edits.Append (Edit);
         else
            Edits.changes (Edits.changes.First).Append (Edit);
         end if;
      end Append;

      Message_Handler : LSP.Ada_Handlers.Message_Handler renames
        LSP.Ada_Handlers.Message_Handler (Handler.all);

      Context         : LSP.Ada_Contexts.Context renames
        Message_Handler.Contexts.Get (Self.Context).all;

      Document  : constant LSP.Ada_Documents.Document_Access :=
        Message_Handler.Get_Open_Document (Self.Where.textDocument.uri);

      Node : Libadalang.Analysis.Ada_Node :=
        Document.Get_Node_At (Context, Self.Where.position);

      Args   : Libadalang.Analysis.Basic_Assoc_List;
      Params : LSP.Types.LSP_String_Vector;
      Index  : Natural := 0;
   begin
      if Client_Supports_documentChanges then
         Edits.documentChanges.Append
           (LSP.Messages.Document_Change'
              (Kind               => LSP.Messages.Text_Document_Edit,
               Text_Document_Edit =>
                 (textDocument => Document.Versioned_Identifier,
                  edits        => <>)));
      else
         declare
            Empty : LSP.Messages.TextEdit_Vector;
         begin
            Edits.changes.Insert (Document.URI, Empty);
         end;
      end if;

      while not Node.Is_Null and then
        Node.Kind not in Libadalang.Common.Ada_Basic_Assoc_List
      loop
         Node := Node.Parent;
      end loop;

      Args := Node.As_Basic_Assoc_List;
      Params := Get_Parameters (Args);
      Index := Args.Children_Count;

      for Arg of reverse Args.Children loop
         exit when Index < Params.First_Index;

         case Arg.Kind is
            when Libadalang.Common.Ada_Param_Assoc =>
               if Arg.As_Param_Assoc.F_Designator.Is_Null then
                  Append (Arg, Params (Index));
               end if;

            when others =>
               null;
         end case;

         Index := Index - 1;
      end loop;

      Client.On_Workspace_Apply_Edit_Request (Apply);
   exception
      when E : others =>
         Error :=
           (Is_Set => True,
            Value  =>
              (code => LSP.Errors.UnknownErrorCode,
               message => LSP.Types.To_LSP_String
                 (Ada.Exceptions.Exception_Information (E)),
               data => <>));
   end Execute;

   --------------------
   -- Get_Parameters --
   --------------------

   function Get_Parameters
     (Args   : Libadalang.Analysis.Basic_Assoc_List)
      return LSP.Types.LSP_String_Vector
   is
      procedure Append (Params : Libadalang.Analysis.Param_Spec_Array);
      --  Append identifiers from Params to Result

      function Get_Params_Spec_Array
        (Decl : Libadalang.Analysis.Basic_Decl)
         return Libadalang.Analysis.Param_Spec_Array;
      --  Return the Param_Spec_Array associated with the given

      Result : LSP.Types.LSP_String_Vector;

      ------------
      -- Append --
      ------------

      procedure Append
        (Params : Libadalang.Analysis.Param_Spec_Array) is
      begin
         for Param of Params loop
            for Id of Param.F_Ids loop
               Result.Append (LSP.Types.To_LSP_String (Id.Text));
            end loop;
         end loop;
      end Append;

      ---------------------------
      -- Get_Params_Spec_Array --
      ---------------------------

      function Get_Params_Spec_Array
        (Decl : Libadalang.Analysis.Basic_Decl)
         return Libadalang.Analysis.Param_Spec_Array is
      begin
         case Decl.Kind is

         when Libadalang.Common.Ada_Base_Subp_Spec =>
            declare
               Params : constant Libadalang.Analysis.Param_Spec_Array :=
                 Decl.As_Base_Subp_Spec.P_Params;
            begin
               return Params;
            end;

         when Libadalang.Common.Ada_Base_Subp_Body =>
            declare
               Spec   : constant Libadalang.Analysis.Subp_Spec :=
                 Decl.As_Base_Subp_Body.F_Subp_Spec;
               Params : constant Libadalang.Analysis.Param_Spec_Array :=
                 Spec.P_Params;
            begin
               return Params;
            end;

         when Libadalang.Common.Ada_Basic_Subp_Decl =>
            declare
               Spec   : constant Libadalang.Analysis.Base_Subp_Spec :=
                 Decl.As_Basic_Subp_Decl.P_Subp_Decl_Spec;
               Params : constant Libadalang.Analysis.Param_Spec_Array :=
                 Spec.P_Params;
            begin
               return Params;
            end;

         when others =>
            return (1 .. 0 => <>);
         end case;
      end Get_Params_Spec_Array;

      Expr        : constant Libadalang.Analysis.Ada_Node := Args.Parent;
      Name        : Libadalang.Analysis.Name;
      Decl        : Libadalang.Analysis.Basic_Decl;
      Is_Dot_Call : Boolean;
   begin
      case Expr.Kind is
         when Libadalang.Common.Ada_Call_Expr =>
            Name := Expr.As_Call_Expr.F_Name;
         when others =>
            return LSP.Types.Empty_Vector;
      end case;

      Decl := Name.P_Referenced_Decl;
      Is_Dot_Call := Name.P_Is_Dot_Call;

      --  Don't append the first parameter if we are dealing with a dot call
      declare
         Params : constant Libadalang.Analysis.Param_Spec_Array :=
           Get_Params_Spec_Array (Decl);
      begin
         if Is_Dot_Call then
            Append (Params (Params'First + 1 .. Params'Last));
         else
            Append (Params);
         end if;
      end;

      return Result;
   end Get_Parameters;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : in out Command'Class;
      Context : LSP.Ada_Contexts.Context;
      Where   : LSP.Messages.TextDocumentPositionParams) is
   begin
      Self.Context := Context.Id;
      Self.Where := Where;
   end Initialize;

   -------------------
   -- Write_Command --
   -------------------

   procedure Write_Command
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Command)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("context");
      LSP.Types.Write (S, V.Context);
      JS.Key ("where");
      LSP.Messages.TextDocumentPositionParams'Write (S, V.Where);
      JS.End_Object;
   end Write_Command;

end LSP.Ada_Handlers.Named_Parameters_Commands;

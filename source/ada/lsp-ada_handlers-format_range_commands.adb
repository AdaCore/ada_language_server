------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2025-2026, AdaCore                     --
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

with VSS.JSON.Streams;

with LSP.Ada_Handlers.Formatting;
with LSP.Structures.LSPAny_Vectors;

package body LSP.Ada_Handlers.Format_Range_Commands is

   ------------
   -- Create --
   ------------

   overriding function Create
     (Any : not null access LSP.Structures.LSPAny_Vector) return Command
   is
      use VSS.JSON.Streams;
      use VSS.Strings;
      use LSP.Structures.JSON_Event_Vectors;

      C : Cursor := Any.First;
   begin
      return Self : Command do
         pragma Assert (Element (C).Kind = Start_Array);
         Next (C);
         pragma Assert (Element (C).Kind = Start_Object);
         Next (C);

         while Has_Element (C) and then Element (C).Kind /= End_Object loop
            pragma Assert (Element (C).Kind = Key_Name);
            declare
               Key : constant Virtual_String := Element (C).Key_Name;
            begin
               Next (C);

               if Key = "textDocument" then
                  Self.textDocument :=
                    LSP.Structures.LSPAny_Vectors.From_Any (C);
               elsif Key = "range" then
                  Self.a_range :=
                    LSP.Structures.LSPAny_Vectors.From_Any (C);
               else
                  LSP.Structures.LSPAny_Vectors.Skip_Value (C);
               end if;
            end;

            Next (C);
         end loop;
      end return;
   end Create;

-------------
-- Execute --
-------------

   overriding procedure Execute
     (Self     : Command;
      Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Response : in out LSP.Structures.LSPAny_Or_Null;
      Error    : in out LSP.Errors.ResponseError_Optional)
   is
      Context  : constant LSP.Ada_Context_Sets.Context_Access :=
        Handler.Get_Best_Context (Self.textDocument.uri);

      Document : constant LSP.Ada_Documents.Document_Access :=
        Handler.Get_Open_Document (Self.textDocument.uri);

      Apply    : LSP.Structures.ApplyWorkspaceEditParams;
      Edits    : LSP.Structures.TextEdit_Vector;
      Success  : Boolean := True;
      Default  : constant LSP.Structures.FormattingOptions :=
        (tabSize => 0, insertSpaces => True, others => <>);
   begin
      Error := (Is_Set => True, Value => <>);

      LSP.Ada_Handlers.Formatting.Range_Format
        (Context  => Context.all,
         Document => Document,
         Span     => Self.a_range,
         Options  =>
           LSP.Ada_Handlers.Formatting.Get_Formatting_Options
             (Context.all, Default),
         Success  => Success,
         Response => Edits,
         Error    => Error.Value);

      if Success then
         Error := (Is_Set => False);

         Apply.label := "Range format";

         Apply.edit.changes.Insert
           (Self.textDocument.uri,
            LSP.Structures.Empty);

         Apply.edit.changes (Self.textDocument.uri).Move (Edits);

         Handler.Sender.On_ApplyEdit_Request
           (Handler.Server.Allocate_Request_Id, Apply);
      end if;
   end Execute;

   --------------------
   -- To_LSP_Command --
   --------------------

   function To_LSP_Command
     (Document : LSP.Structures.TextDocumentIdentifier;
      Span     : LSP.Structures.A_Range) return LSP.Structures.Command
   is
      use all type VSS.JSON.Streams.JSON_Stream_Element_Kind;
      subtype JSON is VSS.JSON.Streams.JSON_Stream_Element;
      Args : LSP.Structures.LSPAny_Vector;
      Doc  : LSP.Structures.LSPAny_Vector;
      Pos  : LSP.Structures.LSPAny_Vector;
   begin
      return Result : LSP.Structures.Command :=
        (title     => "Format range",
         command   => VSS.Strings.Conversions.To_Virtual_String
           (Command'External_Tag),
         arguments => <>)
      do
         LSP.Structures.LSPAny_Vectors.To_Any (Document, Doc);
         LSP.Structures.LSPAny_Vectors.To_Any (Span, Pos);
         Args.Append (JSON'(Kind => Start_Array));
         Args.Append (JSON'(Kind => Start_Object));
         Args.Append (JSON'(Key_Name, "textDocument"));
         Args.Append_Vector (Doc);
         Args.Append (JSON'(Key_Name, "range"));
         Args.Append_Vector (Pos);
         Args.Append (JSON'(Kind => End_Object));
         Args.Append (JSON'(Kind => End_Array));
         Result.arguments.Move (Args);
      end return;
   end To_LSP_Command;
end LSP.Ada_Handlers.Format_Range_Commands;

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

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Strings.Wide_Wide_Unbounded;
with LSP.Types;

package body LSP.Ada_Documents is

   -------------------
   -- Apply_Changes --
   -------------------

   not overriding procedure Apply_Changes
     (Self   : aliased in out Document;
      Vector : LSP.Messages.TextDocumentContentChangeEvent_Vector)
   is
      File : constant LSP.Types.LSP_String :=
        LSP.Types.Delete (Self.URI, 1, 7);  --  Delete file://
   begin
      for Change of reverse Vector loop
         --  If whole document then reparse it
         if not Change.span.Is_Set then
            Self.Unit := Self.LAL.Get_From_Buffer
              (Filename => LSP.Types.To_UTF_8_String (File),
               Charset  => "utf-8",
               Buffer   => LSP.Types.To_UTF_8_String (Change.text));
         end if;
      end loop;
   end Apply_Changes;

   ----------------
   -- Get_Errors --
   ----------------

   not overriding procedure Get_Errors
     (Self   : Document;
      Errors : out LSP.Messages.Diagnostic_Vector)
   is
      Item : LSP.Messages.Diagnostic;
   begin
      Errors.Clear;

      if Self.Unit.Has_Diagnostics then
         for Error of Self.Unit.Diagnostics loop
            Item.span.first.line :=
              LSP.Types.Line_Number (Error.Sloc_Range.Start_Line);
            Item.span.last.line :=
              LSP.Types.Line_Number (Error.Sloc_Range.End_Line);

            Item.span.first.character :=  --  FIXME!
              LSP.Types.UTF_16_Index
                (Error.Sloc_Range.Start_Column);

            Item.span.last.character :=  --  FIXME!
              LSP.Types.UTF_16_Index
                (Error.Sloc_Range.End_Column);

            Item.message := LSP.Types.To_LSP_String
              (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode
                 (Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String
                      (Error.Message)));

            Errors.Append (Item);
         end loop;
      end if;
   end Get_Errors;

   -----------------
   -- Get_Symbols --
   -----------------

   not overriding procedure Get_Symbols
     (Self   : Document;
      Result : out LSP.Messages.SymbolInformation_Vector)
   is
      pragma Unreferenced (Self);
   begin
      Result.Clear;
   end Get_Symbols;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (Self : in out Document;
      LAL  : Libadalang.Analysis.Analysis_Context;
      Item : LSP.Messages.TextDocumentItem)
   is
      File : constant LSP.Types.LSP_String :=
        LSP.Types.Delete (Item.uri, 1, 7);  --  Delete file://
   begin
      Self.Unit := LAL.Get_From_Buffer
        (Filename => LSP.Types.To_UTF_8_String (File),
         Charset  => "utf-8",
         Buffer   => LSP.Types.To_UTF_8_String (Item.text));
      Self.URI := Item.uri;
      Self.LAL := LAL;
   end Initialize;

end LSP.Ada_Documents;

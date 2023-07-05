------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

with Langkit_Support.Slocs;

with Libadalang.Analysis; use Libadalang.Analysis;

with LAL_Refactor.Replace_Type;
use LAL_Refactor.Replace_Type;

with LSP.Messages;

with VSS.Strings.Conversions;
with LSP.Commands;

package body LSP.Ada_Handlers.Refactor.Replace_Type is

   ------------------------
   -- Append_Code_Action --
   ------------------------

   procedure Append_Code_Action
     (Self                        : in out Command;
      Context                     : Context_Access;
      Commands_Vector             : in out LSP.Messages.CodeAction_Vector;
      Where                       : LSP.Messages.Location)
   is
      Pointer     : LSP.Commands.Command_Pointer;
      Code_Action : LSP.Messages.CodeAction;

   begin
      Self.Initialize
        (Context => Context.all,
         Where   => Where);

      Pointer.Set (Self);

      Code_Action :=
        (title       => "Replace Type",
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

               if Key = "context_id" then
                  LSP.Types.Read_String (JS, V.Context_Id);

               elsif Key = "where" then
                  LSP.Messages.Location'Read (JS, V.Where);

               elsif Key = "newType" then
                  LSP.Types.Read_String (JS, V.New_Type);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;

         JS.R.Read_Next;
      end return;
   end Create;

   --------------
   -- Refactor --
   --------------

   overriding procedure Refactor
     (Self    : Command;
      Handler : not null access LSP.Server_Notification_Receivers.
        Server_Notification_Receiver'Class;
      Client : not null access LSP.Client_Message_Receivers.
        Client_Message_Receiver'Class;
      Edits   : out LAL_Refactor.Refactoring_Edits)
   is
      use Langkit_Support.Slocs;
      use LAL_Refactor;
      use LSP.Types;

      Message_Handler : LSP.Ada_Handlers.Message_Handler renames
        LSP.Ada_Handlers.Message_Handler (Handler.all);
      Context         : LSP.Ada_Contexts.Context renames
        Message_Handler.Contexts.Get (Self.Context_Id).all;

      function Analysis_Units return Analysis_Unit_Array is
        (Context.Analysis_Units);
      --  Provides the Context Analysis_Unit_Array to the Mode_Changer

      Replacer : constant Type_Replacer :=
        Create_Type_Replacer
          (Source_Unit      =>
             Context.Get_AU
               (Context.URI_To_File (Self.Where.uri)),
           Source_Type_SLOC =>
             (Langkit_Support.Slocs.Line_Number
                (Self.Where.span.first.line) + 1,
              Langkit_Support.Slocs.Column_Number
                (Self.Where.span.first.character) + 1),
           New_Type         =>
             VSS.Strings.Conversions.To_Unbounded_UTF_8_String
               (Self.New_Type));

   begin
      Edits := Replacer.Refactor (Analysis_Units'Access);
   end Refactor;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self                        : in out Command'Class;
      Context                     : LSP.Ada_Contexts.Context;
      Where                       : LSP.Messages.Location) is
   begin
      Self.Context_Id := Context.Id;
      Self.Where := Where;
      Self.New_Type := VSS.Strings.Empty_Virtual_String;
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
      JS.Key ("context_id");
      LSP.Types.Write_String (S, C.Context_Id);
      JS.Key ("where");
      LSP.Messages.Location'Write (S, C.Where);
      JS.Key ("newType");
      LSP.Types.Write_String (S, C.New_Type);
      JS.End_Object;
   end Write_Command;

end LSP.Ada_Handlers.Refactor.Replace_Type;

------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2021-2023, AdaCore                     --
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

with Libadalang.Analysis; use Libadalang.Analysis;

with Laltools.Common; use Laltools.Common;
with LAL_Refactor.Subprogram_Signature.Remove_Parameter;
use LAL_Refactor.Subprogram_Signature.Remove_Parameter;

with VSS.JSON.Streams;
with VSS.Strings.Conversions;

with LSP.Ada_Contexts;
with LSP.Enumerations;
with LSP.Utils;
with LSP.Structures.LSPAny_Vectors; use LSP.Structures.LSPAny_Vectors;

package body LSP.Ada_Handlers.Refactor.Remove_Parameter is

   ------------------------
   -- Append_Code_Action --
   ------------------------

   procedure Append_Code_Action
     (Self               : in out Command;
      Context            : LSP.Ada_Context_Sets.Context_Access;
      Commands_Vector    : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Target_Subp        : Libadalang.Analysis.Basic_Decl;
      Parameters_Indices : Parameter_Indices_Range_Type)
   is
      Code_Action : LSP.Structures.CodeAction;
      Where       : constant LSP.Structures.Location :=
        LSP.Utils.Get_Node_Location (Target_Subp.P_Defining_Name.F_Name);

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
        (Context         => Context,
         Where           => ((uri => Where.uri), Where.a_range.start),
         First_Parameter => Parameters_Indices.First,
         Last_Parameter  => Parameters_Indices.Last);

      Code_Action :=
        (title       => Create_Code_Action_Title,
         kind        =>
           (Is_Set => True,
            Value  => LSP.Enumerations.RefactorRewrite),
         diagnostics => <>,
         edit        => (Is_Set => False),
         isPreferred => (Is_Set => False),
         disabled    => (Is_Set => False),
         command     =>
           (Is_Set => True,
            Value  =>
              (title     => <>,
               command   => VSS.Strings.Conversions.To_Virtual_String
                 (Command'External_Tag),
               arguments => Self.Write_Command)),
         data        => <>);

      Commands_Vector.Append
        (LSP.Structures.Command_Or_CodeAction'
           (Is_Command => False, CodeAction => Code_Action));
   end Append_Code_Action;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Any : not null access LSP.Structures.LSPAny_Vector)
      return Command
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

         while Has_Element (C)
           and then Element (C).Kind /= End_Object
         loop
            pragma Assert (Element (C).Kind = Key_Name);
            declare
               Key : constant Virtual_String := Element (C).Key_Name;
            begin
               Next (C);

               if Key = "context" then
                  Self.Context := Element (C).String_Value;

               elsif Key = "where" then
                  Self.Where := From_Any (C);

               elsif Key = "first_parameter" then
                  Self.First_Parameter := From_Any (C);

               elsif Key = "last_parameter" then
                  Self.Last_Parameter := From_Any (C);

               else
                  Skip_Value (C);
               end if;
            end;

            Next (C);
         end loop;
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
      use LAL_Refactor;
      Message_Handler : LSP.Ada_Handlers.Message_Handler renames
        LSP.Ada_Handlers.Message_Handler (Handler.all);
      Context         : LSP.Ada_Contexts.Context renames
        Message_Handler.Contexts.Get (Self.Context).all;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Message_Handler.Get_Open_Document (Self.Where.textDocument.uri);
      Node : constant Ada_Node :=
        Document.Get_Node_At (Context, Self.Where.position);

      Target_Subp               : constant Defining_Name :=
        Resolve_Name_Precisely (Get_Node_As_Name (Node));
      Target_Parameters_Indices : constant Parameter_Indices_Range_Type :=
        (First => Positive (Self.First_Parameter),
         Last  => Positive (Self.Last_Parameter));

      Remover : Parameter_Remover;

      function Analysis_Units return Analysis_Unit_Array is
        (Context.Analysis_Units);
      --  Provides the Context Analysis_Unit_Array to the Mode_Changer

   begin
      if Target_Subp.Is_Null then
         Edits := (Diagnostics =>
              [LAL_Refactor.Subprogram_Signature.Create
                   (Subp => Node,
                    Info => VSS.Strings.Conversions.To_Virtual_String
                      ("The target subprogram could not"
                       & " be resolved precisely."))],
               others => <>);
         return;
      end if;

      Remover := Create (Target_Subp.P_Basic_Decl, Target_Parameters_Indices);

      Edits := Remover.Refactor (Analysis_Units'Access);
   end Refactor;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self            : in out Command'Class;
      Context         : LSP.Ada_Context_Sets.Context_Access;
      Where           : LSP.Structures.TextDocumentPositionParams;
      First_Parameter : Integer;
      Last_Parameter  : Integer) is
   begin
      Self.Context := Context.Id;
      Self.Where := Where;
      Self.First_Parameter := First_Parameter;
      Self.Last_Parameter := Last_Parameter;
   end Initialize;

   -------------------
   -- Write_Command --
   -------------------

   function Write_Command
     (Self : Command) return LSP.Structures.LSPAny_Vector
   is
      use VSS.JSON.Streams;

      Result : LSP.Structures.LSPAny_Vector;
   begin
      Result.Append (JSON_Stream_Element'(Kind => Start_Array));
      Result.Append (JSON_Stream_Element'(Kind => Start_Object));

      --  "context"
      Add_Key ("context", Result);
      To_Any (Self.Context, Result);

      --  "where"
      Add_Key ("where", Result);
      To_Any (Self.Where, Result);

      --  "first_parameter"
      Add_Key ("first_parameter", Result);
      To_Any (Self.First_Parameter, Result);

      --  "last_parameter"
      Add_Key ("last_parameter", Result);
      To_Any (Self.Last_Parameter, Result);

      Result.Append (JSON_Stream_Element'(Kind => End_Object));
      Result.Append (JSON_Stream_Element'(Kind => End_Array));

      return Result;
   end Write_Command;

end LSP.Ada_Handlers.Refactor.Remove_Parameter;

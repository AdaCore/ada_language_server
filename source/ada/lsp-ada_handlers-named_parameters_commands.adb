------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2020-2023, AdaCore                     --
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

with VSS.JSON.Streams;
with VSS.String_Vectors;
with VSS.Strings.Conversions;

with Libadalang.Analysis;
with Libadalang.Common;

with LSP.Ada_Contexts;
with LSP.Constants;
with LSP.Enumerations;
with LSP.Structures.LSPAny_Vectors; use LSP.Structures.LSPAny_Vectors;
with LSP.Utils;

package body LSP.Ada_Handlers.Named_Parameters_Commands is

   function Get_Parameters
     (Args : Libadalang.Analysis.Basic_Assoc_List)
      return VSS.String_Vectors.Virtual_String_Vector;
   --  Find list of parameter names from given AST node.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self                : in out Command'Class;
      Context             : LSP.Ada_Context_Sets.Context_Access;
      Where               : LSP.Structures.TextDocumentPositionParams;
      Versioned_Documents : Boolean) is
   begin
      Self.Context             := Context.Id;
      Self.Where               := Where;
      Self.Versioned_Documents := Versioned_Documents;
   end Initialize;

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

               elsif Key = "versioned_documents" then
                  Self.Versioned_Documents := From_Any (C);

               else
                  Skip_Value (C);
               end if;
            end;

            Next (C);
         end loop;
      end return;
   end Create;

   -----------------------
   -- Append_Suggestion --
   -----------------------

   procedure Append_Suggestion
     (Self                : in out Command;
      Context             : LSP.Ada_Context_Sets.Context_Access;
      Commands_Vector     : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Where               : LSP.Structures.Location;
      Versioned_Documents : Boolean)
   is
      Code_Action : LSP.Structures.CodeAction;
   begin
      Self.Initialize
        (Context             => Context,
         Where               => ((uri => Where.uri), Where.a_range.start),
         Versioned_Documents => Versioned_Documents);

      Code_Action :=
        (title       => "Name parameters in the call",
         kind        => (Is_Set => True,
                         Value  => LSP.Enumerations.RefactorRewrite),
         diagnostics => <>,
         disabled    => (Is_Set => False),
         edit        => (Is_Set => False),
         isPreferred => (Is_Set => False),
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
   end Append_Suggestion;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self    : Command;
      Handler : not null access
        LSP.Server_Notification_Receivers.Server_Notification_Receiver'Class;
      Client  : not null access
        LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Id     : LSP.Structures.Integer_Or_Virtual_String;
      Error  : in out LSP.Errors.ResponseError_Optional)
   is
      use LSP.Structures;

      procedure Append
        (Node : Libadalang.Analysis.Ada_Node;
         Name : VSS.Strings.Virtual_String);
      --  Create and append a TextEdit to insert Name & " => " before Node.

      Apply  : LSP.Structures.ApplyWorkspaceEditParams;
      Edits  : LSP.Structures.WorkspaceEdit renames Apply.edit;

      Message_Handler : LSP.Ada_Handlers.Message_Handler renames
        LSP.Ada_Handlers.Message_Handler (Handler.all);

      ------------
      -- Append --
      ------------

      procedure Append
        (Node : Libadalang.Analysis.Ada_Node;
         Name : VSS.Strings.Virtual_String)
      is
         use type VSS.Strings.Virtual_String;

         Loc  : constant LSP.Structures.Location :=
           LSP.Utils.Get_Node_Location (Node);
         Edit : LSP.Structures.AnnotatedTextEdit;

      begin
         Edit.a_range := (Loc.a_range.start, Loc.a_range.start);
         Edit.newText := Name & " => ";

         if Self.Versioned_Documents then
            Edits.documentChanges (1).Variant_1.edits.Append
              (LSP.Structures.TextEdit_Or_AnnotatedTextEdit'
                 (Is_TextEdit => False, AnnotatedTextEdit => Edit));
         else
            if Edits.changes.Contains (Self.Where.textDocument.uri) then
               Edits.changes (Self.Where.textDocument.uri).Append
                 (LSP.Structures.TextEdit (Edit));
            else
               declare
                  Text_Edits : LSP.Structures.TextEdit_Vector;
               begin
                  Text_Edits.Append (LSP.Structures.TextEdit (Edit));
                  Edits.changes.Include
                    (Self.Where.textDocument.uri, Text_Edits);
               end;
            end if;
         end if;
      end Append;

      Context  : LSP.Ada_Contexts.Context renames
        Message_Handler.Contexts.Get (Self.Context).all;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Message_Handler.Get_Open_Document (Self.Where.textDocument.uri);

      Node : Libadalang.Analysis.Ada_Node :=
        Document.Get_Node_At (Context, Self.Where.position);

      Args    : Libadalang.Analysis.Basic_Assoc_List;
      Params  : VSS.String_Vectors.Virtual_String_Vector;
      Index   : Natural := 0;
      Version : constant LSP.Structures.VersionedTextDocumentIdentifier :=
        Document.Versioned_Identifier;

   begin
      Apply.label := VSS.Strings.Conversions.To_Virtual_String
        (Command'External_Tag);

      if Self.Versioned_Documents then
         Edits.documentChanges.Append
           (documentChanges_OfWorkspaceEdit_Item'
              (Kind     =>
                   documentChanges_OfWorkspaceEdit_Item_Variant'(Variant_1),
               Variant_1 =>
                 (textDocument =>
                      (uri      => Version.uri,
                       version  => (Is_Null => False,
                                    Value   => Version.version)),
                  edits        => <>)));
      end if;

      while not Node.Is_Null and then
        Node.Kind not in Libadalang.Common.Ada_Basic_Assoc_List
      loop
         Node := Node.Parent;
      end loop;

      if Node.Is_Null then
         Error :=
           (Is_Set => True,
            Value  =>
              (code    => LSP.Constants.InvalidRequest,
               message => "This is not a valid position to name parameters."));
         return;
      end if;

      Args := Node.As_Basic_Assoc_List;
      Params := Get_Parameters (Args);

      --  If we have more than one argument but no params were found then
      --  we could not resolve this this call expression precisely due to
      --  invalid Ada code.

      if Params.Is_Empty and then Args.Children_Count /= 0
      then
         Error :=
           (Is_Set => True,
            Value  =>
              (code    => LSP.Constants.InvalidRequest,
               message =>
                 "Could not resolve this call expression precisely."));
         return;
      end if;

      Index := Args.Children_Count;

      for Arg of reverse Args.Children loop
         exit when Index < 1;

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

      Client.On_ApplyEdit_Request (Id, Apply);
   exception
      when E : others =>
         Error :=
           (Is_Set => True,
            Value  =>
              (code    => LSP.Constants.UnknownErrorCode,
               message => VSS.Strings.Conversions.To_Virtual_String
                 (Ada.Exceptions.Exception_Information (E))));
   end Execute;

   --------------------
   -- Get_Parameters --
   --------------------

   function Get_Parameters
     (Args   : Libadalang.Analysis.Basic_Assoc_List)
      return VSS.String_Vectors.Virtual_String_Vector
   is
      procedure Append (Params : Libadalang.Analysis.Param_Spec_Array);
      --  Append identifiers from Params to Result

      function Get_Params_Spec_Array
        (Decl : Libadalang.Analysis.Basic_Decl)
         return Libadalang.Analysis.Param_Spec_Array;
      --  Return the Param_Spec_Array associated with the given Decl

      function Get_Subp_Spec
        (Decl : Libadalang.Analysis.Basic_Decl)
         return Libadalang.Analysis.Base_Subp_Spec;
      --  Return the Base_Subp_Spec associated with this Decl

      Result : VSS.String_Vectors.Virtual_String_Vector;

      ------------
      -- Append --
      ------------

      procedure Append
        (Params : Libadalang.Analysis.Param_Spec_Array) is
      begin
         for Param of Params loop
            for Id of Param.F_Ids loop
               Result.Append (VSS.Strings.To_Virtual_String (Id.Text));
            end loop;
         end loop;
      end Append;

      ---------------------------
      -- Get_Params_Spec_Array --
      ---------------------------

      function Get_Params_Spec_Array
        (Decl : Libadalang.Analysis.Basic_Decl)
         return Libadalang.Analysis.Param_Spec_Array
      is
         Spec   : constant Libadalang.Analysis.Base_Subp_Spec :=
           Get_Subp_Spec (Decl);
         Params : constant Libadalang.Analysis.Param_Spec_Array :=
           Spec.P_Params;

      begin
         if Spec.Is_Null or else Params'Length = 0 then
            return (1 .. 0 => <>);
         end if;

         return Params;
      end Get_Params_Spec_Array;

      -------------------
      -- Get_Subp_Spec --
      -------------------

      function Get_Subp_Spec
        (Decl : Libadalang.Analysis.Basic_Decl)
         return Libadalang.Analysis.Base_Subp_Spec
      is
         function Process_Type_Expr
           (TE : Libadalang.Analysis.Type_Expr)
            return Libadalang.Analysis.Base_Subp_Spec;
         --  Checks if TE is associated to an access of a subprogram, and if
         --  so, returns its Base_Subp_Spec.

         -----------------------
         -- Process_Type_Expr --
         -----------------------

         function Process_Type_Expr
           (TE : Libadalang.Analysis.Type_Expr)
            return Libadalang.Analysis.Base_Subp_Spec
         is
            TD : Libadalang.Analysis.Base_Type_Decl;
            --  If TE is not an anonymous type then we'll need to know its
            --  declaration.

         begin
            if TE.Is_Null then
               return Libadalang.Analysis.No_Base_Subp_Spec;
            end if;

            case TE.Kind is
               when Libadalang.Common.Ada_Subtype_Indication_Range =>
                  TD := TE.As_Subtype_Indication.P_Designated_Type_Decl;

                  if TD.Is_Null
                    or else
                      not (TD.Kind in Libadalang.Common.Ada_Type_Decl)
                  then
                     return Libadalang.Analysis.No_Base_Subp_Spec;
                  end if;

                  if TD.As_Type_Decl.F_Type_Def.Kind in
                      Libadalang.Common.Ada_Access_To_Subp_Def_Range
                    and then not TD.As_Type_Decl.F_Type_Def.
                      As_Access_To_Subp_Def.F_Subp_Spec.Is_Null
                  then
                     --  Confirmation that TD is an access to a subprogram
                     return TD.As_Type_Decl.F_Type_Def.As_Access_To_Subp_Def.
                       F_Subp_Spec.As_Base_Subp_Spec;

                  elsif TD.As_Type_Decl.F_Type_Def.Kind in
                      Libadalang.Common.Ada_Array_Type_Def_Range
                    and then not TD.As_Type_Decl.F_Type_Def.
                        As_Array_Type_Def.F_Component_Type.F_Type_Expr.Is_Null
                  then
                     --  If TD is an array type, then it might be an array
                     --  of accesses to subprograms. Therefore, recursively
                     --  call Process_Type_Expr to check the type of the
                     --  components of the array.

                     return Process_Type_Expr
                       (TD.As_Type_Decl.F_Type_Def.As_Array_Type_Def.
                          F_Component_Type.F_Type_Expr);

                  else
                     return Libadalang.Analysis.No_Base_Subp_Spec;
                  end if;

               when Libadalang.Common.Ada_Anonymous_Type_Range =>
                  if TE.As_Anonymous_Type.F_Type_Decl.F_Type_Def.Kind in
                    Libadalang.Common.Ada_Access_To_Subp_Def_Range
                  then
                     return TE.As_Anonymous_Type.F_Type_Decl.F_Type_Def.
                       As_Access_To_Subp_Def.F_Subp_Spec.As_Base_Subp_Spec;

                  else
                     return Libadalang.Analysis.No_Base_Subp_Spec;
                  end if;

               when others =>
                  return Libadalang.Analysis.No_Base_Subp_Spec;
            end case;
         end Process_Type_Expr;

      begin
         if Decl.Is_Null then
            return Libadalang.Analysis.No_Base_Subp_Spec;
         end if;

         --  For Ada_Param_Spec, Ada_Component_Decl or Object_Decl nodes,
         --  it must be an access to a subprogram, so get its spec.

         case Decl.Kind is
            when Libadalang.Common.Ada_Base_Subp_Body =>
               return Decl.As_Base_Subp_Body.F_Subp_Spec.As_Base_Subp_Spec;

            when Libadalang.Common.Ada_Basic_Subp_Decl =>
               return Decl.As_Basic_Subp_Decl.P_Subp_Decl_Spec;

            when Libadalang.Common.Ada_Param_Spec_Range =>
               return Process_Type_Expr (Decl.As_Param_Spec.F_Type_Expr);

            when Libadalang.Common.Ada_Component_Decl_Range =>
               return Process_Type_Expr
                 (Decl.As_Component_Decl.F_Component_Def.F_Type_Expr);

            when  Libadalang.Common.Ada_Object_Decl_Range =>
               return Process_Type_Expr (Decl.As_Object_Decl.F_Type_Expr);

            when others =>
               return Libadalang.Analysis.No_Base_Subp_Spec;
         end case;
      end Get_Subp_Spec;

      Expr        : constant Libadalang.Analysis.Ada_Node := Args.Parent;
      Name        : Libadalang.Analysis.Name;
      Decl        : Libadalang.Analysis.Basic_Decl;
      Is_Dot_Call : Boolean;

      use type Libadalang.Analysis.Basic_Decl;
   begin
      case Expr.Kind is
         when Libadalang.Common.Ada_Call_Expr =>
            Name := Expr.As_Call_Expr.F_Name;
         when others =>
            return VSS.String_Vectors.Empty_Virtual_String_Vector;
      end case;

      Decl := Name.P_Referenced_Decl;

      --  Return an empty Result if we can't resolve this call expression
      --  precisely.

      if Decl = Libadalang.Analysis.No_Basic_Decl then
         return Result;
      end if;

      Is_Dot_Call := Name.P_Is_Dot_Call;

      --  Don't append the first parameter if we are dealing with a dot call.

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

      --  "versioned_documents"
      Add_Key ("versioned_documents", Result);
      To_Any (Self.Versioned_Documents, Result);

      Result.Append (JSON_Stream_Element'(Kind => End_Object));
      Result.Append (JSON_Stream_Element'(Kind => End_Array));

      return Result;
   end Write_Command;

end LSP.Ada_Handlers.Named_Parameters_Commands;

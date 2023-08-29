------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
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

with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Slocs;
with Langkit_Support.Text;
with Libadalang.Common;

with Laltools.Common;

with LAL_Refactor.Safe_Rename;

with LSP.Ada_Contexts;
with LSP.Ada_Handlers.Locations;
with LSP.Utils;

package body LSP.Ada_Handlers.Renaming is

   ----------
   -- Hash --
   ----------

   function Hash (X : File_Edit) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;

      Result : Ada.Containers.Hash_Type :=
        LSP.Structures.documentChanges_OfWorkspaceEdit_Item_Variant'Pos
          (X.Kind);
   begin
      case X.Kind is
         when LSP.Structures.Variant_1 =>
            Result := @ + X.Variant_1.textDocument.uri.Get_Hash;

            for Item of X.Variant_1.edits loop
               case Item.Is_TextEdit is
                  when True =>
                     Result := @ + Ada.Containers.Hash_Type'Mod
                       (Item.TextEdit.newText.Hash);

                     Result := @ + LSP.Utils.Hash (Item.TextEdit.a_range);

                  when False =>
                     Result := @ + Ada.Containers.Hash_Type'Mod
                       (Item.AnnotatedTextEdit.newText.Hash);

                     Result := @ +
                       LSP.Utils.Hash (Item.AnnotatedTextEdit.a_range);
               end case;
            end loop;

         when LSP.Structures.create =>
            Result := @ + X.create.uri.Get_Hash;

         when LSP.Structures.rename =>
            Result := @ + X.rename.newUri.Get_Hash + X.rename.oldUri.Get_Hash;

         when LSP.Structures.delete =>
            Result := @ + X.delete.uri.Get_Hash;
      end case;

      return Result;
   end Hash;

   ---------------------
   -- Process_Context --
   ---------------------

   procedure Process_Context
     (Self      : in out Message_Handler'Class;
      C         : LSP.Ada_Context_Sets.Context_Access;
      Name_Node : Libadalang.Analysis.Name;
      New_Name  : VSS.Strings.Virtual_String;
      Filter    : in out Edit_Sets.Set;
      Result    : in out LSP.Structures.WorkspaceEdit;
      Errors    : in out LAL_Refactor.Refactoring_Diagnostic_Vector)
   is
      use Libadalang.Common;
      use Libadalang.Analysis;
      use LAL_Refactor.Safe_Rename;

      Fix_Comments : constant Boolean := Self.Configuration.Rename_In_Comments;
      Versioned_Documents : constant Boolean := Self.Client.Versioned_Documents;

      Safe_Renamer : LAL_Refactor.Safe_Rename.Safe_Renamer;
      Context_Edits : LAL_Refactor.Refactoring_Edits;
      --  Edits found for a particular context

      Algorithm    : constant LAL_Refactor.Safe_Rename.
        Problem_Finder_Algorithm_Kind :=
          LAL_Refactor.Safe_Rename.Analyse_AST;

      Definition    : constant Defining_Name :=
        Laltools.Common.Resolve_Name_Precisely (Name_Node);

      function Attribute_Value_Provider_Callback
        (Attribute    : GPR2.Q_Attribute_Id;
         Index        : String := "";
         Default      : String := "";
         Use_Extended : Boolean := False)
            return String
      is (C.Project_Attribute_Value
          (Attribute, Index, Default, Use_Extended));

      Attribute_Value_Provider : constant
        GPR2_Attribute_Value_Provider_Access :=
          Attribute_Value_Provider_Callback'Unrestricted_Access;

      function Analysis_Units return Analysis_Unit_Array is (C.Analysis_Units);
      --  Callback needed to provide the analysis units to the safe rename
      --  tool.

      procedure Process_Comments (File_Name : String);
      --  Iterate over all comments and include them in the response when
      --  they contain a renamed word.

      procedure Process_References;
      --  Merges Context_Edits.Text_Edits into All_Edits.Text_Edits and for
      --  each Text_Edit (which represents a reference) processes its
      --  references in comments.

      function To_Edit
        (File       : String;
         Text_Edits : LAL_Refactor.Text_Edit_Ordered_Set)
        return LSP.Structures.TextDocumentEdit;

      procedure Append (Item : File_Edit);
      --  Append Item to Result if Filter does't contain it yet

      ------------
      -- Append --
      ------------

      procedure Append (Item : File_Edit) is
         Ignore   : Edit_Sets.Cursor;
         Inserted : Boolean;
         URI      : LSP.Structures.DocumentUri;
      begin
         Filter.Insert (Item, Ignore, Inserted);
         --  Check if we've already had this reference from another
         --  context by trying to insert it into Filter.

         if not Inserted then
            null; --  This edit has been added already, do nothing

         elsif Versioned_Documents then

            Result.documentChanges.Append (Item);

         else

            URI := Item.Variant_1.textDocument.uri;

            if not Result.changes.Contains (URI) then
               Result.changes.Insert (URI, LSP.Structures.Empty);
            end if;

            for X of Item.Variant_1.edits loop
               Result.changes (URI).Append (X.TextEdit);
            end loop;
         end if;
      end Append;

      -----------------------
      --  Process_Comments --
      -----------------------

      procedure Process_Comments (File_Name : String) is
         use LAL_Refactor;

         Unit : constant Analysis_Unit := C.Get_AU
           (GNATCOLL.VFS.Create_From_UTF8 (File_Name));

         Set       : LAL_Refactor.Text_Edit_Ordered_Set;
         Token     : Token_Reference := Unit.First_Token;
         --  Name      : constant Wide_Wide_String := Name_Node.Text;
         Name      : constant Wide_Wide_String :=
           Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String
             (Laltools.Common.Get_Last_Name (Name_Node));
         Text_Edit : LAL_Refactor.Text_Edit;
         Span      : Langkit_Support.Slocs.Source_Location_Range;
         Current   : Token_Reference;
         Diff      : Integer;

         function Process_Box return Boolean;
         --  Check whether Current is box header/footer and modify it.
         --  Return False when the searching cycle should be stopped.

         -----------------
         -- Process_Box --
         -----------------

         function Process_Box return Boolean is
            use Langkit_Support.Text;
            use Langkit_Support.Slocs;

         begin
            if Current = No_Token then
               return False;
            end if;

            case Kind (Data (Current)) is
               when Ada_Whitespace =>
                  return True;

               when Ada_Comment =>
                  declare
                     Value : constant Text_Type := Text (Current);
                  begin
                     for Idx in Value'Range loop
                        if Value (Idx) /= '-' then
                           return False;
                        end if;
                     end loop;

                     if Diff > 0 then
                        --  Increase '-', Diff is positive
                        declare
                           Sloc : Source_Location_Range :=
                             Sloc_Range (Data (Current));

                        begin
                           Sloc.Start_Column := Sloc.End_Column;

                           Text_Edit :=
                             (Sloc,
                              Ada.Strings.Unbounded."*" (Diff, '-'));

                           Set.Include (Text_Edit);
                        end;

                     else
                        --  Decrease '-', Diff is negative
                        declare
                           Sloc : Source_Location_Range :=
                             Sloc_Range (Data (Current));

                        begin
                           Sloc.Start_Column :=
                             Sloc.End_Column - Column_Number (abs Diff);

                           Text_Edit :=
                             (Sloc,
                              Ada.Strings.Unbounded.Null_Unbounded_String);

                           Set.Include (Text_Edit);
                        end;
                     end if;

                     return False;
                  end;

               when others =>
                  return False;
            end case;
         end Process_Box;

      begin
         Diff := Natural (New_Name.Character_Length) - Name'Length;

         while Token /= No_Token loop
            declare
               This_Span : Langkit_Support.Slocs.Source_Location_Range;
            begin
               if Kind (Data (Token)) = Ada_Comment
                 and then Laltools.Common.Contains
                   (Token, Name, True, This_Span)
               then
                  Text_Edit.Location := This_Span;
                  Text_Edit.Text :=
                    VSS.Strings.Conversions.To_Unbounded_UTF_8_String
                      (New_Name);
                  --  Include corrected comment itself
                  Set.Include (Text_Edit);

                  if Diff /= 0
                    and then Laltools.Common.Contains
                      (Token, "-- " & Name & " --", False, Span)
                  then
                     --  Can be a comment box
                     Current := Previous (Token);
                     loop
                        --  Looking for the box header
                        exit when not Process_Box;
                        Current := Previous (Current);
                     end loop;

                     Current := Next (Token);
                     loop
                        --  Looking for the box footer
                        exit when not Process_Box;
                        Current := Next (Current);
                     end loop;
                  end if;
               end if;
            end;

            Token := Next (Token);
         end loop;

         if not Set.Is_Empty then
            declare
               Item : File_Edit (LSP.Structures.Variant_1);
               Edit : LSP.Structures.TextDocumentEdit renames Item.Variant_1;
            begin
               Edit := To_Edit (File_Name, Set);
               Append (Item);
            end;
         end if;
      end Process_Comments;

      ------------------------
      -- Process_References --
      ------------------------

      procedure Process_References is
         use LAL_Refactor;

         Text_Edits_Cursor : Text_Edit_Ordered_Maps.Cursor :=
           Context_Edits.Text_Edits.First;

         Item : File_Edit (LSP.Structures.Variant_1);
         Edit : LSP.Structures.TextDocumentEdit renames Item.Variant_1;
      begin
         Text_Edits_Cursor := Context_Edits.Text_Edits.First;

         while Text_Edit_Ordered_Maps.Has_Element (Text_Edits_Cursor) loop

            for Text_Edit of
              Text_Edit_Ordered_Maps.Element (Text_Edits_Cursor)
            loop

               Edit := To_Edit
                 (Text_Edit_Ordered_Maps.Key (Text_Edits_Cursor),
                  Context_Edits.Text_Edits (Text_Edits_Cursor));

               Append (Item);
            end loop;

            if Fix_Comments then
               Process_Comments
                 (Text_Edit_Ordered_Maps.Key (Text_Edits_Cursor));
            end if;

            Text_Edits_Cursor :=
              Text_Edit_Ordered_Maps.Next (Text_Edits_Cursor);
         end loop;
      end Process_References;

      -------------
      -- To_Edit --
      -------------

      function To_Edit
        (File       : String;
         Text_Edits : LAL_Refactor.Text_Edit_Ordered_Set)
         return LSP.Structures.TextDocumentEdit is
      begin
         return Result : LSP.Structures.TextDocumentEdit do
            for Item of Text_Edits loop
               declare
                  Edit : LSP.Structures.TextEdit_Or_AnnotatedTextEdit;

                  Text : constant VSS.Strings.Virtual_String :=
                    VSS.Strings.Conversions.To_Virtual_String (Item.Text);

                  Loc : constant LSP.Structures.Location :=
                    Locations.To_LSP_Location
                      (Self, C.all, File, Item.Location);
               begin
                  if Result.textDocument.uri.Is_Empty then
                     Result.textDocument :=
                       Self.Get_Open_Document_Version (Loc.uri);
                  end if;

                  if Versioned_Documents then
                     Edit :=
                       (Is_TextEdit       => False,
                        AnnotatedTextEdit =>
                          (a_range      => Loc.a_range,
                           newText      => Text,
                           annotationId => <>));  --  could it be empty???
                  else
                     Edit :=
                       (Is_TextEdit => True,
                        TextEdit    =>
                          (a_range => Loc.a_range,
                           newText => Text));
                  end if;

                  Result.edits.Append (Edit);
               end;
            end loop;
         end return;
      end To_Edit;

      Is_Create_Supported : constant Boolean :=
        Self.Client.Resource_Create_Supported;

      Is_Rename_Supported : constant Boolean :=
        Self.Client.Resource_Rename_Supported;

      Is_Delete_Supported : constant Boolean :=
        Self.Client.Resource_Delete_Supported;

   begin
      if Definition.Is_Null then
         return;
      end if;

      Safe_Renamer := LAL_Refactor.Safe_Rename.Create_Safe_Renamer
        (Definition               => Definition,
         New_Name                 =>
           VSS.Strings.Conversions.To_Unbounded_Wide_Wide_String (New_Name),
         Algorithm                => Algorithm,
         Attribute_Value_Provider => Attribute_Value_Provider);

      Context_Edits := Safe_Renamer.Refactor (Analysis_Units'Access);

      --  If problems were found, do not continue processing references

      if not Context_Edits.Diagnostics.Is_Empty then
         Errors.Move (Source => Context_Edits.Diagnostics);
         return;
      end if;

      Process_References;

      if Is_Create_Supported then
         for X of Context_Edits.File_Creations loop
            declare
               File : constant String := Ada.Strings.Unbounded.To_String (X.Filepath);
               URI  : constant LSP.Structures.DocumentUri := Self.To_URI (File);

               Create : constant File_Edit :=
                 (Kind => LSP.Structures.create,
                  create =>
                    (kind         => "create",
                     uri          => URI,
                     options      => <>,
                     annotationId => <>));

               Change : constant LAL_Refactor.Text_Edit :=
                 (Location => (Start_Line   => 1,
                               End_Line     => 1,
                               Start_Column => 1,
                               End_Column   => 0),
                  Text     => X.Content);

               Set : constant LAL_Refactor.Text_Edit_Ordered_Set := [Change];

               Item : File_Edit (LSP.Structures.Variant_1);
               Edit : LSP.Structures.TextDocumentEdit renames Item.Variant_1;
            begin
               Append (Create);
               Edit := To_Edit (File, Set);
               Append (Item);
            end;
         end loop;
      end if;

      if Is_Rename_Supported then
         for X of Context_Edits.File_Renames loop
            declare
               Item : constant File_Edit :=
                 (Kind => LSP.Structures.rename,
                  rename =>
                    (kind         => "rename",
                     oldUri          => Self.To_URI
                       (Ada.Strings.Unbounded.To_String (X.Filepath)),
                     newUri          => Self.To_URI
                       (Ada.Strings.Unbounded.To_String (X.New_Name)),
                     options      => <>,
                     annotationId => <>));
            begin
               Append (Item);
            end;
         end loop;
      end if;

      if Is_Delete_Supported then
         for X of Context_Edits.File_Creations loop
            declare
               Item : constant File_Edit :=
                 (Kind => LSP.Structures.delete,
                  delete =>
                    (kind         => "delete",
                     uri          => Self.To_URI
                       (Ada.Strings.Unbounded.To_String (X.Filepath)),
                     options      => <>,
                     annotationId => <>));
            begin
               Append (Item);
            end;
         end loop;
      end if;

   end Process_Context;

end LSP.Ada_Handlers.Renaming;

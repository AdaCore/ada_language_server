------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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

with GNATCOLL.Utils;
with GNATCOLL.Traces;
with Laltools.Common;
with LSP.Ada_Documents;
with LSP.Ada_Documentation;
with LSP.Enumerations;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;
with VSS.Unicode;

package body LSP.Ada_Completions.Generic_Assoc is

   Me_Debug : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("LSP.GENERIC_ASSOC.DEBUG", GNATCOLL.Traces.Off);

   function Match_Designators
     (Children : LSP.Ada_Completions.Generic_Assoc_Utils.Param_Vectors.Vector;
      Parent   : Laltools.Common.Node_Vectors.Vector)
      return Boolean;
   --  Return True if all the designators of Child are also in Parent

   function In_Parameters
     (Node   : Libadalang.Analysis.Ada_Node;
      Params : LSP.Ada_Completions.Generic_Assoc_Utils.Param_Vectors.Vector)
      return Boolean;

   function In_Parent
     (Desg   : Libadalang.Analysis.Ada_Node;
      Parent : Laltools.Common.Node_Vectors.Vector) return Boolean;

   function Is_Signature_Active
     (Parameters      : LSP.Structures.ParameterInformation_Vector;
      Sig_Label       : VSS.Strings.Virtual_String;
      Cursor_Position : Integer;
      Designator      : Libadalang.Analysis.Ada_Node;
      Active_Position : out Integer)
      return Boolean;
   --  Return True if Parameters is valid for the current context.
   --  Active_Position will point to the active parameter inside Parameters.

   procedure Find_Cursor_Position
     (Elem_Node          : Libadalang.Analysis.Ada_Node'Class;
      Cursor             : Langkit_Support.Slocs.Source_Location;
      Prefixed           : Boolean;
      Parameters         :
        LSP.Ada_Completions.Generic_Assoc_Utils.Param_Vectors.Vector;
      Cursor_Position    : out Integer;
      Current_Designator : out Libadalang.Analysis.Ada_Node);

   function Find_Designator_Position
     (Designator       : Libadalang.Analysis.Ada_Node;
      Spec_Designators : Laltools.Common.Node_Vectors.Vector;
      Cursor_Position  : Integer)
      return Integer;

   -----------------------
   -- Match_Designators --
   -----------------------

   function Match_Designators
     (Children : LSP.Ada_Completions.Generic_Assoc_Utils.Param_Vectors.Vector;
      Parent   : Laltools.Common.Node_Vectors.Vector)
      return Boolean is
   begin
      for C of Children loop
         if C.Is_Named
           and then not C.Node.Is_Null
           and then not In_Parent (C.Node, Parent)
         then
            return False;
         end if;
      end loop;

      return True;
   end Match_Designators;

   -------------------
   -- In_Parameters --
   -------------------

   function In_Parameters
     (Node   : Libadalang.Analysis.Ada_Node;
      Params : LSP.Ada_Completions.Generic_Assoc_Utils.Param_Vectors.Vector)
      return Boolean
   is
      Node_Text : constant Langkit_Support.Text.Text_Type := Node.Text;
   begin
      for P of Params loop
         if P.Is_Named and then P.Node.Text = Node_Text then
            return True;
         end if;
      end loop;

      return False;
   end In_Parameters;

   ---------------
   -- In_Parent --
   ---------------

   function In_Parent
     (Desg   : Libadalang.Analysis.Ada_Node;
      Parent : Laltools.Common.Node_Vectors.Vector) return Boolean
   is
      Desg_Text : constant Langkit_Support.Text.Text_Type := Desg.Text;
   begin
      for P of Parent loop
         if P.Text = Desg_Text then
            return True;
         end if;
      end loop;

      return False;
   end In_Parent;

   ------------------------
   -- Propose_Completion --
   ------------------------

   procedure Propose_Completion
     (Self         :
        LSP.Ada_Completions.Parameters.Parameter_Completion_Provider;
      Sloc         : Langkit_Support.Slocs.Source_Location;
      Token        : Libadalang.Common.Token_Reference;
      Node         : Libadalang.Analysis.Ada_Node;
      Limit        : Natural;
      Filter       : in out LSP.Ada_Completions.Filters.Filter;
      Names        : in out Ada_Completions.Completion_Maps.Map;
      Unsorted_Res : in out LSP.Structures.CompletionItem_Vector)
   is
      pragma Unreferenced (Filter, Names);
      use Libadalang.Analysis;
      use Libadalang.Common;
      use LSP.Ada_Completions.Generic_Assoc_Utils;

      Elem_Node         : constant Element := Search_Element (Node);
      Token_Kind        : Libadalang.Common.Token_Kind := Kind (Data (Token));
      Whitespace_Prefix : VSS.Strings.Virtual_String;
      --  Empty if we already have a whitespace before a ","

      Parameters        :
        LSP.Ada_Completions.Generic_Assoc_Utils.Param_Vectors.Vector;
      --  Current list of parameters

      Unnamed_Params    : Natural := 0;
      --  The number of parameters without designators already present

      Using_Name        : Boolean := False;
      --  Are we already using name notation

      Prefix      : VSS.Strings.Virtual_String;
      --  The whole string before the snippet (including whitespaces)

      Column      : Langkit_Support.Slocs.Column_Number;
      --  Use Column as the block indentation

      Prefix_Span : LSP.Structures.A_Range;
      --  The span covering Prefix.

      Prefixed : Boolean;

      function Has_Designator (Unnamed_Params : out Natural) return Boolean;
      --  Return True if we have at least one named designator
      --  Also compute the number of unnamed parameters.

      procedure Generate_Snippets
        (Spec_Designators  : Laltools.Common.Node_Vectors.Vector;
         Param_Types       : Param_To_Type_Maps.Map;
         Decl              : Basic_Decl;
         Title             : VSS.Strings.Virtual_String;
         Snippet_Prefix    : VSS.Strings.Virtual_String;
         Completion_Prefix : VSS.Strings.Virtual_String);

      --------------------
      -- Has_Designator --
      --------------------

      function Has_Designator (Unnamed_Params : out Natural) return Boolean
      is
         use type Langkit_Support.Slocs.Source_Location;
      begin
         Unnamed_Params := 0;

         for Param of Parameters loop
            if Param.Is_Named then
               return True;
            elsif Langkit_Support.Slocs.Start_Sloc (Param.Loc) < Sloc then
               --  Prevent adding fake node because of LAL recovery
               Unnamed_Params := Unnamed_Params + 1;
            end if;
         end loop;

         return False;
      end Has_Designator;

      -----------------------
      -- Generate_Snippets --
      -----------------------

      procedure Generate_Snippets
        (Spec_Designators  : Laltools.Common.Node_Vectors.Vector;
         Param_Types       : Param_To_Type_Maps.Map;
         Decl              : Basic_Decl;
         Title             : VSS.Strings.Virtual_String;
         Snippet_Prefix    : VSS.Strings.Virtual_String;
         Completion_Prefix : VSS.Strings.Virtual_String)
      is
         Params_Snippet     : VSS.Strings.Virtual_String;
         Snippet_Index      : Integer :=
           Integer (Spec_Designators.Length);
         Use_Named_Notation : constant Boolean :=
           Using_Name
           or else (Limit > 0
                    and then (Snippet_Index = 1
                              or else Snippet_Index >= Limit));

         Nb_Params          : Natural := Unnamed_Params;
         --  We already have some unnamed params and we can be prefixed by one

         Total_Params       : constant Natural :=
           Natural (Spec_Designators.Length);
         --  The maximum number of params
      begin
         if Match_Designators (Parameters, Spec_Designators) then

            for Desg of reverse Spec_Designators loop
               declare
                  Name      : constant VSS.Strings.Virtual_String :=
                    VSS.Strings.To_Virtual_String (Desg.Text);
                  Item      : LSP.Structures.CompletionItem;
                  Snippet   : VSS.Strings.Virtual_String;
                  Type_Text : constant VSS.Strings.Virtual_String :=
                    (if Param_Types.Contains (Desg)
                     then VSS.Strings.To_Virtual_String
                       (Param_Types (Desg).Node.Text)
                     else "");
                  Doc       : VSS.Strings.Virtual_String;
               begin
                  --  Check if Desg is already present
                  if not In_Parameters (Desg, Parameters) then
                     --  Add snippet for Desg if it matches the
                     --  current prefix
                     if Token_Kind in Ada_Par_Open | Ada_Comma
                       or else
                         Name.Starts_With
                           (Completion_Prefix,
                            VSS.Strings.Identifier_Caseless)
                     then
                        --  Snippet Format: "Name => "
                        Item.label := Name;
                        Item.insertTextFormat :=
                          (True, LSP.Enumerations.PlainText);
                        Item.insertText.Append (Whitespace_Prefix);
                        Item.insertText.Append (Name);
                        Item.insertText.Append (" => ");
                        Item.kind := (True, LSP.Enumerations.Field);
                        Doc := Item.insertText;

                        if Param_Types (Desg).Is_Value then
                           Item.insertText.Append (Type_Text);
                           Item.label.Append (" => ");
                           Item.label.Append (Type_Text);
                        end if;

                        Item.documentation :=
                          (Is_Set => True,
                           Value  => LSP.Structures.
                             Virtual_String_Or_MarkupContent'
                             (Is_Virtual_String => True,
                              Virtual_String    => Doc));
                        Unsorted_Res.Append (Item);

                     end if;

                     if Use_Named_Notation then
                        --  If Type_Text : "Name => ${idx:Type}"
                        --  Else: "Name => $idx"
                        Snippet.Append (Name);
                        Snippet.Append (" => ");
                        if Param_Types (Desg).Is_Value then
                           Snippet.Append (Type_Text);
                        else
                           Snippet.Append ("$");
                           if not Type_Text.Is_Empty then
                              Snippet.Append ("{");
                           end if;
                           Snippet.Append
                             (VSS.Strings.Conversions.To_Virtual_String
                                (GNATCOLL.Utils.Image
                                     (Snippet_Index, Min_Width => 1)));
                           if not Type_Text.Is_Empty then
                              Snippet.Append (":");
                              Snippet.Append (Type_Text);
                              Snippet.Append ("}");
                           end if;
                        end if;
                     else
                        --  If Type_Text : "${idx:Name : Type}"
                        --  Else: "${idx:Name}"
                        if Param_Types (Desg).Is_Value then
                           Snippet.Append (Type_Text);
                        else
                           Snippet.Append ("${");
                           Snippet.Append
                             (VSS.Strings.Conversions.To_Virtual_String
                                (GNATCOLL.Utils.Image
                                     (Snippet_Index, Min_Width => 1)));
                           Snippet.Append (":");
                           Snippet.Append (Name);
                           if not Type_Text.Is_Empty then
                              Snippet.Append (" : ");
                              Snippet.Append (Type_Text);
                           end if;
                           Snippet.Append ("}");
                        end if;
                     end if;
                     Snippet.Append (", ");
                     Params_Snippet.Prepend (Snippet);
                  end if;
                  Snippet_Index := Snippet_Index - 1;
               end;

               Nb_Params := Nb_Params + 1;
               exit when Nb_Params = Total_Params;
            end loop;

            --  If the string is empty => nothing to do
            if not Params_Snippet.Is_Empty
              and then Token_Kind in Ada_Par_Open | Ada_Comma
            then
               declare
                  Last    : VSS.Strings.Character_Iterators.Character_Iterator
                    := Params_Snippet.At_Last_Character;
                  Success : Boolean with Unreferenced;

               begin
                  --  Remove the last 2 characters which are ", " and
                  --  replace it by ")" and the final tab stop
                  Success := Last.Backward;
                  Success := Last.Backward;

                  Params_Snippet :=
                    Params_Snippet.Slice
                      (Params_Snippet.At_First_Character, Last);
                  Params_Snippet.Append (")$0");
               end;

               Params_Snippet.Prepend (Snippet_Prefix);

               declare
                  Item : LSP.Structures.CompletionItem;
               begin
                  Item.label := Title;
                  Item.insertTextFormat := (True, LSP.Enumerations.Snippet);
                  Item.insertText.Append (Whitespace_Prefix);
                  Item.insertText.Append (Params_Snippet);
                  Item.kind := (True, LSP.Enumerations.Snippet);
                  LSP.Ada_Documents.Set_Completion_Item_Documentation
                    (Context                 => Self.Context.all,
                     BD                      => Decl,
                     Item                    => Item,
                     Compute_Doc_And_Details =>
                       Self.Compute_Doc_And_Details);
                  Pretty_Print_Snippet
                    (Context => Self.Context.all,
                     Prefix  =>
                       VSS.Strings.Conversions.To_UTF_8_String (Prefix),
                     --  "column = offset - 1"
                     Offset  => Integer (Column) - 1,
                     Span    => Prefix_Span,
                     Rule    => Pretty_Print_Rule,
                     Result  => Item);
                  Unsorted_Res.Append (Item);
               end;
            end if;
         end if;
      end Generate_Snippets;

   begin
      if Elem_Node = Null_Element then
         return;
      end if;

      Prefix_Span :=
        Self.Document.To_LSP_Range
          (Langkit_Support.Slocs.Make_Range
             (Langkit_Support.Slocs.Start_Sloc
                (Get_Prefix_Node (Elem_Node, Column => Column).Sloc_Range),
              Sloc));
      Prefix := Self.Document.Get_Text_At
        (Prefix_Span.start, Prefix_Span.an_end);

      Parameters := Get_Parameters (Elem_Node, Prefixed);
      Using_Name := Has_Designator (Unnamed_Params);

      if Token_Kind = Ada_Whitespace then
         Token_Kind := Kind (Data (Previous (Token, Exclude_Trivia => True)));

      elsif Token_Kind = Ada_Comma then
         Whitespace_Prefix.Append (" ");
      end if;

      declare
         Completion_Prefix : constant VSS.Strings.Virtual_String :=
           VSS.Strings.To_Virtual_String (Node.Text);
      begin
         for Spec of Get_Spec_Designators
           (E             => Elem_Node,
            Context       => Self.Context,
            For_Signature => False)
         loop
            --  Too many params to match Spec
            if Natural (Spec.Param_Vector.Length)
              > Natural (Parameters.Length)
            then
               Generate_Snippets
                 (Spec_Designators  => Spec.Param_Vector,
                  Param_Types       => Spec.Param_Types,
                  Decl              => Spec.Decl,
                  Title             => Spec.Title,
                  Snippet_Prefix    => Spec.Prefix,
                  Completion_Prefix => Completion_Prefix);
            end if;
         end loop;
      end;
   end Propose_Completion;

   ------------------------
   -- Propose_Signatures --
   ------------------------

   procedure Propose_Signatures
     (Context         : not null LSP.Ada_Context_Sets.Context_Access;
      Node            : Libadalang.Analysis.Ada_Node;
      Cursor          : Langkit_Support.Slocs.Source_Location;
      Prev_Signatures : LSP.Structures.SignatureHelpContext_Optional;
      Res             : in out LSP.Structures.SignatureHelp;
      Lazy            : Boolean := False)
   is
      use LSP.Ada_Completions.Generic_Assoc_Utils;

      Elem_Node        : constant Element := Search_Element (Node);

      Parameters       :
        LSP.Ada_Completions.Generic_Assoc_Utils.Param_Vectors.Vector;
      --  Current list of designators

      Prefixed         : Boolean;
      --  Are we prefixed by a parameter? (for example: dot call)

      Prev_Active      : Integer :=
        (if Prev_Signatures.Is_Set
         and then Prev_Signatures.Value.activeSignatureHelp.Is_Set
         and then Prev_Signatures.Value.activeSignatureHelp.
           Value.activeSignature.Is_Set
         then Prev_Signatures.Value.activeSignatureHelp.
           Value.activeSignature.Value
         else 0);

      Cursor_Position    : Integer := 0;
      Current_Designator : Libadalang.Analysis.Ada_Node :=
        Libadalang.Analysis.No_Ada_Node;

      Signature_Added    : Boolean := False;

      procedure Add_Signature (Spec : Assoc_Data);

      procedure Filter_Previous_Signatures
        (Signatures : LSP.Structures.SignatureHelp);

      -------------------
      -- Add_Signature --
      -------------------

      procedure Add_Signature (Spec : Assoc_Data) is
         Signature : LSP.Structures.SignatureInformation :=
           (label          => <>,
            activeParameter =>
              (Is_Set => True,
               Value  =>
                 Find_Designator_Position
                   (Designator       => Current_Designator,
                    Spec_Designators => Spec.Param_Vector,
                    Cursor_Position  => Cursor_Position)),
            others          => <>
           );
         Declaration_Text   : VSS.Strings.Virtual_String;
         Qualifier_Text     : VSS.Strings.Virtual_String;
         Location_Text      : VSS.Strings.Virtual_String;
         Documentation_Text : VSS.Strings.Virtual_String;
         Aspects_Text       : VSS.Strings.Virtual_String;

      begin
         LSP.Ada_Documentation.Get_Tooltip_Text
           (BD                 => Spec.Decl,
            Style              => Context.Get_Documentation_Style,
            Declaration_Text   => Declaration_Text,
            Qualifier_Text     => Qualifier_Text,
            Location_Text      => Location_Text,
            Documentation_Text => Documentation_Text,
            Aspects_Text       => Aspects_Text);

         Signature.label := Declaration_Text;
         Signature.documentation :=
           (Is_Set => True,
            Value  =>
              (Is_Virtual_String => True,
               Virtual_String    => Documentation_Text));

         for Param of Spec.Param_Vector loop
            declare
               P : constant LSP.Structures.ParameterInformation :=
                 (label         =>
                    (Is_Virtual_String => True,
                     Virtual_String    =>
                       VSS.Strings.To_Virtual_String (Param.Text)),
                  documentation =>
                    (Is_Set => False)
                 );
            begin
               Signature.parameters.Append (P);
            end;
         end loop;
         Res.signatures.Prepend (Signature);
      end Add_Signature;

      --------------------------------
      -- Filter_Previous_Signatures --
      --------------------------------

      procedure Filter_Previous_Signatures
        (Signatures : LSP.Structures.SignatureHelp) is
      begin
         --  Search for the current designator and the active position
         declare
            Active_Position : Integer := -1;
            Index           : Integer := 0;
         begin
            for S of Signatures.signatures loop
               if Is_Signature_Active
                 (Parameters      => S.parameters,
                  Sig_Label       => S.label,
                  Cursor_Position => Cursor_Position,
                  Designator      => Current_Designator,
                  Active_Position => Active_Position)
               then
                  declare
                     Signature : LSP.Structures.SignatureInformation := S;
                  begin
                     Signature.activeParameter :=
                       (Is_Set => True,
                        Value  => Active_Position);
                     Res.signatures.Append (Signature);
                  end;
               elsif Index = Prev_Active then
                  Prev_Active := 0;
               end if;
               Index := Index + 1;
            end loop;
         end;
      end Filter_Previous_Signatures;

   begin
      if Elem_Node = Null_Element then
         return;
      end if;

      Res.signatures.Clear;
      Res.activeParameter := (Is_Set => True, Value => 0);
      Parameters := Get_Parameters (Elem_Node, Prefixed);

      Find_Cursor_Position
        (Elem_Node          => To_Node (Elem_Node),
         Cursor             => Cursor,
         Prefixed           => Prefixed,
         Parameters         => Parameters,
         Cursor_Position    => Cursor_Position,
         Current_Designator => Current_Designator);

      if Me_Debug.Active then
         GNATCOLL.Traces.Trace
           (Me_Debug, "Cursor: " & Cursor'Image);
         GNATCOLL.Traces.Trace
           (Me_Debug, "Designators:" & Parameters.Length'Image);
         for Param of Parameters loop
            if not Param.Is_Named then
               GNATCOLL.Traces.Trace (Me_Debug, "No name: " & Param.Loc'Image);
            else
               GNATCOLL.Traces.Trace (Me_Debug, Param.Node.Parent.Image);
            end if;
         end loop;
      end if;

      if Prev_Signatures.Is_Set then
         --  Refilter the previous signatures, some can be invalid now
         Filter_Previous_Signatures
           (Prev_Signatures.Value.activeSignatureHelp.Value);
      else
         for Spec of Get_Spec_Designators
           (E             => Elem_Node,
            Context       => Context,
            For_Signature => True)
         loop
            if
              --  Enough params in Spec
              Natural (Parameters.Length)
              <= Natural (Spec.Param_Vector.Length)
              --  Cursor can't point to Length (Spec), it starts at 0
              and then Cursor_Position /= -1
              and then Cursor_Position <
                Integer (Spec.Param_Vector.Length)
              --  The designators matched
              and then Match_Designators (Parameters, Spec.Param_Vector)
            then
               if Signature_Added and then Lazy then
                  --  One signature is enough in this case, they are just
                  --  redundant => remove the parameter highlighting
                  --  (too many possibilities)
                  declare
                     Sign : LSP.Structures.SignatureInformation :=
                       Res.signatures.Last_Element;
                  begin
                     Sign.activeParameter := (Is_Set => False);
                     Res.activeParameter := (Is_Set => False);
                     Res.signatures.Delete_Last;
                     Res.signatures.Prepend (Sign);
                     return;
                  end;
               end if;
               Add_Signature (Spec);
               Signature_Added := True;
            end if;
         end loop;
      end if;

      --  Another generic_assoc can already set the active signature then
      --  keep it
      if not Res.activeSignature.Is_Set
        or else (Res.activeSignature.Value = 0
                 and then Prev_Active /= 0)
      then
         Res.activeSignature := (Is_Set => True, Value => Prev_Active);
      end if;
   end Propose_Signatures;

   -------------------------
   -- Is_Signature_Active --
   -------------------------

   function Is_Signature_Active
     (Parameters      : LSP.Structures.ParameterInformation_Vector;
      Sig_Label       : VSS.Strings.Virtual_String;
      Cursor_Position : Integer;
      Designator      : Libadalang.Analysis.Ada_Node;
      Active_Position : out Integer)
      return Boolean
   is
      use Libadalang.Analysis;
      use type VSS.Strings.Virtual_String;
   begin
      Active_Position := 0;
      if Cursor_Position = -1 then
         return False;
      end if;

      if Designator = Libadalang.Analysis.No_Ada_Node then
         --  Check if Position is valid in Parameters (Note: Position starts
         --  at 0)
         Active_Position := Cursor_Position;
         return Cursor_Position < Integer (Parameters.Length);
      else
         declare
            Name : constant VSS.Strings.Virtual_String :=
              VSS.Strings.To_Virtual_String (Designator.Text);

         begin
            for Param of Parameters loop
               declare
                  use type VSS.Unicode.UTF16_Code_Unit_Offset;

                  First   :
                  VSS.Strings.Character_Iterators.Character_Iterator
                    := Sig_Label.At_First_Character;
                  Last    :
                  VSS.Strings.Character_Iterators.Character_Iterator
                    := Sig_Label.At_First_Character;
                  Success : Boolean with Unreferenced;

               begin
                  if Param.label.Is_Virtual_String then
                     if Param.label.Virtual_String = Name then
                        return True;
                     end if;
                  else
                     --  The code below check that:
                     --  Sig_Label [label.From .. label.Till - 1] = Name

                     while First.First_UTF16_Offset < VSS.Unicode.
                       UTF16_Code_Unit_Index (Param.label.Natural_Tuple (1))
                       and then First.Forward
                     loop
                        null;
                     end loop;

                     while Last.First_UTF16_Offset < VSS.Unicode.
                       UTF16_Code_Unit_Index (Param.label.Natural_Tuple (2))
                       and then Last.Forward
                     loop
                        null;
                     end loop;

                     --  label.Till is exclusive offset so move backward once
                     Success := Last.Backward;

                     if Sig_Label.Slice (First, Last) = Name then
                        return True;
                     end if;
                  end if;

                  Active_Position := Active_Position + 1;
               end;
            end loop;
         end;
      end if;

      return False;
   end Is_Signature_Active;

   --------------------------
   -- Find_Cursor_Position --
   --------------------------

   procedure Find_Cursor_Position
     (Elem_Node          : Libadalang.Analysis.Ada_Node'Class;
      Cursor             : Langkit_Support.Slocs.Source_Location;
      Prefixed           : Boolean;
      Parameters         :
        LSP.Ada_Completions.Generic_Assoc_Utils.Param_Vectors.Vector;
      Cursor_Position    : out Integer;
      Current_Designator : out Libadalang.Analysis.Ada_Node)
   is
      use Langkit_Support.Slocs;

      Is_New_Param  : Boolean  := False;

      function Cursor_On_Last_Par return Boolean;
      --  Return True when the cursor is located on the last ')'

      function Cursor_In_Node
        (N : Libadalang.Analysis.Ada_Node'Class) return Boolean;
      --  Check if N contains the cursor

      ------------------------
      -- Cursor_On_Last_Par --
      ------------------------

      function Cursor_On_Last_Par return Boolean is
         Open_Cpt  : Natural := 0;
         Close_Cpt : Natural := 0;
      begin
         for C of Elem_Node.Text loop
            --  Count the open/closing parentheses
            if C = '(' then
               Open_Cpt := Open_Cpt + 1;
            elsif C = ')' then
               Close_Cpt := Close_Cpt + 1;
            end if;

            --  Monitor if we are finishing by ", *"
            if C = ',' then
               Is_New_Param := True;
            elsif C = ' ' then
               null;
            else
               Is_New_Param := False;
            end if;
         end loop;

         return Langkit_Support.Slocs.End_Sloc (Elem_Node.Sloc_Range) = Cursor
           and then Open_Cpt = Close_Cpt;
      end Cursor_On_Last_Par;

      --------------------
      -- Cursor_In_Node --
      --------------------

      function Cursor_In_Node
        (N : Libadalang.Analysis.Ada_Node'Class) return Boolean is
      begin
         case Libadalang.Analysis.Compare (N, Cursor) is
            when Inside =>
               return True;
            when Before =>
               --  Case to handle:
               --      Foo (1, |
               --      Bar (1, 2);
               --  LAL error recovery will assume that Bar (1, 2); is the
               --  second param of Foo. At this point we are in the Assoc_List
               --  and we need to stop going through the list of parameters.
               return True;
            when After =>
               --  Case to handle: Foo (A => 1|
               return Cursor.Line = N.Sloc_Range.End_Line
                 and then Cursor.Column = N.Sloc_Range.End_Column;
         end case;
      end Cursor_In_Node;

   begin
      Current_Designator := Libadalang.Analysis.No_Ada_Node;
      Cursor_Position := -1;

      if Cursor_On_Last_Par then
         return;
      end if;

      Cursor_Position := 0;

      for Param of Parameters loop
         if Param.Is_Named then
            declare
               Parent : Libadalang.Analysis.Ada_Node := Param.Node.Parent;
            begin
               if Parent.Kind in Libadalang.Common.Ada_Alternatives_List_Range
               then
                  --  Special case for aggregate:
                  --  Aggr_Assoc : X => Y
                  --     AlternativesList : X
                  --        Identifier : X
                  Parent := Parent.Parent;
               end if;

               if Cursor_In_Node (Parent) then
                  Current_Designator := Param.Node;
                  exit;
               end if;
            end;
         else
            if Compare (Param.Loc, Cursor) = Inside then
               --  The cursor is inside an unnamed param
               Current_Designator := Libadalang.Analysis.No_Ada_Node;
               exit;
            end if;
         end if;

         Cursor_Position := Cursor_Position + 1;
      end loop;

      --  New param is only considered if we are using all the previous params
      if Integer (Parameters.Length) = Cursor_Position
        and then Is_New_Param
      then
         Cursor_Position := Cursor_Position + 1;
      end if;

      --  Cursor_Positon starts at 0
      if Cursor_Position > 0 then
         Cursor_Position := Cursor_Position - 1;
      end if;

      if Prefixed then
         Cursor_Position := Cursor_Position + 1;
      end if;

   end Find_Cursor_Position;

   ------------------------------
   -- Find_Designator_Position --
   ------------------------------

   function Find_Designator_Position
     (Designator       : Libadalang.Analysis.Ada_Node;
      Spec_Designators : Laltools.Common.Node_Vectors.Vector;
      Cursor_Position  : Integer)
      return Integer
   is
      use type Libadalang.Analysis.Ada_Node;
      Index : Integer := 0;
   begin
      if Designator = Libadalang.Analysis.No_Ada_Node then
         if Cursor_Position > 0 then
            return Cursor_Position;
         else
            return 0;
         end if;
      end if;

      for D of Spec_Designators loop
         if Designator.Text = D.Text then
            return Index;
         end if;
         Index := Index + 1;
      end loop;

      return Index;
   end Find_Designator_Position;

end LSP.Ada_Completions.Generic_Assoc;

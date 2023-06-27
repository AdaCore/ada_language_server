------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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

with VSS.Characters;
with VSS.Strings.Character_Iterators;
with VSS.String_Vectors;

with Langkit_Support.Text;
with Libadalang.Common;

with GNATdoc.Comments.Helpers;

with LSP.Common;
with LSP.Lal_Utils;

package body LSP.Ada_Documentation is

   use all type Libadalang.Common.Ada_Node_Kind_Type;
   use Libadalang.Analysis;
   use Libadalang.Common;

   function Get_Decl_Text
     (BD : Libadalang.Analysis.Basic_Decl) return VSS.Strings.Virtual_String;
   --  Return the code associated with the given declaration, in a formatted
   --  way.

   function Get_Hover_Text_For_Node
     (Node : Libadalang.Analysis.Ada_Node'Class)
      return VSS.String_Vectors.Virtual_String_Vector;
   --  Return a pretty printed version of the node's text to be
   --  displayed on hover requests, removing unnecessary indentation
   --  whitespaces if needed and attaching extra information in some cases.

   procedure Append_To_Last_Line
     (Lines : in out VSS.String_Vectors.Virtual_String_Vector;
      Item  : VSS.Characters.Virtual_Character);
   --  Append given Item to the last line of the vector. Append new line when
   --  vector is empty.

   procedure Append_To_Last_Line
     (Lines : in out VSS.String_Vectors.Virtual_String_Vector;
      Item  : VSS.Strings.Virtual_String);
   --  Append given Item to the last line of the vector. Append new line when
   --  vector is empty.

   function Get_Hover_Text
     (Decl : Libadalang.Analysis.Basic_Decl'Class)
      return VSS.Strings.Virtual_String;
   --  Return a pretty printed version of the declaration's text to be
   --  displayed on hover requests, removing unnecessary indentation
   --  whitespaces if needed and attaching extra information in some cases.

   -------------------------
   -- Append_To_Last_Line --
   -------------------------

   procedure Append_To_Last_Line
     (Lines : in out VSS.String_Vectors.Virtual_String_Vector;
      Item  : VSS.Characters.Virtual_Character)
   is
      Line : VSS.Strings.Virtual_String :=
        (if Lines.Is_Empty
           then VSS.Strings.Empty_Virtual_String
           else Lines.Element (Lines.Length));

   begin
      Line.Append (Item);

      if Lines.Is_Empty then
         Lines.Append (Line);

      else
         Lines.Replace (Lines.Length, Line);
      end if;
   end Append_To_Last_Line;

   -------------------------
   -- Append_To_Last_Line --
   -------------------------

   procedure Append_To_Last_Line
     (Lines : in out VSS.String_Vectors.Virtual_String_Vector;
      Item  : VSS.Strings.Virtual_String)
   is
      Line : VSS.Strings.Virtual_String :=
        (if Lines.Is_Empty
           then VSS.Strings.Empty_Virtual_String
           else Lines.Element (Lines.Length));

   begin
      Line.Append (Item);

      if Lines.Is_Empty then
         Lines.Append (Line);

      else
         Lines.Replace (Lines.Length, Line);
      end if;
   end Append_To_Last_Line;

   Document_LSP_New_Line_Function : constant VSS.Strings.Line_Terminator :=
     VSS.Strings.LF;
   --  Line terminator to be used to generate replies. It is fixed to LF now.

   -------------------
   -- Get_Decl_Text --
   -------------------

   function Get_Decl_Text
     (BD : Libadalang.Analysis.Basic_Decl) return VSS.Strings.Virtual_String
   is
      Result : VSS.Strings.Virtual_String;
   begin
      --  If the basic declaration is an enum literal, display the whole
      --  enumeration type declaration instead.
      if BD.Kind in Ada_Enum_Literal_Decl then
         Result := Get_Hover_Text
           (BD.As_Enum_Literal_Decl.P_Enum_Type.As_Basic_Decl);
      else
         Result := Get_Hover_Text (BD);
      end if;

      return Result;
   end Get_Decl_Text;

   --------------------
   -- Get_Hover_Text --
   --------------------

   function Get_Hover_Text
     (Decl : Libadalang.Analysis.Basic_Decl'Class)
      return VSS.Strings.Virtual_String
   is
      Decl_Text      : VSS.String_Vectors.Virtual_String_Vector;
      Subp_Spec_Node : Libadalang.Analysis.Base_Subp_Spec;

   begin
      --  Try to retrieve the subprogram spec node, if any : if it's a
      --  subprogram node that does not have any separate declaration we
      --  only want to display its specification, not the body.
      Subp_Spec_Node := Decl.P_Subp_Spec_Or_Null;

      if Subp_Spec_Node /= No_Base_Subp_Spec then
         Decl_Text := Get_Hover_Text_For_Node (Subp_Spec_Node);

         --  Append the aspects to the declaration text, if any.
         declare
            Aspects      : constant Libadalang.Analysis.Aspect_Spec :=
              Decl.F_Aspects;
            Aspects_Text : VSS.String_Vectors.Virtual_String_Vector;

         begin
            if not Aspects.Is_Null then
               for Aspect of Aspects.F_Aspect_Assocs loop
                  if not Aspects_Text.Is_Empty then
                     --  need to add "," for the highlighting

                     Append_To_Last_Line (Aspects_Text, ',');
                  end if;

                  Aspects_Text.Append (Get_Hover_Text_For_Node (Aspect));
               end loop;

               if not Aspects_Text.Is_Empty then
                  Decl_Text.Append ("with");
                  Decl_Text.Append (Aspects_Text);
               end if;
            end if;
         end;

      else
         Decl_Text := Get_Hover_Text_For_Node (Decl);
      end if;

      return Decl_Text.Join_Lines (Document_LSP_New_Line_Function, False);
   end Get_Hover_Text;

   -----------------------------
   -- Get_Hover_Text_For_Node --
   -----------------------------

   function Get_Hover_Text_For_Node
     (Node : Libadalang.Analysis.Ada_Node'Class)
      return VSS.String_Vectors.Virtual_String_Vector
   is
      Result : VSS.String_Vectors.Virtual_String_Vector;

      function Is_Space
        (Char : VSS.Characters.Virtual_Character) return Boolean;
      --  Check if given character is a whitespace

      function Get_Indent
        (Line : VSS.Strings.Virtual_String) return Natural;
      --  Count number of starting spaces

      function Tail_From
        (Line : VSS.Strings.Virtual_String;
         Skip : Natural) return VSS.Strings.Virtual_String;
      --  Return slice of Line from given index to the end of Line

      procedure Get_Basic_Decl_Hover_Text;
      --  Create the hover text for for basic declarations

      procedure Get_Subp_Spec_Hover_Text;
      --  Create the hover text for subprogram declarations

      procedure Get_Package_Decl_Hover_Text;
      --  Create the hover text  for package declarations

      procedure Get_Loop_Var_Hover_Text;
      --  Create the hover text for loop variable declarations

      procedure Get_Aspect_Hover_Text;
      --  Create the hover text for aspect statement

      -------------------------------
      -- Get_Basic_Decl_Hover_Text --
      -------------------------------

      procedure Get_Basic_Decl_Hover_Text is
         Text   : constant VSS.Strings.Virtual_String :=
           LSP.Lal_Utils.To_Virtual_String (Node.Text);

         Lines  : constant VSS.String_Vectors.Virtual_String_Vector :=
           Text.Split_Lines;

      begin
         case Node.Kind is
            when Ada_Package_Body =>

               --  This means that user is hovering on the package declaration
               --  itself: in this case, return a empty response since all the
               --  relevant information is already visible to the user.
               return;

            when others =>
               --  Return an empty hover text if there is no text for this
               --  delclaration (only for safety).
               if Text.Is_Empty then
                  return;
               end if;

               --  If it's a single-line declaration, replace all the
               --  series of whitespaces by only one blankspace. If it's
               --  a multi-line declaration, remove only the unneeded
               --  indentation whitespaces.

               if Lines.Length = 1 then
                  declare
                     Char  : VSS.Characters.Virtual_Character;
                     Line  : constant VSS.Strings.Virtual_String := Lines (1);
                     Value : VSS.Strings.Virtual_String;

                     Skip_Space : Boolean := True;

                     J : VSS.Strings.Character_Iterators.Character_Iterator :=
                       Line.Before_First_Character;

                  begin
                     while J.Forward loop
                        Char := J.Element;

                        if not Is_Space (Char) then
                           Skip_Space := False;
                           Value.Append (Char);
                        elsif not Skip_Space then
                           Skip_Space := True;
                           Value.Append (' ');
                        end if;
                     end loop;

                     Result.Append (Value);
                  end;

               else
                  declare
                     Indent : Natural := Natural'Last;
                  begin
                     Result.Append (Lines (1));

                     --  Count the blankspaces per line and track how many
                     --  blankspaces we should remove on each line by
                     --  finding the common indentation blankspaces.

                     for J in 2 .. Lines.Length loop
                        Indent := Natural'Min (Indent, Get_Indent (Lines (J)));
                     end loop;

                     for J in 2 .. Lines.Length loop
                        declare
                           Line : constant VSS.Strings.Virtual_String :=
                             Lines (J);

                           Value : constant VSS.Strings.Virtual_String :=
                             Tail_From (Line, Indent);
                        begin
                           Result.Append (Value);
                        end;
                     end loop;
                  end;
               end if;
         end case;
      end Get_Basic_Decl_Hover_Text;

      ----------------
      -- Get_Indent --
      ----------------

      function Get_Indent
        (Line : VSS.Strings.Virtual_String) return Natural
      is
         Result : Natural := 0;
         J      : VSS.Strings.Character_Iterators.Character_Iterator :=
           Line.Before_First_Character;

      begin
         while J.Forward and then Is_Space (J.Element) loop
            Result := Result + 1;
         end loop;

         return Result;
      end Get_Indent;

      ------------------------------
      -- Get_Subp_Spec_Hover_Text --
      ------------------------------

      procedure Get_Subp_Spec_Hover_Text is
      begin
         declare
            Text  : constant VSS.Strings.Virtual_String :=
              LSP.Lal_Utils.To_Virtual_String (Node.Text);
            Lines : constant VSS.String_Vectors.Virtual_String_Vector :=
              Text.Split_Lines (LSP.Common.LSP_New_Line_Function_Set);

         begin
            --  For single-line subprogram specifications, we display the
            --  associated text directly.
            --  For multi-line ones, remove the identation blankspaces to
            --  replace them by a fixed number of blankspaces.

            if Lines.Length = 1 then
               Result.Append (Text);

            else
               Result.Append (Lines (1));

               for J in 2 .. Lines.Length loop
                  declare
                     Line   : VSS.Strings.Virtual_String := Lines (J);
                     Indent : constant Natural := Get_Indent (Line);
                  begin
                     if not Line.Is_Empty then
                        Line := Tail_From (Line, Indent);

                        if Line.Starts_With ("(") then
                           Line.Prepend ("  ");
                        else
                           Line.Prepend ("   ");
                        end if;

                        Result.Append (Line);
                     end if;
                  end;
               end loop;
            end if;
         end;

         --  Append "is abstract" to the resulting hover text if the subprogram
         --  specificiation node belongs to an abstract subprogram declaration.
         if not Node.Parent.Is_Null
           and then Node.Parent.Kind in Ada_Abstract_Subp_Decl_Range
         then
            Append_To_Last_Line (Result, " is abstract");
         end if;

         --  Append "is null" to the resulting hover text if the subprogram
         --  specificiation node belongs to an null subprogram declaration.
         if not Node.Parent.Is_Null
           and then Node.Parent.Kind in Ada_Null_Subp_Decl_Range
         then
            Append_To_Last_Line (Result, " is null");
         end if;
      end Get_Subp_Spec_Hover_Text;

      ---------------------------------
      -- Get_Package_Decl_Hover_Text --
      ---------------------------------

      procedure Get_Package_Decl_Hover_Text is
         Text   : VSS.Strings.Virtual_String;
         Decl   : constant Base_Package_Decl := Node.As_Base_Package_Decl;
         Aspect : constant Aspect_Spec := Decl.F_Aspects;
         Name   : constant Defining_Name := Decl.F_Package_Name;
         To     : Token_Reference := --  token before `IS`
           (if not Aspect.Is_Null then
              Aspect.Token_End
            elsif not Name.Is_Null then
              Name.Token_End
            else  --  just-in-case fallback
              Decl.Token_End);
      begin
         --  Return the first line of the package declaration and its
         --  generic parameters if any.
         To := Next (To, Exclude_Trivia => True);   --  Jump to IS
         To := Previous (To, Exclude_Trivia => False);  --  Jump before IS

         if Node.Parent.Kind in Ada_Generic_Decl then
            Text := LSP.Lal_Utils.To_Virtual_String
              (Node.Parent.As_Generic_Decl.F_Formal_Part.Text);

            Result.Append (Text);
         end if;

         Text := LSP.Lal_Utils.To_Virtual_String
           (Libadalang.Common.Text (Node.Token_Start, To));

         Result.Append (Text);
      end Get_Package_Decl_Hover_Text;

      -----------------------------
      -- Get_Loop_Var_Hover_Text --
      -----------------------------

      procedure Get_Loop_Var_Hover_Text is
         Parent_Text : constant Langkit_Support.Text.Text_Type :=
           Node.Parent.Text;

      begin
         Result.Append (LSP.Lal_Utils.To_Virtual_String (Parent_Text));
      end Get_Loop_Var_Hover_Text;

      ---------------------------
      -- Get_Aspect_Hover_Text --
      ---------------------------

      procedure Get_Aspect_Hover_Text is
         Text   : constant VSS.Strings.Virtual_String :=
           LSP.Lal_Utils.To_Virtual_String (Node.Text);

         Lines  : constant VSS.String_Vectors.Virtual_String_Vector :=
           Text.Split_Lines;

         --  Get the indentation for the first line
         Indentation : Integer := Get_Indent (Lines (1));
         Idx         : Integer;
         Line        : VSS.Strings.Virtual_String;
      begin
         Line := Tail_From (Lines (1), Indentation);
         --  Force an indentation of 2 for the first line
         Line.Prepend ("  ");
         Result.Append (Line);

         --  The next line should have one more indentation level
         Indentation := Indentation + 3;

         for J in 2 .. Lines.Length loop
            Line := Lines (J);
            Idx := Get_Indent (Line);

            if Indentation > Idx then
               --  Uncommon indentation: just print the line
               Result.Append (Line);
            else
               --  Remove the uneeded indentation
               Result.Append (Tail_From (Line, Indentation));
            end if;
         end loop;
      end Get_Aspect_Hover_Text;

      --------------
      -- Is_Space --
      --------------

      function Is_Space
        (Char : VSS.Characters.Virtual_Character) return Boolean is
      begin
         return VSS.Characters.Get_General_Category (Char) in
           VSS.Characters.Space_Separator;
      end Is_Space;

      ---------------
      -- Tail_From --
      ---------------

      function Tail_From
        (Line : VSS.Strings.Virtual_String;
         Skip : Natural) return VSS.Strings.Virtual_String
      is
         From : VSS.Strings.Character_Iterators.
           Character_Iterator := Line.At_First_Character;

         To   : constant VSS.Strings.Character_Iterators.
           Character_Iterator := Line.At_Last_Character;

      begin
         for J in 1 .. Skip loop
            exit when not From.Forward;
         end loop;

         return Line.Slice (From, To);
      end Tail_From;

   begin

      case Node.Kind is
         when Ada_Package_Body =>

            --  This means that the user is hovering on the package declaration
            --  itself: in this case, return a empty response since all the
            --  relevant information is already visible to the user.
            null;

         when Ada_Base_Package_Decl =>
            Get_Package_Decl_Hover_Text;

         when Ada_For_Loop_Var_Decl =>
            Get_Loop_Var_Hover_Text;

         when Ada_Base_Subp_Spec =>
            Get_Subp_Spec_Hover_Text;

         when Ada_Aspect_Assoc =>
            Get_Aspect_Hover_Text;

         when others =>
            Get_Basic_Decl_Hover_Text;
      end case;

      return Result;
   end Get_Hover_Text_For_Node;

   ----------------------
   -- Get_Tooltip_Text --
   ----------------------

   procedure Get_Tooltip_Text
     (BD        : Libadalang.Analysis.Basic_Decl;
      Trace     : GNATCOLL.Traces.Trace_Handle;
      Style     : GNATdoc.Comments.Options.Documentation_Style;
      Loc_Text  : out VSS.Strings.Virtual_String;
      Doc_Text  : out VSS.Strings.Virtual_String;
      Decl_Text : out VSS.Strings.Virtual_String)
   is
      pragma Unreferenced (Trace);

      Options    : constant
        GNATdoc.Comments.Options.Extractor_Options :=
          (Style    => Style,
           Pattern  => <>,
           Fallback => True);
      Decl_Lines : VSS.String_Vectors.Virtual_String_Vector;
      Doc_Lines  : VSS.String_Vectors.Virtual_String_Vector;

   begin
      --  Extract documentation with GNATdoc when supported.

      GNATdoc.Comments.Helpers.Get_Plain_Text_Documentation
        (Name          => BD.P_Defining_Name,
         Options       => Options,
         Code_Snippet  => Decl_Lines,
         Documentation => Doc_Lines);

      Decl_Text := Decl_Lines.Join_Lines (VSS.Strings.LF, False);
      Doc_Text := Doc_Lines.Join_Lines (VSS.Strings.LF, False);

      --  If GNATdoc failed to compute the declaration text, use the old engine
      if Decl_Text.Is_Empty
        or else (BD.Kind not in Ada_Enum_Literal_Decl
                   and not BD.P_Subp_Spec_Or_Null.Is_Null)
      then
         --  For subprograms additional information is added, use old code to
         --  obtain it yet.
         Decl_Text := Get_Decl_Text (BD);
      end if;

      Loc_Text := LSP.Lal_Utils.Node_Location_Image (BD);
   end Get_Tooltip_Text;

end LSP.Ada_Documentation;

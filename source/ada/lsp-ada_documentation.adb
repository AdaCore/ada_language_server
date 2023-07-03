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

with LSP.Lal_Utils;

package body LSP.Ada_Documentation is

   use all type Libadalang.Common.Ada_Node_Kind_Type;
   use Libadalang.Analysis;
   use Libadalang.Common;

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

   Document_LSP_New_Line_Function : constant VSS.Strings.Line_Terminator :=
     VSS.Strings.LF;
   --  Line terminator to be used to generate replies. It is fixed to LF now.

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

      procedure Get_Loop_Var_Hover_Text;
      --  Create the hover text for loop variable declarations

      procedure Get_Aspect_Hover_Text;
      --  Create the hover text for aspect statement

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
         when Ada_For_Loop_Var_Decl =>
            Get_Loop_Var_Hover_Text;

         when Ada_Aspect_Assoc =>
            Get_Aspect_Hover_Text;

         when others =>
            null;
      end case;

      return Result;
   end Get_Hover_Text_For_Node;

   ----------------------
   -- Get_Tooltip_Text --
   ----------------------

   procedure Get_Tooltip_Text
     (BD                 : Libadalang.Analysis.Basic_Decl;
      Style              : GNATdoc.Comments.Options.Documentation_Style;
      Declaration_Text   : out VSS.Strings.Virtual_String;
      Qualifier_Text     : out VSS.Strings.Virtual_String;
      Location_Text      : out VSS.Strings.Virtual_String;
      Documentation_Text : out VSS.Strings.Virtual_String;
      Aspects_Text       : out VSS.Strings.Virtual_String)
   is
      Options       : constant
        GNATdoc.Comments.Options.Extractor_Options :=
          (Style    => Style,
           Pattern  => <>,
           Fallback => True);
      Decl_Lines    : VSS.String_Vectors.Virtual_String_Vector;
      Doc_Lines     : VSS.String_Vectors.Virtual_String_Vector;
      Aspects_Lines : VSS.String_Vectors.Virtual_String_Vector;

   begin
      Qualifier_Text.Clear;

      --  Extract documentation with GNATdoc when supported.

      GNATdoc.Comments.Helpers.Get_Plain_Text_Documentation
        (Name          => BD.P_Defining_Name,
         Options       => Options,
         Code_Snippet  => Decl_Lines,
         Documentation => Doc_Lines);

      Declaration_Text   := Decl_Lines.Join_Lines (VSS.Strings.LF, False);
      Documentation_Text := Doc_Lines.Join_Lines (VSS.Strings.LF, False);

      --  If GNATdoc failed to compute the declaration text, use the old engine
      --  Only known case now is loop variable of the 'for' loop.

      if Declaration_Text.Is_Empty then
         Declaration_Text :=
           Get_Hover_Text_For_Node (BD).Join_Lines
             (Document_LSP_New_Line_Function, False);
      end if;

      Location_Text := LSP.Lal_Utils.Node_Location_Image (BD);

      --  For subprograms, do additional analysis and construct qualifier.

      case BD.Kind is
         when Ada_Abstract_Subp_Decl =>
            Qualifier_Text.Append ("abstract");

         when Ada_Null_Subp_Decl =>
            Qualifier_Text.Append ("null");

         when others =>
            null;
      end case;

      --  Extract aspects when declaration has them.

      declare
         Aspects : constant Libadalang.Analysis.Aspect_Spec :=
           BD.F_Aspects;

      begin
         if not Aspects.Is_Null then
            for Aspect of Aspects.F_Aspect_Assocs loop
               if not Aspects_Lines.Is_Empty then
                  --  need to add "," for the highlighting

                  Append_To_Last_Line (Aspects_Lines, ',');
               end if;

               Aspects_Lines.Append (Get_Hover_Text_For_Node (Aspect));
            end loop;
         end if;
      end;

      Aspects_Text := Aspects_Lines.Join_Lines (VSS.Strings.LF, False);
   end Get_Tooltip_Text;

end LSP.Ada_Documentation;

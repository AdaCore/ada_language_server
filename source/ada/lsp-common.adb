------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2020, AdaCore                     --
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

with Ada.Characters.Handling;
with Ada.Exceptions;           use Ada.Exceptions;
with GNAT.Strings;
with GNAT.Traceback.Symbolic;  use GNAT.Traceback.Symbolic;
with GNATCOLL.Utils;           use GNATCOLL.Utils;

with Langkit_Support.Text;
with Libadalang.Common;        use Libadalang.Common;

package body LSP.Common is

   function Get_Hover_Text_For_Node (Node : Ada_Node'Class) return LSP_String;
   --  Return a pretty printed version of the node's text to be
   --  displayed on hover requests, removing unnecessary indentation
   --  whitespaces if needed and attaching extra information in some cases.

   ---------
   -- Log --
   ---------

   procedure Log
     (Trace   : GNATCOLL.Traces.Trace_Handle;
      E       : Ada.Exceptions.Exception_Occurrence;
      Message : String := "") is
   begin
      if Message /= "" then
         Trace.Trace (Message);
      end if;

      Trace.Trace (Exception_Name (E) & ": " & Exception_Message (E)
                   & ASCII.LF & Symbolic_Traceback (E));
   end Log;

   function Get_Hover_Text_For_Node (Node : Ada_Node'Class) return LSP_String
   is
      Text   : constant String := Langkit_Support.Text.To_UTF8
        (Node.Text);
      Lines  : GNAT.Strings.String_List_Access := Split
        (Text,
         On               => ASCII.LF,
         Omit_Empty_Lines => True);

      Result : LSP_String;

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
      begin
         case Node.Kind is
            when Ada_Package_Body =>

               --  This means that user is hovering on the package declaration
               --  itself: in this case, return a empty response since all the
               --  relevant information is already visible to the user.
               return;

            when Ada_For_Loop_Var_Decl =>

               --  Return the first line of the enclosing for loop when
               --  hovering a for loop variable declaration.
               declare
                  Parent_Text : constant String := Langkit_Support.Text.To_UTF8
                    (As_For_Loop_Var_Decl (Node).P_Semantic_Parent.Text);
                  End_Idx     : Natural := Parent_Text'First;
               begin
                  Skip_To_String
                    (Str       => Parent_Text,
                     Index     => End_Idx,
                     Substring => "loop");

                  Result := To_LSP_String
                    (Parent_Text (Parent_Text'First .. End_Idx + 4));
                  return;
               end;

            when others =>
               declare
                  Idx : Integer;
               begin
                  --  Return an empty hover text if there is no text for this
                  --  delclaration (only for safety).
                  if Text = "" then
                     return;
                  end if;

                  --  If it's a single-line declaration, replace all the
                  --  series of whitespaces by only one blankspace. If it's
                  --  a multi-line declaration, remove only the unneeded
                  --  indentation whitespaces.

                  if Lines'Length = 1 then
                     declare
                        Res_Idx : Integer := Text'First;
                        Tmp     : String (Text'First .. Text'Last);
                     begin
                        Idx := Text'First;

                        while Idx <= Text'Last loop
                           Skip_Blanks (Text, Idx);

                           while Idx <= Text'Last
                             and then not Is_Whitespace (Text (Idx))
                           loop
                              Tmp (Res_Idx) := Text (Idx);
                              Idx := Idx + 1;
                              Res_Idx := Res_Idx + 1;
                           end loop;

                           if Res_Idx < Tmp'Last then
                              Tmp (Res_Idx) := ' ';
                              Res_Idx := Res_Idx + 1;
                           end if;
                        end loop;

                        if Res_Idx > Text'First then
                           Result := To_LSP_String
                             (Tmp (Tmp'First .. Res_Idx - 1));
                        end if;
                     end;
                  else
                     declare
                        Blanks_Count_Per_Line : array
                          (Lines'First + 1 .. Lines'Last) of Natural;
                        Indent_Blanks_Count   : Natural := Natural'Last;
                        Start_Idx             : Integer;
                     begin
                        Result := To_LSP_String (Lines (Lines'First).all);

                        --  Count the blankpaces per line and track how many
                        --  blankspaces we should remove on each line by
                        --  finding the common indentation blankspaces.

                        for J in Lines'First + 1 .. Lines'Last loop
                           Idx := Lines (J)'First;
                           Skip_Blanks (Lines (J).all, Idx);

                           Blanks_Count_Per_Line (J) := Idx - Lines (J)'First;
                           Indent_Blanks_Count := Natural'Min
                             (Indent_Blanks_Count,
                              Blanks_Count_Per_Line (J));
                        end loop;

                        for J in Lines'First + 1 .. Lines'Last loop
                           Start_Idx := Lines (J)'First + Indent_Blanks_Count;
                           Result := Result & To_LSP_String
                             (ASCII.LF
                              & Lines (J).all (Start_Idx .. Lines (J)'Last));
                        end loop;
                     end;
                  end if;
               end;
         end case;
      end Get_Basic_Decl_Hover_Text;

      ------------------------------
      -- Get_Subp_Spec_Hover_Text --
      ------------------------------

      procedure Get_Subp_Spec_Hover_Text is
         Idx : Integer;
      begin
         --  For single-line subprogram specifications, we display the
         --  associated text directly.
         --  For multi-line ones, remove the identation blankspaces to replace
         --  them by a fixed number of blankspaces.

         if Lines'Length = 1 then
            Result := To_LSP_String (Text);
         else
            Result := To_LSP_String (Lines (Lines'First).all);

            for J in Lines'First + 1 .. Lines'Last loop
               Idx := Lines (J)'First;
               Skip_Blanks (Lines (J).all, Idx);

               Result := Result
                 & To_LSP_String
                 (ASCII.LF
                  & (if Lines (J).all (Idx) = '(' then "  " else "   ")
                  & Lines (J).all (Idx .. Lines (J).all'Last));
            end loop;
         end if;

         --  Append "is abstract" to the resulting hover text if the subprogram
         --  specificiation node belongs to an abstract subprogram declaration.
         if not Node.Parent.Is_Null
           and then Node.Parent.Kind in Ada_Abstract_Subp_Decl_Range
         then
            Append (Result, " is abstract");
            return;
         end if;

         --  Append "is null" to the resulting hover text if the subprogram
         --  specificiation node belongs to an null subprogram declaration.
         if not Node.Parent.Is_Null
           and then Node.Parent.Kind in Ada_Null_Subp_Decl_Range
         then
            Append (Result, " is null");
         end if;
      end Get_Subp_Spec_Hover_Text;

      ---------------------------------
      -- Get_Package_Decl_Hover_Text --
      ---------------------------------

      procedure Get_Package_Decl_Hover_Text is
         Generic_Params : LSP_String;
         End_Idx        : Natural := Text'First;
      begin
         --  Return the first line of the package declaration and its
         --  generic parameters if any.
         Skip_To_String
           (Str       => Text,
            Index     => End_Idx,
            Substring => " is");

         if Node.Parent /= No_Ada_Node
           and then Node.Parent.Kind in Ada_Generic_Decl
         then
            Generic_Params := To_LSP_String
              (Langkit_Support.Text.To_UTF8
                 (As_Generic_Decl (Node.Parent).F_Formal_Part.Text)
               & ASCII.LF);
         end if;

         Result := Generic_Params
           & To_LSP_String (Text (Text'First .. End_Idx));
      end Get_Package_Decl_Hover_Text;

      -----------------------------
      -- Get_Loop_Var_Hover_Text --
      -----------------------------

      procedure Get_Loop_Var_Hover_Text is
         Parent_Text : constant String := Langkit_Support.Text.To_UTF8
           (As_For_Loop_Var_Decl (Node).P_Semantic_Parent.Text);
         End_Idx     : Natural := Parent_Text'First;
      begin
         --  Return the first line of the enclosing for loop when
         --  hovering a for loop variable declaration.

         Skip_To_String
           (Str       => Parent_Text,
            Index     => End_Idx,
            Substring => "loop");

         Result := To_LSP_String
           (Parent_Text (Parent_Text'First .. End_Idx + 4));
      end Get_Loop_Var_Hover_Text;

      ---------------------------
      -- Get_Aspect_Hover_Text --
      ---------------------------

      procedure Get_Aspect_Hover_Text is
         Indentation : Integer;
         Idx         : Integer;
      begin
         --  Get the indentation for the first line
         Idx := Lines (Lines'First)'First;
         Skip_Blanks (Lines (Lines'First).all, Idx);
         Indentation := Idx - Lines (Lines'First)'First;

         Result := Ada.Characters.Handling.To_Wide_Character (ASCII.LF)
           & (2 * " ")  --  Force an indentation of 2 for the first line
           & To_LSP_String --  Remove the uneeded indentation
           (Lines (Lines'First).all
                (Lines (Lines'First)'First + Indentation
                 .. Lines (Lines'First).all'Last));

         --  The next line should have one more indentation level
         Indentation := Indentation + 3;

         for J in Lines'First + 1 .. Lines'Last loop
            Idx := Lines (J)'First;
            Skip_Blanks (Lines (J).all, Idx);

            if Lines (J)'First + Indentation > Idx then
               --  Uncommon indentation: just print the line
               Result := Result
                 & Ada.Characters.Handling.To_Wide_Character (ASCII.LF)
                 & To_LSP_String (Lines (J).all);
            else
               Result := Result
                 & Ada.Characters.Handling.To_Wide_Character (ASCII.LF)
                 & To_LSP_String --  Remove the uneeded indentation
                 (Lines (J).all
                      (Lines (J)'First + Indentation .. Lines (J).all'Last));
            end if;
         end loop;
      end Get_Aspect_Hover_Text;

   begin

      case Node.Kind is
         when Ada_Package_Body =>

            --  This means that the user is hovering on the package declaration
            --  itself: in this case, return a empty response since all the
            --  relevant information is already visible to the user.
            return Empty_LSP_String;

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

      GNAT.Strings.Free (Lines);

      return Result;
   end Get_Hover_Text_For_Node;

   --------------------
   -- Get_Hover_Text --
   --------------------

   function Get_Hover_Text (Decl : Basic_Decl'Class) return LSP_String is
      Decl_Text      : LSP_String;
      Subp_Spec_Node : Base_Subp_Spec;
      Line_Feed      : constant String := "" & ASCII.LF;
   begin
      --  Try to retrieve the subprogram spec node, if any : if it's a
      --  subprogram node that does not have any separate declaration we
      --  only want to display its specification, not the body.
      Subp_Spec_Node := Decl.P_Subp_Spec_Or_Null;

      if Subp_Spec_Node /= No_Base_Subp_Spec then
         Decl_Text := Get_Hover_Text_For_Node (Subp_Spec_Node);

         --  Append the aspects to the declaration text, if any.
         declare
            Aspects      : constant Aspect_Spec := Decl.F_Aspects;
            Aspects_Text : LSP_String;
         begin
            if not Aspects.Is_Null then
               for Aspect of Aspects.F_Aspect_Assocs loop
                  if Aspects_Text /= Empty_LSP_String then
                     --  need to add "," for the highlighting
                     Append (Aspects_Text, ",");
                  end if;

                  Append (Aspects_Text, Get_Hover_Text_For_Node (Aspect));
               end loop;

               if Aspects_Text /= Empty_LSP_String then
                  Decl_Text := Decl_Text
                    & To_LSP_String (Line_Feed & "with")
                    & Aspects_Text;
               end if;
            end if;
         end;

      else
         Decl_Text := Get_Hover_Text_For_Node (Decl);
      end if;

      return Decl_Text;
   end Get_Hover_Text;

end LSP.Common;

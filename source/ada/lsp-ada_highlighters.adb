------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

with GNATCOLL.Traces;

with Langkit_Support.Slocs;
with Langkit_Support.Text;
with Libadalang.Common; use Libadalang.Common;

with VSS.Strings;

package body LSP.Ada_Highlighters is

   Highlighter_Debug : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("ALS.HIGHLIGHTERS.DEBUG", GNATCOLL.Traces.Off);

   Skip : LSP.Enumerations.SemanticTokenTypes renames LSP.Enumerations.macro;
   --  A dedicated token type for unsupported tokens

   function Is_Ghost_Root_Node
     (Node  : Libadalang.Analysis.Ada_Node'Class) return Boolean;
   --  Check if given node is a declaration and has ghost aspect

   type Is_Ghost_Root_Predicate is
     new Libadalang.Iterators.Ada_Node_Predicate_Interface with null record;

   overriding function Evaluate
     (Self : in out Is_Ghost_Root_Predicate;
      Node : Libadalang.Analysis.Ada_Node) return Boolean is
       (Is_Ghost_Root_Node (Node));

   Is_Ghost_Root : Libadalang.Iterators.Ada_Node_Predicate;

   procedure Highlight_Name
     (Self   : Ada_Highlighter'Class;
      Holder : in out Highlights_Holders.Highlights_Holder;
      Node   : Libadalang.Analysis.Name'Class);
   --  Highlight given name with token Kind

   procedure Get_Result
     (Self       : Ada_Highlighter'Class;
      Holder     : Highlights_Holders.Highlights_Holder;
      From_Token : Libadalang.Common.Token_Reference;
      To_Token   : Libadalang.Common.Token_Reference;
      Result     : out LSP.Structures.Natural_Vector);

   function To_Int
     (Self : Ada_Highlighter'Class;
      Set  : Highlights_Holders.Modifier_Set) return Natural
        with Inline;
   --  Cast set of modifiers to uinteger

   Obsolescent     : Unbounded_Text_Type;
   Ada_Package     : Unbounded_Text_Type;
   System          : Unbounded_Text_Type;
   Interfaces      : Unbounded_Text_Type;

   ------------------------
   -- Highlights_Holders --
   ------------------------

   package body Highlights_Holders is

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Self  : in out Highlights_Holder'Class;
         From  : Libadalang.Common.Token_Reference;
         To    : Libadalang.Common.Token_Reference;
         Empty : out Boolean)
      is
         use type Langkit_Support.Token_Data_Handlers.Token_Index;

         First : Libadalang.Common.Token_Reference := From;
         Last  : Libadalang.Common.Token_Reference := To;

         Count : Langkit_Support.Token_Data_Handlers.Token_Index;
         None  : constant Semantic_Token :=
           (Is_Set => False, Modifiers => (others => False));
      begin
         if Libadalang.Common.Is_Trivia (First) then
            First := Libadalang.Common.Next (First, Exclude_Trivia => True);
         end if;

         if Libadalang.Common.Is_Trivia (Last) then
            Last := Libadalang.Common.Previous (Last, Exclude_Trivia => True);
         end if;

         if Libadalang.Common.No_Token in First | Last
           or else Last < First
         then
            Self.First := 0;
            Self.Last := 0;
            Self.Vector.Clear;
            Empty := True;
            return;
         end if;

         Self.First := Libadalang.Common.Index (First);
         Self.Last := Libadalang.Common.Index (Last);
         Count := Libadalang.Common.Index (Last) - Self.First + 1;

         Self.Vector.Clear;
         Self.Vector.Append (None, Ada.Containers.Count_Type (Count));
         Empty := False;
      end Initialize;

      ---------
      -- Get --
      ---------

      function Get
        (Self  : Highlights_Holder'Class;
         Token : Libadalang.Common.Token_Reference) return Semantic_Token
      is
         use type Langkit_Support.Token_Data_Handlers.Token_Index;

         Index : constant Langkit_Support.Token_Data_Handlers.Token_Index :=
           Libadalang.Common.Index (Token) - Self.First;
      begin
         return Self.Vector (Index);
      end Get;

      ------------------------
      -- Set_Token_Modifier --
      ------------------------

      procedure Set_Token_Modifier
        (Self  : in out Highlights_Holder'Class;
         Token : Libadalang.Common.Token_Reference;
         Value : LSP.Enumerations.SemanticTokenModifiers)
      is
         use type Langkit_Support.Token_Data_Handlers.Token_Index;
         Index : constant Langkit_Support.Token_Data_Handlers.Token_Index'Base
           := Libadalang.Common.Index (Token) - Self.First;
      begin
         if Libadalang.Common.Index (Token) in Self.First .. Self.Last then
            Self.Vector (Index).Modifiers (Value) := True;
         end if;
      end Set_Token_Modifier;

      ------------------------
      -- Set_Token_Modifier --
      ------------------------

      procedure Set_Token_Modifier
        (Self  : in out Highlights_Holder'Class;
         From  : Libadalang.Common.Token_Reference;
         To    : Libadalang.Common.Token_Reference;
         Value : LSP.Enumerations.SemanticTokenModifiers)
      is
         use type Langkit_Support.Token_Data_Handlers.Token_Index;

         Token : Libadalang.Common.Token_Reference := From;
         Index : Langkit_Support.Token_Data_Handlers.Token_Index;
      begin
         loop
            if Libadalang.Common.Index (Token) in Self.First .. Self.Last then
               Index := Libadalang.Common.Index (Token) - Self.First;
               Self.Vector (Index).Modifiers (Value) := True;
            end if;

            exit when Token = To;

            Token := Libadalang.Common.Next (Token, Exclude_Trivia => True);
         end loop;
      end Set_Token_Modifier;

      --------------------
      -- Set_Token_Kind --
      --------------------

      procedure Set_Token_Kind
        (Self  : in out Highlights_Holder'Class;
         Token : Libadalang.Common.Token_Reference;
         Value : LSP.Enumerations.SemanticTokenTypes)
      is
         use type Langkit_Support.Token_Data_Handlers.Token_Index;
         Index : constant Langkit_Support.Token_Data_Handlers.Token_Index'Base
           := Libadalang.Common.Index (Token) - Self.First;
      begin
         if Libadalang.Common.Index (Token) not in Self.First .. Self.Last then
            null;  --  Token index is out of expected range, skip it
         elsif Self.Vector (Index).Is_Set then
            Self.Vector (Index).Kind := Value;
         else
            Self.Vector.Replace_Element
              (Index, (True, Self.Vector (Index).Modifiers, Value));
         end if;
      end Set_Token_Kind;

   end Highlights_Holders;

   ----------------
   -- Get_Tokens --
   ----------------

   function Get_Tokens
     (Self   : Ada_Highlighter'Class;
      Unit   : Libadalang.Analysis.Analysis_Unit;
      Tracer : in out LSP.Tracers.Tracer'Class;
      Span   : LSP.Structures.A_Range)
      return LSP.Structures.Natural_Vector
   is

      First_Token : constant Libadalang.Common.Token_Reference :=
        (if Span.an_end.line = 0 then Unit.First_Token
         else Unit.Lookup_Token
           ((Langkit_Support.Slocs.Line_Number (Span.start.line + 1), 1)));

      Last_Token : constant Libadalang.Common.Token_Reference :=
        (if Span.an_end.line = 0 then Unit.Last_Token
         else Unit.Lookup_Token
           ((Langkit_Support.Slocs.Line_Number (Span.an_end.line + 2), 1)));

      From_Token : constant Libadalang.Common.Token_Reference :=
        (if Libadalang.Common.Is_Trivia (First_Token)
         then Libadalang.Common.Next (First_Token, Exclude_Trivia => True)
         else First_Token);

      To_Token : constant Libadalang.Common.Token_Reference :=
        (if Libadalang.Common.Is_Trivia (Last_Token)
         then Libadalang.Common.Previous (Last_Token, Exclude_Trivia => True)
         else Last_Token);

      function Highlight_Node
        (Node : Libadalang.Analysis.Ada_Node'Class)
         return Libadalang.Common.Visit_Status;
      --  Highlight given node

      Holder : Highlights_Holders.Highlights_Holder;

      --------------------
      -- Highlight_Node --
      --------------------

      function Highlight_Node
        (Node : Libadalang.Analysis.Ada_Node'Class)
         return Libadalang.Common.Visit_Status
      is
         use all type LSP.Enumerations.SemanticTokenModifiers;
      begin
         if Node.Token_End < From_Token or To_Token < Node.Token_Start then
            --  Skip uninteresting nodes to speedup traversal
            return Libadalang.Common.Over;
         elsif Is_Ghost_Root_Node (Node) and then
           Self.Token_Modifiers.Contains (documentation)
         then
            --  Mark all tokens in a ghost element as `documentation`
            Holder.Set_Token_Modifier
              (Node.Token_Start, Node.Token_End, documentation);
         end if;

         case Node.Kind is
            when Libadalang.Common.Ada_Name =>
               Self.Highlight_Name (Holder, Node.As_Name);

            when others =>
               null;
         end case;

         return Libadalang.Common.Into;
      exception
         when E : Libadalang.Common.Property_Error =>
            if Highlighter_Debug.Is_Active then
               Tracer.Trace_Exception
                  (E,
                   "In Highlight_Node at ");

               Tracer.Trace
                 (Langkit_Support.Text.Image (Node.Full_Sloc_Image));
            end if;

            return Libadalang.Common.Into;
      end Highlight_Node;

      Root  : constant Libadalang.Analysis.Ada_Node :=
        Libadalang.Analysis.Root (Unit);

      Empty  : Boolean;

   begin
      if Root.Is_Null or else
        Libadalang.Common.No_Token in From_Token | To_Token
      then
         --  No tokens to highlight
         return LSP.Structures.Empty;
      end if;

      Holder.Initialize (From_Token, To_Token, Empty);

      if Empty then
         return LSP.Structures.Empty;
      end if;

      --  Traverse whole tree, look for intresting nodes and mark their
      --  tokens in Holder for further processing
      Root.Traverse (Highlight_Node'Access);

      return Result : LSP.Structures.Natural_Vector do
         Self.Get_Result (Holder, From_Token, To_Token, Result);
      end return;
   end Get_Tokens;

   ----------------
   -- Get_Result --
   ----------------

   procedure Get_Result
     (Self   : Ada_Highlighter'Class;
      Holder : Highlights_Holder;
      Unit   : Libadalang.Analysis.Analysis_Unit;
      Result : out LSP.Structures.Natural_Vector) is
   begin
      Self.Get_Result
        (Holder.Value, Unit.First_Token, Unit.Last_Token, Result);
   end Get_Result;

   ----------------
   -- Get_Result --
   ----------------

   procedure Get_Result
     (Self       : Ada_Highlighter'Class;
      Holder     : Highlights_Holders.Highlights_Holder;
      From_Token : Libadalang.Common.Token_Reference;
      To_Token   : Libadalang.Common.Token_Reference;
      Result     : out LSP.Structures.Natural_Vector)
   is
      use all type LSP.Enumerations.SemanticTokenTypes;
      use type Langkit_Support.Slocs.Line_Number;
      use type Langkit_Support.Slocs.Column_Number;

      subtype uint is Natural;

      Last : Langkit_Support.Slocs.Source_Location := (1, 1);

      Token : Libadalang.Common.Token_Reference := From_Token;
   begin
      if Libadalang.Common.Is_Trivia (Token) then
         Token := Libadalang.Common.Next (Token, Exclude_Trivia => True);
      end if;

      --  Scan over all tokens and find a corresponding value in Holder
      while Token < To_Token loop

         declare
            Value : constant Highlights_Holders.Semantic_Token :=
              Holder.Get (Token);

            Token_Data : constant Libadalang.Common.Token_Data_Type :=
              Libadalang.Common.Data (Token);
         begin
            declare
               use type Highlights_Holders.Modifier_Set;

               Sloc_Range                                  : constant
                 Langkit_Support.Slocs.Source_Location_Range :=
                   Libadalang.Common.Sloc_Range (Token_Data);

               Start : constant Langkit_Support.Slocs.Source_Location :=
                 Langkit_Support.Slocs.Start_Sloc (Sloc_Range);

               Map                                 : constant array (Libadalang.Common.Token_Kind) of
                 LSP.Enumerations.SemanticTokenTypes :=
                   (Ada_All .. Ada_Xor | Ada_With => keyword,
                    Ada_Par_Close .. Ada_Target   => operator,
                    Ada_String | Ada_Char         => LSP.Enumerations.string,
                    Ada_Decimal | Ada_Integer     => number,
                    Ada_Comment                   => comment,
                    Ada_Identifier                => Skip,
                    others                        => Skip);

               Mapped_Token : constant LSP.Enumerations.SemanticTokenTypes :=
                 Map (Libadalang.Common.Kind (Token_Data));
            begin
               --  If we have no token type calculated and no modifiers then
               --  skip this token. For instance skip string literals those
               --  are not in GHost code. This lets VS Code use rule-based
               --  (lexical level) highlighter. Such highlighter is capable
               --  to highlight character escape sequences inside a string
               --  literal, or +/- before exponent in numeric literal, etc.
               if Value.Is_Set or
                 (Mapped_Token /= Skip and then
                 Self.Token_Types.Contains (Mapped_Token) and then
                 Value.Modifiers /= Highlights_Holders.Empty)
               then
                  pragma Assert
                    (Sloc_Range.End_Line = Sloc_Range.Start_Line);

                  --  deltaLine
                  Result.Append (uint (Start.Line - Last.Line));
                  --  deltaStartChar
                  Result.Append
                    (uint
                       (Start.Column -
                            (if Start.Line = Last.Line
                             then Last.Column else 1)));
                  --  length
                  Result.Append
                    (uint
                       (Sloc_Range.End_Column - Sloc_Range.Start_Column));
                  --  tokenType
                  Result.Append
                    (Self.Token_Types
                       (if Value.Is_Set then Value.Kind else Mapped_Token));
                  --  tokenModifiers
                  Result.Append (Self.To_Int (Value.Modifiers));

                  Last := Start;
               end if;
            end;

            Token := Libadalang.Common.Next (Token, Exclude_Trivia => True);

            exit when not (Token < To_Token);
         end;
      end loop;
   end Get_Result;

   --------------------
   -- Highlight_Name --
   --------------------

   procedure Highlight_Name
     (Self   : Ada_Highlighter'Class;
      Holder : in out Highlights_Holders.Highlights_Holder;
      Node   : Libadalang.Analysis.Name'Class)
   is
      use all type LSP.Enumerations.SemanticTokenTypes;
      use all type LSP.Enumerations.SemanticTokenModifiers;
      use type Libadalang.Analysis.Defining_Name;

      procedure Highlight_Token
        (Token : Libadalang.Common.Token_Reference;
         Kind  : LSP.Enumerations.SemanticTokenTypes);
      --  Highlight given Token with token Kind

      procedure Highlight_Token
        (Token : Libadalang.Common.Token_Reference;
         Kind  : LSP.Enumerations.SemanticTokenModifiers);
      --  Highlight given Token with token Kind

      function To_Kind (Decl : Libadalang.Analysis.Basic_Decl)
        return LSP.Enumerations.SemanticTokenTypes;

      function Has_Abstract (Decl : Libadalang.Analysis.Basic_Decl)
        return Boolean;

      function Is_Predefined (Decl : Libadalang.Analysis.Basic_Decl)
        return Boolean;

      ------------------
      -- Has_Abstract --
      ------------------

      function Has_Abstract (Decl : Libadalang.Analysis.Basic_Decl)
        return Boolean
      is
      begin
         case Decl.Kind is
            when Ada_Abstract_Formal_Subp_Decl |
                 Ada_Abstract_Subp_Decl
               =>
               return True;
            when others =>
               return False;
         end case;
      end Has_Abstract;

      ---------------------
      -- Highlight_Token --
      ---------------------

      procedure Highlight_Token
        (Token : Libadalang.Common.Token_Reference;
         Kind  : LSP.Enumerations.SemanticTokenTypes) is
      begin
         if not Self.Token_Types.Contains (Kind) then
            --  Skip unsupported tokens
            return;
         end if;

         Holder.Set_Token_Kind (Token, Kind);
      end Highlight_Token;

      ---------------------
      -- Highlight_Token --
      ---------------------

      procedure Highlight_Token
        (Token : Libadalang.Common.Token_Reference;
         Kind  : LSP.Enumerations.SemanticTokenModifiers) is
      begin
         if not Self.Token_Modifiers.Contains (Kind) then
            --  Skip unsupported tokens
            return;
         end if;

         Holder.Set_Token_Modifier (Token, Kind);
      end Highlight_Token;

      -------------------
      -- Is_Predefined --
      -------------------

      function Is_Predefined (Decl : Libadalang.Analysis.Basic_Decl)
        return Boolean
      is

         function Is_Synthetic
           (Node : Libadalang.Analysis.Ada_Node'Class) return Boolean;

         function Is_Synthetic
           (Node : Libadalang.Analysis.Ada_Node'Class) return Boolean
         is
            Std  : constant String := "__standard";
            File : constant String := Node.Unit.Get_Filename;
         begin
            return File'Length >= Std'Length
              and then File (File'Last - Std'Length + 1 .. File'Last) = Std;
         end Is_Synthetic;

         Name : Libadalang.Analysis.Name :=
           Decl.P_Enclosing_Compilation_Unit.P_Decl.P_Defining_Name.F_Name;
      begin
         if Is_Synthetic (Decl) then
            return True;  --  In Standard package
         end if;

         while not Name.Is_Null and then Name.Kind = Ada_Dotted_Name loop
            Name := Name.As_Dotted_Name.F_Prefix;
         end loop;

         if not Name.Is_Null
           and then Name.Kind = Ada_Identifier
           and then (Name.P_Name_Is (Ada_Package)
                     or Name.P_Name_Is (System)
                     or Name.P_Name_Is (Interfaces))
         then
            return True;
         else
            return False;
         end if;
      end Is_Predefined;

      -------------
      -- To_Kind --
      -------------

      function To_Kind (Decl : Libadalang.Analysis.Basic_Decl)
        return LSP.Enumerations.SemanticTokenTypes is
      begin
         case Decl.Kind is
            when Libadalang.Common.Ada_Basic_Subp_Decl =>
               if Decl.Kind = Ada_Enum_Literal_Decl then
                  return enumMember;
               else
                  return a_function;
               end if;
            when Libadalang.Common.Ada_Base_Type_Decl =>
               begin
                  if Decl.Kind = Ada_Single_Task_Type_Decl then
                     return variable;
                  elsif Decl.As_Base_Type_Decl.P_Is_Enum_Type then
                     return enum;
                  elsif Decl.As_Base_Type_Decl.P_Is_Interface_Type then
                     return an_interface;
                  elsif Decl.As_Base_Type_Decl.P_Is_Tagged_Type then
                     return class;
                  elsif Decl.As_Base_Type_Decl.P_Is_Record_Type then
                     return struct;
                  else
                     return a_type;
                  end if;
               exception
                  when Property_Error =>
                     --  If an error occurs while analysing the type (e.g.
                     --  incomplete code), default to "type".
                     return a_type;
               end;

            when Ada_Base_Formal_Param_Decl =>
               case Ada_Base_Formal_Param_Decl'(Decl.Kind) is
                  when Ada_Component_Decl =>
                     return property;
                  when Ada_Discriminant_Spec =>
                     return typeParameter;
                  when Ada_Param_Spec =>
                     return parameter;
                  when Ada_Generic_Formal_Obj_Decl =>
                     return variable;
                  when Ada_Generic_Formal_Package =>
                     return namespace;
                  when Ada_Generic_Formal_Subp_Decl =>
                     return a_function;
                  when Ada_Generic_Formal_Type_Decl =>
                     return a_type;  --  class/enum/interface/struct...?
                  when Ada_Synthetic_Formal_Param_Decl =>
                     --  Synthetic nodes do not correspond to source text
                     return Skip;
               end case;

            when Libadalang.Common.Ada_Base_Package_Decl =>
               return namespace;

            when Ada_Body_Node =>
               begin
                  declare
                     Spec : constant Libadalang.Analysis.Basic_Decl :=
                     Decl.As_Body_Node.P_Decl_Part (True);
                  begin
                     if not Spec.Is_Null then
                        --  If there's a spec, use it to determine the kind
                        return To_Kind (Spec);
                     end if;
                  end;
               exception
                  when Property_Error =>
                     --  In case of errors while trying to obtain the spec
                     --  (e.g. incomplete code), continue the logic below
                     --  based on the body node.
                     null;
               end;

               --  If the above fails, handle the kinds of bodies directly
               case Ada_Body_Node'(Decl.Kind) is
                  when Ada_Accept_Stmt_Body =>
                     return Skip;
                  when Ada_Base_Subp_Body =>
                     return a_function;
                  when Ada_Package_Body_Stub =>
                     return namespace;
                  when Ada_Protected_Body_Stub =>
                     return variable;
                  when Ada_Subp_Body_Stub =>
                     return a_function;
                  when Ada_Task_Body_Stub =>
                     return variable;
                  when Ada_Entry_Body =>
                     return variable;
                  when Ada_Package_Body =>
                     return namespace;
                  when Ada_Protected_Body =>
                     return variable;
                  when Ada_Task_Body =>
                     return variable;
               end case;

            when Ada_Entry_Index_Spec =>
               return variable;
            when Ada_Exception_Decl =>
               return a_type;
            when Ada_Exception_Handler =>
               return variable;
            when Ada_Object_Decl =>
               return variable;
            when Ada_For_Loop_Var_Decl =>
               return variable;
            when Ada_Generic_Package_Decl =>
               return namespace;
            when Ada_Generic_Subp_Decl =>
               return a_function;
            when Ada_Generic_Package_Instantiation =>
               return namespace;
            when Ada_Generic_Subp_Instantiation =>
               return a_function;
            when Ada_Generic_Package_Renaming_Decl =>
               return namespace;
            when Ada_Generic_Subp_Renaming_Decl =>
               return a_function;
            when Ada_Named_Stmt_Decl =>
               return namespace;
            when Ada_Number_Decl =>
               return number;
            when Ada_Package_Renaming_Decl =>
               return namespace;
            when Ada_Single_Protected_Decl =>
               return variable;
            when Ada_Single_Task_Decl =>
               return variable;
            when others =>
               return Skip;
         end case;
      end To_Kind;

      Failsafe_Def : Libadalang.Analysis.Refd_Def;
      Def  : Libadalang.Analysis.Defining_Name;
      Decl : Libadalang.Analysis.Basic_Decl;
      Kind : LSP.Enumerations.SemanticTokenTypes;
   begin
      if Node.Kind not in Ada_Identifier | Ada_String_Literal then
         --  Highlight only identifiers and operator symbols
         return;
      end if;

      if Node.P_Is_Defining then
         Def := Node.P_Enclosing_Defining_Name;

         begin
            declare
               Is_Canonical : constant Boolean :=
                  not Def.Is_Null and then Def.P_Canonical_Part = Def;
            begin
               if Is_Canonical then
                  Highlight_Token (Node.Token_Start, declaration);
               else
                  Highlight_Token (Node.Token_Start, definition);
               end if;
            end;
         exception
            when Property_Error =>
               --  In case of errors (e.g. incomplete code) consider it
               --  a canonical declaration.
               Highlight_Token (Node.Token_Start, declaration);
         end;
      else
         Failsafe_Def := Node.P_Failsafe_Referenced_Def_Name (True);
         Def :=  Libadalang.Analysis.Defining_Name (Failsafe_Def.Def_Name);
      end if;

      if Node.Kind in Libadalang.Common.Ada_Name then
         begin
            if Node.As_Name.P_Is_Write_Reference (True) then
               Highlight_Token (Node.Token_Start, modification);
            end if;
         exception
            when Libadalang.Common.Property_Error => null;
         end;
      end if;

      if not Def.Is_Null then
         Decl := Def.P_Basic_Decl;

         if not Decl.Is_Null then
            Kind := To_Kind (Decl);
            if Kind /= Skip then
               Highlight_Token (Node.Token_Start, Kind);
            end if;

            begin
               if Kind in variable | parameter | typeParameter | property
                 and then Decl.P_Is_Constant_Object
               then
                  Highlight_Token (Node.Token_Start, readonly);
               end if;
            exception
               when Libadalang.Common.Property_Error => null;
            end;

            begin
               if Decl.P_Is_Static_Decl then
                  Highlight_Token (Node.Token_Start, static);
               end if;
            exception
               when Libadalang.Common.Property_Error => null;
            end;

            begin
               --  P_Has_Aspect checks the existence of either an aspect or
               --  a pragma.
               if Def.P_Has_Aspect (Obsolescent) then
                  Highlight_Token (Node.Token_Start, deprecated);
               end if;
            exception
               when Libadalang.Common.Property_Error => null;
            end;

            if Has_Abstract (Decl) then
               Highlight_Token (Node.Token_Start, an_abstract);
            end if;

            if Is_Predefined (Decl) then
               Highlight_Token (Node.Token_Start, defaultLibrary);
            end if;

            return;
         end if;
      elsif Node.Kind = Ada_String_Literal then
         return;  --  This is not an operator symbol, so do nothing
      end if;

      if Node.P_Is_Operator_Name then
         Highlight_Token (Node.Token_Start, operator);
      else
         --  In case of unresolved identifiers, do not set any semantic
         --  highlighting
         null;
      end if;
   end Highlight_Name;

   --------------------
   -- Highlight_Node --
   --------------------

   procedure Highlight_Node
     (Self   : Ada_Highlighter'Class;
      Holder : in out Highlights_Holder;
      Node   : Libadalang.Analysis.Ada_Node'Class) is
   begin
      if Is_Ghost_Root_Node (Node) then
         --  Mark all tokens in a ghost element as `documentation`
         Holder.Value.Set_Token_Modifier
           (Node.Token_Start, Node.Token_End, LSP.Enumerations.documentation);
      end if;

      case Node.Kind is
         when Libadalang.Common.Ada_Name =>
            Self.Highlight_Name (Holder.Value, Node.As_Name);

         when others =>
            null;
      end case;

   exception
      when Libadalang.Common.Property_Error =>
         null;
   end Highlight_Node;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self      : in out Ada_Highlighter'Class;
      Client    : LSP.Ada_Client_Capabilities.Client_Capability;
      Types     : out LSP.Structures.Virtual_String_Vector;
      Modifiers : out LSP.Structures.Virtual_String_Vector)
   is
      use all type LSP.Enumerations.SemanticTokenTypes;
      use all type LSP.Enumerations.SemanticTokenModifiers;

      procedure Append_Type
        (Kind  : LSP.Enumerations.SemanticTokenTypes;
         Image : VSS.Strings.Virtual_String);
      --  Update Legend.tokenTypes if client understands given Kind

      procedure Append_Modifier
        (Kind  : LSP.Enumerations.SemanticTokenModifiers;
         Image : VSS.Strings.Virtual_String);
      --  Update Legend.tokenModifiers if client understands given Kind

      function "+" (Source : Wide_Wide_String) return Unbounded_Text_Type
        renames Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String;

      procedure Append_Modifier
        (Kind  : LSP.Enumerations.SemanticTokenModifiers;
         Image : VSS.Strings.Virtual_String) is
      begin
         if Client.Token_Modifiers.Contains (Image) then
            Self.Token_Modifiers.Insert
              (Kind, Natural (2 ** Modifiers.Length));

            Modifiers.Append (Image);
         end if;
      end Append_Modifier;

      -----------------
      -- Append_Type --
      -----------------

      procedure Append_Type
        (Kind  : LSP.Enumerations.SemanticTokenTypes;
         Image : VSS.Strings.Virtual_String) is
      begin
         if Client.Token_Types.Contains (Image) then
            Self.Token_Types.Insert (Kind, Types.Length);

            Types.Append (Image);
         end if;
      end Append_Type;

   begin
      Append_Type (namespace, "namespace");
      Append_Type (a_type, "type");
      Append_Type (class, "class");
      Append_Type (enum, "enum");
      Append_Type (an_interface, "interface");
      Append_Type (struct, "struct");
      Append_Type (typeParameter, "typeParameter");
      Append_Type (parameter, "parameter");
      Append_Type (variable, "variable");
      Append_Type (property, "property");
      Append_Type (enumMember, "enumMember");
      --  Append_Type (event, "event");
      Append_Type (a_function, "function");
      --  Append_Type (method, "method");
      --  Append_Type (macro, "macro");
      Append_Type (keyword, "keyword");
      Append_Type (modifier, "modifier");
      Append_Type (comment, "comment");
      Append_Type (LSP.Enumerations.string, "string");
      Append_Type (number, "number");
      --  Append_Type (regexp, "regexp");
      Append_Type (operator, "operator");

      Append_Modifier (declaration, "declaration");
      Append_Modifier (definition, "definition");
      Append_Modifier (readonly, "readonly");
      Append_Modifier (static, "static");
      Append_Modifier (deprecated, "deprecated");
      Append_Modifier (an_abstract, "abstract");
      --  Append_Modifier (async, "async");
      Append_Modifier (modification, "modification");
      Append_Modifier (documentation, "documentation");
      Append_Modifier (defaultLibrary, "defaultLibrary");

      Obsolescent := +"Obsolescent";
      Ada_Package := +"Ada";
      System      := +"System";
      Interfaces := +"Interfaces";

      Is_Ghost_Root.Set (Is_Ghost_Root_Predicate'(null record));
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Holder : out Highlights_Holder;
      Unit   : Libadalang.Analysis.Analysis_Unit)
   is
      Ignore : Boolean;
   begin
      Highlights_Holders.Initialize
        (Holder.Value,
         Unit.First_Token,
         Unit.Last_Token,
         Ignore);
   end Initialize;

   ------------------------
   -- Is_Ghost_Root_Node --
   ------------------------

   function Is_Ghost_Root_Node
     (Node  : Libadalang.Analysis.Ada_Node'Class) return Boolean is
   begin
      case Node.Kind is
         when Libadalang.Common.Ada_Basic_Decl =>
            declare
               Name : constant Libadalang.Analysis.Defining_Name :=
                 Node.As_Basic_Decl.P_Defining_Name;
            begin
               return not Name.Is_Null and then Name.P_Is_Ghost_Code;
            end;
         when Libadalang.Common.Ada_Aspect_Spec =>
            --  Mark all aspects as a ghost code, because most of aspects
            --  are contract specifications.
            return True;
         when others =>
            return False;
      end case;
   exception
      when Libadalang.Common.Property_Error =>
         return False;
   end Is_Ghost_Root_Node;

   -----------------------
   -- Need_Highlighting --
   -----------------------

   function Need_Highlighting return Libadalang.Iterators.Ada_Node_Predicate is
      use Libadalang.Iterators;

   begin
      return Libadalang.Iterators.Kind_In
        (Libadalang.Common.Ada_Name'First, Libadalang.Common.Ada_Name'Last)
          or Is_Ghost_Root;
   end Need_Highlighting;

   ------------
   -- To_Int --
   ------------

   function To_Int
     (Self : Ada_Highlighter'Class;
      Set  : Highlights_Holders.Modifier_Set) return Natural
   is

      Result : Natural := 0;
   begin
      for J in Set'Range loop
         if Set (J) then
            Result := Result + Self.Token_Modifiers (J);
         end if;
      end loop;

      return Result;
   end To_Int;

end LSP.Ada_Highlighters;

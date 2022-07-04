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

with Ada.Containers.Vectors;

with Langkit_Support.Slocs;
with Langkit_Support.Token_Data_Handlers;
with Libadalang.Common;

with LSP.Common;
with LSP.Lal_Utils;

with VSS.Strings;

package body LSP.Ada_Highlighters is

   Highlighter_Debug : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("ALS.HIGHLIGHTERS.DEBUG", GNATCOLL.Traces.Off);

   package Highlights_Holders is
      type Highlights_Holder is tagged limited private;
      --  Highlights_Holder stores style for each token in the range given
      --  on initialization.

      procedure Initialize
        (Self  : in out Highlights_Holder'Class;
         From  : Libadalang.Common.Token_Reference;
         To    : Libadalang.Common.Token_Reference;
         Empty : out Boolean);
      --  Initialize holder by providing token range. If From or To is a trivia
      --  holder uses corresponding non-trivia token instead.

      procedure Set_Token_Kind
        (Self  : in out Highlights_Holder'Class;
         Token : Libadalang.Common.Token_Reference;
         Value : LSP.Messages.SemanticTokenTypes)
           with Pre => not Libadalang.Common.Is_Trivia (Token);

      procedure Set_Token_Modifier
        (Self  : in out Highlights_Holder'Class;
         Token : Libadalang.Common.Token_Reference;
         Value : LSP.Messages.SemanticTokenModifiers)
           with Pre => not Libadalang.Common.Is_Trivia (Token);

      procedure Set_Token_Modifier
        (Self  : in out Highlights_Holder'Class;
         From  : Libadalang.Common.Token_Reference;
         To    : Libadalang.Common.Token_Reference;
         Value : LSP.Messages.SemanticTokenModifiers)
           with Pre => not Libadalang.Common.Is_Trivia (From) and then
                       not Libadalang.Common.Is_Trivia (To);
      --  Set a modifier on each token in the range From .. To

      type Modifier_Set is
        array (LSP.Messages.SemanticTokenModifiers) of Boolean
          with Pack;

      Empty : constant Modifier_Set := (others => False);

      type Semantic_Token (Is_Set : Boolean := False) is record
         Modifiers  : Modifier_Set;

         case Is_Set is
            when True =>
               Kind : LSP.Messages.SemanticTokenTypes;
            when False =>
               null;
         end case;
      end record;

      function Get
        (Self  : Highlights_Holder'Class;
         Token : Libadalang.Common.Token_Reference)
           return Semantic_Token
             with Pre => not Libadalang.Common.Is_Trivia (Token);

   private

      package Semantic_Token_Vectors is new Ada.Containers.Vectors
        (Index_Type   => Langkit_Support.Token_Data_Handlers.Token_Index,
         Element_Type => Semantic_Token);

      type Highlights_Holder is tagged limited record
         First  : Langkit_Support.Token_Data_Handlers.Token_Index;
         Vector : Semantic_Token_Vectors.Vector;
      end record;
   end Highlights_Holders;

   function To_Int
     (Self : Ada_Highlighter'Class;
      Set  : Highlights_Holders.Modifier_Set)
        return LSP.Messages.uinteger
           with Inline;
   --  Cast set of modifiers to uinteger

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
         use type Libadalang.Common.Token_Reference;

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
            Self.Vector.Clear;
            Empty := True;
            return;
         end if;

         Self.First := Libadalang.Common.Index (First);
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
         Value : LSP.Messages.SemanticTokenModifiers)
      is
         use type Langkit_Support.Token_Data_Handlers.Token_Index;
         Index : constant Langkit_Support.Token_Data_Handlers.Token_Index :=
           Libadalang.Common.Index (Token) - Self.First;
      begin
         Self.Vector (Index).Modifiers (Value) := True;
      end Set_Token_Modifier;

      ------------------------
      -- Set_Token_Modifier --
      ------------------------

      procedure Set_Token_Modifier
        (Self  : in out Highlights_Holder'Class;
         From  : Libadalang.Common.Token_Reference;
         To    : Libadalang.Common.Token_Reference;
         Value : LSP.Messages.SemanticTokenModifiers)
      is
         use type Libadalang.Common.Token_Reference;
         use type Langkit_Support.Token_Data_Handlers.Token_Index;

         Token : Libadalang.Common.Token_Reference := From;
         Index : Langkit_Support.Token_Data_Handlers.Token_Index;
      begin
         loop
            if Libadalang.Common.Index (Token) >= Self.First then
               Index := Libadalang.Common.Index (Token) - Self.First;

               if Index <= Self.Vector.Last_Index then
                  Self.Vector (Index).Modifiers (Value) := True;
               end if;
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
         Value : LSP.Messages.SemanticTokenTypes)
      is
         use type Langkit_Support.Token_Data_Handlers.Token_Index;
         Index : constant Langkit_Support.Token_Data_Handlers.Token_Index :=
           Libadalang.Common.Index (Token) - Self.First;
      begin
         if Self.Vector (Index).Is_Set then
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
     (Self  : Ada_Highlighter'Class;
      Unit  : Libadalang.Analysis.Analysis_Unit;
      Trace : GNATCOLL.Traces.Trace_Handle;
      Span  : LSP.Messages.Span)
      return LSP.Messages.uinteger_Vector
   is
      use type LSP.Types.Line_Number;
      use type Libadalang.Common.Token_Reference;

      First_Token : constant Libadalang.Common.Token_Reference :=
        (if Span.last.line = 0 then Unit.First_Token
         else Unit.Lookup_Token
           ((Langkit_Support.Slocs.Line_Number (Span.first.line + 1), 1)));

      Last_Token : constant Libadalang.Common.Token_Reference :=
        (if Span.last.line = 0 then Unit.Last_Token
         else Unit.Lookup_Token
           ((Langkit_Support.Slocs.Line_Number (Span.last.line + 2), 1)));

      From_Token : constant Libadalang.Common.Token_Reference :=
        (if Libadalang.Common.Is_Trivia (First_Token)
         then Libadalang.Common.Next (First_Token, Exclude_Trivia => True)
         else First_Token);

      To_Token : constant Libadalang.Common.Token_Reference :=
        (if Libadalang.Common.Is_Trivia (Last_Token)
         then Libadalang.Common.Previous (Last_Token, Exclude_Trivia => True)
         else Last_Token);

      function Is_Ghost_Root_Node
        (Node  : Libadalang.Analysis.Ada_Node'Class) return Boolean;
      --  Check if given node is a declaration and has ghost aspect

      procedure Highlight_Token
        (Token : Libadalang.Common.Token_Reference;
         Kind  : LSP.Messages.SemanticTokenTypes);
      --  Highlight given Token with token Kind

      procedure Highlight_Token
        (Token : Libadalang.Common.Token_Reference;
         Kind  : LSP.Messages.SemanticTokenModifiers);
      --  Highlight given Token with token Kind

      function Highlight_Node
        (Node : Libadalang.Analysis.Ada_Node'Class)
         return Libadalang.Common.Visit_Status;
      --  Highlight given node

      procedure Highlight_Name
        (Node : Libadalang.Analysis.Name'Class);
      --  Highlight given name with token Kind

      procedure Get_Result
        (Holder : Highlights_Holders.Highlights_Holder;
         Result : out LSP.Messages.uinteger_Vector);

      ----------------
      -- Get_Result --
      ----------------

      procedure Get_Result
        (Holder : Highlights_Holders.Highlights_Holder;
         Result : out LSP.Messages.uinteger_Vector)
      is
         use all type LSP.Messages.SemanticTokenTypes;
         use all type Libadalang.Common.Token_Kind;
         use type Langkit_Support.Slocs.Line_Number;
         use type Langkit_Support.Slocs.Column_Number;

         subtype uint is LSP.Messages.uinteger;

         Last : Langkit_Support.Slocs.Source_Location := (1, 1);

         Token : Libadalang.Common.Token_Reference := From_Token;
      begin
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

                  Sloc_Range : constant
                    Langkit_Support.Slocs.Source_Location_Range :=
                      Libadalang.Common.Sloc_Range (Token_Data);

                  Start : constant Langkit_Support.Slocs.Source_Location :=
                    Langkit_Support.Slocs.Start_Sloc (Sloc_Range);

                  Skip  : LSP.Messages.SemanticTokenTypes renames macro;

                  Map   : constant array (Libadalang.Common.Token_Kind) of
                    LSP.Messages.SemanticTokenTypes :=
                      (Ada_All .. Ada_Xor | Ada_With => keyword,
                       Ada_Par_Close .. Ada_Target => operator,
                       Ada_String | Ada_Char => a_string,
                       Ada_Decimal | Ada_Integer => number,
                       Ada_Comment => comment,
                       Ada_Identifier => modifier,
                       others => Skip);

                  Mapped_Token : constant LSP.Messages.SemanticTokenTypes :=
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

      procedure Highlight_Name (Node : Libadalang.Analysis.Name'Class) is
         use all type LSP.Messages.SemanticTokenTypes;
         use all type LSP.Messages.SemanticTokenModifiers;
         use all type Libadalang.Common.Ada_Node_Kind_Type;
         use type Libadalang.Analysis.Defining_Name;

         function To_Kind (Decl : Libadalang.Analysis.Basic_Decl)
           return LSP.Messages.SemanticTokenTypes;

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

         -------------------
         -- Is_Predefined --
         -------------------

         function Is_Predefined (Decl : Libadalang.Analysis.Basic_Decl)
           return Boolean
         is
            Name : Libadalang.Analysis.Name :=
              Decl.P_Enclosing_Compilation_Unit.P_Decl.P_Defining_Name.F_Name;
         begin
            if LSP.Lal_Utils.Is_Synthetic (Decl) then
               return True;  --  In Standard package
            end if;

            while not Name.Is_Null and then Name.Kind = Ada_Dotted_Name loop
               Name := Name.As_Dotted_Name.F_Prefix;
            end loop;

            if not Name.Is_Null
              and then Name.Kind = Ada_Identifier
              and then (Name.P_Name_Is (Self.Ada)
                        or Name.P_Name_Is (Self.System)
                        or Name.P_Name_Is (Self.Interfaces))
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
           return LSP.Messages.SemanticTokenTypes is
         begin
            case Decl.Kind is
               when Libadalang.Common.Ada_Basic_Subp_Decl =>
                  if Decl.Kind = Ada_Enum_Literal_Decl then
                     return enumMember;
                  else
                     return a_function;
                  end if;
               when Libadalang.Common.Ada_Base_Type_Decl =>
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

                  --  Begin Base_Formal_Param_Decl
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
                  --  End Base_Formal_Param_Decl

               when Libadalang.Common.Ada_Base_Package_Decl =>
                  return namespace;

               when Libadalang.Common.Ada_Body_Node =>
                  declare
                     Spec : constant Libadalang.Analysis.Basic_Decl :=
                       Decl.As_Body_Node.P_Decl_Part (True);
                  begin
                     if Spec.Is_Null then
                        return a_function;
                     else
                        return To_Kind (Spec);
                     end if;
                  end;

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
                  return modifier;
            end case;
         end To_Kind;

         Def  : Libadalang.Analysis.Defining_Name;
         Decl : Libadalang.Analysis.Basic_Decl;
         Kind : LSP.Messages.SemanticTokenTypes;
      begin
         if Node.Kind not in Ada_Identifier | Ada_String_Literal then
            --  Highlight only identifiers and operator symbols
            return;
         end if;

         if Node.P_Is_Defining then
            Def := Node.P_Enclosing_Defining_Name;

            if not Def.Is_Null and then Def.P_Canonical_Part = Def then
               Highlight_Token (Node.Token_Start, declaration);
            else
               Highlight_Token (Node.Token_Start, definition);
            end if;
         else
            Def := Node.P_Referenced_Defining_Name (True);
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
               Highlight_Token (Node.Token_Start, Kind);

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
                  if Def.P_Has_Aspect (Self.Obsolescent)
                    or else not Def.P_Get_Pragma (Self.Obsolescent).Is_Null
                  then
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
            --  Fallback to some default for any unresolved identifier
            Highlight_Token (Node.Token_Start, modifier);
         end if;
      end Highlight_Name;

      Holder : Highlights_Holders.Highlights_Holder;

      --------------------
      -- Highlight_Node --
      --------------------

      function Highlight_Node
        (Node : Libadalang.Analysis.Ada_Node'Class)
         return Libadalang.Common.Visit_Status
      is
         use all type LSP.Messages.SemanticTokenModifiers;
      begin
         if Node.Token_End < From_Token or To_Token < Node.Token_Start then
            --  Skip uninteresting nodes to speedup traversal
            return Libadalang.Common.Over;
         elsif Is_Ghost_Root_Node (Node) then
            --  Mark all tokens in a ghost element as `documentation`
            Holder.Set_Token_Modifier
              (Node.Token_Start, Node.Token_End, documentation);
         end if;

         case Node.Kind is
            when Libadalang.Common.Ada_Name =>
               Highlight_Name (Node.As_Name);

            when others =>
               null;
         end case;

         return Libadalang.Common.Into;
      exception
         when E : Libadalang.Common.Property_Error =>
            if Highlighter_Debug.Is_Active then
               LSP.Common.Log (Trace, E, "In Highlight_Node");
            end if;
            return Libadalang.Common.Into;
      end Highlight_Node;

      ---------------------
      -- Highlight_Token --
      ---------------------

      procedure Highlight_Token
        (Token : Libadalang.Common.Token_Reference;
         Kind  : LSP.Messages.SemanticTokenTypes) is
      begin
         if Token < From_Token or To_Token < Token then
            --  Skip uninteresting tokens
            return;
         end if;

         Holder.Set_Token_Kind (Token, Kind);
      end Highlight_Token;

      ---------------------
      -- Highlight_Token --
      ---------------------

      procedure Highlight_Token
        (Token : Libadalang.Common.Token_Reference;
         Kind  : LSP.Messages.SemanticTokenModifiers) is
      begin
         if Token < From_Token or To_Token < Token then
            --  Skip uninteresting tokens
            return;
         end if;

         Holder.Set_Token_Modifier (Token, Kind);
      end Highlight_Token;

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
                  return (not Name.Is_Null) and then Name.P_Is_Ghost_Code;
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

      Root  : constant Libadalang.Analysis.Ada_Node :=
        Libadalang.Analysis.Root (Unit);

      Empty  : Boolean;

   begin
      if Root.Is_Null or else
        Libadalang.Common.No_Token in From_Token | To_Token
      then
         --  No tokens to highlight
         return LSP.Messages.Empty;
      end if;

      Holder.Initialize (From_Token, To_Token, Empty);

      if Empty then
         return LSP.Messages.Empty;
      end if;

      --  Traverse whole tree, look for intresting nodes and mark their
      --  tokens in Holder for further processing
      Root.Traverse (Highlight_Node'Access);

      return Result : LSP.Messages.uinteger_Vector do
         Get_Result (Holder, Result);
      end return;
   end Get_Tokens;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : in out Ada_Highlighter'Class;
      Client : LSP.Messages.SemanticTokensClientCapabilities;
      Legend : out LSP.Messages.SemanticTokensLegend)
   is
      use all type LSP.Messages.SemanticTokenTypes;
      use all type LSP.Messages.SemanticTokenModifiers;

      procedure Append_Type
        (Kind  : LSP.Messages.SemanticTokenTypes;
         Image : VSS.Strings.Virtual_String);
      --  Update Legend.tokenTypes if client understands given Kind

      procedure Append_Modifier
        (Kind  : LSP.Messages.SemanticTokenModifiers;
         Image : VSS.Strings.Virtual_String);
      --  Update Legend.tokenModifiers if client understands given Kind

      function "+" (Source : Wide_Wide_String) return Unbounded_Text_Type
        renames Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String;

      procedure Append_Modifier
        (Kind  : LSP.Messages.SemanticTokenModifiers;
         Image : VSS.Strings.Virtual_String) is
      begin
         if Client.tokenModifiers.Contains (Kind) then
            Self.Token_Modifiers.Insert
              (Kind,
               LSP.Messages.uinteger (2 ** Legend.tokenModifiers.Length));

            Legend.tokenModifiers.Append (Image);
         end if;
      end Append_Modifier;

      -----------------
      -- Append_Type --
      -----------------

      procedure Append_Type
        (Kind  : LSP.Messages.SemanticTokenTypes;
         Image : VSS.Strings.Virtual_String) is
      begin
         if Client.tokenTypes.Contains (Kind) then
            Self.Token_Types.Insert
              (Kind,
               LSP.Messages.uinteger (Legend.tokenTypes.Length));

            Legend.tokenTypes.Append (Image);
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
      Append_Type (a_string, "string");
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

      Self.Obsolescent := +"Obsolescent";
      Self.Ada := +"Ada";
      Self.System := +"System";
      Self.Interfaces := +"Interfaces";
   end Initialize;

   ------------
   -- To_Int --
   ------------

   function To_Int
     (Self : Ada_Highlighter'Class;
      Set  : Highlights_Holders.Modifier_Set)
        return LSP.Messages.uinteger
   is
      use type LSP.Messages.uinteger;

      Result : LSP.Messages.uinteger := 0;
   begin
      for J in Set'Range loop
         if Set (J) then
            Result := Result + Self.Token_Modifiers (J);
         end if;
      end loop;

      return Result;
   end To_Int;

end LSP.Ada_Highlighters;

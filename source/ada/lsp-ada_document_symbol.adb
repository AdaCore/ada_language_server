------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2024, AdaCore                       --
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

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation;

with GNATCOLL.VFS;

with Langkit_Support.Text;
with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Iterators;

with VSS.Characters;
with VSS.Strings;

with LSP.Ada_Context_Sets;
with LSP.Ada_Request_Jobs;
with LSP.Client_Message_Receivers;
with LSP.Constants;
with LSP.Enumerations;
with LSP.Search;
with LSP.Server_Requests.DocumentSymbol;
with LSP.Structures;
with LSP.Utils;

package body LSP.Ada_Document_Symbol is

   type Search_Pattern_Access is access LSP.Search.Search_Pattern'Class;

   procedure Free is new Ada.Unchecked_Deallocation
     (LSP.Search.Search_Pattern'Class, Search_Pattern_Access);

   type Traverse_Iterator_Access is access
     Libadalang.Iterators.Traverse_Iterator'Class;

   procedure Free is new Ada.Unchecked_Deallocation
     (Libadalang.Iterators.Traverse_Iterator'Class, Traverse_Iterator_Access);

   type Flat_Document_Symbol_Job
     (Parent : not null access constant Ada_Document_Symbol_Handler) is limited
   new LSP.Ada_Request_Jobs.Ada_Request_Job
     (Priority => LSP.Server_Jobs.Low)
   with record
      Pattern  : Search_Pattern_Access;
      Cursor   : Traverse_Iterator_Access;
      Response : LSP.Structures.DocumentSymbol_Result;
   end record;

   overriding procedure Execute_Ada_Request
     (Self   : in out Flat_Document_Symbol_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   type Stack_Item is record
      Node     : Libadalang.Analysis.Ada_Node;
      Children : LSP.Structures.DocumentSymbol_Vector;
   end record;

   package Stack_Item_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Stack_Item);

   type Full_Document_Symbol_Job
     (Parent : not null access constant Ada_Document_Symbol_Handler) is limited
   new LSP.Ada_Request_Jobs.Ada_Request_Job
     (Priority => LSP.Server_Jobs.Low)
   with record
      Pattern  : Search_Pattern_Access;
      Node     : Libadalang.Analysis.Ada_Node;
      Stack    : Stack_Item_Lists.List;
   end record;

   overriding procedure Execute_Ada_Request
     (Self   : in out Full_Document_Symbol_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   function "or"
     (Left  : LSP.Structures.AlsSearchKind_Optional;
      Right : LSP.Enumerations.AlsSearchKind)
        return LSP.Enumerations.AlsSearchKind is
          (if Left.Is_Set then Left.Value else Right);

   function Get_Profile
     (Node : Libadalang.Analysis.Basic_Decl) return VSS.Strings.Virtual_String;

   function Is_Function
     (Node : Libadalang.Analysis.Basic_Decl) return Boolean;

   function Get_Visibility
     (Node : Libadalang.Analysis.Basic_Decl)
      return LSP.Structures.AlsVisibility_Optional;

   function Is_Declaration (Node : Libadalang.Analysis.Ada_Node)
      return LSP.Structures.Boolean_Optional is
        (case Node.Kind is
         when Libadalang.Common.Ada_Base_Package_Decl |
              Libadalang.Common.Ada_Generic_Package_Decl |
              Libadalang.Common.Ada_Generic_Package_Instantiation |
              Libadalang.Common.Ada_Generic_Package_Renaming_Decl |
              Libadalang.Common.Ada_Package_Renaming_Decl |
              Libadalang.Common.Ada_Abstract_Subp_Decl |
              Libadalang.Common.Ada_Formal_Subp_Decl |
              Libadalang.Common.Ada_Subp_Decl |
              Libadalang.Common.Ada_Subp_Renaming_Decl |
              Libadalang.Common.Ada_Generic_Subp_Instantiation |
              Libadalang.Common.Ada_Generic_Subp_Renaming_Decl |
              Libadalang.Common.Ada_Generic_Subp_Decl |
              Libadalang.Common.Ada_Null_Subp_Decl |
              Libadalang.Common.Ada_Expr_Function |
              Libadalang.Common.Ada_Protected_Type_Decl |
              Libadalang.Common.Ada_Single_Protected_Decl |
              Libadalang.Common.Ada_Entry_Decl |
              Libadalang.Common.Ada_Type_Decl |
              Libadalang.Common.Ada_Single_Task_Decl |
              Libadalang.Common.Ada_Task_Type_Decl =>

               (Is_Set => True, Value => True),

            when others =>
               (Is_Set => True, Value => False));

   ----------------
   -- Create_Job --
   ----------------

   overriding function Create_Job
     (Self    : Ada_Document_Symbol_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
        return LSP.Server_Jobs.Server_Job_Access
   is
      use type LSP.Structures.Boolean_Optional;

      Value : LSP.Structures.DocumentSymbolParams
        renames LSP.Server_Requests.DocumentSymbol.Request
          (Message.all).Params;

      File : constant GNATCOLL.VFS.Virtual_File :=
        Self.Context.To_File (Value.textDocument.uri);

      Context : constant LSP.Ada_Context_Sets.Context_Access :=
        Self.Context.Get_Best_Context (Value.textDocument.uri);

      Unit : constant Libadalang.Analysis.Analysis_Unit :=
        Context.Get_AU (File);

      Is_Defining_Name : constant Libadalang.Iterators.Ada_Node_Predicate :=
        Libadalang.Iterators.Kind_Is (Libadalang.Common.Ada_Defining_Name);
      --  This object will be deallocated by Cursor's finalization

      function Flat_Job return LSP.Server_Jobs.Server_Job_Access is
        (new Flat_Document_Symbol_Job'
           (Parent  => Self'Unchecked_Access,
            Request => LSP.Ada_Request_Jobs.Request_Access (Message),
            Cursor  => new Libadalang.Iterators.Traverse_Iterator'Class'
              (Libadalang.Iterators.Find (Unit.Root, Is_Defining_Name)),
            Pattern => new LSP.Search.Search_Pattern'Class'
              (LSP.Search.Build
                 (Pattern        => Value.query,
                  Case_Sensitive => Value.case_sensitive = LSP.Constants.True,
                  Whole_Word     => Value.whole_word = LSP.Constants.True,
                  Negate         => Value.negate = LSP.Constants.True,
                  Kind           => Value.kind
                                     or LSP.Enumerations.Start_Word_Text)),
            Response => <>));

      function Full_Job return LSP.Server_Jobs.Server_Job_Access is
        (new Full_Document_Symbol_Job'
           (Parent  => Self'Unchecked_Access,
            Request => LSP.Ada_Request_Jobs.Request_Access (Message),
            Node    => Unit.Root,
            Stack   => [(Node     => Libadalang.Analysis.No_Ada_Node,
                         Children => <>)],
            Pattern => new LSP.Search.Search_Pattern'Class'
              (LSP.Search.Build
                 (Pattern        => Value.query,
                  Case_Sensitive => Value.case_sensitive = LSP.Constants.True,
                  Whole_Word     => Value.whole_word = LSP.Constants.True,
                  Negate         => Value.negate = LSP.Constants.True,
                  Kind           => Value.kind
                                     or LSP.Enumerations.Start_Word_Text))));
   begin
      if Self.Context.Client.Hierarchical_Symbol then
         return Full_Job;
      else
         return Flat_Job;
      end if;
   end Create_Job;

   -------------------------
   -- Execute_Ada_Request --
   -------------------------

   overriding procedure Execute_Ada_Request
     (Self   : in out Flat_Document_Symbol_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is
      Message : LSP.Server_Requests.DocumentSymbol.Request
        renames LSP.Server_Requests.DocumentSymbol.Request (Self.Message.all);

      Element : Libadalang.Analysis.Ada_Node;
   begin
      if Self.Cursor.Next (Element) then
         declare
            use type LSP.Enumerations.SymbolKind;

            Item : LSP.Structures.SymbolInformation;
            Kind : constant LSP.Enumerations.SymbolKind :=
              LSP.Utils.Get_Decl_Kind
                (Element.As_Defining_Name.P_Basic_Decl, Ignore_Local => True);
         begin
            if Kind /= LSP.Enumerations.A_Null
              and then Self.Pattern.Match
                (VSS.Strings.To_Virtual_String (Element.Text))
            then
               Item :=
                 (name              =>
                    VSS.Strings.To_Virtual_String (Element.Text),
                  kind              => Kind,
                  tags              => LSP.Constants.Empty,
                  deprecated        => <>,
                  location          =>
                    Self.Parent.Context.To_LSP_Location (Element),
                  containerName     => <>);

               Self.Response.Variant_1.Append (Item);
            end if;

            Status := LSP.Server_Jobs.Continue;
         end;
      else
         Client.On_DocumentSymbol_Response (Message.Id, Self.Response);

         Free (Self.Pattern);
         Free (Self.Cursor);
         Status := LSP.Server_Jobs.Done;
      end if;
   end Execute_Ada_Request;

   -------------------------
   -- Execute_Ada_Request --
   -------------------------

   overriding procedure Execute_Ada_Request
     (Self   : in out Full_Document_Symbol_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is
      use type Ada.Containers.Count_Type;

      function Is_Leaf_Symbol
        (Node : Libadalang.Analysis.Ada_Node) return Boolean;
      --  Node has a symbol and can't contain nested symbols.
      --  Also return True for uninteresting symbols to avoid
      --  descend under their subtrees

      function Is_Namespace_Symbol
        (Node : Libadalang.Analysis.Ada_Node) return Boolean;
      --  Node has a symbol and may contain nested symbols

      procedure Skip_Node (Node : in out Libadalang.Analysis.Ada_Node);
      --  If we are leaving the node at top of the stack, then append
      --  namespace symbol to the next stack element.
      --  Go to node sibling if any or skip the its parent otherwise.

      procedure Continue (Node : in out Libadalang.Analysis.Ada_Node);
      --  Set Node to Node.First_Child if any, do Skip_Node (Node) otherwise

      procedure Append_Leaf_Symbol (Node : Libadalang.Analysis.Ada_Node);
      procedure Append_Namespace_Symbol
        (Node     : Libadalang.Analysis.Ada_Node;
         Children : in out LSP.Structures.DocumentSymbol_Vector);

      ------------------------
      -- Append_Leaf_Symbol --
      ------------------------

      procedure Append_Leaf_Symbol (Node : Libadalang.Analysis.Ada_Node) is

         procedure Append_Name
           (Name   : Libadalang.Analysis.Name'Class;
            Kind   : LSP.Enumerations.SymbolKind;
            Detail : VSS.Strings.Virtual_String :=
              VSS.Strings.Empty_Virtual_String);

         procedure Append_Name
           (Name   : Libadalang.Analysis.Name'Class;
            Kind   : LSP.Enumerations.SymbolKind;
            Detail : VSS.Strings.Virtual_String :=
              VSS.Strings.Empty_Virtual_String)
         is
            Node_Span : constant LSP.Structures.A_Range :=
              Self.Parent.Context.To_LSP_Location (Node).a_range;

            Name_Span : constant LSP.Structures.A_Range :=
              Self.Parent.Context.To_LSP_Location (Name).a_range;

            Top       : Stack_Item renames Self.Stack (Self.Stack.Last);

            Item      : constant LSP.Structures.DocumentSymbol :=
              (name           => VSS.Strings.To_Virtual_String (Name.Text),
               detail         => Detail,
               kind           => Kind,
               a_range        => Node_Span,
               selectionRange => Name_Span,
               others         => <>);
         begin
            Top.Children.Append (Item);
         end Append_Name;

      begin
         case Node.Kind is
            when Libadalang.Common.Ada_With_Clause_Range =>
               for Name of Node.As_With_Clause.F_Packages loop
                  Append_Name (Name, LSP.Enumerations.Namespace);
               end loop;

            when Libadalang.Common.Ada_Pragma_Node =>
               if Self.Stack.Length < 3 then
                  declare
                     Pragma_Node : constant Libadalang.Analysis.Pragma_Node :=
                       Node.As_Pragma_Node;
                  begin
                     if not
                       (Pragma_Node.F_Id.Is_Null
                        and then Pragma_Node.F_Args.Is_Null)
                     then
                        Append_Name
                          (Node.As_Pragma_Node.F_Id,
                           Kind   => LSP.Enumerations.Property,
                           Detail =>
                             VSS.Strings.To_Virtual_String
                               ("("
                                & Node.As_Pragma_Node.F_Args.Text & ")"));
                     end if;
                  end;
               end if;

            when others =>
               null;  --  Ignore other nodes filtered by Is_Leaf_Symbol
         end case;
      end Append_Leaf_Symbol;

      -----------------------------
      -- Append_Namespace_Symbol --
      -----------------------------

      procedure Append_Namespace_Symbol
        (Node     : Libadalang.Analysis.Ada_Node;
         Children : in out LSP.Structures.DocumentSymbol_Vector)
      is
         procedure Append_Name
           (Name    : Libadalang.Analysis.Ada_Node'Class;
            Text    : VSS.Strings.Virtual_String;
            Kind    : LSP.Enumerations.SymbolKind;
            Is_Proc : LSP.Structures.Boolean_Optional := (Is_Set => False);
            Visible : LSP.Structures.AlsVisibility_Optional :=
              (Is_Set => False);
            Detail  : VSS.Strings.Virtual_String :=
              VSS.Strings.Empty_Virtual_String);

         procedure Append_Name
           (Name    : Libadalang.Analysis.Ada_Node'Class;
            Text    : VSS.Strings.Virtual_String;
            Kind    : LSP.Enumerations.SymbolKind;
            Is_Proc : LSP.Structures.Boolean_Optional := (Is_Set => False);
            Visible : LSP.Structures.AlsVisibility_Optional :=
              (Is_Set => False);
            Detail  : VSS.Strings.Virtual_String :=
              VSS.Strings.Empty_Virtual_String)
         is
            Node_Span : constant LSP.Structures.A_Range :=
              Self.Parent.Context.To_LSP_Location (Node).a_range;

            Name_Span : constant LSP.Structures.A_Range :=
              Self.Parent.Context.To_LSP_Location (Name).a_range;

            Top       : Stack_Item renames Self.Stack (Self.Stack.Last);

            Item      : constant LSP.Structures.DocumentSymbol :=
              (name              => Text,
               detail            => Detail,
               kind              => Kind,
               a_range           => Node_Span,
               selectionRange    => Name_Span,
               children          => Children,
               alsIsDeclaration  => Is_Declaration (Node),
               alsIsAdaProcedure => Is_Proc,
               alsVisibility     => Visible,
               others            => <>);
         begin
            if Self.Pattern.Match (Text) then
               Top.Children.Append (Item);
            end if;
         end Append_Name;

      begin
         case Node.Kind is
            when Libadalang.Common.Ada_Ada_Node_List_Range =>
               Append_Name
                 (Name => Node.As_Ada_Node_List.Last_Child,
                  Text => "With clauses",
                  Kind => LSP.Enumerations.Namespace);

            when Libadalang.Common.Ada_Basic_Decl =>
               for Name of Node.As_Basic_Decl.P_Defining_Names loop

                  exit when Name.Is_Null;

                  Append_Name
                    (Name   => Name,
                     Text   => VSS.Strings.To_Virtual_String (Name.Text),
                     Kind   => LSP.Utils.Get_Decl_Kind
                       (Node.As_Basic_Decl,
                        Ignore_Local => Self.Stack.Length > 2),
                     Is_Proc => (if Is_Function (Node.As_Basic_Decl)
                                 then (Is_Set => False)
                                 else (Is_Set => True, Value => True)),
                     Visible => Get_Visibility (Node.As_Basic_Decl),
                     Detail => Get_Profile (Node.As_Basic_Decl));
               end loop;

            when others =>
               null;  --  Unexpected
         end case;
      end Append_Namespace_Symbol;

      --------------------
      -- Is_Leaf_Symbol --
      --------------------

      function Is_Leaf_Symbol
        (Node : Libadalang.Analysis.Ada_Node) return Boolean is
      begin
         case Node.Kind is
            when Libadalang.Common.Ada_With_Clause_Range =>
               return True;
            when Libadalang.Common.Ada_Pragma_Node =>
               return True;
            when Libadalang.Common.Ada_Basic_Decl =>
               declare
                  use type LSP.Enumerations.SymbolKind;

                  Decl : constant Libadalang.Analysis.Basic_Decl :=
                    Node.As_Basic_Decl;

                  Kind : constant LSP.Enumerations.SymbolKind :=
                    LSP.Utils.Get_Decl_Kind
                      (Decl, Ignore_Local => Self.Stack.Length > 2);
               begin
                  return Kind = LSP.Enumerations.A_Null;
               end;

            when others =>
               return False;
         end case;
      end Is_Leaf_Symbol;

      -------------------------
      -- Is_Namespace_Symbol --
      -------------------------

      function Is_Namespace_Symbol
        (Node : Libadalang.Analysis.Ada_Node) return Boolean is
      begin
         case Node.Kind is
            when Libadalang.Common.Ada_Ada_Node_List_Range =>
               --  An artifical "With clauses" node
               return Self.Stack.Length < 2
                 and then Node.As_Ada_Node_List.Ada_Node_List_Has_Element (1);

            when Libadalang.Common.Ada_Basic_Decl =>
               return True;

            when others =>
               return False;
         end case;
      end Is_Namespace_Symbol;

      ---------------
      -- Skip_Node --
      ---------------

      procedure Skip_Node (Node : in out Libadalang.Analysis.Ada_Node) is
         use type Libadalang.Analysis.Ada_Node;

         function Next_Sibling return Libadalang.Analysis.Ada_Node;

         Parent : constant Libadalang.Analysis.Ada_Node := Node.Parent;

         ------------------
         -- Next_Sibling --
         ------------------

         function Next_Sibling return Libadalang.Analysis.Ada_Node is
            Index : constant Positive := Node.Child_Index + 1;
            --  Turn 0-based Child_Index into 1-based child index
            Result : Libadalang.Analysis.Ada_Node;
         begin
            for J in Index + 1 .. Parent.Children_Count loop
               Result := Parent.Child (J);

               exit when not Result.Is_Null;
            end loop;

            return Result;
         end Next_Sibling;

         Sibling : constant Libadalang.Analysis.Ada_Node :=
           (if Parent.Is_Null then Parent else Next_Sibling);
      begin
         --  We are leaving Node, so check if it is on the top os the stack
         if Self.Stack.Last_Element.Node = Node then
            declare
               Children : LSP.Structures.DocumentSymbol_Vector :=
                 Self.Stack (Self.Stack.Last).Children;
            begin
               Self.Stack.Delete_Last;
               Append_Namespace_Symbol (Node, Children);
            end;
         end if;

         if Sibling.Is_Null then
            Node := Parent;

            if not Parent.Is_Null then
               Skip_Node (Node);
            end if;
         else
            Node := Sibling;
         end if;
      end Skip_Node;

      --------------
      -- Continue --
      --------------

      procedure Continue (Node : in out Libadalang.Analysis.Ada_Node) is
         Next : constant Libadalang.Analysis.Ada_Node := Self.Node.First_Child;
      begin
         if Next.Is_Null then
            Skip_Node (Node);
         else
            Node := Next;
         end if;
      end Continue;

      Message : LSP.Server_Requests.DocumentSymbol.Request
        renames LSP.Server_Requests.DocumentSymbol.Request (Self.Message.all);

   begin
      Status := LSP.Server_Jobs.Continue;

      while not Self.Node.Is_Null loop
         if Is_Leaf_Symbol (Self.Node) then
            Append_Leaf_Symbol (Self.Node);
            Skip_Node (Self.Node);

            exit;

         elsif Is_Namespace_Symbol (Self.Node) then
            Self.Stack.Append ((Node => Self.Node, Children => <>));
            Continue (Self.Node);

         else
            Continue (Self.Node);
         end if;
      end loop;

      if Self.Node.Is_Null then
         Free (Self.Pattern);
         Client.On_DocumentSymbol_Response
           (Message.Id,
            (Kind      => LSP.Structures.Variant_2,
             Variant_2 => Self.Stack.Last_Element.Children));
         Status := LSP.Server_Jobs.Done;
      end if;
   end Execute_Ada_Request;

   -----------------
   -- Get_Profile --
   -----------------

   function Get_Profile
     (Node : Libadalang.Analysis.Basic_Decl) return VSS.Strings.Virtual_String
   is
      use Libadalang.Analysis;
      use Libadalang.Common;

      function To_Text
        (Node : Ada_Node'Class) return VSS.Strings.Virtual_String;
      --  Retrieve the node text and format it

      function To_Profile
        (Node : Libadalang.Analysis.Subp_Spec'Class)
         return VSS.Strings.Virtual_String;

      -------------
      -- To_Text --
      -------------

      function To_Text
        (Node : Ada_Node'Class) return VSS.Strings.Virtual_String
      is
         Node_Text : constant Langkit_Support.Text.Text_Type := Node.Text;
         Was_Space : Boolean := False;
         Result    : VSS.Strings.Virtual_String;
      begin
         for I in Node_Text'Range loop
            if Node_Text (I) = ' ' then
               --  Trim multiple whitespace to only keep one

               if not Was_Space then
                  Result.Append
                    (VSS.Characters.Virtual_Character (Node_Text (I)));
               end if;

               Was_Space := True;

               --  Remove the new line character

            elsif Node_Text (I) /= Ada.Characters.Wide_Wide_Latin_1.LF then
               Was_Space := False;
                  Result.Append
                    (VSS.Characters.Virtual_Character (Node_Text (I)));
            end if;
         end loop;

         return Result;
      end To_Text;

      ----------------
      -- To_Profile --
      ----------------

      function To_Profile
        (Node : Libadalang.Analysis.Subp_Spec'Class)
         return VSS.Strings.Virtual_String
      is
         Result  : VSS.Strings.Virtual_String;
         Params  : constant Param_Spec_Array := Node.P_Params;
         Returns : constant Type_Expr := Node.F_Subp_Returns;

      begin
         if Params'Length > 0 then
            Result.Append ('(');
         end if;

         for Param of Params loop
            declare
               use type VSS.Strings.Character_Count;

               Names : constant Defining_Name_List := Param.F_Ids;
               Init  : constant Expr := Param.F_Default_Expr;
               Item  : VSS.Strings.Virtual_String;
               Mode  : constant Ada_Mode := Param.F_Mode;

            begin
               Item.Append (" :");

               case Mode is
                  when Ada_Mode_Default | Ada_Mode_In =>
                     Item.Append (" in ");
                  when Ada_Mode_In_Out =>
                     Item.Append (" in out ");
                  when Ada_Mode_Out =>
                     Item.Append (" out ");
               end case;

               Item.Append (To_Text (Param.F_Type_Expr));

               if not Init.Is_Null then
                  Item.Append (" := ");
                  Item.Append (To_Text (Init));
               end if;

               for J in Names.First_Child_Index .. Names.Last_Child_Index loop
                  if Result.Character_Length /= 1 then
                     Result.Append ("; ");
                  end if;

                  Result.Append (To_Text (Names.Child (J)));
                  Result.Append (Item);
               end loop;
            end;
         end loop;

         if Params'Length > 0 then
            Result.Append (')');
         end if;

         if not Returns.Is_Null then
            Result.Append (" return ");
            Result.Append (To_Text (Returns));
         end if;

         return Result;
      end To_Profile;

   begin
      case Node.Kind is
         when Ada_Classic_Subp_Decl =>
            return To_Profile (Node.As_Classic_Subp_Decl.F_Subp_Spec);
         when Ada_Base_Subp_Body    =>
            return To_Profile (Node.As_Base_Subp_Body.F_Subp_Spec);
         when Ada_Generic_Subp_Decl =>
            return To_Profile
              (Node.As_Generic_Subp_Decl.F_Subp_Decl.F_Subp_Spec);
         when others =>
            return VSS.Strings.Empty_Virtual_String;
      end case;
   end Get_Profile;

   --------------------
   -- Get_Visibility --
   --------------------

   function Get_Visibility
     (Node : Libadalang.Analysis.Basic_Decl)
      return LSP.Structures.AlsVisibility_Optional
   is
      use Libadalang.Common;
   begin
      for Parent of Node.Parents loop
         if Parent.Kind = Ada_Private_Part then
            return (True, LSP.Enumerations.Als_Private);
         elsif Parent.Kind in Ada_Protected_Body | Ada_Protected_Def then
            return (True, LSP.Enumerations.Als_Protected);
         end if;
      end loop;
      return (True, LSP.Enumerations.Als_Public);
   end Get_Visibility;

   -----------------
   -- Is_Function --
   -----------------

   function Is_Function
     (Node : Libadalang.Analysis.Basic_Decl) return Boolean
   is

      function Has_Returns
        (Node : Libadalang.Analysis.Subp_Spec'Class)
         return Boolean is (not Node.F_Subp_Returns.Is_Null);

   begin
      case Node.Kind is
         when Libadalang.Common.Ada_Classic_Subp_Decl =>
            return Has_Returns (Node.As_Classic_Subp_Decl.F_Subp_Spec);

         when Libadalang.Common.Ada_Base_Subp_Body    =>
            return Has_Returns (Node.As_Base_Subp_Body.F_Subp_Spec);

         when Libadalang.Common.Ada_Generic_Subp_Decl =>
            return Has_Returns
              (Node.As_Generic_Subp_Decl.F_Subp_Decl.F_Subp_Spec);

         when others =>
            return False;
      end case;
   end Is_Function;

end LSP.Ada_Document_Symbol;

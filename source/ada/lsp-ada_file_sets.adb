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

with Langkit_Support.Symbols;     use Langkit_Support.Symbols;
with Libadalang.Common;           use Libadalang.Common;
with Libadalang.Iterators;
with Libadalang.Sources;

package body LSP.Ada_File_Sets is

   procedure Flush_File_Index
     (Self : in out Indexed_File_Set'Class;
      URI  : LSP.Messages.DocumentUri);
   --  Remove all names defined in the Unit (identified by URI) from the
   --  internal symbol index.

   type In_Private_Or_Body_Predicate is
     new Libadalang.Iterators.Ada_Node_Predicate_Interface with null record;
   --  A custom node predicate to filter private parts and bodies subtree.

   overriding function Evaluate
     (Ignore : in out In_Private_Or_Body_Predicate;
      Node   : Libadalang.Analysis.Ada_Node) return Boolean;
   --  Evaluate the In_Private_Or_Body_Predicate filter

   type Restricted_Decl_Kind_Predicate is
     new Libadalang.Iterators.Ada_Node_Predicate_Interface with null record;
   --  A custom node predicate to filter some declaration kinds.

   overriding function Evaluate
     (Ignore : in out Restricted_Decl_Kind_Predicate;
      Node   : Libadalang.Analysis.Ada_Node) return Boolean;
   --  Evaluate the Restricted_Decl_Kind_Predicate filter

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Indexed_File_Set'Class) is
   begin
      Self.Files.Clear;
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains
     (Self : Indexed_File_Set'Class;
      File : GNATCOLL.VFS.Virtual_File) return Boolean is
   begin
      return Self.Files.Contains (File);
   end Contains;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Ignore : in out In_Private_Or_Body_Predicate;
      Node   : Libadalang.Analysis.Ada_Node) return Boolean
   is
      Decl : constant Libadalang.Analysis.Basic_Decl :=
        Node.As_Defining_Name.P_Basic_Decl;
      Next : Libadalang.Analysis.Ada_Node := Decl.Parent;
   begin
      while not Next.Is_Null loop
         if Next.Kind in
           Libadalang.Common.Ada_Body_Node
           | Libadalang.Common.Ada_Private_Part
         then
            return True;
         end if;

         Next := Next.Parent;
      end loop;

      return False;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Ignore : in out Restricted_Decl_Kind_Predicate;
      Node   : Libadalang.Analysis.Ada_Node) return Boolean
   is
      Decl : constant Libadalang.Analysis.Basic_Decl :=
        Node.As_Defining_Name.P_Basic_Decl;
   begin
      if not Decl.Is_Null and then
        Decl.Kind in Libadalang.Common.Ada_For_Loop_Var_Decl
          | Libadalang.Common.Ada_Base_Formal_Param_Decl
          | Libadalang.Common.Ada_Anonymous_Expr_Decl
          | Libadalang.Common.Ada_Exception_Handler
          | Libadalang.Common.Ada_Label_Decl
          | Libadalang.Common.Ada_Named_Stmt_Decl
          | Libadalang.Common.Ada_Entry_Index_Spec
          | Libadalang.Common.Ada_Entry_Decl
      then
         return True;
      elsif not Decl.Is_Null and then
        Decl.Kind in Libadalang.Common.Ada_Object_Decl and then
        Decl.Parent.Kind = Libadalang.Common.Ada_Generic_Formal_Obj_Decl
      then
         --  This is a special case for the formal_object_declaration
         return True;
      end if;

      return False;
   end Evaluate;

   ----------------------
   -- Flush_File_Index --
   ----------------------

   procedure Flush_File_Index
     (Self : in out Indexed_File_Set'Class;
      URI  : LSP.Messages.DocumentUri)
   is
      use type LSP.Messages.DocumentUri;
      Index : Positive;
   begin
      --  Delete all Defining_Names with given URI
      for Vector of Self.All_Symbols loop
         Index := 1;

         while Index <= Vector.Last_Index loop
            if Vector (Index).URI = URI then
               Vector.Swap (Index, Vector.Last_Index);
               Vector.Delete_Last;
            else
               Index := Index + 1;
            end if;
         end loop;
      end loop;
   end Flush_File_Index;

   -------------------------------
   -- Get_Any_Symbol_Completion --
   -------------------------------

   procedure Get_Any_Symbol_Completion
     (Self     : Indexed_File_Set'Class;
      Prefix   : VSS.Strings.Virtual_String;
      Callback : not null access procedure
        (URI  : LSP.Messages.DocumentUri;
         Name : Libadalang.Analysis.Defining_Name;
         Stop : in out Boolean))
   is
      Stop   : Boolean := False;
      Cursor : Symbol_Maps.Cursor :=
        Self.All_Symbols.Ceiling (Prefix);
   begin
      Each_Prefix :
      while Symbol_Maps.Has_Element (Cursor) loop
         declare
            Value : constant VSS.Strings.Virtual_String :=
              Symbol_Maps.Key (Cursor);
         begin
            exit Each_Prefix when not Value.Starts_With (Prefix);

            for Item of Self.All_Symbols (Cursor) loop
               Callback (Item.URI, Item.Name, Stop);
               exit Each_Prefix when Stop;
            end loop;

            Symbol_Maps.Next (Cursor);
         end;
      end loop Each_Prefix;
   end Get_Any_Symbol_Completion;

   -------------
   -- Include --
   -------------

   procedure Include
     (Self : in out Indexed_File_Set'Class;
      File : GNATCOLL.VFS.Virtual_File) is
   begin
      Self.Files.Include (File);
   end Include;

   ----------------
   -- Index_File --
   ----------------

   procedure Index_File
     (Self : in out Indexed_File_Set'Class;
      URI  : LSP.Messages.DocumentUri;
      Unit : Libadalang.Analysis.Analysis_Unit)
   is
      use Libadalang.Iterators;

      Inserted   : Boolean;
      Ignore     : String_Sets.Cursor;
      Node       : Libadalang.Analysis.Ada_Node;

      function In_Private_Or_Body return
        Libadalang.Iterators.Ada_Node_Predicate;

      function Restricted_Decl_Kind return
        Libadalang.Iterators.Ada_Node_Predicate;

      ------------------------
      -- In_Private_Or_Body --
      ------------------------

      function In_Private_Or_Body return
        Libadalang.Iterators.Ada_Node_Predicate is
      begin
         return Result : Libadalang.Iterators.Ada_Node_Predicate do
            Result.Set (In_Private_Or_Body_Predicate'(null record));
         end return;
      end In_Private_Or_Body;

      --------------------------
      -- Restricted_Decl_Kind --
      --------------------------

      function Restricted_Decl_Kind return
        Libadalang.Iterators.Ada_Node_Predicate is
      begin
         return Result : Libadalang.Iterators.Ada_Node_Predicate do
            Result.Set (Restricted_Decl_Kind_Predicate'(null record));
         end return;
      end Restricted_Decl_Kind;

      --  Find all definings names excluding private parts and bodies content
      It : Libadalang.Iterators.Traverse_Iterator'Class :=
        Libadalang.Iterators.Find
          (Unit.Root,
           Libadalang.Iterators.Kind_Is (Ada_Defining_Name)
             and not In_Private_Or_Body
             and not Restricted_Decl_Kind);
   begin
      Self.Indexed.Insert (URI, Ignore, Inserted);

      if not Inserted then
         --  URI has been indexed already, clear index from names of the unit
         Self.Flush_File_Index (URI);
      end if;

      while It.Next (Node) loop
         declare
            Text : constant Wide_Wide_String :=
              Libadalang.Common.Text (Node.Token_End);

            Canonical : constant Symbolization_Result :=
              Libadalang.Sources.Canonicalize (Text);

            Inserted  : Boolean;
            Cursor    : Symbol_Maps.Cursor;
         begin
            if Canonical.Success then
               Self.All_Symbols.Insert
                 (VSS.Strings.To_Virtual_String (Canonical.Symbol),
                  Name_Vectors.Empty_Vector,
                  Cursor,
                  Inserted);

               Self.All_Symbols (Cursor).Append ((URI, Node.As_Defining_Name));
            end if;
         end;
      end loop;
   end Index_File;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Self : Indexed_File_Set'Class)
      return File_Sets.Set_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Self.Files.Iterate;
   end Iterate;

   ------------
   -- Length --
   ------------

   function Length (Self : Indexed_File_Set'Class) return Natural is
   begin
      return Natural (Self.Files.Length);
   end Length;

end LSP.Ada_File_Sets;

------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

with LSP.Lal_Utils;
with LSP.Messages;

package body LSP.Ada_File_Sets is

   procedure Flush_File_Index
     (Self : in out Indexed_File_Set'Class;
      File : GNATCOLL.VFS.Virtual_File);
   --  Remove all names defined in the unit (identified by File) from the
   --  internal symbol index.

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

   ----------------------
   -- Flush_File_Index --
   ----------------------

   procedure Flush_File_Index
     (Self : in out Indexed_File_Set'Class;
      File : GNATCOLL.VFS.Virtual_File)
   is
      use type GNATCOLL.VFS.Virtual_File;
      Index : Positive;
   begin
      --  Delete all Defining_Names with given URI
      for Vector of Self.All_Symbols loop
         Index := 1;

         while Index <= Vector.Last_Index loop
            if Vector (Index).File = File then
               Vector.Swap (Index, Vector.Last_Index);
               Vector.Delete_Last;
            else
               Index := Index + 1;
            end if;
         end loop;
      end loop;
   end Flush_File_Index;

   --------------------
   -- Get_Any_Symbol --
   --------------------

   procedure Get_Any_Symbol
     (Self              : Indexed_File_Set'Class;
      Pattern           : LSP.Search.Search_Pattern'Class;
      Only_Public       : Boolean;
      Get_Defining_Name : not null access function
        (File : GNATCOLL.VFS.Virtual_File;
         Loc  : Langkit_Support.Slocs.Source_Location)
      return Libadalang.Analysis.Defining_Name;
      Callback          : not null access procedure
        (File          : GNATCOLL.VFS.Virtual_File;
         Defining_Name : Libadalang.Analysis.Defining_Name;
         Stop          : in out Boolean))
   is
      use type LSP.Messages.Search_Kind;

      Stop          : Boolean := False;
      Cursor        : Symbol_Maps.Cursor;
      Defining_Name : Libadalang.Analysis.Defining_Name;
      Use_Celling   : constant Boolean :=
        not Pattern.Get_Case_Sensitive
        and then not Pattern.Get_Negate
        and then ((Pattern.Get_Kind = LSP.Messages.Full_Text
                   and then Pattern.Get_Whole_Word)
                  or else Pattern.Get_Kind = LSP.Messages.Start_Word_Text);

   begin
      if Use_Celling then
         Cursor := Self.All_Symbols.Ceiling (Pattern.Get_Canonical_Pattern);
      else
         Cursor := Self.All_Symbols.First;
      end if;

      Each_Prefix :
      while Symbol_Maps.Has_Element (Cursor) loop
         if Pattern.Get_Case_Sensitive then
            --  Match each element individually because
            --  All_Symbols is case insensitive
            for Item of Self.All_Symbols (Cursor) loop
               Defining_Name := Get_Defining_Name (Item.File, Item.Loc);

               if not Defining_Name.Is_Null
                 and then Pattern.Match
                   (LSP.Lal_Utils.To_Virtual_String
                      (Defining_Name.As_Ada_Node.Text))
               then
                  if not Only_Public or else Item.Is_Public then
                     Callback (Item.File, Defining_Name, Stop);
                  end if;

                  exit Each_Prefix when Stop;
               end if;
            end loop;

         elsif Pattern.Match (Symbol_Maps.Key (Cursor)) then
            --  All_Symbols is case insensitive so if the key is matched
            --  this means that all elements are also matched the pattern
            for Item of Self.All_Symbols (Cursor) loop
               if not Only_Public or else Item.Is_Public then
                  Defining_Name := Get_Defining_Name (Item.File, Item.Loc);
                  if not Defining_Name.Is_Null then
                     Callback (Item.File, Defining_Name, Stop);
                  end if;
               end if;

               exit Each_Prefix when Stop;
            end loop;

         else
            --  All_Symbols is ordered so we will not find any
            --  matches more
            exit Each_Prefix when Use_Celling;
         end if;

         Symbol_Maps.Next (Cursor);
      end loop Each_Prefix;
   end Get_Any_Symbol;

   -------------
   -- Include --
   -------------

   procedure Include
     (Self : in out Indexed_File_Set'Class;
      File : GNATCOLL.VFS.Virtual_File) is
   begin
      Self.Files.Include (File);
   end Include;

   -------------
   -- Exclude --
   -------------

   procedure Exclude
     (Self : in out Indexed_File_Set'Class;
      File : GNATCOLL.VFS.Virtual_File) is
   begin
      Self.Files.Exclude (File);
   end Exclude;

   ----------------
   -- Index_File --
   ----------------

   procedure Index_File
     (Self : in out Indexed_File_Set'Class;
      File : GNATCOLL.VFS.Virtual_File;
      Unit : Libadalang.Analysis.Analysis_Unit)
   is
      use Libadalang.Iterators;

      Inserted   : Boolean;
      Ignore     : Hashed_File_Sets.Cursor;
      Node       : Libadalang.Analysis.Ada_Node;

      Global_Visible : constant Libadalang.Iterators.Ada_Node_Predicate :=
        LSP.Lal_Utils.Is_Global_Visible;

      Restricted_Kind : constant Libadalang.Iterators.Ada_Node_Predicate :=
        LSP.Lal_Utils.Is_Restricted_Kind;

      --  Find all definings names excluding private parts and bodies content
      It : Libadalang.Iterators.Traverse_Iterator'Class :=
        Libadalang.Iterators.Find
          (Unit.Root,
           Libadalang.Iterators.Kind_Is (Ada_Defining_Name)
             and not Restricted_Kind);
   begin
      Self.Indexed.Insert (File, Ignore, Inserted);

      if not Inserted then
         --  URI has been indexed already, clear index from names of the unit
         Self.Flush_File_Index (File);
      end if;

      while It.Next (Node) loop
         declare
            Token : constant Libadalang.Common.Token_Reference :=
              Node.Token_End;

            Sloc_Range : constant Langkit_Support.Slocs.Source_Location_Range
              := Libadalang.Common.Sloc_Range (Libadalang.Common.Data (Token));

            Start_Sloc : constant Langkit_Support.Slocs.Source_Location :=
              Langkit_Support.Slocs.Start_Sloc (Sloc_Range);

            Text : constant Wide_Wide_String :=
              Libadalang.Common.Text (Token);

            Canonical : constant Symbolization_Result :=
              Libadalang.Sources.Canonicalize (Text);

            Inserted  : Boolean;
            Cursor    : Symbol_Maps.Cursor;
         begin
            if Canonical.Success then
               Self.All_Symbols.Insert
                 (LSP.Lal_Utils.To_Virtual_String (Canonical.Symbol),
                  Name_Vectors.Empty_Vector,
                  Cursor,
                  Inserted);

               Self.All_Symbols (Cursor).Append
                 (Name_Information'
                    (File,
                     Start_Sloc,
                     Global_Visible.Unchecked_Get.Evaluate (Node)));
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

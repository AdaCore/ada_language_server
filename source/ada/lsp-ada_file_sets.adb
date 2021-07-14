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
     (Self        : Indexed_File_Set'Class;
      Prefix      : VSS.Strings.Virtual_String;
      Only_Public : Boolean;
      Callback : not null access procedure
        (File : GNATCOLL.VFS.Virtual_File;
         Loc  : Langkit_Support.Slocs.Source_Location;
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
               if not Only_Public or else Item.Is_Public then
                  Callback (Item.File, Item.Loc, Stop);
               end if;

               exit Each_Prefix when Stop;
            end loop;

            Symbol_Maps.Next (Cursor);
         end;
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
                 ((File,
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

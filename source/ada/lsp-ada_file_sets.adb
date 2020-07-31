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

with Libadalang.Common;           use Libadalang.Common;
with Libadalang.Iterators;
with Libadalang.Sources;

package body LSP.Ada_File_Sets is

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
      URI  : LSP.Messages.DocumentUri;
      Unit : Libadalang.Analysis.Analysis_Unit)
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

      Self.Index_File (URI, Unit);
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
            exit Each_Prefix when not Value.Starts (Prefix);

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

      Node       : Libadalang.Analysis.Ada_Node;

      It         : Libadalang.Iterators.Traverse_Iterator'Class :=
        Libadalang.Iterators.Find
          (Unit.Root,
           Libadalang.Iterators.Kind_Is (Ada_Defining_Name));
   begin

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

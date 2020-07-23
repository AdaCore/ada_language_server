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

with Langkit_Support.Slocs;
with Libadalang.Common;

with VSS.Strings.Conversions;

with LSP.Types;

package body LSP.Ada_Completion_Sets is

   Details : constant LSP.Types.Optional_String :=
     (Is_Set => True,
      Value  => LSP.Types.To_LSP_String
        (Wide_Wide_String'("An invisible entity.")));

   Sort_Prefix : constant Wide_Character := '~';

   ------------
   -- Append --
   ------------

   procedure Append
     (Self : in out Completion_Result'Class;
      Name : Libadalang.Analysis.Defining_Name;
      Item : LSP.Messages.CompletionItem)
   is
      Sloc : constant Langkit_Support.Slocs.Source_Location_Range :=
        Name.Sloc_Range;

      Start : constant Langkit_Support.Slocs.Source_Location :=
        Langkit_Support.Slocs.Start_Sloc (Sloc);

      Key : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Conversions.To_Virtual_String
          (Name.Unit.Get_Filename & ":" &
           Langkit_Support.Slocs.Image (Start));

      Ignore   : Key_Sets.Cursor;
      Inserted : Boolean;
   begin
      Self.Unique_Keys.Insert
        (New_Item => Key,
         Position => Ignore,
         Inserted => Inserted);

      if Inserted then
         Self.Completion_List.Append (Item);
      end if;
   end Append;

   -----------------------------
   -- Append_Invisible_Symbol --
   -----------------------------

   procedure Append_Invisible_Symbol
     (Self      : in out Completion_Result'Class;
      Canonical : VSS.Strings.Virtual_String;
      Name      : Libadalang.Analysis.Defining_Name)
   is
      use Libadalang.Common;
      use type LSP.Types.LSP_String;

      Token : constant Token_Reference := Name.Token_End;

      Text : constant Wide_Wide_String :=
        Libadalang.Common.Text (Token);

      Item : constant LSP.Messages.CompletionItem :=
        (label    => LSP.Types.To_LSP_String (Text),
         kind     => (True, LSP.Messages.Text),
         detail   => Details,
         sortText =>
           (True, Sort_Prefix & LSP.Types.To_LSP_String (Canonical)),
         others   => <>);
   begin
      Self.Append (Name, Item);
   end Append_Invisible_Symbol;

end LSP.Ada_Completion_Sets;

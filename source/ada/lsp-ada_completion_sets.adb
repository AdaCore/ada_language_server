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

with Libadalang.Common;
with Libadalang.Sources;

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
     (Self   : in out Completion_Result'Class;
      Symbol : Wide_Wide_String;
      Item   : LSP.Messages.CompletionItem)
   is
      Canonical : constant Libadalang.Common.Symbolization_Result :=
        Libadalang.Sources.Canonicalize (Symbol);
   begin
      if Canonical.Success then
         Self.Unique_Symbols.Include
           (VSS.Strings.To_Virtual_String (Canonical.Symbol));
      end if;

      Self.Completion_List.Append (Item);
   end Append;

   -----------------------------
   -- Append_Invisible_Symbol --
   -----------------------------

   procedure Append_Invisible_Symbol
     (Self       : in out Completion_Map'Class;
      Cannonical : VSS.Strings.Virtual_String;
      Original   : LSP.Types.LSP_String) is
   begin
      if not Self.Contains (Cannonical) then
         declare
            use type LSP.Types.LSP_String;

            Item : constant LSP.Messages.CompletionItem :=
              (label    => Original,
               kind     => (True, LSP.Messages.Text),
               detail   => Details,
               sortText => (True, Sort_Prefix & Original),
               others   => <>);
         begin
            Self.Insert (Cannonical, Item);
         end;
      end if;
   end Append_Invisible_Symbol;

   -----------------------
   -- Write_Completions --
   -----------------------

   procedure Write_Completions
     (Self   : in out Completion_Map'Class;
      Limit  : Ada.Containers.Count_Type;
      Result : in out Completion_Result)
   is
      use type Ada.Containers.Count_Type;
   begin
      for Item in Self.Iterate loop
         declare
            Key : constant VSS.Strings.Virtual_String :=
              Completion_Maps.Key (Item);
         begin
            if not Result.Unique_Symbols.Contains (Key) then
               Result.Completion_List.Append (Self (Item));
            end if;
         end;
      end loop;

      if Self.Length >= Limit then
         Result.Is_Incomplete := True;
      end if;
   end Write_Completions;

end LSP.Ada_Completion_Sets;

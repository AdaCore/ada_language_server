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

with GNATCOLL.VFS;

with VSS.Strings;

with LSP.Lal_Utils;

package body LSP.Ada_Handlers.Invisibles is

   ------------------------
   -- Propose_Completion --
   ------------------------

   overriding procedure Propose_Completion
     (Self   :     Invisible_Completion_Provider;
      Sloc   :     Langkit_Support.Slocs.Source_Location;
      Token  :     Libadalang.Common.Token_Reference;
      Node   :     Libadalang.Analysis.Ada_Node;
      Names  : out Ada_Completions.Completion_Maps.Map;
      Result : out LSP.Messages.CompletionList)
   is
      pragma Unreferenced (Result);
      use all type Libadalang.Common.Token_Kind;
      use type Ada.Containers.Count_Type;

      procedure On_Inaccessible_Name
        (File : GNATCOLL.VFS.Virtual_File;
         Name : Libadalang.Analysis.Defining_Name;
         Stop : in out Boolean);

      Limit : constant := 10;

      --------------------------
      -- On_Inaccessible_Name --
      --------------------------

      procedure On_Inaccessible_Name
        (File : GNATCOLL.VFS.Virtual_File;
         Name : Libadalang.Analysis.Defining_Name;
         Stop : in out Boolean) is
      begin
         --  Skip all names in open documents, because they could have
         --  stale references. Then skip already provided results.
         if not Self.Handler.Open_Documents.Contains (File)
           and then not Names.Contains (Name)
         then
            Names.Insert
              (Name,
               (Is_Dot_Call  => False,
                Is_Visible   => False,
                Use_Snippets => False));

            Stop := Names.Length >= Limit;
         end if;
      end On_Inaccessible_Name;

      Dot_Token   : constant Libadalang.Common.Token_Data_Type :=
        Libadalang.Common.Data
          (if Libadalang.Common.Is_Trivia (Token)
           then Libadalang.Common.Previous (Token, True)
           else Token);

   begin
      if Libadalang.Common.Kind (Dot_Token) = Ada_Dot then
         --  Don't provide completion after a dot
         return;
      end if;

      --  Return immediately if we are dealing with a null node or if the
      --  node's parent is a Defining_Name, meaning that we are declaring a
      --  new symbol.

      if Node.Is_Null or else
        (not Node.Parent.Is_Null and then Node.Parent.Kind in
           Libadalang.Common.Ada_Defining_Name_Range)
      then
         return;
      end if;

      --  Return without asing Libadalang for completion results we are dealing
      --  with a syntax error.
      if Node.Kind in Libadalang.Common.Ada_Error_Decl_Range then
         return;
      end if;

      declare
         Word : constant VSS.Strings.Virtual_String :=
           LSP.Lal_Utils.To_Virtual_String
             (if Libadalang.Common.Is_Trivia (Token) then ""
              else Libadalang.Common.Text (Token));

         Canonical_Prefix : constant VSS.Strings.Virtual_String :=
           LSP.Lal_Utils.Canonicalize (Word);

      begin
         if not Word.Is_Empty then
            Self.Context.Get_Any_Symbol
              (Prefix      => Canonical_Prefix,
               Only_Public => True,
               Callback    => On_Inaccessible_Name'Access);

            for Doc of Self.Handler.Open_Documents loop
               Doc.Get_Any_Symbol
                 (Self.Context.all,
                  Canonical_Prefix,
                  Limit,
                  True,
                  Names);
            end loop;
         end if;
      end;
   end Propose_Completion;

end LSP.Ada_Handlers.Invisibles;

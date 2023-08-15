------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
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

package body LSP.Ada_Completions is
   pragma Warnings (Off);

   ------------------
   -- Is_End_Token --
   ------------------

   function Is_End_Token
     (Token : Libadalang.Common.Token_Reference)
      return Boolean
   is
      use Libadalang.Common;
      End_Token : constant Libadalang.Common.Token_Data_Type :=
        Libadalang.Common.Data (Token);

      Token_Kind : constant Libadalang.Common.Token_Kind :=
        Libadalang.Common.Kind (End_Token);
   begin
      return Token_Kind = Libadalang.Common.Ada_End;
   end Is_End_Token;

   ------------------------
   -- Is_Full_Sloc_Equal --
   ------------------------

   function Is_Full_Sloc_Equal
     (Left, Right : Libadalang.Analysis.Defining_Name) return Boolean
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Is_Full_Sloc_Equal unimplemented");
      return
        raise Program_Error with "Unimplemented function Is_Full_Sloc_Equal";
   end Is_Full_Sloc_Equal;

   -----------------------
   -- Skip_Dotted_Names --
   -----------------------

   function Skip_Dotted_Names
     (Node : Libadalang.Analysis.Ada_Node)
      return Libadalang.Analysis.Ada_Node
   is
      use Libadalang.Common;
      Parent : Libadalang.Analysis.Ada_Node := Node;
   begin
      while not Parent.Is_Null
        and then Parent.Kind = Libadalang.Common.Ada_Dotted_Name
      loop
         Parent := Parent.Parent;
      end loop;

      return Parent;
   end Skip_Dotted_Names;

   -----------------------
   -- Write_Completions --
   -----------------------

   procedure Write_Completions
     (Context                  :        LSP.Ada_Contexts.Context;
      Document                 :        LSP.Ada_Documents.Document;
      Sloc                     :        Langkit_Support.Slocs.Source_Location;
      Node :        Libadalang.Analysis.Ada_Node; Names : Completion_Maps.Map;
      Named_Notation_Threshold :    Natural; Compute_Doc_And_Details : Boolean;
      Result                   : in out LSP.Structures.CompletionItem_Vector)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Write_Completions unimplemented");
      raise Program_Error with "Unimplemented procedure Write_Completions";
   end Write_Completions;

   --------------------------
   -- Pretty_Print_Snippet --
   --------------------------

   procedure Pretty_Print_Snippet
     (Context :    LSP.Ada_Contexts.Context; Prefix : String; Offset : Natural;
      Span    : LSP.Structures.A_Range; Rule : Libadalang.Common.Grammar_Rule;
      Result  : in out LSP.Structures.CompletionItem)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Pretty_Print_Snippet unimplemented");
      raise Program_Error with "Unimplemented procedure Pretty_Print_Snippet";
   end Pretty_Print_Snippet;

   ---------------------------
   -- Generic_Write_Symbols --
   ---------------------------

   procedure Generic_Write_Symbols
     (Names  :        Completion_Maps.Map;
      Result : in out LSP.Structures.DocumentSymbol_Vector)
   is
   begin
      pragma Assert (not Has_Been_Canceled);
      pragma Compile_Time_Warning
        (Standard.True, "Generic_Write_Symbols unimplemented");
      raise Program_Error with "Unimplemented procedure Generic_Write_Symbols";
   end Generic_Write_Symbols;

end LSP.Ada_Completions;

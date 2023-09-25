------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
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

with LSP.Constants;
with LSP.Enumerations;

package body LSP.GPR_Files.Symbols is

   function Get_File
     (Provider  : LSP.GPR_Files.File_Provider_Access;
      File_Name : GPR2.Path_Name.Object) return File_Access
   is
     (LSP.GPR_Files.Parse (Provider, File_Name));

   function To_Symbol_Kind
     (Symbol : LSP.GPR_Files.Symbol) return LSP.Enumerations.SymbolKind;

   function To_Range
     (Symbol : LSP.GPR_Files.Symbol) return LSP.Structures.A_Range;

   -----------------
   -- Get_Symbols --
   -----------------

   procedure Get_Symbols
     (Provider     : LSP.GPR_Files.File_Provider_Access;
      Document_URI : LSP.Structures.DocumentUri;
      File_Name    : GPR2.Path_Name.Object;
      Result       : out LSP.Structures.SymbolInformation_Vector)
   is
      File : constant File_Access := Get_File (Provider, File_Name);

   begin
      for Symbol of File.Document_Symbols.Document_Symbols loop
         declare
            Item : LSP.Structures.SymbolInformation;
            Kind : constant LSP.Enumerations.SymbolKind :=
              To_Symbol_Kind (Symbol);

         begin
            Item :=
              (name          => Symbol.Name,
               kind          => Kind,
               tags          => LSP.Constants.Empty,
               deprecated    => <>,
               location      =>
                 (uri     => Document_URI,
                  a_range => To_Range (Symbol),
                  alsKind => LSP.Constants.Empty),
               containerName => <>);
            Result.Append (Item);
         end;
      end loop;
   end Get_Symbols;

   ---------------------------
   -- Get_Symbols_Hierarchy --
   ---------------------------

   procedure Get_Symbols_Hierarchy
     (Provider     : LSP.GPR_Files.File_Provider_Access;
      Document_URI : LSP.Structures.DocumentUri;
      File_Name    : GPR2.Path_Name.Object;
      Result       : out LSP.Structures.DocumentSymbol_Vector)
   is
      pragma Unreferenced (Document_URI);

      File : constant File_Access := Get_File (Provider, File_Name);

      procedure Walk
        (Symbols : LSP.GPR_Files.Symbol_List;
         Result  : in out LSP.Structures.DocumentSymbol_Vector);

      ----------
      -- Walk --
      ----------

      procedure Walk
        (Symbols : LSP.GPR_Files.Symbol_List;
         Result  : in out LSP.Structures.DocumentSymbol_Vector) is
      begin
         for Symbol of Symbols loop
            declare
               Item : LSP.Structures.DocumentSymbol :=
                 (name              => Symbol.Name,
                  detail            => <>,
                  kind              => To_Symbol_Kind (Symbol),
                  tags              => LSP.Constants.Empty,
                  deprecated        => <>,
                  a_range           => To_Range (Symbol),
                  selectionRange    => To_Range (Symbol),
                  alsIsDeclaration  => (Is_Set => False),
                  alsIsAdaProcedure => <>,
                  alsVisibility     => <>,
                  children          => <>);

            begin
               if Symbol.Children /= Gpr_Parser.Common.No_Token then
                  declare
                     C : constant LSP.GPR_Files.Symbols_Maps.Cursor :=
                       File.Document_Symbols.Children.Find
                         (Symbol.Children);

                  begin
                     if LSP.GPR_Files.Symbols_Maps.Has_Element (C) then
                        Walk
                          (LSP.GPR_Files.Symbols_Maps.Element (C),
                           Item.children);
                     end if;
                  end;
               end if;

               Result.Append (Item);
            end;
         end loop;
      end Walk;

   begin
      Walk (File.Document_Symbols.Document_Symbols, Result);
   end Get_Symbols_Hierarchy;

   --------------
   -- To_Range --
   --------------

   function To_Range
     (Symbol : LSP.GPR_Files.Symbol) return LSP.Structures.A_Range
   is
     (start  =>
        (line      => Symbol.Start_Position.Line - 1,
         character => Symbol.Start_Position.Column - 1),
      an_end =>
        (line      => Symbol.End_Position.Line - 1,
         character => Symbol.End_Position.Column - 1));
   --  XXX Incorrect conversion

   --------------------
   -- To_Symbol_Kind --
   --------------------

   function To_Symbol_Kind
     (Symbol : LSP.GPR_Files.Symbol) return LSP.Enumerations.SymbolKind is
   begin
      case Symbol.Kind is
         when K_Imported =>
            return LSP.Enumerations.Namespace;
         when K_Project =>
            return LSP.Enumerations.Module;
         when K_Type =>
            return LSP.Enumerations.Enum;
         when K_Variable =>
            return LSP.Enumerations.Variable;
         when K_Attribute =>
            return LSP.Enumerations.Property;
         when K_Package =>
            return LSP.Enumerations.A_Package;
      end case;
   end To_Symbol_Kind;

end LSP.GPR_Files.Symbols;

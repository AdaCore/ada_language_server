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

      procedure Walk (Symbol : LSP.GPR_Files.Symbol);

      ----------
      -- Walk --
      ----------

      procedure Walk
        (Symbol : LSP.GPR_Files.Symbol) is
         Item : constant LSP.Structures.SymbolInformation :=
                  (name          => Symbol.Name,
                   kind          => To_Symbol_Kind (Symbol),
                   tags          => LSP.Constants.Empty,
                   deprecated    => <>,
                   location      =>
                     (uri     => Document_URI,
                      a_range => To_Range (Symbol),
                      alsKind => LSP.Constants.Empty),
                   containerName => <>);
         C : constant LSP.GPR_Files.Symbol_List_Maps.Cursor :=
               File.Document_Symbols.Children_Map.Find
                 (Symbol.Id);
      begin
         Result.Append (Item);
         if Symbol_List_Maps.Has_Element (C) then
            for Symbol of Symbol_List_Maps.Element (C) loop
               Walk (Symbol);
            end loop;
         end if;
      end Walk;

   begin
      for Imported of File.Document_Symbols.Imported_Symbols loop
         declare
            Item : LSP.Structures.SymbolInformation;
            Kind : constant LSP.Enumerations.SymbolKind :=
                     To_Symbol_Kind (Imported);
         begin
            Item :=
              (name          => Imported.Name,
               kind          => Kind,
               tags          => LSP.Constants.Empty,
               deprecated    => <>,
               location      =>
                 (uri     => Document_URI,
                  a_range => To_Range (Imported),
                  alsKind => LSP.Constants.Empty),
               containerName => <>);
            Result.Append (Item);
         end;
      end loop;

      Walk (File.Document_Symbols.Project);

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

      procedure Append_Project;
      procedure Append_With_Clauses;

      -------------------------
      -- Append_With_Clauses --
      -------------------------

      procedure Append_With_Clauses is
         Item : LSP.Structures.DocumentSymbol :=
                  (name              => "with clauses",
                   detail            => <>,
                   kind              => LSP.Enumerations.Namespace,
                   tags              => LSP.Constants.Empty,
                   deprecated        => <>,
                   a_range           => ((0, 0), (0, 0)),
                   selectionRange    => ((0, 0), (0, 0)),
                   alsIsDeclaration  => (Is_Set => False),
                   alsIsAdaProcedure => <>,
                   alsVisibility     => <>,
                   children          => <>);
      begin
         for Imported of File.Document_Symbols.Imported_Symbols loop
            declare
               S : constant LSP.Structures.DocumentSymbol :=
                     (name              => Imported.Name,
                      detail            => <>,
                      kind              => To_Symbol_Kind (Imported),
                      tags              => LSP.Constants.Empty,
                      deprecated        => <>,
                      a_range           => To_Range (Imported),
                      selectionRange    => To_Range (Imported),
                      alsIsDeclaration  => (Is_Set => False),
                      alsIsAdaProcedure => <>,
                      alsVisibility     => <>,
                      children          => <>);

            begin
               Item.children.Append (S);
            end;
         end loop;

         Result.Append (Item);
      end Append_With_Clauses;

      --------------------
      -- Append_Project --
      --------------------

      procedure Append_Project is

         procedure Walk
           (Symbol : LSP.GPR_Files.Symbol;
            Result : in out LSP.Structures.DocumentSymbol_Vector);

         ----------
         -- Walk --
         ----------

         procedure Walk
           (Symbol : LSP.GPR_Files.Symbol;
            Result : in out LSP.Structures.DocumentSymbol_Vector) is
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

            C : constant LSP.GPR_Files.Symbol_List_Maps.Cursor :=
                  File.Document_Symbols.Children_Map.Find
                    (Symbol.Id);
         begin
            if LSP.GPR_Files.Symbol_List_Maps.Has_Element (C) then
               for S of LSP.GPR_Files.Symbol_List_Maps.Element (C) loop
                  Walk (S, Item.children);
               end loop;
            end if;
            Result.Append (Item);
         end Walk;

      begin
         Walk (File.Document_Symbols.Project, Result);
      end Append_Project;

   begin
      if not File.Document_Symbols.Imported_Symbols.Is_Empty then
         Append_With_Clauses;
      end if;
      Append_Project;
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
         when K_Case =>
            return LSP.Enumerations.A_Package;
         when K_When =>
            return LSP.Enumerations.A_Package;
      end case;
   end To_Symbol_Kind;

end LSP.GPR_Files.Symbols;

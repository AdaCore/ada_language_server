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
--
--  This package provides Get_Symbols request implementation

with LSP.GPR_Handlers;
with LSP.Types;

package body LSP.GPR_Files.Symbols is

   package Added_Lists is
     new Ada.Containers.Indefinite_Vectors
       (Positive, Gpr_Parser.Common.Token_Reference);
   subtype Added_List is Added_Lists.Vector;

   function Get_File
     (Provider : LSP.GPR_Files.File_Provider_Access;
      Request  : LSP.Messages.Server_Requests.Document_Symbols_Request)
      return File_Access is
     (LSP.GPR_Files.Parse
        (Provider,
         LSP.GPR_Handlers.To_File
           (Request.params.textDocument.uri, Provider.Follow_Symlinks)));

   function To_Symbol_Kind
     (Symbol : LSP.GPR_Files.Symbol) return LSP.Messages.SymbolKind;

   function To_Span (Symbol : LSP.GPR_Files.Symbol) return LSP.Messages.Span is
     (first =>
        (line      =>
              LSP.Types.Line_Number (Symbol.Start_Position.Line - 1),
         character =>
            LSP.Types.UTF_16_Index (Symbol.Start_Position.Column - 1)),
      last  =>
        (line      =>
              LSP.Types.Line_Number (Symbol.End_Position.Line - 1),
         character =>
            LSP.Types.UTF_16_Index (Symbol.End_Position.Column - 1)));

   --------------------
   -- To_Symbol_Kind --
   --------------------

   function To_Symbol_Kind
     (Symbol : LSP.GPR_Files.Symbol) return LSP.Messages.SymbolKind is
   begin
      case Symbol.Kind is
         when K_Imported =>
            return LSP.Messages.Namespace;
         when K_Project =>
            return LSP.Messages.Module;
         when K_Type =>
            return LSP.Messages.Enum;
         when K_Variable =>
            return LSP.Messages.Variable;
         when K_Attribute =>
            return LSP.Messages.Property;
         when K_Package =>
            return LSP.Messages.A_Package;
      end case;
   end To_Symbol_Kind;

   -----------------
   -- Get_Symbols --
   -----------------

   procedure Get_Symbols
     (Provider : LSP.GPR_Files.File_Provider_Access;
      Request  : LSP.Messages.Server_Requests.Document_Symbols_Request;
      Result   : out LSP.Messages.Symbol_Vector) is
      File    : constant File_Access := Get_File (Provider, Request);
   begin
      for Symbol of File.Document_Symbols.Document_Symbols loop
         declare
            Item : LSP.Messages.SymbolInformation;
            Kind : constant LSP.Messages.SymbolKind :=
              To_Symbol_Kind (Symbol);
         begin
            Item :=
              (name              => Symbol.Name,
               kind              => Kind,
               alsIsAdaProcedure => <>,
               tags              => LSP.Messages.Empty,
               deprecated        => <>,
               location          =>
                 (uri     => Request.params.textDocument.uri,
                  span    => To_Span (Symbol),
                  alsKind => LSP.Messages.Empty_Set),
               containerName     => <>);
            Result.Vector.Append (Item);
         end;
      end loop;
   end Get_Symbols;

   ---------------------------
   -- Get_Symbols_Hierarchy --
   ---------------------------

   procedure Get_Symbols_Hierarchy
     (Provider : LSP.GPR_Files.File_Provider_Access;
      Request  : LSP.Messages.Server_Requests.Document_Symbols_Request;
      Result   : out LSP.Messages.Symbol_Vector) is

      use LSP.Messages;

      File : constant File_Access := Get_File (Provider, Request);

      Added : Added_List;
      --  Used to protect against infinite loop on incorrect tree.

      procedure Walk
        (Symbols : LSP.GPR_Files.Symbol_List;
         Cursor  : LSP.Messages.DocumentSymbol_Trees.Cursor;
         Tree    : in out LSP.Messages.DocumentSymbol_Tree);

      ----------
      -- Walk --
      ----------

      procedure Walk
        (Symbols : LSP.GPR_Files.Symbol_List;
         Cursor  : LSP.Messages.DocumentSymbol_Trees.Cursor;
         Tree    : in out LSP.Messages.DocumentSymbol_Tree) is
         Next : LSP.Messages.DocumentSymbol_Trees.Cursor := Cursor;
      begin
         for Symbol of Symbols loop
            if not Added.Contains (Symbol.Ref) then
               Added.Append (Symbol.Ref);

               declare
                  Item : constant LSP.Messages.DocumentSymbol :=
                    (name              => Symbol.Name,
                     detail            => <>,
                     kind              => To_Symbol_Kind (Symbol),
                     tags              => LSP.Messages.Empty,
                     deprecated        => <>,
                     span              => To_Span (Symbol),
                     selectionRange    => To_Span (Symbol),
                     alsIsDeclaration  => (Is_Set => False),
                     alsIsAdaProcedure => <>,
                     alsVisibility     => <>,
                     children          =>
                       Symbol.Children /= Gpr_Parser.Common.No_Token);
               begin
                  Tree.Insert_Child
                    (Parent   => Cursor,
                     Before   =>
                       LSP.Messages.DocumentSymbol_Trees.No_Element,
                     New_Item => Item,
                     Position => Next);
               end;

               if Symbol.Children /= Gpr_Parser.Common.No_Token then
                  declare
                     C : constant LSP.GPR_Files.Symbols_Maps.Cursor :=
                           File.Document_Symbols.Children.Find
                             (Symbol.Children);
                  begin
                     if LSP.GPR_Files.Symbols_Maps.Has_Element (C) then
                        Walk
                          (Symbols => LSP.GPR_Files.Symbols_Maps.Element (C),
                           Cursor  => Next,
                           Tree    => Tree);
                     end if;
                  end;
               end if;
            end if;
         end loop;
      end Walk;

   begin
      Result := (Is_Tree => True, others => <>);
      Walk (File.Document_Symbols.Document_Symbols,
            Result.Tree.Root,
            Result.Tree);
   end Get_Symbols_Hierarchy;

end LSP.GPR_Files.Symbols;

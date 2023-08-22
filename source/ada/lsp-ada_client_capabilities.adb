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

with VSS.String_Vectors;

with LSP.Constants;
with LSP.Enumerations;
with LSP.Structures.Unwrap;

package body LSP.Ada_Client_Capabilities is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : in out Client_Capability'Class;
      Value : LSP.Structures.InitializeParams) is
   begin
      Self.Value := Value;

      if Value.rootUri.Is_Null
        and then Value.rootPath.Is_Set
        and then not Value.rootPath.Value.Is_Null
      then
         --  URI isn't provided, rollback to deprecated rootPath
         Self.Root := Value.rootPath.Value.Value;
      elsif not Value.rootUri.Is_Null then
         Self.Root := VSS.Strings.Virtual_String (Value.rootUri.Value);
      end if;
   end Initialize;

   -----------------------
   -- Set_Root_If_Empty --
   -----------------------

   procedure Set_Root_If_Empty
     (Self  : in out Client_Capability'Class;
      Value : VSS.Strings.Virtual_String) is
   begin
      if Self.Root.Is_Empty then
         Self.Root := Value;
      end if;
   end Set_Root_If_Empty;

   ----------------------------
   -- To_Server_Capabilities --
   ----------------------------

   function To_Server_Capabilities
     (Self : Client_Capability'Class;
      Incremental_Text_Changes : Boolean)
      return LSP.Structures.ServerCapabilities
   is
      pragma Unreferenced (Self);
   begin
      return Result : LSP.Structures.ServerCapabilities do
         Result.textDocumentSync :=
           (Is_Set => True,
            Value  =>
              (Is_TextDocumentSyncOptions => False,
               TextDocumentSyncKind       =>
                 (if Incremental_Text_Changes then
                       LSP.Enumerations.Incremental
                  else
                     LSP.Enumerations.Full)));

         Result.completionProvider :=
           (Is_Set => True,
            Value  => (triggerCharacters => [".", ",", "'", "("],
                       resolveProvider   => LSP.Constants.True,
                       others            => <>));

         Result.callHierarchyProvider := LSP.Constants.True;
         Result.declarationProvider := LSP.Constants.True;
         Result.definitionProvider := LSP.Constants.True;
         Result.foldingRangeProvider := LSP.Constants.True;
         Result.implementationProvider := LSP.Constants.True;
      end return;
   end To_Server_Capabilities;

   -----------------------
   -- Line_Folding_Only --
   -----------------------

   function Line_Folding_Only
     (Self : Client_Capability'Class) return Boolean
   is
      use LSP.Structures.Unwrap;

      Result : constant LSP.Structures.Boolean_Optional :=
        lineFoldingOnly (foldingRange (Self.Value.capabilities.textDocument));
   begin
      return (if Result.Is_Set then Result.Value else False);
   end Line_Folding_Only;

   --------------------
   -- Resolve_Lazily --
   --------------------

   function Resolve_Lazily (Self : Client_Capability'Class) return Boolean is
      use LSP.Structures.Unwrap;

      List : constant VSS.String_Vectors.Virtual_String_Vector :=
        properties
          (resolveSupport
             (completionItem
                (completion
                   (Self.Value.capabilities.textDocument))));

   begin
      return List.Contains ("detail") and then List.Contains ("documentation");
   end Resolve_Lazily;

end LSP.Ada_Client_Capabilities;

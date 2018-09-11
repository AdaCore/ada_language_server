------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
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

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with GPR2.Path_Name;
with GPR2.Project.Source.Set;
pragma Unreferenced (GPR2.Project.Source.Set);

with GPR2.Source;
with Ada.Wide_Wide_Characters.Unicode;

package body LSP.Ada_Contexts is

   ------------------
   -- Get_Document --
   ------------------

   not overriding function Get_Document
     (Self : Context;
      URI  : LSP.Messages.DocumentUri)
        return LSP.Ada_Documents.Document_Access is
   begin
      return Self.Documents (URI);
   end Get_Document;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (Self : in out Context;
      Root : LSP.Types.LSP_String)
   is
      function To_Unit_Name (Name : GPR2.Name_Type) return Wide_Wide_String;
      function To_URI
        (Path : GPR2.Path_Name.Full_Name) return LSP.Messages.DocumentUri;

      function To_Unit_Name (Name : GPR2.Name_Type) return Wide_Wide_String is
         Result : Wide_Wide_String :=
           Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode
             (Ada.Strings.UTF_Encoding.UTF_8_String (Name));
      begin
         for X of Result loop
            X := Ada.Wide_Wide_Characters.Unicode.To_Lower_Case (X);
         end loop;

         return Result;
      end To_Unit_Name;

      function To_URI
        (Path : GPR2.Path_Name.Full_Name) return LSP.Messages.DocumentUri is
      begin
         return LSP.Types.To_LSP_String ("file://" & Path);
      end To_URI;

      Dir  : constant GPR2.Name_Type :=
        GPR2.Name_Type (LSP.Types.To_UTF_8_String (Root));

      Path : constant GPR2.Path_Name.Object :=
        GPR2.Path_Name.Create_File
          (Name      => "gnat/lsp.gpr",
           Directory => Dir);
   begin
      Self.Project_Tree.Load
        (Filename => Path,
         Context  => Self.GPR_Context);

      for Source of Self.Project_Tree.Root_Project.Sources loop
         declare
            Object : constant GPR2.Source.Object := Source.Source;
            Name   : constant GPR2.Name_Type := Object.Unit_Name;
            Kind   : constant GPR2.Source.Kind_Type := Object.Kind;
            File   : constant GPR2.Path_Name.Object := Object.Path_Name;
         begin
            if Kind in GPR2.Source.S_Spec then
               Self.Specs.Insert (To_Unit_Name (Name), To_URI (File.Value));
            else
               Self.Bodies.Insert (To_Unit_Name (Name), To_URI (File.Value));
            end if;
         end;
      end loop;

      Self.Root := Root;
   end Initialize;

   -------------------
   -- Load_Document --
   -------------------

   not overriding procedure Load_Document
     (Self  : in out Context;
      Item  : LSP.Messages.TextDocumentItem)
   is
      Object : constant LSP.Ada_Documents.Document_Access :=
        new LSP.Ada_Documents.Document;
   begin
      Object.Initialize (Item);
      Object.Update;
      Self.Documents.Insert (Item.uri, Object);
   end Load_Document;

   ---------------------
   -- Update_Document --
   ---------------------

   not overriding procedure Update_Document
     (Self : in out Context;
      Item : not null LSP.Ada_Documents.Document_Access)
   is
      pragma Unreferenced (Self);
   begin
      Item.Update;
   end Update_Document;

end LSP.Ada_Contexts;

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

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Self : Unit_Provider;
      Name : Wide_Wide_String;
      Kind : Libadalang.Common.Unit_Kind)
      return String
   is
      URI : constant LSP.Types.LSP_String := Self.Get_Unit_URI (Name, Kind);
   begin
      if not LSP.Types.Is_Empty (URI) then
         return LSP.Types.To_UTF_8_String
           (LSP.Types.Delete (URI, 1, 7));  --  Drop 'file://'
      end if;

      return "";
   end Get_Unit_Filename;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Self    : Unit_Provider;
      Context : Libadalang.Analysis.Analysis_Context'Class;
      Name    : Wide_Wide_String;
      Kind    : Libadalang.Common.Unit_Kind;
      Charset : String := "";
      Reparse : Boolean := False)
      return Libadalang.Analysis.Analysis_Unit'Class
   is
      File : constant String := Self.Get_Unit_Filename (Name, Kind);
   begin
      return Libadalang.Analysis.Get_From_File
        (Context  => Context,
         Filename => String (File),
         Charset  => Charset,
         Reparse  => Reparse);
   end Get_Unit;

   ------------------
   -- Get_Unit_URI --
   ------------------

   not overriding function Get_Unit_URI
     (Self : Unit_Provider;
      Name : Wide_Wide_String;
      Kind : Libadalang.Common.Unit_Kind)
      return LSP.Types.LSP_String
   is
      Cursor : Unit_Maps.Cursor;
   begin
      case Kind is
         when Libadalang.Common.Unit_Specification =>
            Cursor := Self.Context.Specs.Find (Name);
         when Libadalang.Common.Unit_Body =>
            Cursor := Self.Context.Bodies.Find (Name);
      end case;

      if Unit_Maps.Has_Element (Cursor) then
         return Unit_Maps.Element (Cursor);
      end if;

      return LSP.Types.Empty_LSP_String;
   end Get_Unit_URI;

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
      Self.Root := Root;

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

      Self.LAL_Context := Libadalang.Analysis.Create_Context
        (Unit_Provider => Libadalang.Analysis.Create_Unit_Provider_Reference
          (Self.Unit_Provider),
         With_Trivia   => True,
         Charset       => "utf-8");
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

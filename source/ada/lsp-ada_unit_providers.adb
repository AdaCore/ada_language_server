------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with Ada.Directories;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with GNATCOLL.VFS;
with Libadalang.Unit_Files;

package body LSP.Ada_Unit_Providers is

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Self    : Unit_Provider;
      Context : Analysis_Context'Class;
      Name    : Text_Type;
      Kind    : Analysis_Unit_Kind;
      Charset : String := "";
      Reparse : Boolean := False) return Analysis_Unit'Class
   is
      Filename : constant String := Self.Get_Unit_Filename (Name, Kind);
   begin
      if Filename = "" then
         declare
            Str_Name : constant String :=
               Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Name);
            Dummy_File : constant String :=
               Libadalang.Unit_Files.File_From_Unit (Str_Name, Kind);
            Kind_Name  : constant String :=
              (case Kind is
               when Unit_Specification => "specification file",
               when Unit_Body          => "body file");
            Error      : constant String :=
               "Could not find source file for " & Str_Name & " (" & Kind_Name
               & ")";
         begin
            return Context.Get_With_Error (Dummy_File, Error, Charset);
         end;
      else
         return Context.Get_From_File (Filename, Charset, Reparse);
      end if;
   end Get_Unit;

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Self : Unit_Provider;
      Name : Text_Type;
      Kind : Analysis_Unit_Kind)
      return String
   is
      use all type GNATCOLL.VFS.Filesystem_String;

      Root : constant GNATCOLL.Projects.Project_Type :=
        Self.Project_Tree.Root_Project;

      Convert : constant array (Analysis_Unit_Kind)
        of GNATCOLL.Projects.Unit_Parts :=
          (Unit_Specification => GNATCOLL.Projects.Unit_Spec,
           Unit_Body          => GNATCOLL.Projects.Unit_Body);

      File : constant GNATCOLL.VFS.Filesystem_String := Root.File_From_Unit
        (Unit_Name =>
           Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Name),
         Part      => Convert (Kind),
         Language  => "Ada");
   begin
      if File'Length = 0 then
         return "";
      end if;

      declare
         Path : constant GNATCOLL.VFS.Virtual_File :=
           Self.Project_Tree.Create (File);
         Full_Path : constant String := +Full_Name (Path);
      begin
         if Full_Path = "" then
            return Full_Path;
         else
            --  Sometimes Path contains double directory separators
            --  like '/path//file' or 'C:\path\\file'. This prevents correct
            --  file identification in Libadalang. Let's normalize Path:
            return Ada.Directories.Full_Name (Full_Path);
         end if;
      end;
   end Get_Unit_Filename;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Provider : in out Unit_Provider) is
   begin
      null;
   end Release;

end LSP.Ada_Unit_Providers;

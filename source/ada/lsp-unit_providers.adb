------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.VFS;          use GNATCOLL.VFS;

with Libadalang.Unit_Files;
with Langkit_Support.Text; use Langkit_Support.Text;

with LSP.Preprocessor; use LSP.Preprocessor;

package body LSP.Unit_Providers is

   package US renames Ada.Strings.Unbounded;
   use type US.Unbounded_String;

   type Project_Unit_Provider is new LAL.Unit_Provider_Interface with record
      Tree             : Prj.Project_Tree_Access;
      Project          : Prj.Project_Type;
      Env              : Prj.Project_Environment_Access;
      Is_Project_Owner : Boolean;
      Default_Charset  : Unbounded_String;
   end record;
   --  Unit provider backed up by a project file

   overriding function Get_Unit_Filename
     (Provider : Project_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String;

   overriding function Get_Unit
     (Provider    : Project_Unit_Provider;
      Context     : LAL.Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return LAL.Analysis_Unit'Class;

   overriding procedure Release (Provider : in out Project_Unit_Provider);

   ----------------------------------
   -- Create_Project_Unit_Provider --
   ----------------------------------

   function Create_Project_Unit_Provider
     (Tree             : Prj.Project_Tree_Access;
      Project          : Prj.Project_Type;
      Env              : Prj.Project_Environment_Access;
      Default_Charset  : String;
      Is_Project_Owner : Boolean := True)
      return LAL.Unit_Provider_Reference
   is
      Provider : constant Project_Unit_Provider :=
        (Tree             => Tree,
         Project          => Project,
         Env              => Env,
         Is_Project_Owner => Is_Project_Owner,
         Default_Charset  => To_Unbounded_String (Default_Charset));
   begin
      return LAL.Create_Unit_Provider_Reference (Provider);
   end Create_Project_Unit_Provider;

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Provider : Project_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String
   is
      Str_Name : constant String :=
        Libadalang.Unit_Files.Unit_String_Name (Name);
      File     : constant Filesystem_String := Prj.File_From_Unit
        (Project   => Provider.Project,
         Unit_Name => Str_Name,
         Part      => Convert (Kind),
         Language  => "Ada");
   begin
      --  Look for a source file corresponding to Name/Kind in all projects
      --  associated to this Provider. Note that unlike what is documented,
      --  it's not because File_From_Unit returns an non-empty string that the
      --  unit does belong to the project, so we must also check
      --  Create_From_Project's result.
      if File'Length /= 0 then
         declare
            Path : constant GNATCOLL.VFS.Virtual_File :=
              Prj.Create_From_Project (Provider.Project, File).File;
            Fullname : constant String := +Path.Full_Name;
         begin
            if Fullname'Length /= 0 then
               return Fullname;
            end if;
         end;
      end if;

      return "";
   end Get_Unit_Filename;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : Project_Unit_Provider;
      Context     : LAL.Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return LAL.Analysis_Unit'Class
   is
      Filename : constant String := Provider.Get_Unit_Filename (Name, Kind);
      Buffer   : Unbounded_String;

      --  If a charset has been passed explicitely, use it; otherwise, use
      --  the default charset provided.
      Charset_To_Use : constant String :=
        (if Charset = "" then
            To_String (Provider.Default_Charset)
         else
            Charset);
   begin
      if Filename /= "" then
         -------------------------------------------------------------
         --  NOTE: this is the part of this package which differs from
         --  the Libadalang Unit provider: see if we can improve code
         --  reuse.

         --  If Reparse is False and the context already has this unit,
         --  simply return it...
         if not Reparse
           and then Context.Has_Unit (Filename)
         then
            return LAL.Get_With_Error
              (Context, Filename, "", Charset_To_Use);
         end if;

         --  ... otherwise, load the file from disk and preprocess it
         Buffer := Preprocess_File (Filename, Charset_To_Use);

         --  After preprocessing, buffer is utf-8
         return LAL.Get_From_Buffer
           (Context, Filename, "utf-8", Buffer);

         --  /NOTE
         -------------------------------------------------------------
      else
         declare
            Dummy_File : constant String :=
               Libadalang.Unit_Files.File_From_Unit (Name, Kind);
            Kind_Name  : constant Text_Type :=
              (case Kind is
               when Unit_Specification => "specification file",
               when Unit_Body          => "body file");
            Error      : constant Text_Type :=
               "Could not find source file for " & Name & " (" & Kind_Name
               & ")";
         begin
            return LAL.Get_With_Error (Context, Dummy_File, Error, Charset);
         end;
      end if;
   end Get_Unit;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Provider : in out Project_Unit_Provider)
   is
      --  Dummy : GNATCOLL.Locks.Scoped_Lock (Libadalang.GPR_Lock.Lock'Access);
   begin
      if Provider.Is_Project_Owner then
         Prj.Unload (Provider.Tree.all);
         Prj.Free (Provider.Tree);
         Prj.Free (Provider.Env);
      end if;
      Provider.Project := Prj.No_Project;
      Provider.Tree := null;
      Provider.Env := null;
      Provider.Is_Project_Owner := False;
   end Release;

end LSP.Unit_Providers;

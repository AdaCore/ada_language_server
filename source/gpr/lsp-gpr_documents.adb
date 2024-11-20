------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                    Copyright (C) 2023-2024, AdaCore                      --
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

with Ada.Exceptions;

with GPR2.Message;
with GPR2.Options;
with GPR2.Project.View;
with GPR2.Source_Reference;
with GPR2.Project.Typ.Set;

with VSS.Strings.Conversions;

package body LSP.GPR_Documents is

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Self : in out Document) is
   begin
      Self.Tree.Unload;
   end Cleanup;

   ----------------
   -- Get_Errors --
   ----------------

   procedure Get_Errors
     (Self      : in out Document;
      Root_File : GPR2.Path_Name.Object;
      Changed   : out Boolean;
      Errors    : out Message_Map) is
   begin
      Changed := Self.Errors_Changed;
      Self.Errors_Changed := False;

      Errors.Clear;

      --  Insert empty log for unopened files that had logs on previous
      --  publish diags call to let client clear problems

      for File of Self.Published_Files_With_Diags loop
         if not Self.File_Provider.Is_Openened_Document (File.Virtual_File)
         then
            Errors.Insert (File, GPR2.Log.Undefined);
         end if;
      end loop;

      --  Root_File should always have diagnostic published to clear diags on
      --  corrected file.

      if not Errors.Contains (Root_File) then
         Errors.Insert (Root_File, GPR2.Log.Undefined);
      end if;

      if Changed then

         --  for error & warning messages
         for C in Self.Tree.Log_Messages.Iterate (False) loop
            declare
               Message  : constant GPR2.Message.Object := C.Element;
               File     : constant GPR2.Path_Name.Object :=
                            (if Message.Sloc.Is_Defined and then
                                Message.Sloc.Has_Source_Reference
                             then GPR2.Path_Name.Create_File
                               (GPR2.Filename_Type (Message.Sloc.Filename))
                             else Root_File);
               --  message without location will be attached to root_file.

               C        : constant Message_Maps.Cursor :=
                             Errors.Find (File);
            begin
               if C.Has_Element then
                  declare
                     Log : GPR2.Log.Object := C.Element;
                     --  existing log to update
                  begin
                     Log.Append (Message);
                     Errors.Replace_Element (C, Log);
                  end;
               else
                  declare
                     Log : GPR2.Log.Object;
                     --  new log to be inserted
                  begin
                     Log.Append (Message);
                     Errors.Insert (File, Log);
                  end;
               end if;
            end;
         end loop;
      end if;
   end Get_Errors;

   ---------------------
   -- Has_Diagnostics --
   ---------------------

   function Has_Diagnostics
     (Self    : Document)
      return Boolean is
   begin
      return Self.Has_Messages;
   end Has_Diagnostics;

   ----------------
   -- Has_Errors --
   ----------------

   function Has_Errors
     (Self : Document)
      return Boolean is
   begin
      return Self.Tree.Log_Messages.Has_Error
        or else not Self.Tree.Root_Project.Is_Defined;
   end Has_Errors;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self        : in out Document;
      URI         : LSP.Structures.DocumentUri;
      File        : GPR2.Path_Name.Object;
      Text        : VSS.Strings.Virtual_String;
      Provider    : LSP.GPR_Files.File_Provider_Access) is
   begin
      LSP.Text_Documents.Constructors.Initialize (Self, URI, Text);

      Self.File          := File;
      Self.File_Provider := Provider;
   end Initialize;

   ----------
   -- Load --
   ----------

   procedure Load
     (Self          : in out Document;
      Client        : LSP.Ada_Client_Capabilities.Client_Capability;
      Configuration : LSP.Ada_Configurations.Configuration) is

      procedure Update_Diagnostics;
      --  Update Self.Messages, Self.Errors_Changed, Self.Has_Diagnostics

      ------------------------
      -- Update_Diagnostics --
      ------------------------

      procedure Update_Diagnostics is
         New_Messages : constant GPR2.Log.Object :=
                          Self.Tree.Log_Messages.all;
      begin
         if New_Messages /= Self.Messages then
            Self.Messages := New_Messages;
            Self.Errors_Changed := True;

            Self.Has_Messages := False;
            for C in Self.Tree.Log_Messages.Iterate (False) loop
               Self.Has_Messages := True;
               return;
            end loop;
         end if;
      end Update_Diagnostics;

   begin
      Self.Errors_Changed := True;

      --  Unload it to clean log.
      Self.Tree.Unload;

      declare
         Opts       : GPR2.Options.Object;
         Success    : Boolean;
         Update_Log : GPR2.Log.Object;
      begin
         Opts.Add_Switch (GPR2.Options.P, String (Self.File.Value));

         Success := Self.Tree.Load
            (Opts,
             With_Runtime     => True,
             Absent_Dir_Error => GPR2.No_Error,
             File_Reader      => Self.File_Provider.Get_File_Reader,
             Environment      => LSP.GPR_Files.Environment);

         if Success then
            Self.Tree.Update_Sources (Update_Log);
            if Update_Log.Has_Error then
               Self.Has_Messages := True;
            else
               Success := Self.Tree.Set_Context (Configuration.Context);
            end if;
         end if;

         if not Success then
            Self.Has_Messages := True;
         end if;

         --  Collect all messages coming from Load...
         for C in Self.Tree.Log_Messages.Iterate loop
            Self.Tracer.Trace (C.Element.Format);
            Self.Messages.Append (C.Element);
         end loop;

         --  ... and all messages coming from Update_Sources
         for C in Update_Log.Iterate loop
            Self.Tracer.Trace (C.Element.Format);
            Self.Messages.Append (C.Element);
         end loop;
      end;

      Update_Diagnostics;

   exception
      when GPR2.Project_Error =>

         Update_Diagnostics;

      when E : others =>

         Self.Tracer.Trace_Exception (E);

         Self.Tree.Log_Messages.Append
           (GPR2.Message.Create
              (Level   => GPR2.Message.Error,
               Message => "GPR parser unexpected " &
                 Ada.Exceptions.Exception_Information (E),
               Sloc    => GPR2.Source_Reference.Create
                 (Filename => Self.File.Value,
                  Line     => 1,
                  Column   => 1)));

         Update_Diagnostics;
   end Load;

   -----------------------------
   -- Update_Files_With_Diags --
   -----------------------------

   procedure Update_Files_With_Diags
     (Self : in out Document'Class; Files : GPR2.Path_Name.Set.Object) is
   begin
      Self.Published_Files_With_Diags := Files;
   end Update_Files_With_Diags;

   ------------------
   -- Get_Variable --
   ------------------

   function Get_Variable
     (Self      : Document'Class;
      Root_File : LSP.GPR_Files.File_Access;
      Reference : LSP.GPR_Files.References.Reference)
      return GPR2.Project.Variable.Object is

      function Variable
        (View : GPR2.Project.View.Object) return GPR2.Project.Variable.Object;

      function Variable
        (View : GPR2.Project.View.Object) return GPR2.Project.Variable.Object
      is
         Pack : constant GPR2.Package_Id :=
                  LSP.GPR_Files.References.Referenced_Package (Reference);
         Name : constant GPR2.Name_Type :=
                  GPR2.Name_Type
                    (VSS.Strings.Conversions.To_UTF_8_String
                       (LSP.GPR_Files.Image
                          (LSP.GPR_Files.References.Referenced_Variable
                             (Reference))));

         use type GPR2.Package_Id;
      begin
         if Pack = GPR2.Project_Level_Scope then
            if View.Has_Variables (Name) then
               return View.Variable (Name);
            end if;
         else
            if View.Has_Variables (Pack, Name) then
               return View.Variable (Pack, Name);
            end if;
         end if;
         return GPR2.Project.Variable.Undefined;
      end Variable;

   begin
      if LSP.GPR_Files.References.Is_Variable_Reference (Reference)
        and then not Self.Has_Errors
      then
         declare
            File : constant LSP.GPR_Files.File_Access :=
                     LSP.GPR_Files.References.Referenced_File
                       (File      => Root_File,
                        Reference => Reference);
            Path : constant GPR2.Path_Name.Object :=
                     LSP.GPR_Files.Path (File.all);
            Root : constant GPR2.Project.View.Object := Self.Tree.Root_Project;
         begin
            if Root.Path_Name = Path then
               return Variable (Root);
            end if;
            if Root.Is_Extended then
               declare
                  Extending : constant GPR2.Project.View.Object :=
                                Root.Extending;
               begin
                  if Extending.Path_Name = Path then
                     return Variable (Extending);
                  end if;
               end;
            end if;
            for Import of Root.Imports loop
               if Import.Path_Name = Path then
                  return Variable (Import);
               end if;
            end loop;
         end;
      end if;
      return GPR2.Project.Variable.Undefined;
   end Get_Variable;

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute
     (Self      : Document'Class;
      Root_File : LSP.GPR_Files.File_Access;
      Reference : LSP.GPR_Files.References.Reference)
      return GPR2.Project.Attribute.Object is

      function Attribute
        (View : GPR2.Project.View.Object) return GPR2.Project.Attribute.Object;

      Attr_Def : constant LSP.GPR_Files.References.Attribute_Definition :=
                   LSP.GPR_Files.References.Referenced_Attribute (Reference);

      ---------------
      -- Attribute --
      ---------------

      function Attribute
        (View : GPR2.Project.View.Object) return GPR2.Project.Attribute.Object
      is
         Result : GPR2.Project.Attribute.Object;
      begin
         if View.Check_Attribute
           (Name   => Attr_Def.Name,
            Index  => Attr_Def.Index,
            At_Pos => Attr_Def.At_Pos,
            Result => Result)
         then
            return Result;
         else
            return GPR2.Project.Attribute.Undefined;
         end if;
      end Attribute;

   begin
      if LSP.GPR_Files.References.Is_Attribute_Reference (Reference)
        and then not Self.Has_Errors
      then
         declare
            File : constant LSP.GPR_Files.File_Access :=
                     LSP.GPR_Files.References.Referenced_File
                       (File      => Root_File,
                        Reference => Reference);
            Path : constant GPR2.Path_Name.Object :=
                     LSP.GPR_Files.Path (File.all);
            Root : constant GPR2.Project.View.Object := Self.Tree.Root_Project;
         begin
            if Root.Path_Name = Path then
               return Attribute (Root);
            end if;
            if Root.Is_Extended then
               declare
                  Extending : constant GPR2.Project.View.Object :=
                                Root.Extending;
               begin
                  if Extending.Path_Name = Path then
                     return Attribute (Extending);
                  end if;
               end;
            end if;
            for Import of Root.Imports loop
               if Import.Path_Name = Path then
                  return Attribute (Import);
               end if;
            end loop;
         end;
      end if;
      return GPR2.Project.Attribute.Undefined;
   end Get_Attribute;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (Self      : Document'Class;
      Root_File : LSP.GPR_Files.File_Access;
      Reference : LSP.GPR_Files.References.Reference)
      return GPR2.Project.Typ.Object is

      function Typ
        (View : GPR2.Project.View.Object) return GPR2.Project.Typ.Object;

      ---------
      -- Typ --
      ---------

      function Typ
        (View : GPR2.Project.View.Object) return GPR2.Project.Typ.Object
      is
         Name : constant GPR2.Optional_Name_Type :=
                  GPR2.Optional_Name_Type
                    (VSS.Strings.Conversions.To_UTF_8_String
                       (LSP.GPR_Files.Image (Reference.Referenced_Type)));

      begin
         if View.Has_Types (Name) then
            declare
               Set : constant GPR2.Project.Typ.Set.Object := View.Types;
            begin
               return Set (Name);
            end;
         else
            return GPR2.Project.Typ.Undefined;
         end if;
      end Typ;

   begin
      if LSP.GPR_Files.References.Is_Type_Reference (Reference)
        and then not Self.Has_Errors
      then
         declare
            File : constant LSP.GPR_Files.File_Access :=
                     LSP.GPR_Files.References.Referenced_File
                       (File      => Root_File,
                        Reference => Reference);
            Path : constant GPR2.Path_Name.Object :=
                     LSP.GPR_Files.Path (File.all);
            Root : constant GPR2.Project.View.Object := Self.Tree.Root_Project;
         begin
            if Root.Path_Name = Path then
               return Typ (Root);
            end if;
            if Root.Is_Extended then
               declare
                  Extending : constant GPR2.Project.View.Object :=
                                Root.Extending;
               begin
                  if Extending.Path_Name = Path then
                     return Typ (Extending);
                  end if;
               end;
            end if;
            for Import of Root.Imports loop
               if Import.Path_Name = Path then
                  return Typ (Import);
               end if;
            end loop;
         end;
      end if;
      return GPR2.Project.Typ.Undefined;
   end Get_Type;

end LSP.GPR_Documents;

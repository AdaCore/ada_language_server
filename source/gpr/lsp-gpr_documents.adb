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
with GPR2.Source_Reference;

package body LSP.GPR_Documents is

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Self : in out Document) is
   begin
      Self.Tree.Unload (True);
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

   procedure Load  (Self : in out Document) is

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

      Self.Tree.Load_Autoconf
        (Filename          => Self.File,
         Context           => Self.Context,
         File_Reader       => Self.File_Provider.Get_File_Reader,
         Environment       => Self.Environment);

      Update_Diagnostics;

   exception
      when GPR2.Project_Error | GPR2.Processing_Error =>

         Update_Diagnostics;

      when E : others =>

         Self.Tracer.Trace_Exception (E);

         Self.Tree.Log_Messages.Append
           (GPR2.Message.Create
              (Level   => GPR2.Message.Error,
               Message => "GPR parser unexpected " &
                 Ada.Exceptions.Exception_Name (E) & " " &
                 Ada.Exceptions.Exception_Message (E),
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

end LSP.GPR_Documents;

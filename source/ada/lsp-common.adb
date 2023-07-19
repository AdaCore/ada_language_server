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

with Ada.Characters.Latin_1;
with Ada.Characters.Handling;  use Ada.Characters.Handling;

with Ada.Exceptions;           use Ada.Exceptions;

with GNAT.Expect.TTY;
with GNAT.Traceback.Symbolic;  use GNAT.Traceback.Symbolic;
with GNATCOLL.Utils;           use GNATCOLL.Utils;

with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;

package body LSP.Common is

   ---------
   -- Log --
   ---------

   procedure Log
     (Trace   : GNATCOLL.Traces.Trace_Handle;
      E       : Ada.Exceptions.Exception_Occurrence;
      Message : String := "") is
   begin
      if Trace.Is_Active and then Message /= "" then
         Trace.Trace (Message);
      end if;

      if Trace.Is_Active then
         Trace.Trace (Exception_Name (E) & ": " & Exception_Message (E)
                     & Ada.Characters.Latin_1.LF & Symbolic_Traceback (E));
      end if;
   end Log;

   ----------------
   -- Get_Output --
   ----------------

   function Get_Output
     (Exe  : Virtual_File;
      Args : GNAT.OS_Lib.Argument_List) return String
   is
   begin
      if Exe = No_File then
         return "";
      end if;

      declare
         Fd : aliased GNAT.Expect.TTY.TTY_Process_Descriptor;
      begin
         GNAT.Expect.Non_Blocking_Spawn
           (Descriptor  => Fd,
            Command     => Exe.Display_Full_Name,
            Buffer_Size => 0,
            Args        => Args,
            Err_To_Out  => True);
         declare
            S : constant String :=
              GNATCOLL.Utils.Get_Command_Output (Fd'Access);
         begin
            GNAT.Expect.TTY.Close (Fd);

            return S;
         end;
      exception
         when GNAT.Expect.Process_Died =>
            GNAT.Expect.TTY.Close (Fd);
            return "";
      end;
   end Get_Output;

   ----------------------
   -- Is_Ada_Separator --
   ----------------------

   function Is_Ada_Separator
     (Item : VSS.Characters.Virtual_Character) return Boolean is
   begin
      --  Ada 2012's RM defines separator as 'separator_space',
      --  'format_efector' or end of a line, with some exceptions inside
      --  comments.
      --
      --  'separator_space' is defined as a set of characters with
      --  'General Category' defined as 'Separator, Space'.
      --
      --  'format_effector' is set of characters:
      --    - CHARACTER TABULATION
      --    - LINE FEED
      --    - LINE TABULATION
      --    - FORM FEED
      --    - CARRIAGE RETURN
      --    - NEXT LINE
      --    - characters with General Category defined as
      --      'Separator, Line'
      --    - characters with General Category defined as
      --      'Separator, Paragraph'

      return
        Item in
            VSS.Characters.Virtual_Character'Val (16#09#)
          | VSS.Characters.Virtual_Character'Val (16#0A#)
          | VSS.Characters.Virtual_Character'Val (16#0B#)
          | VSS.Characters.Virtual_Character'Val (16#0C#)
          | VSS.Characters.Virtual_Character'Val (16#0D#)
          | VSS.Characters.Virtual_Character'Val (16#85#)
        or VSS.Characters.Get_General_Category (Item) in
            VSS.Characters.Space_Separator
          | VSS.Characters.Line_Separator
          | VSS.Characters.Paragraph_Separator;
   end Is_Ada_Separator;
begin

   GPR2.Project.Registry.Pack.Add
     (Name     => Pretty_Printer_Id,
      Projects => GPR2.Project.Registry.Pack.Everywhere);

   GPR2.Project.Registry.Attribute.Add
     (Name                 => Pretty_Printer.Switches,
      Index_Type           => GPR2.Project.Registry.Attribute.
                                 FileGlob_Or_Language_Index,
      Index_Optional       => True,
      Value                => GPR2.Project.Registry.Attribute.List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => GPR2.Project.Registry.Attribute.Everywhere);

   GPR2.Project.Registry.Attribute.Add_Alias
     (Pretty_Printer.Default_Switches, Pretty_Printer.Switches);

end LSP.Common;

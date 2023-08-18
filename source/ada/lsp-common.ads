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

--  A place for commonly used utilities, such as trace or debug functions.

with Ada.Exceptions;
with GNAT.OS_Lib;
with GNATCOLL.Traces;
with GNATCOLL.VFS;          use GNATCOLL.VFS;

with VSS.Characters;
with VSS.Strings;

with GPR2.Project.Attribute_Index;

with LSP.Messages;

package LSP.Common is

   use GPR2;

   LSP_New_Line_Function_Set : constant VSS.Strings.Line_Terminator_Set :=
     (VSS.Strings.CR | VSS.Strings.CRLF | VSS.Strings.LF => True,
      others => False);
   --  LSP allows to use three kinds of line terminators: CR, CR+LF and LF.

   Is_Parent : constant LSP.Messages.AlsReferenceKind_Set :=
     (Is_Server_Side => True,
      As_Flags => (LSP.Messages.Parent => True, others => False));
   Is_Child : constant LSP.Messages.AlsReferenceKind_Set :=
     (Is_Server_Side => True,
      As_Flags => (LSP.Messages.Child => True, others => False));
   --  Convenient constants

   Pretty_Printer_Id : constant GPR2.Package_Id := +"pretty_printer";

   package Pretty_Printer is
      Default_Switches : constant Q_Attribute_Id :=
        (Pretty_Printer_Id, +"default_switches");
      Switches         : constant Q_Attribute_Id :=
        (Pretty_Printer_Id, +"switches");
   end Pretty_Printer;

   Ada_Index : constant GPR2.Project.Attribute_Index.Object :=
                  GPR2.Project.Attribute_Index.Create
                     (GPR2.Ada_Language);

   procedure Log
     (Trace   : GNATCOLL.Traces.Trace_Handle;
      E       : Ada.Exceptions.Exception_Occurrence;
      Message : String := "");
   --  Log an exception in the given traces, with an optional message

   function Is_Ada_Separator
     (Item : VSS.Characters.Virtual_Character) return Boolean;
   --  Return True when given character belongs to 'separator' category,
   --  defined by Ada 2012 Reference Manual.

end LSP.Common;

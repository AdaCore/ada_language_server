------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2022, AdaCore                     --
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
with GNATCOLL.Projects;
with GNATCOLL.Traces;
with GNATCOLL.VFS;          use GNATCOLL.VFS;

with VSS.Characters;
with VSS.Strings;
with VSS.String_Vectors;

with LSP.Messages;
with Libadalang.Analysis;   use Libadalang.Analysis;

package LSP.Common is

   LSP_New_Line_Function_Set : constant VSS.Strings.Line_Terminator_Set :=
     (VSS.Strings.CR | VSS.Strings.CRLF | VSS.Strings.LF => True,
      others => False);
   --  LSP allows to use three kinds of line terminators: CR, CR+LF and LF.

   Document_LSP_New_Line_Function : constant VSS.Strings.Line_Terminator :=
     VSS.Strings.LF;
   --  Line terminator to be used to generate replies. It is fixed to LF now.

   Is_Parent : constant LSP.Messages.AlsReferenceKind_Set :=
     (Is_Server_Side => True,
      As_Flags => (LSP.Messages.Parent => True, others => False));
   Is_Child : constant LSP.Messages.AlsReferenceKind_Set :=
     (Is_Server_Side => True,
      As_Flags => (LSP.Messages.Child => True, others => False));
   --  Convenient constants

   procedure Log
     (Trace   : GNATCOLL.Traces.Trace_Handle;
      E       : Ada.Exceptions.Exception_Occurrence;
      Message : String := "");
   --  Log an exception in the given traces, with an optional message

   function Get_Output
     (Exe  : Virtual_File;
      Args : GNAT.OS_Lib.Argument_List) return String;
   --  Run the given command line and return the output.

   function Get_Hover_Text
     (Decl         : Basic_Decl'Class;
      Code_Snippet : VSS.String_Vectors.Virtual_String_Vector :=
        VSS.String_Vectors.Empty_Virtual_String_Vector)
      return VSS.Strings.Virtual_String;
   --  Return a pretty printed version of the declaration's text to be
   --  displayed on hover requests, removing unnecessary indentation
   --  whitespaces if needed and attaching extra information in some cases.

   function Is_Ada_Separator
     (Item : VSS.Characters.Virtual_Character) return Boolean;
   --  Return True when given character belongs to 'separator' category,
   --  defined by Ada 2012 Reference Manual.

   function Is_Ada_File
     (Tree : GNATCOLL.Projects.Project_Tree_Access;
      File : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Return whether the file is an Ada file according to the project's
   --  naming scheme.

end LSP.Common;

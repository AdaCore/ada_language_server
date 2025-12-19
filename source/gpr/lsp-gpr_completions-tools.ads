------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2025, AdaCore                       --
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

--  This package provides utility tools for GPR completion operations.

with LSP.GPR_Files;
with LSP.Structures;
with GPR2;
with VSS.Strings;

package LSP.GPR_Completions.Tools is

   procedure Fill_Tools_Completion_Response
     (File            : LSP.GPR_Files.File_Access;
      Current_Package : GPR2.Package_Id;
      Prefix          : VSS.Strings.Virtual_String;
      Response        : in out LSP.Structures.Completion_Result);
   --  Fill the given completion response with tool switches
   --  for the tool associated with the current package,
   --  filtered by the given prefix.
   --  (i.e: if the current package is "compiler", the tool is "gnat",
   --  and the completion response will be filled with gnat switches
   --  matching the given prefix)

   procedure Load_Database (Has_Label_Details_Support : Boolean);
   --  Load the tools switches database and populate the cache
   --  Has_Label_Details_Support indicates whether the client supports
   --  label details in completion items.

   procedure Get_Tool_Switches
     (Tool_Name : VSS.Strings.Virtual_String;
      Result    : in out LSP.Structures.CompletionItem_Vector);
   --  Return completion items for all switches of the given tool,
   --  as defined in the gnat_tools_help.json database.

end LSP.GPR_Completions.Tools;

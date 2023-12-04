------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

--  This package provides completion related requests handler for GPR language.

with LSP.GPR_Files;

with LSP.Structures;

package LSP.GPR_Completions is

   procedure Fill_Completion_Response
     (File_Provider           : LSP.GPR_Files.File_Provider_Access;
      Value                   : LSP.Structures.CompletionParams;
      Compute_Doc_And_Details : Boolean;
      Response                : in out LSP.Structures.Completion_Result);

   procedure Fill_Completion_Resolve_Response
     (Response : in out LSP.Structures.CompletionItem);

end LSP.GPR_Completions;

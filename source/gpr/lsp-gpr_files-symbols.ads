------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
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
--
--  This package provides Get_Symbols requests implementation

with LSP.Messages;
with LSP.Messages.Server_Requests;

package LSP.GPR_Files.Symbols is

   procedure Get_Symbols
     (Provider : LSP.GPR_Files.File_Provider_Access;
      Request  : LSP.Messages.Server_Requests.Document_Symbols_Request;
      Result   : out LSP.Messages.Symbol_Vector);

   procedure Get_Symbols_Hierarchy
     (Provider : LSP.GPR_Files.File_Provider_Access;
      Request  : LSP.Messages.Server_Requests.Document_Symbols_Request;
      Result   : out LSP.Messages.Symbol_Vector);

end LSP.GPR_Files.Symbols;

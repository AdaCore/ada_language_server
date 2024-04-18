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

with Libadalang.Common;

with LSP.Ada_Handlers.Locations;
with LSP.Constants;
with LSP.Utils;

package body LSP.Ada_Handlers.Symbols is

   -------------------
   -- Write_Symbols --
   -------------------

   procedure Write_Symbols
     (Self   : in out Message_Handler'Class;
      Names  : LSP.Ada_Completions.Completion_Maps.Map;
      Result : in out LSP.Structures.SymbolInformation_Vector) is
   begin
      for Cursor in Names.Iterate loop
         declare
            Name : constant Libadalang.Analysis.Defining_Name :=
              LSP.Ada_Completions.Completion_Maps.Key (Cursor);
            Node : Libadalang.Analysis.Ada_Node := Name.As_Ada_Node;
         begin
            while not Node.Is_Null and then
              Node.Kind not in Libadalang.Common.Ada_Basic_Decl
            loop
               Node := Node.Parent;
            end loop;

            if not Node.Is_Null then
               Result.Append
                 (LSP.Structures.SymbolInformation'
                    (name     => VSS.Strings.To_Virtual_String (Name.Text),
                     kind     => LSP.Utils.Get_Decl_Kind
                                  (Node.As_Basic_Decl),
                     location => Locations.To_LSP_Location
                                  (Self, Name),
                     tags          => LSP.Constants.Empty,
                     deprecated    => (Is_Set => False),
                     containerName => <>));
            end if;
         end;
      end loop;
   end Write_Symbols;

end LSP.Ada_Handlers.Symbols;

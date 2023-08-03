------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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

with VSS.Strings;

with LSP_Gen.Entities;
with LSP_Gen.Meta_Models;

package LSP_Gen.Requests is

   procedure Write (Model : LSP_Gen.Meta_Models.Meta_Model);

   use all type LSP_Gen.Entities.Enum.AType_Variant;

   function Param_Type
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Method : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String
   is (if Model.Request (Method).params.Value.Union.Kind = an_and
       then Model.Request (Method).params.Value.Union.an_and.items (1)
               .Union.reference.name
       else Model.Request (Method).params.Value.Union.reference.name);

end LSP_Gen.Requests;

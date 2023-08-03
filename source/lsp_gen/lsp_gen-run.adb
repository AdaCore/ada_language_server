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

with VSS.Application;
with VSS.Strings;

with LSP_Gen.Dependencies;
with LSP_Gen.Enumerations;
with LSP_Gen.Factories;
with LSP_Gen.Inputs;
with LSP_Gen.Meta_Models;
with LSP_Gen.Notifications;
with LSP_Gen.Outputs;
with LSP_Gen.Progress_Reports;
with LSP_Gen.Requests;
with LSP_Gen.Responses;
with LSP_Gen.Structures;

procedure LSP_Gen.Run is
   Model_Name  : constant VSS.Strings.Virtual_String :=
     VSS.Application.Arguments.Element (1);

   Config_Name : constant VSS.Strings.Virtual_String :=
     VSS.Application.Arguments.Element (2);

   Model  : LSP_Gen.Meta_Models.Meta_Model;
   Done   : LSP_Gen.Dependencies.Dependency_Map;
begin
   Model.Read_Model (Model_Name, Config_Name);

   LSP_Gen.Structures.Write_Types (Model, Done);
   LSP_Gen.Progress_Reports.Write (Model, Done);
   LSP_Gen.Responses.Write (Model, Done);
   LSP_Gen.Enumerations.Write_Types (Model);
   LSP_Gen.Requests.Write (Model);
   LSP_Gen.Notifications.Write (Model);
   LSP_Gen.Outputs.Write (Model, Done);
   LSP_Gen.Inputs.Write (Model, Done);
   LSP_Gen.Factories.Write (Model, Done);
end LSP_Gen.Run;

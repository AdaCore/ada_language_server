------------------------------------------------------------------------------
--                               GNAT Studio                                --
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

--  Implementation of the command to fetch dependencies for a given GPR file.

with Ada.Containers.Vectors;
with LSP.Ada_Commands;
with LSP.Errors;
with LSP.Server_Jobs;

package LSP.Ada_Handlers.GPR_Dependencies_Commands is

   type Command is new LSP.Ada_Commands.Command with private;

private

   type GPR_Dependency_Kind is (Aggregated, Extended, Extending, Imported);

   type GPR_Dependencies_Direction is (Show_Outgoing, Show_Incoming);

   type Command is new LSP.Ada_Commands.Command with record

      URI       : LSP.Structures.DocumentUri;
      Direction : GPR_Dependencies_Direction := Show_Incoming;
   end record;

   type GPR_Dependency_Item is record
      URI  : LSP.Structures.DocumentUri;
      Kind : GPR_Dependency_Kind;
   end record;

   package GPR_Dependency_Item_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => GPR_Dependency_Item);

   type GPR_Dependency_Item_Vector is new GPR_Dependency_Item_Vectors.Vector
   with null record;

   overriding
   function Create
     (Any : not null access LSP.Structures.LSPAny_Vector) return Command;

   overriding
   procedure Execute
     (Self     : Command;
      Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Response : in out LSP.Structures.LSPAny_Or_Null;
      Error    : in out LSP.Errors.ResponseError_Optional);

   overriding
   function Priority (Self : Command) return LSP.Server_Jobs.Job_Priority
   is (LSP.Server_Jobs.High);

   for Command'External_Tag use "als-gpr-dependencies";

end LSP.Ada_Handlers.GPR_Dependencies_Commands;

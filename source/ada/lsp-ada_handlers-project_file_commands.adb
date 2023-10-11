------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2021, AdaCore                       --
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

with GPR2.Project.View;

with VSS.JSON.Streams;

package body LSP.Ada_Handlers.Project_File_Commands is

   ------------
   -- Create --
   ------------

   overriding function Create
     (Any : not null access LSP.Structures.LSPAny_Vector)
       return Command is
   begin
      --  We have no arguments for this command
      return V : Command;
   end Create;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self     : Command;
      Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Response : in out LSP.Structures.LSPAny_Or_Null;
      Error    : in out LSP.Errors.ResponseError_Optional)
   is
      procedure Append (Item : VSS.JSON.Streams.JSON_Stream_Element);

      ------------
      -- Append --
      ------------

      procedure Append (Item : VSS.JSON.Streams.JSON_Stream_Element) is
      begin
         Response.Value.Append (Item);
      end Append;

      Value   : VSS.Strings.Virtual_String;
      Element : GPR2.Project.View.Object;
   begin
      if Handler.Project_Tree.Is_Defined then
         Element := Handler.Project_Tree.Root_Project;
         Value := VSS.Strings.Conversions.To_Virtual_String
           (Element.Path_Name.Value);
      end if;

      Response := (Is_Null => False, Value => <>);
      Append ((VSS.JSON.Streams.String_Value, Value));
   end Execute;

end LSP.Ada_Handlers.Project_File_Commands;

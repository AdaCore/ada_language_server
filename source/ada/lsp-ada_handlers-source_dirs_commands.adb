------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2020-2023, AdaCore                  --
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

with VSS.JSON.Streams;
with VSS.Strings.Conversions;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with URIs;

package body LSP.Ada_Handlers.Source_Dirs_Commands is

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

      Source_Dirs : constant GNATCOLL.VFS.File_Array :=
        Handler.Contexts.All_Source_Directories
          (Include_Externally_Built => True);
   begin
      Response := (Is_Null => False, Value => <>);
      Append ((Kind => VSS.JSON.Streams.Start_Array));

      for Dir of Source_Dirs loop
         Append ((Kind => VSS.JSON.Streams.Start_Object));
         Append ((VSS.JSON.Streams.Key_Name, "name"));
         Append ((VSS.JSON.Streams.String_Value, VSS.Strings.Conversions.To_Virtual_String
            (Dir.Display_Base_Dir_Name)));
         Append ((VSS.JSON.Streams.Key_Name, "uri"));
         Append ((VSS.JSON.Streams.String_Value,  VSS.Strings.Conversions.To_Virtual_String
            (URIs.Conversions.From_File (Dir.Display_Full_Name))));
         Append ((Kind => VSS.JSON.Streams.End_Object));
      end loop;

      Append ((Kind => VSS.JSON.Streams.End_Array));
   end Execute;

end LSP.Ada_Handlers.Source_Dirs_Commands;

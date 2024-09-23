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

with Ada.Containers.Indefinite_Vectors;
with GPR2.Project.View;

with VSS.JSON.Streams;

package body LSP.Ada_Handlers.Mains_Commands is

   package Main_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

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

      Element    : GPR2.Project.View.Object;
      Main_Paths : Main_Vectors.Vector;
   begin
      Response := (Is_Null => False, Value => <>);
      Append ((Kind => VSS.JSON.Streams.Start_Array));

      if Handler.Project_Tree.Is_Defined then
         Element := Handler.Project_Tree.Root_Project;

         if Element.Has_Mains then
            for Main of Element.Mains loop
               declare
                  Main_Path : constant String := String (Main.Source.Value);
               begin
                  --  Avoid duplicates coming from GPR2
                  --  Workaround for eng/ide/gpr-issues#417
                  if not Main_Paths.Contains (Main_Path) then
                     Main_Paths.Append (Main_Path);
                  end if;
               end;
            end loop;
         end if;
      end if;

      for Main_Path of Main_Paths loop
         Append
           ((VSS.JSON.Streams.String_Value,
            VSS.Strings.Conversions.To_Virtual_String (Main_Path)));
      end loop;

      Append ((Kind => VSS.JSON.Streams.End_Array));
   end Execute;

end LSP.Ada_Handlers.Mains_Commands;

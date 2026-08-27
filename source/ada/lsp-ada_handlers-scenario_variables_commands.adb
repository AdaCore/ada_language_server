------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2026, AdaCore                         --
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

with Ada.Strings.Unbounded;

with GNATCOLL.JSON;

with VSS.JSON.Pull_Readers.Simple;
with VSS.JSON.Streams;
with VSS.Stream_Element_Vectors.Conversions;
with VSS.Text_Streams.Memory_UTF8_Input;

with GPR2;
with GPR2.Context;
with GPR2.Project.External;

package body LSP.Ada_Handlers.Scenario_Variables_Commands is

   ------------
   -- Create --
   ------------

   overriding
   function Create
     (Any : not null access LSP.Structures.LSPAny_Vector) return Command
   is
      pragma Unreferenced (Any);
   begin
      return (LSP.Ada_Commands.Command with null record);
   end Create;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute
     (Self     : Command;
      Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Response : in out LSP.Structures.LSPAny_Or_Null;
      Error    : in out LSP.Errors.ResponseError_Optional)
   is
      pragma Unreferenced (Self, Error);

      procedure Append (Item : VSS.JSON.Streams.JSON_Stream_Element);
      --  Append a stream element to the response

      procedure Append_JSON_String (JSON_Text : String);
      --  Parse a JSON string and append its elements to the response

      ------------
      -- Append --
      ------------

      procedure Append (Item : VSS.JSON.Streams.JSON_Stream_Element) is
      begin
         Response.Value.Append (Item);
      end Append;

      ------------------------
      -- Append_JSON_String --
      ------------------------

      procedure Append_JSON_String (JSON_Text : String) is
         use all type VSS.JSON.Streams.JSON_Stream_Element_Kind;

         Buffer : aliased VSS.Text_Streams.Memory_UTF8_Input.Memory_UTF8_Input_Stream;
         Reader : VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
         Data   : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      begin
         Data :=
           VSS.Stream_Element_Vectors.Conversions.Unchecked_From_Unbounded_String
             (Ada.Strings.Unbounded.To_Unbounded_String (JSON_Text));

         Buffer.Set_Data (Data);
         Reader.Set_Stream (Buffer'Unchecked_Access);

         while not Reader.At_End loop
            declare
               Kind : constant VSS.JSON.Streams.JSON_Stream_Element_Kind :=
                 Reader.Read_Next;
            begin
               case Kind is
                  when None | Invalid | Start_Document | End_Document =>
                     null;
                  when Comment =>
                     null;
                  when Start_Array =>
                     Append ((Kind => Start_Array));
                  when End_Array =>
                     Append ((Kind => End_Array));
                  when Start_Object =>
                     Append ((Kind => Start_Object));
                  when End_Object =>
                     Append ((Kind => End_Object));
                  when Key_Name =>
                     Append ((Kind => Key_Name, Key_Name => Reader.Key_Name));
                  when String_Value =>
                     Append ((Kind => String_Value, String_Value => Reader.String_Value));
                  when Number_Value =>
                     Append ((Kind => Number_Value, Number_Value => Reader.Number_Value));
                  when Boolean_Value =>
                     Append ((Kind => Boolean_Value, Boolean_Value => Reader.Boolean_Value));
                  when Null_Value =>
                     Append ((Kind => Null_Value));
               end case;
            end;
         end loop;
      end Append_JSON_String;

      JSON_Res  : constant GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.Create_Object;
      Vars      : GNATCOLL.JSON.JSON_Array;

   begin
      if not Handler.Project_Tree_Is_Defined then
         Response := (Is_Null => True);
         return;
      end if;

      declare
         Externals : constant GPR2.Project.External.External_Arr :=
           GPR2.Project.External.Externals
             (Tree => Handler.Project_Tree, Root_Only => False);
         --  The context GPR2 actually resolved the project with: whatever
         --  value the language server applied for each external, whether it
         --  came from the .als.json file, the client's scenarioVariables
         --  setting, or the OS environment. This is the authoritative
         --  "current value" for a scenario variable, regardless of which of
         --  those sources it came from.
         Ctx       : constant GPR2.Context.Object := Handler.Project_Tree.Context;
      begin
         for Ext of Externals loop
            declare
               Var_Obj         : constant GNATCOLL.JSON.JSON_Value :=
                 GNATCOLL.JSON.Create_Object;
               Possible_Values : GNATCOLL.JSON.JSON_Array;
               Key             : constant GPR2.External_Name_Type :=
                 GPR2.External_Name_Type (GPR2.Project.External.Name (Ext));
            begin
               GNATCOLL.JSON.Set_Field
                 (Var_Obj, "name", GPR2.Project.External.Name (Ext));
               GNATCOLL.JSON.Set_Field
                 (Var_Obj, "typed", GPR2.Project.External.Is_Typed (Ext));
               GNATCOLL.JSON.Set_Field
                 (Var_Obj,
                  "conflicting",
                  GPR2.Project.External.Is_Conflicting (Ext));

               if Ctx.Contains (Key) then
                  GNATCOLL.JSON.Set_Field
                    (Var_Obj, "value", String (Ctx.Element (Key)));
               end if;

               for Value of GPR2.Project.External.Possible_Values_Of (Ext) loop
                  GNATCOLL.JSON.Append
                    (Possible_Values,
                     GNATCOLL.JSON.Create
                       (Ada.Strings.Unbounded.To_String (Value)));
               end loop;
               GNATCOLL.JSON.Set_Field
                 (Var_Obj, "possible-values", Possible_Values);

               GNATCOLL.JSON.Append (Vars, Var_Obj);
            end;
         end loop;
      end;

      GNATCOLL.JSON.Set_Field (JSON_Res, "variables", Vars);

      Response := (Is_Null => False, Value => <>);

      Append_JSON_String (GNATCOLL.JSON.Write (JSON_Res));
   end Execute;

end LSP.Ada_Handlers.Scenario_Variables_Commands;

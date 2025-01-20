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

with GPR2;                          use GPR2;
with VSS.String_Vectors;
with VSS.JSON.Streams;
with LSP.Enumerations;
with LSP.Structures.LSPAny_Vectors; use LSP.Structures.LSPAny_Vectors;

package body LSP.Ada_Handlers.Project_Attributes_Commands is

   ------------
   -- Create --
   ------------

   overriding function Create
     (Any : not null access LSP.Structures.LSPAny_Vector)
      return Command
   is
      use VSS.JSON.Streams;
      use VSS.Strings;
      use LSP.Structures.JSON_Event_Vectors;

      C : Cursor := Any.First;
   begin
      return Self : Command do
         pragma Assert (Element (C).Kind = Start_Array);
         Next (C);
         pragma Assert (Element (C).Kind = Start_Object);
         Next (C);

         while Has_Element (C) and then Element (C).Kind /= End_Object loop
            pragma Assert (Element (C).Kind = Key_Name);
            declare
               Key : constant Virtual_String := Element (C).Key_Name;
            begin
               Next (C);

               if Key = "pkg" then
                  Self.Pkg := Element (C).String_Value;
               elsif Key = "attribute" then
                  Self.Attribute := Element (C).String_Value;
               elsif Key = "index" then
                  Self.Index := Element (C).String_Value;
               else
                  Skip_Value (C);
               end if;
            end;

            Next (C);
         end loop;
      end return;
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
      procedure Append (Item : VSS.JSON.Streams.JSON_Stream_Element);
      --  Append the given item to the JSON response

      ------------
      -- Append --
      ------------

      procedure Append (Item : VSS.JSON.Streams.JSON_Stream_Element) is
      begin
         Response.Value.Append (Item);
      end Append;

      Attr_Id           : constant GPR2.Q_Optional_Attribute_Id :=
        (Pack =>
           GPR2."+"
             (Optional_Name_Type
                (VSS.Strings.Conversions.To_UTF_8_String (Self.Pkg))),
         Attr =>
           GPR2."+"
             (Optional_Name_Type
                (VSS.Strings.Conversions.To_UTF_8_String (Self.Attribute))));
      Is_List_Attribute : Boolean;
      Is_Known         : Boolean;
      Values            : constant VSS.String_Vectors.Virtual_String_Vector :=
        LSP.Ada_Contexts.Project_Attribute_Values
          (View              => Handler.Project_Tree.Root_Project,
           Attribute         => Attr_Id,
           Index             =>
             VSS.Strings.Conversions.To_UTF_8_String (Self.Index),
           Is_List_Attribute => Is_List_Attribute,
           Is_Known          => Is_Known);
   begin
      --  Return an error if the attribute is not known.
      if not Is_Known then
         Error :=
           (Is_Set => True,
            Value  =>
              (code    => LSP.Enumerations.InvalidParams,
               message => "The queried attribute is not known"));
         return;
      end if;

      Response := (Is_Null => False, Value => <>);

      --  Return a list object if we are dealing with a string list attribute,
      --  or  a string otherwise.
      if Is_List_Attribute then
         Append ((Kind => VSS.JSON.Streams.Start_Array));
         for Value of Values loop
            Append (Item => (VSS.JSON.Streams.String_Value, Value));
         end loop;
         Append ((Kind => VSS.JSON.Streams.End_Array));
      else
         Append
           (Item => (VSS.JSON.Streams.String_Value, Values.First_Element));
      end if;
   end Execute;

end LSP.Ada_Handlers.Project_Attributes_Commands;

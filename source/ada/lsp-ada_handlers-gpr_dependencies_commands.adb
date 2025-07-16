------------------------------------------------------------------------------
--                               GNAT Studio                                --
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

with GPR2.Project.View.Vector;
with Interfaces;

with VSS.JSON;
with VSS.JSON.Streams;
with VSS.Strings; use VSS.Strings;

with GPR2;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Project.View.Set;

with GNATCOLL.VFS;
with Ada.Containers; use Ada.Containers;

package body LSP.Ada_Handlers.GPR_Dependencies_Commands is

   ------------
   -- Create --
   ------------

   overriding
   function Create
     (Any : not null access LSP.Structures.LSPAny_Vector) return Command
   is
      use all type VSS.JSON.Streams.JSON_Stream_Element_Kind;

      Index : Natural := Any.First_Index;
      Key   : VSS.Strings.Virtual_String;

      use type Interfaces.Integer_64;
      use all type VSS.JSON.JSON_Number_Kind;
   begin
      return Result : Command do
         if Index < Any.Last_Index and then Any (Index).Kind = Start_Array then
            Index := Index + 1;

            if Index < Any.Last_Index and then Any (Index).Kind = Start_Object
            then
               Index := Index + 1;

               while Index < Any.Last_Index
                 and then Any (Index).Kind = Key_Name
               loop
                  Key := Any (Index).Key_Name;
                  Index := Index + 1;

                  if Key = "uri"
                    and then Index < Any.Last_Index
                    and then Any (Index).Kind = String_Value
                  then
                     Result.URI := (Any (Index).String_Value with null record);
                  elsif Key = "direction"
                    and then Index < Any.Last_Index
                    and then Any (Index).Kind = Number_Value
                    and then Any (Index).Number_Value.Kind = JSON_Integer
                    and then Any (Index).Number_Value.Integer_Value in 1 .. 2
                  then
                     Result.Direction :=
                       GPR_Dependencies_Direction'Val
                         (Any (Index).Number_Value.Integer_Value - 1);
                  end if;

                  Index := Index + 1;
               end loop;
            end if;
         end if;
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

      procedure Append_Item
        (Result    : out GPR_Dependency_Item_Vector;
         Path_Name : GPR2.Path_Name.Object;
         Kind      : GPR_Dependency_Kind);

      procedure Append_Import
        (View       : GPR2.Project.View.Object;
         Result     : out GPR_Dependency_Item_Vector;
         Child_Name : GPR2.Name_Type := "");

      procedure Append_Aggregate
        (View       : GPR2.Project.View.Object;
         Result     : out GPR_Dependency_Item_Vector;
         Child_Name : GPR2.Name_Type := "");

      procedure Append_Extend
        (View       : GPR2.Project.View.Object;
         Result     : out GPR_Dependency_Item_Vector;
         Child_Name : GPR2.Name_Type := "");

      procedure Get_Outgoing_Dependencies
        (Base_Name : GPR2.Name_Type;
         Tree      : GPR2.Project.Tree.Object;
         Result    : out GPR_Dependency_Item_Vector);

      procedure Get_Incoming_Dependencies
        (Base_Name : GPR2.Name_Type;
         Tree      : GPR2.Project.Tree.Object;
         Result    : out GPR_Dependency_Item_Vector);

      ------------
      -- Append --
      ------------

      procedure Append (Item : VSS.JSON.Streams.JSON_Stream_Element) is
      begin
         Response.Value.Append (Item);
      end Append;

      -----------------
      -- Append_Item --
      -----------------

      procedure Append_Item
        (Result    : out GPR_Dependency_Item_Vector;
         Path_Name : GPR2.Path_Name.Object;
         Kind      : GPR_Dependency_Kind) is
      begin
         declare
            Item : constant GPR_Dependency_Item :=
              (Handler.To_URI (Path_Name.String_Value), Kind);
         begin
            Result.Append (Item);
         end;
      end Append_Item;

      -------------------
      -- Append_Import --
      -------------------

      procedure Append_Import
        (View       : GPR2.Project.View.Object;
         Result     : out GPR_Dependency_Item_Vector;
         Child_Name : GPR2.Name_Type := "") is
      begin
         if View.Has_Imports then
            for Imported_View of View.Imports (False) loop
               if Child_Name."=" ("") then
                  Append_Item (Result, Imported_View.Path_Name, Imported);
               elsif Child_Name."=" (Imported_View.Name) then
                  Append_Item (Result, View.Path_Name, Imported);
               end if;
            end loop;
         end if;
      end Append_Import;

      ----------------------
      -- Append_Aggregate --
      ----------------------

      procedure Append_Aggregate
        (View       : GPR2.Project.View.Object;
         Result     : out GPR_Dependency_Item_Vector;
         Child_Name : GPR2.Name_Type := "") is
         use type GPR2.Optional_Name_Type;
      begin
         if View.Kind in GPR2.Aggregate_Kind then
            for Aggregated_View of View.Aggregated (False) loop
               if Child_Name = "" then
                  Append_Item (Result, Aggregated_View.Path_Name, Aggregated);
               elsif Child_Name = Aggregated_View.Name then
                  Append_Item (Result, View.Path_Name, Aggregated);
               end if;
               --  If Child Name is empty, we are searching for children so we add the Aggregated_View.
               --  If not, we are searching for the parent so we add the original View.
            end loop;
         end if;
      end Append_Aggregate;

      -------------------
      -- Append_Extend --
      -------------------

      procedure Append_Extend
        (View       : GPR2.Project.View.Object;
         Result     : out GPR_Dependency_Item_Vector;
         Child_Name : GPR2.Name_Type := "") is
      begin
         if View.Is_Extending then
            for Extended_View of View.Extended loop
               if Child_Name."=" ("") then
                  Append_Item (Result, Extended_View.Path_Name, Extended);
               elsif Child_Name."=" (Extended_View.Name) then
                  Append_Item (Result, View.Path_Name, Extended);
               end if;
               --  If Child Name is empty, we are searching for children so we add the Extended_View.
               --  If not, we are searching for the parent so we add the original View.
            end loop;
         end if;
      end Append_Extend;

      --------------------------
      -- Get_Sub_Dependencies --
      --------------------------

      procedure Get_Outgoing_Dependencies
        (Base_Name : GPR2.Name_Type;
         Tree      : GPR2.Project.Tree.Object;
         Result    : out GPR_Dependency_Item_Vector)
      is
         View              : GPR2.Project.View.Object;
         Ag_View           : GPR2.Project.View.Object;
         Namespace_Projects : GPR2.Project.View.Set.Object;

      begin
         Tree.Update_Sources;
         View := Tree.Root_Project.View_For (Base_Name);
         --  Find the view associated with the gpr file.

         if not View.Is_Defined then
            Namespace_Projects := Tree.Namespace_Root_Projects;
            for Name_Space of Namespace_Projects loop
               Ag_View := Name_Space.View_For (Base_Name);
               if Ag_View.Is_Defined then
                  View := Ag_View;
                  exit;
               end if;
            end loop;
         end if;
         --  If the view is not defined, try to search for it in all the namespace root
         --  project.

         if View.Is_Defined then
            Append_Import (View, Result);
            --  Check if there is depency imported using the "with" keyword.

            Append_Aggregate (View, Result);
            --  Check if the project is an aggregate project and add the aggregated projects.

            Append_Extend (View, Result);
         --  if View.Is_Extended then
         --     Append_Item (Result, View.Extending.Path_Name, Extending);
         --  end if;
         --  Check if the projet is extended, which can happens multiple times, or if
         --  he is extending, which can happens only once.

         end if;
      end Get_Outgoing_Dependencies;

      ----------------------------
      -- Get_Super_Dependencies --
      ----------------------------

      procedure Get_Incoming_Dependencies
        (Base_Name : GPR2.Name_Type;
         Tree      : GPR2.Project.Tree.Object;
         Result    : out GPR_Dependency_Item_Vector)
      is
         Views : GPR2.Project.View.Vector.Object;
      begin
         Tree.Update_Sources;
         Views := Tree.Ordered_Views;

         for View of Views loop
            if View.Is_Defined and not View.Is_Externally_Built then
               Append_Aggregate (View, Result, Base_Name);

               Append_Import (View, Result, Base_Name);

               Append_Extend (View, Result, Base_Name);
            end if;
         end loop;

      end Get_Incoming_Dependencies;

      File : constant GNATCOLL.VFS.Virtual_File := Handler.To_File (Self.URI);

      Base_Name : constant GPR2.Name_Type :=
        GPR2.Name_Type (File.Base_Name (".gpr"));

      Result : GPR_Dependency_Item_Vector;

      use all type VSS.JSON.JSON_Number_Kind;
   begin
      if Handler.Project_Tree_Is_Defined then
         if Self.Direction = Show_Outgoing then
            Get_Outgoing_Dependencies (Base_Name, Handler.Project_Tree, Result);
         else
            Get_Incoming_Dependencies (Base_Name, Handler.Project_Tree, Result);
         end if;
      end if;

      Response := (Is_Null => False, Value => <>);
      Append ((Kind => VSS.JSON.Streams.Start_Array));

      for Item of Result loop
         declare
            JSON_Value : constant VSS.JSON.JSON_Number :=
              (Kind          => JSON_Integer,
               String_Value  =>
                 VSS.Strings.Conversions.To_Virtual_String
                   (GPR_Dependency_Kind'Image (Item.Kind)),
               Integer_Value => GPR_Dependency_Kind'Pos (Item.Kind));
         begin
            Append ((Kind => VSS.JSON.Streams.Start_Object));
            Append ((VSS.JSON.Streams.Key_Name, "uri"));
            Append
              ((VSS.JSON.Streams.String_Value, Virtual_String (Item.URI)));
            Append ((VSS.JSON.Streams.Key_Name, "kind"));
            Append ((VSS.JSON.Streams.Number_Value, JSON_Value));
            Append ((Kind => VSS.JSON.Streams.End_Object));
         end;
      end loop;

      Append ((Kind => VSS.JSON.Streams.End_Array));
   end Execute;

end LSP.Ada_Handlers.GPR_Dependencies_Commands;

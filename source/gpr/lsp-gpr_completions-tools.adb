------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2025, AdaCore                       --
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.JSON;

with VSS.Strings.Conversions;

with LSP.Enumerations;
with LSP.Utils; use LSP.Utils;
with LSP.GPR_Completions.Tools.Database;

package body LSP.GPR_Completions.Tools is

   package Tool_Switches_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => LSP.Structures.CompletionItem_Vector,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => LSP.Structures."=");

   package Package_To_Tool_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => VSS.Strings.Virtual_String,
        Element_Type => VSS.Strings.Virtual_String,
        "<"          => VSS.Strings."<",
        "="          => VSS.Strings."=");

   Switches_Cache : Tool_Switches_Maps.Map;
   --  Cache of switches per tool

   Package_To_Tool : constant Package_To_Tool_Maps.Map :=
     ["compiler" => "gnat", "prove" => "gnatprove"];
   --  Map from GPR package name to tool name

   Database_Loaded : Boolean := False;
   --  Flag indicating whether the database has been loaded

   -------------------
   -- Load_Database --
   -------------------

   procedure Load_Database is
      use GNATCOLL.JSON;

      Root : JSON_Value;

      procedure Process_Tool
        (Tool_Name_Str : String;
         Tool_Object   : JSON_Value);
      --  Process each tool in the database

      ------------------
      -- Process_Tool --
      ------------------

      procedure Process_Tool
        (Tool_Name_Str : String;
         Tool_Object   : JSON_Value)
      is
         Switches : JSON_Value;
         Items    : LSP.Structures.CompletionItem_Vector;

         procedure Process_Switch (Name : String; Value : JSON_Value);
         --  Create a completion item for each switch

         --------------------
         -- Process_Switch --
         --------------------

         procedure Process_Switch (Name : String; Value : JSON_Value) is
            Item         : LSP.Structures.CompletionItem;
            Switch_Doc   : constant String := Value.Get;
            Equals_Pos   : Natural;
            Question_Pos : Natural;
            Bracket_Pos  : Natural;
         begin
            Item.label := VSS.Strings.Conversions.To_Virtual_String (Name);

            --  Check if switch expects a parameter (contains '=')
            Equals_Pos := Ada.Strings.Fixed.Index (Name, "=");

            if Equals_Pos > 0 then
               --  Switch expects a parameter, create a snippet
               declare
                  Prefix      : constant String :=
                    Name (Name'First .. Equals_Pos);
                  Placeholder : constant String :=
                    Name (Equals_Pos + 1 .. Name'Last);
               begin
                  Item.insertText :=
                    VSS.Strings.Conversions.To_Virtual_String
                      ("""" & Prefix & "${1:" & Placeholder & "}""");
                  Item.insertTextFormat :=
                    (Is_Set => True, Value => LSP.Enumerations.Snippet);
               end;
            else
               --  Check if switch has '?' or '[?...]' modifiers
               Question_Pos := Ada.Strings.Fixed.Index (Name, "?");
               Bracket_Pos := Ada.Strings.Fixed.Index (Name, "[");

               if Bracket_Pos > 0 and then Question_Pos > Bracket_Pos then
                  --  Switch with optional modifier [?...], e.g., -gnatn[?], -gnatn[??]
                  declare
                     Prefix     : constant String :=
                       Name (Name'First .. Bracket_Pos - 1);
                     Close_Pos  : constant Natural :=
                       Ada.Strings.Fixed.Index (Name, "]", Bracket_Pos);
                     Placeholder : constant String :=
                       (if Close_Pos > Bracket_Pos + 1 then
                          Name (Bracket_Pos + 1 .. Close_Pos - 1)
                        else "?");
                  begin
                     Item.insertText :=
                       VSS.Strings.Conversions.To_Virtual_String
                         ("""" & Prefix & "${1:" & Placeholder & "}""");
                     Item.insertTextFormat :=
                       (Is_Set => True, Value => LSP.Enumerations.Snippet);
                  end;
               elsif Question_Pos > 0 then
                  --  Switch with '?' modifiers, e.g., -gnato?, -gnato??
                  declare
                     Prefix      : constant String :=
                       Name (Name'First .. Question_Pos - 1);
                     Placeholder : constant String :=
                       Name (Question_Pos .. Name'Last);
                  begin
                     Item.insertText :=
                       VSS.Strings.Conversions.To_Virtual_String
                         ("""" & Prefix & "${1:" & Placeholder & "}""");
                     Item.insertTextFormat :=
                       (Is_Set => True, Value => LSP.Enumerations.Snippet);
                  end;
               else
                  --  Regular switch without parameter
                  Item.insertText :=
                    VSS.Strings.Conversions.To_Virtual_String ("""" & Name & """");
               end if;
            end if;

            Item.kind := (Is_Set => True, Value => LSP.Enumerations.Value);
            Item.detail :=
              VSS.Strings.Conversions.To_Virtual_String (Tool_Name_Str);
            Item.documentation :=
              (Is_Set => True,
               Value  =>
                 LSP.Structures.Virtual_String_Or_MarkupContent'
                   (Is_Virtual_String => True,
                    Virtual_String    =>
                      VSS.Strings.Conversions.To_Virtual_String (Switch_Doc)));

            Items.Append (Item);
         end Process_Switch;

      begin
         --  Get the switches object
         if not Tool_Object.Has_Field ("switches") then
            return;
         end if;

         Switches := Tool_Object.Get ("switches");

         --  Iterate over all switches and create completion items
         Switches.Map_JSON_Object (Process_Switch'Access);

         --  Store in cache
         Switches_Cache.Insert (Tool_Name_Str, Items);
      end Process_Tool;

   begin
      --  Load the JSON database from the embedded string
      Root := GNATCOLL.JSON.Read (LSP.GPR_Completions.Tools.Database.Db);

      --  Process all tools in the database
      Root.Map_JSON_Object (Process_Tool'Access);

      Database_Loaded := True;

   exception
      when others =>
         --  Silently ignore any errors when reading the database
         return;
   end Load_Database;

   -----------------------
   -- Get_Tool_Switches --
   -----------------------

   procedure Get_Tool_Switches
     (Tool_Name : VSS.Strings.Virtual_String;
      Result    : in out LSP.Structures.CompletionItem_Vector)
   is
      use VSS.Strings.Conversions;

      Tool_Name_Str : constant String := To_UTF_8_String (Tool_Name);
      Cursor        : Tool_Switches_Maps.Cursor;
   begin
      --  Load the database if not already loaded
      if not Database_Loaded then
         Load_Database;
      end if;

      --  Look up the tool in the cache
      Cursor := Switches_Cache.Find (Tool_Name_Str);

      if Tool_Switches_Maps.Has_Element (Cursor) then
         Result := Tool_Switches_Maps.Element (Cursor);
      end if;
   end Get_Tool_Switches;

   ----------------------------------------
   -- Fill_Tools_Completion_Response --
   ----------------------------------------

   procedure Fill_Tools_Completion_Response
     (File            : LSP.GPR_Files.File_Access;
      Current_Package : GPR2.Package_Id;
      Prefix          : VSS.Strings.Virtual_String;
      Response        : in out LSP.Structures.Completion_Result)
   is
      pragma Unreferenced (File);
      use VSS.Strings;
      use VSS.Strings.Conversions;

      Package_Name : constant VSS.Strings.Virtual_String :=
        To_Lower (To_Virtual_String (GPR2.Image (Current_Package)));
      Tool_Name_Cursor : Package_To_Tool_Maps.Cursor;
      Tool_Name        : VSS.Strings.Virtual_String;
      All_Switches     : LSP.Structures.CompletionItem_Vector;
   begin
      --  Load the database if not already loaded
      if not Database_Loaded then
         Load_Database;
      end if;

      --  Map package name to tool name
      Tool_Name_Cursor := Package_To_Tool.Find (Package_Name);

      if not Package_To_Tool_Maps.Has_Element (Tool_Name_Cursor) then
         --  No tool switches for the queried package
         return;
      end if;

      Tool_Name := Package_To_Tool_Maps.Element (Tool_Name_Cursor);

      --  Get all switches for the tool
      Get_Tool_Switches (Tool_Name, All_Switches);

      --  Filter switches by prefix and add to response
      for Item of All_Switches loop
         if Prefix.Is_Empty or else Item.label.Starts_With (Prefix) then
            Response.Variant_2.items.Append (Item);
         end if;
      end loop;
   end Fill_Tools_Completion_Response;

end LSP.GPR_Completions.Tools;
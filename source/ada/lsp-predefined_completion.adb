------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2020-2024, AdaCore                     --
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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with GNAT.Expect.TTY;
with GNAT.OS_Lib;

with GNATCOLL.JSON;     use GNATCOLL.JSON;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Utils;

with VSS.Strings.Conversions;
with VSS.Transformers.Caseless;

with LSP.Enumerations;
with LSP.Predefined_Completion.Ada2012;
with Libadalang.Common;

package body LSP.Predefined_Completion is

   Aspects          : CompletionItem_Vector;
   Attributes       : CompletionItem_Vector;
   Pragmas          : CompletionItem_Vector;

   type Ada_Version_Type is (Ada_83, Ada_95, Ada_2005, Ada_2012, Ada_2020);

   procedure Load_Predefined_Items
     (Trace   : Trace_Handle;
      Version : Ada_Version_Type);
   --  Load all the predefined completion items for the given Ada version.

   procedure Filter_Items
     (Prefix : VSS.Strings.Virtual_String;
      Items  : CompletionItem_Vector;
      Result : in out CompletionItem_Vector);
   --  Filter all the given items using Prefix (i.e: remove the items that
   --  don't match with Prefix).

   function Get_Output
     (Exe  : Virtual_File;
      Args : GNAT.OS_Lib.Argument_List) return String;
   --  Run the given command line and return the output.

   ---------------------------
   -- Load_Predefined_Items --
   ---------------------------

   procedure Load_Predefined_Items
     (Trace   : Trace_Handle;
      Version : Ada_Version_Type)
   is
      Root : JSON_Value := JSON_Null;

      procedure Load_Items_From_Key
        (Key   : String;
         Items : in out CompletionItem_Vector);

      procedure Load_Item
        (Value : GNATCOLL.JSON.JSON_Value;
         Items : in out CompletionItem_Vector);

      ---------------
      -- Load_Item --
      ---------------

      procedure Load_Item
        (Value : GNATCOLL.JSON.JSON_Value;
         Items : in out CompletionItem_Vector)
      is
         Item : CompletionItem;
      begin
         Item.label :=
           VSS.Strings.Conversions.To_Virtual_String
             (String'(Value.Get ("_name")));
         Item.detail :=
           (VSS.Strings.Conversions.To_Virtual_String
              (String'(Value.Get ("_origin"))));
         Item.documentation :=
           (Is_Set => True,
            Value  => Virtual_String_Or_MarkupContent'
              (Is_Virtual_String => True,
               Virtual_String    =>
                 VSS.Strings.Conversions.To_Virtual_String
                   (String'(Value.Get ("DOC")))));
         Item.kind := (Is_Set => True, Value => LSP.Enumerations.EnumMember);

         Items.Append (Item);
      end Load_Item;

      -------------------------
      -- Load_Items_From_Key --
      -------------------------

      procedure Load_Items_From_Key
        (Key   : String;
         Items : in out CompletionItem_Vector)
      is
         Array_Node : constant JSON_Array := Root.Get (Key);
      begin
         for Value of Array_Node loop
            Load_Item (Value, Items);
         end loop;
      end Load_Items_From_Key;

   begin
      --  TODO: load the proper database once we have one for each Ada version
      case Version is
         when Ada_83 =>
            Root := Read (LSP.Predefined_Completion.Ada2012.Db);
         when Ada_95 =>
            Root := Read (LSP.Predefined_Completion.Ada2012.Db);
         when Ada_2005 =>
            Root := Read (LSP.Predefined_Completion.Ada2012.Db);
         when Ada_2012 =>
            Root := Read (LSP.Predefined_Completion.Ada2012.Db);
         when Ada_2020 =>
            Root := Read (LSP.Predefined_Completion.Ada2012.Db);
      end case;

      Root := Root.Get ("PREDEFINED_ADA");

      if Root.Has_Field ("ASPECT") then
         Load_Items_From_Key (Key => "ASPECT", Items => Aspects);
      end if;

      if Root.Has_Field ("ATTRIBUTE") then
         Load_Items_From_Key (Key => "ATTRIBUTE", Items => Attributes);
      end if;

      if Root.Has_Field ("PRAGMA") then
         Load_Items_From_Key (Key => "PRAGMA", Items => Pragmas);
      end if;

   exception
      when E : others =>
         Trace.Trace (E);
         return;
   end Load_Predefined_Items;

   -----------------------------------
   -- Load_Predefined_Completion_Db --
   -----------------------------------

   procedure Load_Predefined_Completion_Db (Trace : Trace_Handle) is
      Version  : Ada_Version_Type := Ada_2012;
      GNATmake_Exe : constant Virtual_File := Locate_On_Path ("gnatmake");
   begin
      --  Check "gnatmake --help" output to determine which database we should
      --  use for predefined completion items, depending on the compiler's
      --  default Ada version.

      if GNATmake_Exe /= No_File then
         declare
            GNATmake_Help_Arg : aliased String := "--help";
            GNATmake_Help     : constant String := Get_Output
                 (Exe  => GNATmake_Exe,
                  Args => (1 => GNATmake_Help_Arg'Unrestricted_Access));
         begin
            if Index (GNATmake_Help, "gnat20") /= 0 then
               Version := Ada_2020;
            elsif Index (GNATmake_Help, "gnat12") /= 0 then
               Version := Ada_2012;
            elsif Index (GNATmake_Help, "gnat05") /= 0 then
               Version := Ada_2005;
            elsif Index (GNATmake_Help, "gnat95") /= 0 then
               Version := Ada_95;
            else
               Version := Ada_83;
            end if;
         end;
      end if;

      Trace.Trace
        ("Ada version used for predefined completion: " & Version'Img);

      Load_Predefined_Items (Trace => Trace, Version => Version);
   end Load_Predefined_Completion_Db;

   ------------------
   -- Filter_Items --
   ------------------

   procedure Filter_Items
     (Prefix : VSS.Strings.Virtual_String;
      Items  : CompletionItem_Vector;
      Result : in out CompletionItem_Vector) is
   begin
      for Item of Items loop
         if Item.label.Starts_With
           (Prefix, VSS.Transformers.Caseless.To_Identifier_Caseless)
         then
            Result.Append (Item);
         end if;
      end loop;
   end Filter_Items;

   -----------------
   -- Get_Aspects --
   -----------------

   procedure Get_Aspects
     (Prefix  : VSS.Strings.Virtual_String;
      Result  : in out CompletionItem_Vector) is
   begin
      Filter_Items (Prefix => Prefix, Items => Aspects, Result => Result);
   end Get_Aspects;

   --------------------
   -- Get_Attributes --
   --------------------

   procedure Get_Attributes
     (Prefix  : VSS.Strings.Virtual_String;
      Result  : in out CompletionItem_Vector) is
   begin
      Filter_Items (Prefix => Prefix, Items => Attributes, Result => Result);
   end Get_Attributes;

   -----------------
   -- Get_Pragmas --
   -----------------

   procedure Get_Pragmas
     (Prefix  : VSS.Strings.Virtual_String;
      Result  : in out CompletionItem_Vector) is
   begin
      Filter_Items (Prefix => Prefix, Items => Pragmas, Result => Result);
   end Get_Pragmas;

   ----------------
   -- Get_Output --
   ----------------

   function Get_Output
     (Exe  : Virtual_File;
      Args : GNAT.OS_Lib.Argument_List) return String
   is
   begin
      if Exe = No_File then
         return "";
      end if;

      declare
         Fd : aliased GNAT.Expect.TTY.TTY_Process_Descriptor;
      begin
         GNAT.Expect.Non_Blocking_Spawn
           (Descriptor  => Fd,
            Command     => Exe.Display_Full_Name,
            Buffer_Size => 0,
            Args        => Args,
            Err_To_Out  => True);
         declare
            S : constant String :=
              GNATCOLL.Utils.Get_Command_Output (Fd'Access);
         begin
            GNAT.Expect.TTY.Close (Fd);

            return S;
         end;
      exception
         when GNAT.Expect.Process_Died =>
            GNAT.Expect.TTY.Close (Fd);
            return "";
      end;
   end Get_Output;

   ----------------------
   -- Get_Tooltip_Text --
   ----------------------

   procedure Get_Tooltip_Text
     (Node               : Libadalang.Analysis.Identifier;
      Declaration_Text   : out VSS.Strings.Virtual_String;
      Documentation_Text : out VSS.Strings.Virtual_String)
   is
      use Libadalang.Common;

      Filtered_Items : CompletionItem_Vector;
      Prefix         : VSS.Strings.Virtual_String;
      Parent         : constant Libadalang.Analysis.Ada_Node :=
        (if Node.Is_Null then Libadalang.Analysis.No_Ada_Node
         else Node.Parent);
      Item           : CompletionItem;
   begin
      --  Return immediately if the node is null of if its parent is null
      if Node.Is_Null or else Parent.Is_Null then
         return;
      end if;

      --  Get the attribute/aspect/pragma completion item corresponding to the
      --  given node

      Prefix := VSS.Strings.To_Virtual_String (Node.Text);

      case Parent.Kind is
         when Ada_Attribute_Ref_Range =>
            Get_Attributes (Prefix => Prefix, Result => Filtered_Items);

         when Ada_Aspect_Assoc_Range =>
            Get_Aspects (Prefix => Prefix, Result => Filtered_Items);

         when Ada_Pragma_Node_Range =>
            Get_Pragmas (Prefix => Prefix, Result => Filtered_Items);

         when others =>
            return;
      end case;

      Item := Filtered_Items.First_Element;

      Declaration_Text := Item.detail;
      Documentation_Text := Item.documentation.Value.Virtual_String;
   end Get_Tooltip_Text;

end LSP.Predefined_Completion;

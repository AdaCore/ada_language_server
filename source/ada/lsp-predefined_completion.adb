with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNATCOLL.JSON;     use GNATCOLL.JSON;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with VSS.Strings.Conversions;

with LSP.Common;
with LSP.Types;         use LSP.Types;
with LSP.Predefined_Completion.Ada2012;

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
     (Prefix : Ada.Strings.UTF_Encoding.UTF_8_String;
      Items  : CompletionItem_Vector;
      Result : in out CompletionItem_Vector);
   --  Filter all the given items using Prefix (i.e: remove the items that
   --  don't match with Prefix).

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
         Item.insertTextFormat := (True, PlainText);
         Item.label :=
           VSS.Strings.Conversions.To_Virtual_String
             (String'(Value.Get ("_name")));
         Item.detail := (True, To_LSP_String (String'(Value.Get ("_origin"))));
         Item.insertText := (True, LSP.Types.To_LSP_String (Item.label));
         Item.documentation :=
           (Is_Set => True,
            Value  => String_Or_MarkupContent'
              (Is_String => True,
               String    =>
                 VSS.Strings.Conversions.To_Virtual_String
                   (String'(Value.Get ("DOC")))));
         Item.kind := (True, Keyword);

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
            GNATmake_Help     : constant String := LSP.Common.Get_Output
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
     (Prefix : Ada.Strings.UTF_Encoding.UTF_8_String;
      Items  : CompletionItem_Vector;
      Result : in out CompletionItem_Vector) is
   begin
      for Item of Items loop
         if Starts_With
           (Text           =>
              LSP.Types.LSP_String'(LSP.Types.To_LSP_String (Item.label)),
            Prefix         => Prefix,
            Case_Sensitive => False)
         then
            Result.Append (Item);
         end if;
      end loop;
   end Filter_Items;

   -----------------
   -- Get_Aspects --
   -----------------

   procedure Get_Aspects
     (Prefix  : Ada.Strings.UTF_Encoding.UTF_8_String;
      Result  : in out CompletionItem_Vector) is
   begin
      Filter_Items (Prefix => Prefix, Items => Aspects, Result => Result);
   end Get_Aspects;

   --------------------
   -- Get_Attributes --
   --------------------

   procedure Get_Attributes
     (Prefix  : Ada.Strings.UTF_Encoding.UTF_8_String;
      Result  : in out CompletionItem_Vector) is
   begin
      Filter_Items (Prefix => Prefix, Items => Attributes, Result => Result);
   end Get_Attributes;

   -----------------
   -- Get_Pragmas --
   -----------------

   procedure Get_Pragmas
     (Prefix  : Ada.Strings.UTF_Encoding.UTF_8_String;
      Result  : in out CompletionItem_Vector) is
   begin
      Filter_Items (Prefix => Prefix, Items => Pragmas, Result => Result);
   end Get_Pragmas;

end LSP.Predefined_Completion;

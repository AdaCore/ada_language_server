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

with Interfaces;

with Libadalang.Analysis;
with Libadalang.Common;

with VSS.JSON.Streams;
with VSS.String_Vectors;

package body LSP.Ada_Handlers.Show_Dependencies_Commands is

   procedure Get_Imported_Units
     (Handler       : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Unit          : Libadalang.Analysis.Analysis_Unit;
      Show_Implicit : Boolean;
      Result        : out VSS.String_Vectors.Virtual_String_Vector);

   procedure Get_Importing_Units
     (Handler       : not null access LSP.Ada_Handlers.Message_Handler'Class;
      All_Units     : Libadalang.Analysis.Analysis_Unit_Array;
      Unit          : Libadalang.Analysis.Analysis_Unit;
      Show_Implicit : Boolean;
      Result        : out VSS.String_Vectors.Virtual_String_Vector);

   ------------
   -- Create --
   ------------

   overriding function Create
     (Any : not null access LSP.Structures.LSPAny_Vector)
      return Command
   is
      use type VSS.Strings.Virtual_String;
      use type Interfaces.Integer_64;
      use all type VSS.JSON.Streams.JSON_Stream_Element_Kind;
      use all type VSS.JSON.JSON_Number_Kind;

      Index : Natural := Any.First_Index;
      Key : VSS.Strings.Virtual_String;
   begin
      return Result : Command do
         if Index < Any.Last_Index
           and then Any (Index).Kind = Start_Array
         then
            Index := Index + 1;

            if Index < Any.Last_Index
              and then Any (Index).Kind = Start_Object
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
                  elsif Key = "kind"
                    and then Index < Any.Last_Index
                    and then Any (Index).Kind = Number_Value
                    and then Any (Index).Number_Value.Kind = JSON_Integer
                    and then Any (Index).Number_Value.Integer_Value in 1 .. 2
                  then
                     Result.Kind := Show_Dependencies_Kind'Val
                       (Any (Index).Number_Value.Integer_Value - 1);
                  elsif Key = "showImplicit"
                    and then Index < Any.Last_Index
                    and then Any (Index).Kind = Boolean_Value
                  then
                     Result.Show_Implicit := Any (Index).Boolean_Value;
                  end if;

                  Index := Index + 1;
               end loop;
            end if;
         end if;
      end return;
   end Create;

   ------------------------
   -- Get_Imported_Units --
   ------------------------

   procedure Get_Imported_Units
     (Handler       : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Unit          : Libadalang.Analysis.Analysis_Unit;
      Show_Implicit : Boolean;
      Result        : out VSS.String_Vectors.Virtual_String_Vector)
   is
      Root : constant Libadalang.Analysis.Ada_Node := Unit.Root;

      procedure Append (Units : Libadalang.Analysis.Compilation_Unit_Array);

      procedure Append_Withed
        (Unit : Libadalang.Analysis.Compilation_Unit'Class);

      ------------
      -- Append --
      ------------

      procedure Append (Units : Libadalang.Analysis.Compilation_Unit_Array) is
      begin
         for Unit of Units loop
            Result.Append (Handler.To_URI (Unit.Unit.Get_Filename));
         end loop;
      end Append;

      -------------------
      -- Append_Withed --
      -------------------

      procedure Append_Withed
        (Unit : Libadalang.Analysis.Compilation_Unit'Class) is
      begin
         if Show_Implicit then
            Append (Unit.P_Unit_Dependencies);
         else
            Append (Unit.P_Withed_Units);
         end if;
      end Append_Withed;

   begin
      case Root.Kind is
         when Libadalang.Common.Ada_Compilation_Unit_Range =>
            Append_Withed (Root.As_Compilation_Unit);

         when Libadalang.Common.Ada_Compilation_Unit_List_Range =>
            declare
               List : constant Libadalang.Analysis.Compilation_Unit_List :=
                 Root.As_Compilation_Unit_List;
            begin
               for Unit of List loop
                  Append_Withed (Unit);
               end loop;
            end;

         when others =>
            null;
      end case;
   end Get_Imported_Units;

   -------------------------
   -- Get_Importing_Units --
   -------------------------

   procedure Get_Importing_Units
     (Handler       : not null access LSP.Ada_Handlers.Message_Handler'Class;
      All_Units     : Libadalang.Analysis.Analysis_Unit_Array;
      Unit          : Libadalang.Analysis.Analysis_Unit;
      Show_Implicit : Boolean;
      Result        : out VSS.String_Vectors.Virtual_String_Vector)
   is
      Root : constant Libadalang.Analysis.Ada_Node := Unit.Root;

      procedure Append_Importing
        (Unit : Libadalang.Analysis.Compilation_Unit'Class);

      ----------------------
      -- Append_Importing --
      ----------------------

      procedure Append_Importing
        (Unit : Libadalang.Analysis.Compilation_Unit'Class)
      is
         List : constant Libadalang.Analysis.Analysis_Unit_Array :=
           Unit.P_Filter_Is_Imported_By
             (Units      => All_Units,
              Transitive => Show_Implicit);
      begin
         for Item of List loop
            Result.Append (Handler.To_URI (Item.Get_Filename));
         end loop;
      end Append_Importing;

   begin
      case Root.Kind is
         when Libadalang.Common.Ada_Compilation_Unit_Range =>
            Append_Importing (Root.As_Compilation_Unit);

         when Libadalang.Common.Ada_Compilation_Unit_List_Range =>
            declare
               List : constant Libadalang.Analysis.Compilation_Unit_List :=
                 Root.As_Compilation_Unit_List;
            begin
               for Unit of List loop
                  Append_Importing (Unit);
               end loop;
            end;

         when others =>
            null;
      end case;
   end Get_Importing_Units;

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

      File : constant GNATCOLL.VFS.Virtual_File := Handler.To_File (Self.URI);

      Root : VSS.Strings.Virtual_String := Handler.Client.Root;

      Result  : VSS.String_Vectors.Virtual_String_Vector;
   begin
      if not Root.Starts_With ("file:") then
         Root := VSS.Strings.Virtual_String
           (Handler.To_URI (VSS.Strings.Conversions.To_UTF_8_String (Root)));
      end if;

      case Self.Kind is
         when Show_Imported =>
            declare
               Context : constant LSP.Ada_Context_Sets.Context_Access :=
                 Handler.Contexts.Get_Best_Context (Self.URI);
            begin
               Get_Imported_Units
                 (Handler,
                  Context.Get_AU (File),
                  Self.Show_Implicit,
                  Result);
            end;

         when Show_Importing =>
            declare
               Contexts : constant LSP.Ada_Context_Sets.Context_Lists.List :=
                 Handler.Contexts_For_URI (Self.URI);
            begin
               for Context of Contexts loop
                  Get_Importing_Units
                    (Handler,
                     Context.Analysis_Units,
                     Context.Get_AU (File),
                     Self.Show_Implicit,
                     Result);
               end loop;
            end;
      end case;

      Response := (Is_Null => False, Value => <>);
      Append ((Kind => VSS.JSON.Streams.Start_Array));

      for URI of Result loop
         Append ((Kind => VSS.JSON.Streams.Start_Object));
         Append ((VSS.JSON.Streams.Key_Name, "uri"));
         Append ((VSS.JSON.Streams.String_Value, URI));
         Append ((VSS.JSON.Streams.Key_Name, "projectUri"));
         Append ((VSS.JSON.Streams.String_Value, Root));
         Append ((Kind => VSS.JSON.Streams.End_Object));
      end loop;

      Append ((Kind => VSS.JSON.Streams.End_Array));
   end Execute;

end LSP.Ada_Handlers.Show_Dependencies_Commands;

------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2024, AdaCore                       --
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

with Ada.Unchecked_Deallocation;

with Libadalang.Analysis;
with Libadalang.Common;

with VSS.Strings;

with LSP.Ada_Context_Sets;
with LSP.Ada_File_Sets;
with LSP.Ada_Request_Jobs;
with LSP.Client_Message_Receivers;
with LSP.Locations;
with LSP.Server_Requests.Subtypes;
with LSP.Structures;
with LSP.Utils;

package body LSP.Ada_Type_Hierarchy_Subtypes is

   subtype Reversible_Iterator is LSP.Ada_File_Sets.File_Sets
     .Set_Iterator_Interfaces.Reversible_Iterator'Class;

   type Iterator_Access is access Reversible_Iterator;

   procedure Free is new Ada.Unchecked_Deallocation
     (Reversible_Iterator, Iterator_Access);

   type Ada_References_Job
     (Parent : not null access constant Ada_Type_Hierarchy_Subtype_Handler) is
   limited new LSP.Ada_Request_Jobs.Ada_Request_Job
     (Priority => LSP.Server_Jobs.Low) with
   record
      Response : LSP.Structures.TypeHierarchyItem_Vector_Or_Null;
      Filter   : LSP.Locations.File_Span_Sets.Set;
      Context  : LSP.Ada_Context_Sets.Context_Access;
      Iterator : Iterator_Access;
      Cursor   : LSP.Ada_File_Sets.File_Sets.Cursor;
      Decl     : Libadalang.Analysis.Basic_Decl;
   end record;

   type Ada_References_Job_Access is access all Ada_References_Job;

   overriding procedure Execute_Ada_Request
     (Self   : in out Ada_References_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   ----------------
   -- Create_Job --
   ----------------

   overriding function Create_Job
     (Self    : Ada_Type_Hierarchy_Subtype_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
        return LSP.Server_Jobs.Server_Job_Access
   is
      Value : LSP.Server_Requests.Subtypes.Request
        renames LSP.Server_Requests.Subtypes.Request
          (Message.all);

      Context : constant LSP.Ada_Context_Sets.Context_Access :=
        Self.Context.Get_Best_Context (Value.Params.item.uri);

      Node : constant Libadalang.Analysis.Ada_Node :=
        Self.Context.Get_Node_At
          (Context.all,
           (LSP.Structures.TextDocumentPositionParams'
             ((uri => Value.Params.item.uri),
              Value.Params.item.selectionRange.start)));
      --  We expect here an identifier in the type declaration

      Decl : constant Libadalang.Analysis.Basic_Decl :=
        (if not Node.Is_Null
           and then Node.Kind in Libadalang.Common.Ada_Name
           and then Node.As_Name.P_Is_Defining
         then Node.As_Name.P_Enclosing_Defining_Name.P_Basic_Decl
         else Libadalang.Analysis.No_Basic_Decl);

      Result : constant Ada_References_Job_Access :=
        new Ada_References_Job'
          (Parent   => Self'Unchecked_Access,
           Request  => LSP.Ada_Request_Jobs.Request_Access (Message),
           Context  => Context,
           Iterator => new Reversible_Iterator'(Context.List_Files),
           Decl     => Decl,
           others   => <>);
   begin
      Result.Cursor := Result.Iterator.First;

      return LSP.Server_Jobs.Server_Job_Access (Result);
   end Create_Job;

   -------------------------
   -- Execute_Ada_Request --
   -------------------------

   overriding procedure Execute_Ada_Request
     (Self   : in out Ada_References_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is

      Message : LSP.Server_Requests.Subtypes.Request
        renames LSP.Server_Requests.Subtypes.Request (Self.Message.all);

      Ignore : Boolean;
      Unit   : Libadalang.Analysis.Analysis_Unit;
      Loc    : LSP.Structures.Location;
      Item   : LSP.Structures.TypeHierarchyItem;
      Name   : Libadalang.Analysis.Defining_Name;
   begin
      if LSP.Ada_File_Sets.File_Sets.Has_Element (Self.Cursor) then
         Unit := Self.Context.Get_AU
           (LSP.Ada_File_Sets.File_Sets.Element (Self.Cursor));

         for Part of Self.Decl.P_All_Parts
           when Part.Kind in Libadalang.Common.Ada_Base_Type_Decl
         loop
            for Tipe of Part.As_Base_Type_Decl.P_Find_Derived_Types
              (Root   => Unit.Root,
               Origin => Self.Decl)
            loop
               Name := Tipe.P_Defining_Name.P_Canonical_Part;

               Loc := Self.Parent.Context.To_LSP_Location (Name.P_Basic_Decl);

               if not Self.Filter.Contains (Loc) then
                  Item :=
                    (name           => VSS.Strings.To_Virtual_String
                       (Name.Text),
                     kind           => LSP.Utils.Get_Decl_Kind
                       (Name.P_Basic_Decl),
                     tags           => <>,
                     detail         => LSP.Utils.Node_Location_Image
                       (Name),
                     uri            => Loc.uri,
                     a_range        => Loc.a_range,
                     selectionRange => Self.Parent.Context.To_LSP_Location
                       (Name).a_range,
                     data           => <>);

                  Self.Filter.Insert (Loc);
                  Self.Response.Append (Item);
               end if;
            end loop;
         end loop;

         Self.Cursor := Self.Iterator.Next (Self.Cursor);
         Status := LSP.Server_Jobs.Continue;
      else
         Free (Self.Iterator);
         Client.On_Subtypes_Response (Message.Id, Self.Response);
         Status := LSP.Server_Jobs.Done;
      end if;
   end Execute_Ada_Request;

end LSP.Ada_Type_Hierarchy_Subtypes;

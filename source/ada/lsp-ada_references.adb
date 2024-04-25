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

with GNATCOLL.VFS;

with Libadalang.Analysis;
with Libadalang.Common;

with Laltools.Common;

with LSP.Ada_Context_Sets;
with LSP.Ada_File_Sets;
with LSP.Ada_Handlers.Locations;
with LSP.Ada_Id_Iterators;
with LSP.Client_Message_Receivers;
with LSP.Enumerations;
with LSP.Locations;
with LSP.Server_Request_Jobs;
with LSP.Server_Requests.References;
with LSP.Structures;

package body LSP.Ada_References is

   subtype AlsReferenceKind_Array is LSP.Structures.AlsReferenceKind_Set;

   subtype Reversible_Iterator is LSP.Ada_File_Sets.File_Sets
     .Set_Iterator_Interfaces.Reversible_Iterator'Class;

   type Iterator_Access is access Reversible_Iterator;

   procedure Free is new Ada.Unchecked_Deallocation
     (Reversible_Iterator, Iterator_Access);

   type Ada_References_Job
     (Parent : not null access constant Ada_References_Handler) is limited
   new LSP.Server_Request_Jobs.Server_Request_Job
     (Priority => LSP.Server_Jobs.Low) with
   record
      Is_Enum    : Boolean := False;
      Response   : LSP.Structures.Location_Vector_Or_Null;
      Filter     : LSP.Locations.File_Span_Sets.Set;
      Contexts   : LSP.Ada_Context_Sets.Context_Lists.List;
      Context    : LSP.Ada_Context_Sets.Context_Access;
      Iterator   : Iterator_Access;
      Cursor     : LSP.Ada_File_Sets.File_Sets.Cursor;
      Definition : Libadalang.Analysis.Defining_Name;
   end record;

   type Ada_References_Job_Access is access all Ada_References_Job;

   overriding procedure Execute_Request
     (Self   : in out Ada_References_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   function Get_Reference_Kind
     (Self               : Ada_References_Job'Class;
      Node               : Libadalang.Analysis.Ada_Node'Class;
      Is_Overriding_Decl : Boolean := False)
         return AlsReferenceKind_Array;
   --  Fetch reference kind for given node.

   ----------------
   -- Create_Job --
   ----------------

   overriding function Create_Job
     (Self    : Ada_References_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
      return LSP.Server_Jobs.Server_Job_Access
   is
      Value : LSP.Server_Requests.References.Request
        renames LSP.Server_Requests.References.Request
          (Message.all);

      File : constant GNATCOLL.VFS.Virtual_File :=
        Self.Context.To_File (Value.Params.textDocument.uri);

      Result : constant Ada_References_Job_Access :=
        new Ada_References_Job'
          (Parent  => Self'Unchecked_Access,
           Request => LSP.Server_Request_Jobs.Request_Access (Message),
           others  => <>);
   begin
      Result.Contexts := Self.Context.Contexts_For_File (File);

      return LSP.Server_Jobs.Server_Job_Access (Result);
   end Create_Job;

   ---------------------
   -- Execute_Request --
   ---------------------

   overriding procedure Execute_Request
     (Self   : in out Ada_References_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is
      procedure Callback
        (Node   : Libadalang.Analysis.Base_Id;
         Kind   : Libadalang.Common.Ref_Result_Kind;
         Cancel : in out Boolean);

      function Is_Enum
        (Definition : Libadalang.Analysis.Defining_Name) return Boolean;

      Message : LSP.Server_Requests.References.Request
        renames LSP.Server_Requests.References.Request (Self.Message.all);

      --------------
      -- Callback --
      --------------

      procedure Callback
        (Node   : Libadalang.Analysis.Base_Id;
         Kind   : Libadalang.Common.Ref_Result_Kind;
         Cancel : in out Boolean)
      is
         pragma Unreferenced (Kind);
      begin
         if not Laltools.Common.Is_End_Label (Node.As_Ada_Node) then

            Self.Parent.Context.Append_Location
              (Self.Response,
               Self.Filter,
               Node,
               Self.Get_Reference_Kind (Node));
         end if;

         Cancel := Message.Canceled;
      end Callback;

      -------------
      -- Is_Enum --
      -------------

      function Is_Enum
        (Definition : Libadalang.Analysis.Defining_Name) return Boolean
      is
         use all type Libadalang.Common.Ada_Node_Kind_Type;

         Decl : constant Libadalang.Analysis.Basic_Decl :=
           Definition.P_Basic_Decl;
      begin
         return not Decl.Is_Null and then Decl.Kind = Ada_Enum_Literal_Decl;
      end Is_Enum;

      Ignore : Boolean;
      Units : Libadalang.Analysis.Analysis_Unit_Array (1 .. 1);
   begin
      --  Next file to process
      if LSP.Ada_File_Sets.File_Sets.Has_Element (Self.Cursor) then
         Self.Cursor := Self.Iterator.Next (Self.Cursor);
      end if;

      while not LSP.Ada_File_Sets.File_Sets.Has_Element (Self.Cursor)
        and then not Self.Contexts.Is_Empty
      loop
         Self.Context := Self.Contexts.First_Element;
         Self.Contexts.Delete_First;
         Free (Self.Iterator);
         Self.Iterator := new Reversible_Iterator'
           (Self.Context.List_Files);

         Self.Cursor := Self.Iterator.First;

         Self.Definition := Self.Parent.Context.Imprecise_Resolve_Name
           (Self.Context.all, Message.Params);

         if Self.Definition.Is_Null then
            Self.Cursor := LSP.Ada_File_Sets.File_Sets.No_Element;
         else
            Self.Is_Enum := Is_Enum (Self.Definition);

            --  Find all the overriding declarations, if any
            for Subp of Self.Context.Find_All_Overrides
              (Self.Definition.P_Basic_Decl, Ignore)
            loop
               Self.Parent.Context.Append_Location
                 (Self.Response,
                  Self.Filter,
                  Subp.P_Defining_Name,
                  Self.Get_Reference_Kind
                    (Self.Definition,
                     Is_Overriding_Decl => True));
            end loop;

            if Message.Params.context.includeDeclaration then
               Self.Parent.Context.Append_Location
                 (Self.Response,
                  Self.Filter,
                  Self.Definition,
                  Self.Get_Reference_Kind (Self.Definition));
            end if;
         end if;
      end loop;

      if LSP.Ada_File_Sets.File_Sets.Has_Element (Self.Cursor) then
         Units (1) := Self.Context.Get_AU
           (LSP.Ada_File_Sets.File_Sets.Element (Self.Cursor));

         LSP.Ada_Id_Iterators.Find_All_References
           (Self.Definition, Units, Callback'Access);

         Status := LSP.Server_Jobs.Continue;
      else
         Free (Self.Iterator);
         LSP.Ada_Handlers.Locations.Sort (Self.Response);
         Client.On_References_Response (Message.Id, Self.Response);
         Status := LSP.Server_Jobs.Done;
      end if;
   end Execute_Request;

   ------------------------
   -- Get_Reference_Kind --
   ------------------------

   function Get_Reference_Kind
     (Self               : Ada_References_Job'Class;
      Node               : Libadalang.Analysis.Ada_Node'Class;
      Is_Overriding_Decl : Boolean := False)
         return AlsReferenceKind_Array
   is
      use type AlsReferenceKind_Array;
      use all type LSP.Enumerations.AlsReferenceKind;

      Id     : constant Libadalang.Analysis.Name :=
        Laltools.Common.Get_Node_As_Name (Node.As_Ada_Node);

      Result : AlsReferenceKind_Array := [others => False];
   begin
      begin
         Result (write) := Id.P_Is_Write_Reference;
      exception
         when E : Libadalang.Common.Property_Error =>
            Self.Parent.Context.Trace_Exception (E);
      end;

      begin
         Result (an_access) :=
           Laltools.Common.Is_Access_Ref (Id.As_Ada_Node);
      exception
         when E : Libadalang.Common.Property_Error =>
            Self.Parent.Context.Trace_Exception (E);
      end;

      begin
         Result (call) := Id.P_Is_Static_Call;
      exception
         when E : Libadalang.Common.Property_Error =>
            Self.Parent.Context.Trace_Exception (E);
      end;

      begin
         Result (dispatching_call) :=
           Id.P_Is_Dispatching_Call;
      exception
         when E : Libadalang.Common.Property_Error =>
            Self.Parent.Context.Trace_Exception (E);
      end;

      begin
         Result (child) :=
           Laltools.Common.Is_Type_Derivation (Id.As_Ada_Node);
      exception
         when E : Libadalang.Common.Property_Error =>
            Self.Parent.Context.Trace_Exception (E);
      end;

      Result (an_overriding) := Is_Overriding_Decl;

      --  If the result has not any set flags at this point, flag it as a
      --  simple reference.
      if Result = [Result'Range => False] then
         Result (reference) := True;
      end if;

      --  Set additional "reference" kind for enumeration literal
      if Self.Is_Enum then
         Result (reference) := True;
      end if;

      return Result;
   end Get_Reference_Kind;

end LSP.Ada_References;

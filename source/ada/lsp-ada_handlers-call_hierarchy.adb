------------------------------------------------------------------------------
--                         Language Server Protocol                         --
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

with GNATCOLL.Traces;

with Libadalang.Common;

with Laltools.Common;
with Laltools.Call_Hierarchy;

with LSP.GNATCOLL_Tracers.Handle;
with LSP.Ada_Handlers.Locations;
with LSP.Utils;

package body LSP.Ada_Handlers.Call_Hierarchy is

   function Containing_Entity
     (Ref       : Libadalang.Analysis.Ada_Node;
      Canonical : Boolean := True) return Libadalang.Analysis.Defining_Name;

   -----------------------
   -- Containing_Entity --
   -----------------------

   function Containing_Entity
     (Ref       : Libadalang.Analysis.Ada_Node;
      Canonical : Boolean := True) return Libadalang.Analysis.Defining_Name
   is
      use all type Libadalang.Common.Ada_Node_Kind_Type;

      Parents : constant Libadalang.Analysis.Ada_Node_Array := Ref.Parents;
   begin
      for Parent of Parents loop
         if Parent.Kind in Ada_Subp_Decl
                         | Ada_Subp_Body
                         | Ada_Task_Def
                         | Ada_Task_Body
                         | Ada_Package_Body
                         | Ada_Package_Decl
         then
            if Canonical then
               return Parent.As_Basic_Decl.P_Canonical_Part.P_Defining_Name;
            else
               return Parent.As_Basic_Decl.P_Defining_Name;
            end if;
         end if;
      end loop;

      return Libadalang.Analysis.No_Defining_Name;
   end Containing_Entity;

   -------------------------
   -- Find_Incoming_Calls --
   -------------------------

   procedure Find_Incoming_Calls
     (Self        : in out Message_Handler;
      Response    : in out LSP.Structures.CallHierarchyIncomingCall_Vector;
      Filter      : in out LSP.Locations.File_Span_Sets.Set;
      Context     : LSP.Ada_Contexts.Context;
      Definition  : Libadalang.Analysis.Defining_Name)
   is

      procedure Add_Incoming_Call
        (Node  : Libadalang.Analysis.Defining_Name;
         Refs  : Laltools.Common.References_Sets.Set);
      --  LSP.Structures.AlsReferenceKind_Vector)
      --  Add an incoming call in results. Use Filter to prevent having
      --  duplicates

      -----------------------
      -- Add_Incoming_Call --
      -----------------------

      procedure Add_Incoming_Call
        (Node  : Libadalang.Analysis.Defining_Name;
         Refs  : Laltools.Common.References_Sets.Set)
      is
         Call : LSP.Structures.CallHierarchyIncomingCall;
         Span : constant LSP.Structures.Location :=
           Locations.To_LSP_Location (Self, Node);
      begin
         if not Filter.Contains (Span) then
            declare
               Decl     : constant Libadalang.Analysis.Basic_Decl :=
                 Node.P_Basic_Decl;
               Location : constant LSP.Structures.Location :=
                 Locations.To_LSP_Location (Self, Node);
            begin
               Call.from := LSP.Structures.CallHierarchyItem'
                 (name           => VSS.Strings.To_Virtual_String (Node.Text),
                  kind           => Utils.Get_Decl_Kind (Decl),
                  tags           => <>,
                  detail         => <>,
                  uri            => Location.uri,
                  a_range        => Location.a_range,
                  selectionRange => Location.a_range,
                  data           => <>);

               for Ref of Refs loop
                  declare
                     Ref_Location : constant LSP.Structures.Location :=
                       Locations.To_LSP_Location (Self, Ref);
                  begin
                     Call.fromRanges.Append (Ref_Location.a_range);

                     if Ref.P_Is_Dispatching_Call then
                        Call.dispatching_calls.Append (True);
                     else
                        Call.dispatching_calls.Append (False);
                     end if;
                  end;
               end loop;

               Response.Append (Call);
               Filter.Insert (Span);
            end;
         end if;
      end Add_Incoming_Call;

      procedure Callback
        (Ref    : Libadalang.Analysis.Base_Id;
         Kind   : Libadalang.Common.Ref_Result_Kind;
         Cancel : in out Boolean);

      Result     : Laltools.Common.References_By_Subprogram.Map;

      --------------
      -- Callback --
      --------------

      procedure Callback
        (Ref     : Libadalang.Analysis.Base_Id;
         Kind    : Libadalang.Common.Ref_Result_Kind;
         Cancel  : in out Boolean)
      is
         pragma Unreferenced (Kind);
         Containing : Libadalang.Analysis.Defining_Name;
      begin
         --  We have a reference, and this a call: find the containing
         --  subprogram or task
         Containing := Containing_Entity
           (Ref.As_Ada_Node, Canonical => False);

         if not Containing.Is_Null then
            if Result.Contains (Containing) then
               Result (Containing).Include (Ref);
            else
               declare
                  L : Laltools.Common.References_Sets.Set;
               begin
                  L.Include (Ref);
                  Result.Insert (Containing, L);
               end;
            end if;
         end if;

         if Self.Is_Canceled.all then
            Cancel := True;
         end if;
      end Callback;

      Cursor  : Laltools.Common.References_By_Subprogram.Cursor;

   begin
      --  Go through all references to Definition, organising them by
      --  containing subprogram.

      --  Obtain all the references
      Context.Find_All_Calls (Definition, Callback'Access);

      Cursor := Result.First;
      --  Iterate through all the results, converting them to protocol
      --  objects.
      while Laltools.Common.References_By_Subprogram.Has_Element (Cursor) loop
         declare
            Node     : constant Libadalang.Analysis.Defining_Name :=
              Laltools.Common.References_By_Subprogram.Key (Cursor);
            Refs     : constant Laltools.Common.References_Sets.Set :=
              Laltools.Common.References_By_Subprogram.Element (Cursor);
         begin
            Add_Incoming_Call (Node, Refs);

            Laltools.Common.References_By_Subprogram.Next (Cursor);
         end;
      end loop;
   end Find_Incoming_Calls;

   -------------------------
   -- Find_Outgoing_Calls --
   -------------------------

   procedure Find_Outgoing_Calls
     (Self        : in out Message_Handler;
      Response    : in out LSP.Structures.CallHierarchyOutgoingCall_Vector;
      Filter      : in out LSP.Locations.File_Span_Sets.Set;
      Definition  : Libadalang.Analysis.Defining_Name)
   is
      use Laltools.Common.References_By_Subprogram;
      use Laltools.Common.References_Sets;

      Trace     : constant GNATCOLL.Traces.Trace_Handle :=
        LSP.GNATCOLL_Tracers.Handle (Self.Tracer.all);

      Result : Laltools.Common.References_By_Subprogram.Map;

      procedure Callback (Subp_Call : Libadalang.Analysis.Ada_Node'Class);
      procedure Add_Outgoing_Call
        (Node  : Libadalang.Analysis.Defining_Name;
         Refs  : Laltools.Common.References_Sets.Set);
      --  LSP.Structures.AlsReferenceKind_Vector)
      --  Add an Outgoing call in results. Use Filter to prevent having
      --  duplicates

      -----------------------
      -- Add_Outgoing_Call --
      -----------------------

      procedure Add_Outgoing_Call
        (Node  : Libadalang.Analysis.Defining_Name;
         Refs  : Laltools.Common.References_Sets.Set)
      is
         Call : LSP.Structures.CallHierarchyOutgoingCall;
         Span : constant LSP.Structures.Location :=
           Locations.To_LSP_Location (Self, Node);
      begin
         if not Filter.Contains (Span) then
            declare
               Decl     : constant Libadalang.Analysis.Basic_Decl :=
                 Node.P_Basic_Decl;
               Location : constant LSP.Structures.Location :=
                 Locations.To_LSP_Location (Self, Node);
            begin
               Call.to := LSP.Structures.CallHierarchyItem'
                 (name           => VSS.Strings.To_Virtual_String (Node.Text),
                  kind           => Utils.Get_Decl_Kind (Decl),
                  tags           => <>,
                  detail         => <>,
                  uri            => Location.uri,
                  a_range        => Location.a_range,
                  selectionRange => Location.a_range,
                  data           => <>);

               for Ref of Refs loop
                  declare
                     Ref_Location : constant LSP.Structures.Location :=
                       Locations.To_LSP_Location (Self, Ref);
                  begin
                     Call.fromRanges.Append (Ref_Location.a_range);

                     if Ref.P_Is_Dispatching_Call then
                        Call.dispatching_calls.Append (True);
                     else
                        Call.dispatching_calls.Append (False);
                     end if;
                  end;
               end loop;

               Response.Append (Call);
               Filter.Insert (Span);
            end;
         end if;
      end Add_Outgoing_Call;

      --------------
      -- Callback --
      --------------

      procedure Callback (Subp_Call : Libadalang.Analysis.Ada_Node'Class) is
         Dummy : Libadalang.Common.Ref_Result_Kind;
         Call_Definition : Libadalang.Analysis.Defining_Name;
         Subp_Call_Name  : constant Libadalang.Analysis.Name :=
           Laltools.Common.Get_Node_As_Name (Subp_Call.As_Ada_Node);
      begin

         --  First try to resolve the called function

         Call_Definition := Laltools.Common.Resolve_Name
           (Subp_Call_Name, Trace, Dummy);

         if not Call_Definition.Is_Null then
            if Result.Contains (Call_Definition) then
               declare
                  R              : constant
                    Laltools.Common.References_By_Subprogram.
                      Reference_Type :=
                        Result.Reference (Call_Definition);
               begin
                  R.Include (Subp_Call.As_Base_Id);
               end;
            else
               declare
                  L : Laltools.Common.References_Sets.Set;
               begin
                  L.Include (Subp_Call.As_Base_Id);
                  Result.Insert (Call_Definition, L);
               end;
            end if;
         end if;
      end Callback;

      Ignore : Libadalang.Common.Ref_Result_Kind;
      Cursor : Laltools.Common.References_By_Subprogram.Cursor;

   begin
      Laltools.Call_Hierarchy.Find_Outgoing_Calls
           (Definition => Definition,
            Callback   => Callback'Access,
            Trace      => Trace,
            Imprecise  => Ignore);

      Cursor := Result.First;
      --  Iterate through all the results, converting them to protocol
      --  objects.
      while Laltools.Common.References_By_Subprogram.Has_Element (Cursor) loop
         declare
            Node     : constant Libadalang.Analysis.Defining_Name :=
              Laltools.Common.References_By_Subprogram.Key (Cursor);
            Refs     : constant Laltools.Common.References_Sets.Set :=
              Laltools.Common.References_By_Subprogram.Element (Cursor);
         begin
            Add_Outgoing_Call (Node, Refs);

            Laltools.Common.References_By_Subprogram.Next (Cursor);
         end;
      end loop;
   end Find_Outgoing_Calls;

end LSP.Ada_Handlers.Call_Hierarchy;

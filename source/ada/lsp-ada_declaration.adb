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

with GNATCOLL.Traces;
with GNATCOLL.VFS;

with Libadalang.Analysis;
with Libadalang.Common;

with Laltools.Common;

with LSP.Ada_Context_Sets;
with LSP.Ada_Handlers.Locations;
with LSP.Client_Message_Receivers;
with LSP.Enumerations;
with LSP.Locations;
with LSP.Server_Request_Jobs;
with LSP.Server_Requests.Declaration;
with LSP.Structures;

package body LSP.Ada_Declaration is

   subtype AlsReferenceKind_Array is LSP.Structures.AlsReferenceKind_Set;

   function Is_Parent return AlsReferenceKind_Array is
     ([LSP.Enumerations.parent => True, others => False]);

   function Is_Child return AlsReferenceKind_Array is
     ([LSP.Enumerations.child => True, others => False]);

   type Ada_Declaration_Job
     (Parent : not null access constant Ada_Declaration_Handler) is limited
   new LSP.Server_Request_Jobs.Server_Request_Job
     (Priority => LSP.Server_Jobs.High)
   with record
      Response : LSP.Structures.Location_Vector;
      Filter   : LSP.Locations.File_Span_Sets.Set;
      Contexts : LSP.Ada_Context_Sets.Context_Lists.List;
   end record;

   type Ada_Declaration_Job_Access is access all Ada_Declaration_Job;

   overriding procedure Execute_Request
     (Self   : in out Ada_Declaration_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   function "or"
     (Left :
        LSP.Structures.AlsDisplayMethodAncestryOnNavigationPolicy_Optional;
      Right : LSP.Enumerations.AlsDisplayMethodAncestryOnNavigationPolicy)
        return LSP.Enumerations.AlsDisplayMethodAncestryOnNavigationPolicy is
          (if Left.Is_Set then Left.Value else Right);

   ----------------
   -- Create_Job --
   ----------------

   overriding function Create_Job
     (Self    : Ada_Declaration_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
      return LSP.Server_Jobs.Server_Job_Access
   is
      Value : LSP.Server_Requests.Declaration.Request
        renames LSP.Server_Requests.Declaration.Request
          (Message.all);

      File : constant GNATCOLL.VFS.Virtual_File :=
        Self.Context.To_File (Value.Params.textDocument.uri);

      Result : constant Ada_Declaration_Job_Access :=
        new Ada_Declaration_Job'
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
     (Self   : in out Ada_Declaration_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is
      use type
        LSP.Structures.AlsDisplayMethodAncestryOnNavigationPolicy_Optional;

      use all type LSP.Enumerations.AlsDisplayMethodAncestryOnNavigationPolicy;

      Message : LSP.Server_Requests.Declaration.Request
        renames LSP.Server_Requests.Declaration.Request (Self.Message.all);

      Value : LSP.Structures.DeclarationParams renames Message.Params;

      Context : LSP.Ada_Context_Sets.Context_Access;

      Display_Method_Policy : constant
        LSP.Enumerations.AlsDisplayMethodAncestryOnNavigationPolicy :=
          Value.alsDisplayMethodAncestryOnNavigation
          or
          Self.Parent.Context.Get_Configuration.Display_Method_Ancestry_Policy;

      Trace : constant GNATCOLL.Traces.Trace_Handle :=
        Self.Parent.Context.Get_Trace_Handle;

      Name_Node               : Libadalang.Analysis.Name;

      Definition              : Libadalang.Analysis.Defining_Name;
      --  A defining name that corresponds to Name_Node
      First_Part              : Libadalang.Analysis.Defining_Name;
      --  "Canonical part" of Definition
      Prev_Part               : Libadalang.Analysis.Defining_Name;
      --  A previous name for Definition
      Decl_For_Find_Overrides : Libadalang.Analysis.Basic_Decl :=
        Libadalang.Analysis.No_Basic_Decl;

      On_Defining_Name        : Boolean := False;
      --  Set to True if we are on a denfining name node

      Imprecise_Ignore : Boolean;
      Result_Kind      : Libadalang.Common.Ref_Result_Kind;
   begin
      if Self.Contexts.Is_Empty then
         --  No more contexts to process, sort and return collected results
         LSP.Ada_Handlers.Locations.Sort (Self.Response);

         Client.On_Declaration_Response
           (Message.Id,
            (Kind      => LSP.Structures.Variant_1,
             Variant_1 => Self.Response));

         Status := LSP.Server_Jobs.Done;

         return;
      else
         Status := LSP.Server_Jobs.Continue;
      end if;

      Context := Self.Contexts.First_Element;
      Self.Contexts.Delete_First;

      Name_Node := Laltools.Common.Get_Node_As_Name
        (Self.Parent.Context.Get_Node_At (Context.all, Value));

      if Name_Node.Is_Null then
         return;
      end if;

      --  Check if we are on some defining name
      Definition := Laltools.Common.Get_Name_As_Defining (Name_Node);

      if Definition.Is_Null then
         --  If we aren't on a defining_name already then try to resolve
         Definition := Laltools.Common.Resolve_Name
           (Name_Node, Trace, Result_Kind);
      else
         On_Defining_Name := True;
      end if;

      if Result_Kind in Libadalang.Common.Error or else Definition.Is_Null then
         return;  --  Name resolution fails, nothing to do.
      end if;

      --  Display the method ancestry in three cases:
      --
      --   . When the preference is set to Always
      --
      --   . When we are on a usage node (e.g: subprogram call) and if the
      --     preference is set to Usage_And_Abstract_Only
      --
      --   . When we are on a defining name node and if the preference is
      --     set to Definition_Only

      if Display_Method_Policy = Always
        or else (Display_Method_Policy = Usage_And_Abstract_Only
                and then not On_Defining_Name)
        or else (Display_Method_Policy = Definition_Only
                and then On_Defining_Name)
      then
         First_Part := Laltools.Common.Find_Canonical_Part (Definition, Trace);

         Decl_For_Find_Overrides :=
           (if First_Part.Is_Null then Definition.P_Basic_Decl
            else First_Part.P_Basic_Decl);
      end if;

      begin
         Prev_Part := Definition.P_Previous_Part;
      exception
         when E :  Libadalang.Common.Property_Error =>
            Self.Parent.Context.Trace_Exception (E);
            Prev_Part := Libadalang.Analysis.No_Defining_Name;
      end;

      if not Prev_Part.Is_Null then
         --  We have found previous part, return it.
         Self.Parent.Context.Append_Location
           (Self.Response,
            Self.Filter,
            Prev_Part);
      elsif not Definition.Is_Null then
         --  No previous part, return definition itself.
         Self.Parent.Context.Append_Location
           (Self.Response,
            Self.Filter,
            Definition);
      end if;

      if not Decl_For_Find_Overrides.Is_Null then
         declare
            Overriding_Result_Kind : Libadalang.Common.Ref_Result_Kind;
            Bases_Result_Kind      : Libadalang.Common.Ref_Result_Kind;
            Overridings : constant Libadalang.Analysis.Basic_Decl_Array :=
              Context.Find_All_Overrides
                (Decl_For_Find_Overrides,
                 Result_Kind => Overriding_Result_Kind);

            Bases       : constant Libadalang.Analysis.Basic_Decl_Array :=
              Context.Find_All_Base_Declarations
                (Decl_For_Find_Overrides,
                 Result_Kind => Bases_Result_Kind);
         begin
            if Overriding_Result_Kind in Libadalang.Common.Error
              or else Bases_Result_Kind in Libadalang.Common.Error
            then
               --  Abort
               return;
            end if;

            for Subp of Bases loop
               Self.Parent.Context.Append_Location
                 (Self.Response,
                  Self.Filter,
                  Subp.P_Defining_Name,
                  Is_Parent);
            end loop;

            for Subp of Overridings loop
               Self.Parent.Context.Append_Location
                 (Self.Response,
                  Self.Filter,
                  Subp.P_Defining_Name,
                  Is_Child);
            end loop;
         end;
      end if;
   end Execute_Request;

end LSP.Ada_Declaration;

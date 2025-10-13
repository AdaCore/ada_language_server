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
with LSP.Ada_Documents;
with LSP.Ada_Handlers.Locations;
with LSP.Ada_Request_Jobs;
with LSP.Client_Message_Receivers;
with LSP.Enumerations;
with LSP.Locations;
with LSP.Server_Requests.Definition;
with LSP.Structures;

package body LSP.Ada_Definition is

   subtype AlsReferenceKind_Array is LSP.Structures.AlsReferenceKind_Set;

   function Is_Parent return AlsReferenceKind_Array is
     ([LSP.Enumerations.parent => True, others => False]);

   function Is_Child return AlsReferenceKind_Array is
     ([LSP.Enumerations.child => True, others => False]);

   type Ada_Definition_Job
     (Parent : not null access constant Ada_Definition_Handler) is limited
   new LSP.Ada_Request_Jobs.Ada_Request_Job
     (Priority => LSP.Server_Jobs.High)
   with record
      Response : LSP.Structures.Location_Vector;
      Filter   : LSP.Locations.File_Span_Sets.Set;
      Contexts : LSP.Ada_Context_Sets.Context_Lists.List;
   end record;

   type Ada_Definition_Job_Access is access all Ada_Definition_Job;

   overriding procedure Execute_Ada_Request
     (Self   : in out Ada_Definition_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   ----------------
   -- Create_Job --
   ----------------

   overriding function Create_Job
     (Self    : Ada_Definition_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
      return LSP.Server_Jobs.Server_Job_Access
   is
      Value : LSP.Server_Requests.Definition.Request
        renames LSP.Server_Requests.Definition.Request
          (Message.all);

      File : constant GNATCOLL.VFS.Virtual_File :=
        Self.Context.To_File (Value.Params.textDocument.uri);

      Result : constant Ada_Definition_Job_Access :=
        new Ada_Definition_Job'
          (Parent  => Self'Unchecked_Access,
           Request => LSP.Ada_Request_Jobs.Request_Access (Message),
           others  => <>);
   begin
      Result.Contexts := Self.Context.Contexts_For_File (File);

      return LSP.Server_Jobs.Server_Job_Access (Result);
   end Create_Job;

   -------------------------
   -- Execute_Ada_Request --
   -------------------------

   overriding procedure Execute_Ada_Request
     (Self   : in out Ada_Definition_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is
      use all type LSP.Enumerations.AlsDisplayMethodAncestryOnNavigationPolicy;

      Message : LSP.Server_Requests.Definition.Request
        renames LSP.Server_Requests.Definition.Request (Self.Message.all);

      Value : LSP.Structures.DefinitionParams renames Message.Params;

      Context : LSP.Ada_Context_Sets.Context_Access;

      Display_Method_Policy : constant
        LSP.Enumerations.AlsDisplayMethodAncestryOnNavigationPolicy :=
          (if Value.alsDisplayMethodAncestryOnNavigation.Is_Set
           then Value.alsDisplayMethodAncestryOnNavigation.Value
           else Self.Parent.Context.Get_Configuration
                 .Display_Method_Ancestry_Policy);

      Trace : constant GNATCOLL.Traces.Trace_Handle :=
        Self.Parent.Context.Get_Trace_Handle;

      Name_Node               : Libadalang.Analysis.Name;
      Definition              : Libadalang.Analysis.Defining_Name;
      Other_Part              : Libadalang.Analysis.Defining_Name;
      Manual_Fallback         : Libadalang.Analysis.Defining_Name;
      Definition_Node         : Libadalang.Analysis.Basic_Decl;
      Decl_For_Find_Overrides : Libadalang.Analysis.Basic_Decl;

      Ignore : Boolean;

      procedure Retrieve_Alternate_Part_Location;
      --  Retrieve the location of the alternate part of a package, task,
      --  protected type, or subprogram.
      --  This is used when the user queries the definition of a token that
      --  does not belong to a defining name.
      --  If the queried token is equal to 'is', retrieve the location of the
      --  private part of the package, task, protected type, or the begin
      --  of the subprogram body.

      procedure Retrieve_Alternate_Part_Location is
         use Libadalang.Common;

         procedure Append_Prev_Token_Location
           (Node : Libadalang.Analysis.Ada_Node'Class);
         --  Append the previous token's location to the response.

         -----------------------
         -- Append_Prev_Token --
         -----------------------

         procedure Append_Prev_Token_Location
           (Node : Libadalang.Analysis.Ada_Node'Class) is
         begin
            if not Node.Is_Null then
               declare
                  Prev : constant Libadalang.Common.Token_Reference :=
                    Libadalang.Common.Previous (Node.Token_Start, True);
               begin
                  Self.Parent.Context.Append_Location
                    (Result => Self.Response,
                     Filter => Self.Filter,
                     Unit   => Node.Unit,
                     Token  => Prev);
               end;
            end if;
         end Append_Prev_Token_Location;

         Document : constant LSP.Ada_Documents.Document_Access :=
           Self.Parent.Context.Get_Open_Document (Value.textDocument.uri);
         Token    : Libadalang.Common.Token_Reference :=
           Document.Get_Token_At (Context.all, Value.position);
         Node     : Libadalang.Analysis.Ada_Node;
         Position : LSP.Structures.Position;
      begin
         if Token /= No_Token then
            if Token.Data.Kind = Ada_Is then
               Node := Document.Get_Node_At (Context => Context.all, Position => Value.position);

               if not Node.Is_Null and then Node.Kind in Ada_Subp_Body_Range
               then
                  --  We are on the 'is' of a subprogram body: retrieve the
                  --  enclosing subprogram's defining name instead of checking
                  --  for the previous token's node since it might refer to the
                  --  subprogram's parameter list.
                  Definition := Node.As_Subp_Body.P_Defining_Name;
               else
                  --  We are on the 'is' of a package, task, or protected type: retrieve
                  --  the previous token's node, which should be the defining name.
                  Token := Token.Previous (True);

                  if Token = No_Token then
                     --  Return if there is no previous token.
                     return;
                  end if;

                  Position :=
                    Document.To_A_Range (Token.Data.Sloc_Range).start;
                  Node :=
                    Document.Get_Node_At
                      (Context => Context.all, Position => Position);

                  Definition := Laltools.Common.Resolve_Name_Precisely (Node.As_Name);
               end if;

               --  Append private part, begin locations to the response
               for Part of Definition.P_Basic_Decl.P_All_Parts loop
                  case Part.Kind is

                     when Libadalang.Common.Ada_Package_Body_Range =>
                        Append_Prev_Token_Location (Part.As_Package_Body.F_Stmts);

                     when Libadalang.Common.Ada_Subp_Body_Range =>
                        Append_Prev_Token_Location (Part.As_Subp_Body.F_Stmts);

                     when Libadalang.Common.Ada_Task_Body_Range =>
                        Append_Prev_Token_Location (Part.As_Task_Body.F_Stmts);

                     when Libadalang.Common.Ada_Base_Package_Decl =>
                        Append_Prev_Token_Location
                          (Part.As_Base_Package_Decl.F_Private_Part);

                     when Libadalang.Common.Ada_Protected_Type_Decl_Range =>
                        Append_Prev_Token_Location
                          (Part
                             .As_Protected_Type_Decl
                             .F_Definition
                             .F_Private_Part);

                     when Libadalang.Common.Ada_Single_Protected_Decl_Range =>
                        Append_Prev_Token_Location
                          (Part
                             .As_Single_Protected_Decl
                             .F_Definition
                             .F_Private_Part);

                     when Libadalang.Common.Ada_Task_Type_Decl_Range =>
                        Append_Prev_Token_Location
                          (Part.As_Task_Type_Decl.F_Definition.F_Private_Part);

                     when Libadalang.Common.Ada_Single_Task_Decl_Range =>
                        Append_Prev_Token_Location
                          (Part
                             .As_Single_Task_Decl
                             .F_Task_Type
                             .F_Definition
                             .F_Private_Part);

                     when others =>
                        null;
                  end case;
               end loop;
            end if;
         end if;
      end Retrieve_Alternate_Part_Location;

   begin
      if Self.Contexts.Is_Empty then
         --  No more contexts to process, sort and return collected results
         LSP.Ada_Handlers.Locations.Sort (Self.Response);

         Client.On_Definition_Response
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
         Retrieve_Alternate_Part_Location;
         return;
      end if;

      --  Check if we are on some defining name
      Definition := Laltools.Common.Get_Name_As_Defining (Name_Node);

      if Definition.Is_Null then
         Definition := Self.Parent.Context.Imprecise_Resolve_Name (Name_Node);

         if not Definition.Is_Null then
            Self.Parent.Context.Append_Location
              (Self.Response,
               Self.Filter,
               Definition);

            if Display_Method_Policy in Usage_And_Abstract_Only | Always then
               Decl_For_Find_Overrides := Definition.P_Basic_Decl;
            end if;
         end if;
      else  --  If we are on a defining_name already
         Other_Part := Laltools.Common.Find_Next_Part (Definition, Trace);

         Definition_Node := Definition.P_Basic_Decl;

         --  Search for overriding subprograms only if we are on an
         --  abstract subprogram.
         if Display_Method_Policy /= Never
           and then
             (Display_Method_Policy /= Usage_And_Abstract_Only
              or else Definition_Node.Kind in
                Libadalang.Common.Ada_Abstract_Subp_Decl_Range)
         then
            Decl_For_Find_Overrides := Definition_Node;
         end if;

         --  Search for accept statements only if we are on an entry
         if Definition_Node.Kind in Libadalang.Common.Ada_Entry_Decl_Range then
            declare
               Entry_Decl_Node   : constant Libadalang.Analysis.Entry_Decl :=
                 Definition_Node.As_Entry_Decl;
               Entry_Parent_Node : constant Libadalang.Analysis.Basic_Decl :=
                 Entry_Decl_Node.P_Parent_Basic_Decl;
            begin
               --  P_Accept_Stmts is only valid for entries declared in tasks
               if Entry_Parent_Node.Kind in
                 Libadalang.Common.Ada_Task_Type_Decl_Range
               then
                  for Accept_Node of Entry_Decl_Node.P_Accept_Stmts loop
                     Self.Parent.Context.Append_Location
                       (Self.Response,
                        Self.Filter,
                        Accept_Node.F_Body_Decl.F_Name);
                  end loop;

               --  Others entries are are handled as simple subprograms
               else
                  declare
                     Other_Part_For_Decl : constant
                       Libadalang.Analysis.Basic_Decl :=
                       Laltools.Common.Find_Next_Part_For_Decl
                         (Definition_Node, Trace);
                  begin
                     if not Other_Part_For_Decl.Is_Null then
                        Other_Part := Other_Part_For_Decl.P_Defining_Name;
                     end if;
                  end;
               end if;
            end;

         elsif Definition_Node.Kind in
           Libadalang.Common.Ada_Single_Task_Type_Decl_Range |
           Libadalang.Common.Ada_Protected_Type_Decl_Range
         then
            --  These node types are not handled by Find_Next_Part
            --  (LAL design limitations)
            declare
               Other_Part_For_Decl : constant Libadalang.Analysis.Basic_Decl :=
                 Laltools.Common.Find_Next_Part_For_Decl
                   (Definition_Node, Trace);
            begin
               if not Other_Part_For_Decl.Is_Null then
                  Other_Part := Other_Part_For_Decl.P_Defining_Name;
               end if;
            end;
         end if;

         if Other_Part.Is_Null then
            --  No next part is found. Check first defining name
            Other_Part := Laltools.Common.Find_Canonical_Part
              (Definition, Trace);
         end if;

         if Other_Part.Is_Null then
            --  We were on a defining name, but did not manage to find
            --  an answer using Find_Next_Part / Find_Canonical_Part.
            --  Use the manual fallback to attempt to find a good enough
            --  result.
            Manual_Fallback := Laltools.Common.Find_Other_Part_Fallback
              (Definition, Trace);

            if not Manual_Fallback.Is_Null then
               --  We have found a result using the imprecise heuristics.
               --  We'll warn the user and send the result.
               Self.Parent.Context.Append_Location
                 (Self.Response,
                  Self.Filter,
                  Manual_Fallback);
            end if;
         else
            Self.Parent.Context.Append_Location
              (Self.Response,
               Self.Filter,
               Other_Part);

         end if;
      end if;

      if not Decl_For_Find_Overrides.Is_Null then
         declare
            Overridings : constant Libadalang.Analysis.Basic_Decl_Array :=
              Context.Find_All_Overrides
                (Decl_For_Find_Overrides,
                 Imprecise_Results => Ignore);

            Bases       : constant Libadalang.Analysis.Basic_Decl_Array :=
              Context.Find_All_Base_Declarations
                (Decl_For_Find_Overrides,
                 Imprecise_Results => Ignore);
         begin
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
   end Execute_Ada_Request;

end LSP.Ada_Definition;

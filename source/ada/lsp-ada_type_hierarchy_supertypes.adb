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

with Libadalang.Analysis;
with Libadalang.Common;

with VSS.Strings;

with LSP.Ada_Context_Sets;
with LSP.Ada_Request_Jobs;
with LSP.Client_Message_Receivers;
with LSP.Server_Request_Jobs;
with LSP.Server_Requests.Supertypes;
with LSP.Structures;
with LSP.Utils;

package body LSP.Ada_Type_Hierarchy_Supertypes is

   type Ada_Type_Hierarchy_Supertype_Job
     (Parent : not null access constant Ada_Type_Hierarchy_Supertype_Handler) is
       limited new LSP.Ada_Request_Jobs.Ada_Request_Job
         (Priority => LSP.Server_Jobs.Low)
   with null record;

   overriding procedure Execute_Ada_Request
     (Self   : in out Ada_Type_Hierarchy_Supertype_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);
   --  Execute Supertypes request

   -------------------------
   -- Execute_Ada_Request --
   -------------------------

   overriding procedure Execute_Ada_Request
     (Self   : in out Ada_Type_Hierarchy_Supertype_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is
      use type LSP.Structures.A_Range;
      use type LSP.Structures.DocumentUri;

      Message : LSP.Server_Requests.Supertypes.Request
        renames LSP.Server_Requests.Supertypes.Request (Self.Message.all);

      Context : constant LSP.Ada_Context_Sets.Context_Access :=
        Self.Parent.Context.Get_Best_Context (Message.Params.item.uri);

      Response : LSP.Structures.TypeHierarchyItem_Vector_Or_Null;

      Node : constant Libadalang.Analysis.Ada_Node :=
        Self.Parent.Context.Get_Node_At
          (Context.all,
           (LSP.Structures.TextDocumentPositionParams'
             ((uri => Message.Params.item.uri),
              Message.Params.item.selectionRange.start)));
      --  We expect here an identifier in the type declaration

      Decl : constant Libadalang.Analysis.Basic_Decl :=
        (if not Node.Is_Null
           and then Node.Kind in Libadalang.Common.Ada_Name
           and then Node.As_Name.P_Is_Defining
         then Node.As_Name.P_Enclosing_Defining_Name.P_Basic_Decl
         else Libadalang.Analysis.No_Basic_Decl);

      Loc  : LSP.Structures.Location;
      Item : LSP.Structures.TypeHierarchyItem;
      Name : Libadalang.Analysis.Defining_Name;
   begin
      --  Iterate over all type completion parts and find parent types for each
      --  part.
      if not Decl.Is_Null then
         for Part of Decl.P_All_Parts
           when Part.Kind in Libadalang.Common.Ada_Base_Type_Decl
         loop
            for Tipe of Part.As_Base_Type_Decl.P_Base_Types (Part) loop
               Name := Tipe.P_Defining_Name.P_Canonical_Part;

               Loc := Self.Parent.Context.To_LSP_Location (Name.P_Basic_Decl);

               if not
                 (for some X of Response =>
                    X.uri = Loc.uri and X.a_range = Loc.a_range)
               then
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

                  Response.Append (Item);
               end if;
            end loop;
         end loop;
      end if;

      Client.On_Supertypes_Response (Message.Id, Response);
      Status := LSP.Server_Jobs.Done;
   end Execute_Ada_Request;

   ----------------
   -- Create_Job --
   ----------------

   overriding function Create_Job
     (Self    : Ada_Type_Hierarchy_Supertype_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
        return LSP.Server_Jobs.Server_Job_Access is
   begin
      return new Ada_Type_Hierarchy_Supertype_Job'
        (Parent  => Self'Unchecked_Access,
         Request => LSP.Server_Request_Jobs.Request_Access (Message));
   end Create_Job;

end LSP.Ada_Type_Hierarchy_Supertypes;

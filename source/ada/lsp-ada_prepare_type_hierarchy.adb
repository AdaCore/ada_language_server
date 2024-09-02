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

with Libadalang;
with Libadalang.Analysis;
with Libadalang.Common;

with VSS.Strings;

with LSP.Ada_Context_Sets;
with LSP.Ada_Request_Jobs;
with LSP.Client_Message_Receivers;
with LSP.Server_Request_Jobs;
with LSP.Server_Requests.PrepareTypeHierarchy;
with LSP.Structures;
with LSP.Utils;

package body LSP.Ada_Prepare_Type_Hierarchy is

   type Ada_Prepare_Type_Hierarchy_Job
     (Parent : not null access constant Ada_Prepare_Type_Hierarchy_Handler) is
       limited new LSP.Ada_Request_Jobs.Ada_Request_Job
         (Priority => LSP.Server_Jobs.Low)
   with null record;

   overriding procedure Execute_Ada_Request
     (Self   : in out Ada_Prepare_Type_Hierarchy_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);
   --  Execute PrepareTypeHierarchy request

   function Skip_Subtypes
     (Tipe : Libadalang.Analysis.Base_Type_Decl)
       return Libadalang.Analysis.Base_Type_Decl;

   -------------------------
   -- Execute_Ada_Request --
   -------------------------

   overriding procedure Execute_Ada_Request
     (Self   : in out Ada_Prepare_Type_Hierarchy_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is
      Message : LSP.Server_Requests.PrepareTypeHierarchy.Request
        renames LSP.Server_Requests.PrepareTypeHierarchy.Request
          (Self.Message.all);

      Context : constant LSP.Ada_Context_Sets.Context_Access :=
        Self.Parent.Context.Get_Best_Context (Message.Params.textDocument.uri);

      Response : LSP.Structures.TypeHierarchyItem_Vector_Or_Null;

      Name : constant Libadalang.Analysis.Defining_Name :=
        Self.Parent.Context.Imprecise_Resolve_Name
          (Context.all, Message.Params);
      --  Canonical defining name

      Type_Decl : constant Libadalang.Analysis.Base_Type_Decl :=
        (if not Name.Is_Null
           and then not Name.P_Basic_Decl.Is_Null
           and then Name.P_Basic_Decl.Kind in
             Libadalang.Common.Ada_Base_Type_Decl
         then Skip_Subtypes (Name.P_Basic_Decl.As_Base_Type_Decl)
             else Libadalang.Analysis.No_Base_Type_Decl);

      Loc  : LSP.Structures.Location;
      Decl : Libadalang.Analysis.Basic_Decl;
      Item : LSP.Structures.TypeHierarchyItem;
   begin
      if not Type_Decl.Is_Null then
         --  We have got a type, return its the very first declaration "part"
         Decl := Type_Decl.P_Canonical_Part;

         Loc := Self.Parent.Context.To_LSP_Location (Decl);

         Item :=
           (name           => VSS.Strings.To_Virtual_String
              (Decl.P_Defining_Name.Text),
            kind           => LSP.Utils.Get_Decl_Kind (Decl),
            tags           => <>,
            detail         => LSP.Utils.Node_Location_Image
              (Decl.P_Defining_Name),
            uri            => Loc.uri,
            a_range        => Loc.a_range,
            selectionRange => Self.Parent.Context.To_LSP_Location
              (Decl.P_Defining_Name).a_range,
            data           => <>);

         Response.Append (Item);
      end if;

      Client.On_PrepareTypeHierarchy_Response (Message.Id, Response);
      Status := LSP.Server_Jobs.Done;
   end Execute_Ada_Request;

   ----------------
   -- Create_Job --
   ----------------

   overriding function Create_Job
     (Self    : Ada_Prepare_Type_Hierarchy_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
        return LSP.Server_Jobs.Server_Job_Access is
   begin
      return new Ada_Prepare_Type_Hierarchy_Job'
        (Parent  => Self'Unchecked_Access,
         Request => LSP.Server_Request_Jobs.Request_Access (Message));
   end Create_Job;

   -------------------
   -- Skip_Subtypes --
   -------------------

   function Skip_Subtypes
     (Tipe : Libadalang.Analysis.Base_Type_Decl)
       return Libadalang.Analysis.Base_Type_Decl
   is
      use type Libadalang.Analysis.Base_Type_Decl;

      Result : constant Libadalang.Analysis.Base_Type_Decl :=
        Tipe.P_Base_Subtype;
   begin
      return (if Tipe = Result then Tipe else Skip_Subtypes (Result));
   exception
      when others =>
         return Tipe;
   end Skip_Subtypes;

end LSP.Ada_Prepare_Type_Hierarchy;

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

with VSS.Characters.Latin;
with VSS.Strings;

with LSP.Ada_Context_Sets;
with LSP.Ada_Documentation;
with LSP.Client_Message_Receivers;
with LSP.Predefined_Completion;
with LSP.Server_Request_Jobs;
with LSP.Server_Requests.Hover;
with LSP.Structures;
with LSP.Utils;

package body LSP.Ada_Hover is

   type Ada_Hover_Job
     (Parent : not null access constant Ada_Hover_Handler) is limited
   new LSP.Server_Request_Jobs.Server_Request_Job
     (Priority => LSP.Server_Jobs.High)
        with null record;

   type Ada_Hover_Job_Access is access all Ada_Hover_Job;

   overriding procedure Execute_Request
     (Self   : in out Ada_Hover_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   ----------------
   -- Create_Job --
   ----------------

   overriding function Create_Job
     (Self    : Ada_Hover_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
      return LSP.Server_Jobs.Server_Job_Access
   is
      Result : constant Ada_Hover_Job_Access :=
        new Ada_Hover_Job'
          (Parent  => Self'Unchecked_Access,
           Request => LSP.Server_Request_Jobs.Request_Access (Message));
   begin
      return LSP.Server_Jobs.Server_Job_Access (Result);
   end Create_Job;

   ---------------------
   -- Execute_Request --
   ---------------------

   overriding procedure Execute_Request
     (Self   : in out Ada_Hover_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is

      Message : LSP.Server_Requests.Hover.Request
        renames LSP.Server_Requests.Hover.Request (Self.Message.all);

      Value : LSP.Structures.HoverParams renames Message.Params;

      Response : LSP.Structures.Hover_Or_Null;

      Context : constant LSP.Ada_Context_Sets.Context_Access :=
        Self.Parent.Context.Get_Best_Context
          (Value.textDocument.uri);
      --  For the Hover request, we're only interested in the "best"
      --  response value, not in the list of values for all contexts

      Defining_Name_Node : constant Libadalang.Analysis.Defining_Name :=
        Self.Parent.Context.Imprecise_Resolve_Name (Context.all, Value);
      Decl               : constant Libadalang.Analysis.Basic_Decl :=
        (if Defining_Name_Node.Is_Null
         then Libadalang.Analysis.No_Basic_Decl
         else Defining_Name_Node.P_Basic_Decl);
      --  Associated basic declaration, if any

      Decl_Text          : VSS.Strings.Virtual_String;
      Qualifier_Text     : VSS.Strings.Virtual_String;
      Documentation_Text : VSS.Strings.Virtual_String;
      Location_Text      : VSS.Strings.Virtual_String;
      Aspects_Text       : VSS.Strings.Virtual_String;

   begin
      Status := LSP.Server_Jobs.Done;

      if Decl.Is_Null then

         --  There is no declaration for the hovered node: ask the predefined
         --  entities' completion provider (attributes, pragmas, aspects) for
         --  a tooltip text if it can.
         declare
            Node : constant Libadalang.Analysis.Ada_Node :=
              Self.Parent.Context.Get_Node_At (Context.all, Value);
         begin
            if not Node.Is_Null
              and then Node.Kind in Libadalang.Common.Ada_Identifier_Range
            then
               LSP.Predefined_Completion.Get_Tooltip_Text
                 (Node               => Node.As_Identifier,
                  Declaration_Text   => Decl_Text,
                  Documentation_Text => Documentation_Text);
            end if;
         end;
      else
         --  We have resolved the hovered node to its declaration: use
         --  the default tooltip provider, based on GNATdoc.
         LSP.Ada_Documentation.Get_Tooltip_Text
           (BD                 => Decl,
            Style              => Context.Get_Documentation_Style,
            Declaration_Text   => Decl_Text,
            Qualifier_Text     => Qualifier_Text,
            Location_Text      => Location_Text,
            Documentation_Text => Documentation_Text,
            Aspects_Text       => Aspects_Text);
      end if;

      --  Return if no provider has been able to compute text for a tooltip.
      if Decl_Text.Is_Empty then
         Client.On_Hover_Response (Message.Id, (Is_Null => True));
         return;
      end if;

      Response := (Is_Null => False, others => <>);
      Response.Value.contents := (Is_MarkupContent => False, others => <>);

      --  Append the whole declaration text to the response

      Response.Value.contents.MarkedString_Vector.Append
        (LSP.Structures.MarkedString'
           (Is_Virtual_String => False,
            value             => Decl_Text,
            language          => "ada"));

      --  Append qualifier text if any

      if not Qualifier_Text.Is_Empty then
         Response.Value.contents.MarkedString_Vector.Append
           (LSP.Structures.MarkedString'
              (Is_Virtual_String => True,
               Virtual_String    => Qualifier_Text));
      end if;

      --  Append the declaration's location.
      --
      --  In addition, append the project's name if we are dealing with an
      --  aggregate project.

      if not Decl.Is_Null then
         Location_Text := LSP.Utils.Node_Location_Image (Decl);

         if Self.Parent.Context.Project_Tree_Is_Aggregate then
            Location_Text.Append (VSS.Characters.Latin.Line_Feed);
            Location_Text.Append ("As defined in project ");
            Location_Text.Append (Context.Id);
            Location_Text.Append (" (other projects skipped).");
         end if;

         Response.Value.contents.MarkedString_Vector.Append
           (LSP.Structures.MarkedString'
              (Is_Virtual_String => True,
               Virtual_String    => Location_Text));
      end if;

      --  Append the comments associated with the basic declaration if any.

      if not Documentation_Text.Is_Empty then
         Response.Value.contents.MarkedString_Vector.Append
           (LSP.Structures.MarkedString'
              (Is_Virtual_String => False,
               language          => "plaintext",
               value             => Documentation_Text));
      end if;

      --  Append text of aspects

      if not Aspects_Text.Is_Empty then
         Response.Value.contents.MarkedString_Vector.Append
           (LSP.Structures.MarkedString'
              (Is_Virtual_String => False,
               value             => Aspects_Text,
               language          => "ada"));
      end if;

      Client.On_Hover_Response (Message.Id, Response);
   end Execute_Request;

end LSP.Ada_Hover;

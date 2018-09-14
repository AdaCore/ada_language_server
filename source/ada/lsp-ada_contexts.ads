------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
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

with Ada.Containers.Hashed_Maps;

private with GNATCOLL.Projects;
private with Libadalang.Analysis;
private with Libadalang.Common;

with LSP.Messages;

with LSP.Ada_Documents;
with LSP.Types;

package LSP.Ada_Contexts is
   type Context is tagged limited private;

   not overriding procedure Initialize
     (Self : in out Context;
      Root : LSP.Types.LSP_String);

   not overriding procedure Load_Document
     (Self  : in out Context;
      Item  : LSP.Messages.TextDocumentItem);

   not overriding function Get_Document
     (Self : Context;
      URI  : LSP.Messages.DocumentUri)
        return LSP.Ada_Documents.Document_Access;

   not overriding procedure Update_Document
     (Self : in out Context;
      Item : not null LSP.Ada_Documents.Document_Access);
   --  Reparse document after changes

private

   type Unit_Provider (Context : access LSP.Ada_Contexts.Context) is
     new Libadalang.Analysis.Unit_Provider_Interface with null record;

   overriding function Get_Unit_Filename
     (Self : Unit_Provider;
      Name : Wide_Wide_String;
      Kind : Libadalang.Common.Unit_Kind)
      return String;

   overriding function Get_Unit
     (Self    : Unit_Provider;
      Context : Libadalang.Analysis.Analysis_Context'Class;
      Name    : Wide_Wide_String;
      Kind    : Libadalang.Common.Unit_Kind;
      Charset : String := "";
      Reparse : Boolean := False)
      return Libadalang.Analysis.Analysis_Unit'Class;

   overriding procedure Release (Self : in out Unit_Provider) is null;

   package Document_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => LSP.Messages.DocumentUri,
      Element_Type    => LSP.Ada_Documents.Document_Access,
      Hash            => LSP.Types.Hash,
      Equivalent_Keys => LSP.Types."=",
      "="             => LSP.Ada_Documents."=");

   type Context is tagged limited record
      Unit_Provider : Ada_Contexts.Unit_Provider (Context'Unchecked_Access);
      LAL_Context   : Libadalang.Analysis.Analysis_Context;

      Project_Env   : GNATCOLL.Projects.Project_Environment_Access;
      Project_Tree  : aliased GNATCOLL.Projects.Project_Tree;
      Root          : LSP.Types.LSP_String;

      Documents     : Document_Maps.Map;
   end record;

end LSP.Ada_Contexts;

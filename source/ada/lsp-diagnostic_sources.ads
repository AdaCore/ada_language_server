------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;
with GNATCOLL.VFS;
with LSP.Structures;
limited with LSP.Ada_Contexts;

package LSP.Diagnostic_Sources is

   type Diagnostic_Source is limited interface;
   --  A source of diagnostic (errors/warnings) messages
   type Diagnostic_Source_Access is
     access LSP.Diagnostic_Sources.Diagnostic_Source'Class;
   package Diagnostic_Source_Vectors is new
     Ada.Containers.Vectors
       (Positive,
        LSP.Diagnostic_Sources.Diagnostic_Source_Access,
        LSP.Diagnostic_Sources."=");

   procedure Get_Diagnostic
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context;
      Errors  : out LSP.Structures.Diagnostic_Vector) is abstract;
   --  Fill diagnostics for the given context.

   function Has_New_Diagnostic
     (Self : in out Diagnostic_Source; Context : LSP.Ada_Contexts.Context)
      return Boolean
   is abstract;
   --  Return True if diagnostic changed since last call to Get_Diagnostic or
   --  if Get_Diagnostic was never called and any diagnostic presents.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Diagnostic_Source'Class, Diagnostic_Source_Access);

   type Workspace_Diagnostic_Source is limited interface;
   --  Interface for workspace diagnostics (i.e: diagnostics that
   --  are not specific to a given context/document).
   --  Currently published workspace diagnostics are always cleared
   --  before querying new workspace diagnostics.

   type Workspace_Diagnostic_Source_Access is
     access LSP.Diagnostic_Sources.Workspace_Diagnostic_Source'Class;
   package Workspace_Diagnostic_Source_Vectors is new
     Ada.Containers.Vectors
       (Positive,
        LSP.Diagnostic_Sources.Workspace_Diagnostic_Source_Access,
        LSP.Diagnostic_Sources."=");

   procedure Get_Diagnostics
     (Self          : in out Workspace_Diagnostic_Source;
      Diagnostics   : out LSP.Structures.Diagnostic_Vector;
      Target_File   : out GNATCOLL.VFS.Virtual_File)
   is abstract;

   function Has_New_Diagnostic
     (Self : in out Workspace_Diagnostic_Source)
      return Boolean
   is abstract;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Workspace_Diagnostic_Source'Class, Workspace_Diagnostic_Source_Access);

end LSP.Diagnostic_Sources;

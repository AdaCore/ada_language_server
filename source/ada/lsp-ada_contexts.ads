------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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
--
--  This package provides a context of Ada Language server.

with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding;

with GNATCOLL.Projects;
with GNATCOLL.Traces;
with GNATCOLL.VFS;

with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;

with Libadalang.Analysis;
with Libadalang.Common;

with Utils.Command_Lines;
with Pp.Command_Lines;

with VSS.Strings;

with LSP.Messages;
with LSP.Ada_Documents;
with LSP.Ada_File_Sets; use LSP.Ada_File_Sets;
with LSP.Types;

package LSP.Ada_Contexts is

   type Context (Trace : GNATCOLL.Traces.Trace_Handle) is
     tagged limited private;
   --  A context contains a non-aggregate project tree and its associated
   --  libadalang context.

   procedure Initialize
     (Self                : in out Context;
      File_Reader         : File_Reader_Interface'Class;
      Follow_Symlinks     : Boolean;
      As_Fallback_Context : Boolean := False);
   --  Initialize the context, set Follow_Symlinks flag.
   --  As_Fallback_Context should be set when we are creating the "fallback"
   --  context based on the empty project.

   procedure Load_Project
     (Self     : in out Context;
      Tree     : not null GNATCOLL.Projects.Project_Tree_Access;
      Root     : GNATCOLL.Projects.Project_Type;
      Charset  : String);
   --  Use the given project tree, and root project within this project
   --  tree, as project for this context. Root must be a non-aggregate
   --  project tree representing the root of a hierarchy inside Tree.

   function Id (Self : Context) return VSS.Strings.Virtual_String;
   --  Return unique identifier of the context.

   procedure Reload (Self : in out Context);
   --  Reload the current context. This will invalidate and destroy any
   --  Libadalang related data, and recreate it from scratch.

   procedure Free (Self : in out Context);
   --  Release the memory associated to Self. You should not use the
   --  context after calling this.

   function URI_To_File
     (Self : Context;
      URI  : LSP.Types.LSP_URI)
      return Ada.Strings.UTF_Encoding.UTF_8_String;

   function Get_Node_At
     (Self     : Context;
      Document : LSP.Ada_Documents.Document_Access;
      Position : LSP.Messages.TextDocumentPositionParams'Class)
      return Libadalang.Analysis.Ada_Node;
   --  Return the node at the given location.
   --  If Document is not null, get the location from the document, otherwise
   --  get it from the file.

   procedure Format
     (Self     : in out Context;
      Document : LSP.Ada_Documents.Document_Access;
      Span     : LSP.Messages.Span;
      Options  : LSP.Messages.FormattingOptions;
      Edit     : out LSP.Messages.TextEdit_Vector;
      Success  : out Boolean);

   procedure Find_All_References
     (Self       : Context;
      Definition : Libadalang.Analysis.Defining_Name;
      Callback   : not null access procedure
        (Base_Id : Libadalang.Analysis.Base_Id;
         Kind    : Libadalang.Common.Ref_Result_Kind;
         Cancel  : in out Boolean));
   --  Finds all references to a given defining name in all units of the
   --  context.

   function Find_All_Overrides
     (Self              : Context;
      Decl              : Libadalang.Analysis.Basic_Decl;
      Imprecise_Results : out Boolean)
      return Libadalang.Analysis.Basic_Decl_Array;
   --  Finds all overriding subprograms of the given basic declaration.
   --  This is used to propose all the implementations of a given subprogram
   --  when textDocument/definition requests happen on dispatching calls.
   --  Imprecise_Results is set to True if we don't know whether the results
   --  are precise.
   --  Returns an empty array if Decl is null.

   function Find_All_Base_Declarations
     (Self              : Context;
      Decl              : Libadalang.Analysis.Basic_Decl;
      Imprecise_Results : out Boolean)
      return Libadalang.Analysis.Basic_Decl_Array;
   --  Given a subprogram declaration in Decl, find all the base subprograms
   --  that it inherits, not including self.
   --  Imprecise_Results is set to True if we don't know whether the results
   --  are precise.
   --  Returns an empty array if Decl is null.

   procedure Find_All_Calls
     (Self       : Context;
      Definition : Libadalang.Analysis.Defining_Name;
      Callback   : not null access procedure
        (Base_Id : Libadalang.Analysis.Base_Id;
         Kind    : Libadalang.Common.Ref_Result_Kind;
         Cancel  : in out Boolean));
   --  Return all the enclosing entities that call Definition in all sources
   --  known to this project.

   function Find_All_Env_Elements
     (Self     : Context;
      Name     : Libadalang.Analysis.Name;
      Seq      : Boolean := True;
      Seq_From : Libadalang.Analysis.Ada_Node'Class :=
        Libadalang.Analysis.No_Ada_Node)
      return Libadalang.Analysis.Ada_Node_Array;
   --  Return all elements lexically named like Name.
   --  If Seq is True and Seq_From is not empty, reduce the scope to the
   --  node above Seq_From.

   procedure Get_References_For_Renaming
     (Self              : Context;
      Definition        : Libadalang.Analysis.Defining_Name;
      Imprecise_Results : out Boolean;
      Callback          : not null access procedure
        (Base_Id : Libadalang.Analysis.Base_Id;
         Kind    : Libadalang.Common.Ref_Result_Kind;
         Cancel  : in out Boolean));
   --  Get all the references to a given defining name in all units for
   --  renaming purposes: for instance, when called on a tagged type primitive
   --  definition, references to the base subprograms it inherits and to the
   --  overriding ones are also returned.

   function Is_Part_Of_Project
     (Self : Context;
      File : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Check if given file belongs to the project loaded in the Context

   function List_Files (Self : Context)
     return File_Sets.Set_Iterator_Interfaces.Reversible_Iterator'Class;
   --  Return the list of files known to this context.

   function File_Count (Self : Context) return Natural;
   --  Return number of files known to this context.

   function Analysis_Units
     (Self : Context) return Libadalang.Analysis.Analysis_Unit_Array;
   --  Return the analysis units for all Ada sources known to this context

   function List_Source_Directories
     (Self : Context) return LSP.Ada_File_Sets.File_Sets.Set;
   --  List the source directories in non-externally-built projects

   procedure Index_File
     (Self    : in out Context;
      File    : GNATCOLL.VFS.Virtual_File;
      Reparse : Boolean := True;
      PLE     : Boolean := True);
   --  Index the given file. This translates to refreshing the Libadalang
   --  Analysis_Unit associated to it.
   --  If PLE is True, Populate_Lexcial_Env is called at the end, which will
   --  increase the speef of semantic requests.

   procedure Index_Document
     (Self     : in out Context;
      Document : in out LSP.Ada_Documents.Document);
   --  Index/reindex the given document in this context

   procedure Flush_Document
     (Self : in out Context;
      File : GNATCOLL.VFS.Virtual_File);
   --  Revert a document to the state of the file discarding any changes

   procedure Append_Declarations
     (Self                    : Context;
      Document                : LSP.Ada_Documents.Document_Access;
      Position                : LSP.Messages.TextDocumentPositionParams;
      Display_Method_Ancestry_Policy :
         LSP.Messages.AlsDisplayMethodAncestryOnNavigationPolicy;
      Result                  : in out LSP.Messages.Location_Or_Link_Vector;
      Imprecise               : in out Boolean);
   --  Find corresponding declarations for a name at given Position and append
   --  their locations to Result.
   --  Document is the document from which the request originates; it can
   --  be null if no document is open for this location.
   --
   --  Here we follow C terminology, where 'Declaration' equals to
   --  Ada subprogram specification while 'Definition' equals to
   --  Ada subprogram body (completion).

   function LAL_Context
     (Self : Context) return Libadalang.Analysis.Analysis_Context;
   --  Return the LAL context corresponding to Self

   procedure Get_Any_Symbol_Completion
     (Self   : Context;
      Prefix : VSS.Strings.Virtual_String;
      Callback : not null access procedure
        (File : GNATCOLL.VFS.Virtual_File;
         Name : Libadalang.Analysis.Defining_Name;
         Stop : in out Boolean));
   --  Find symbols starting with given Prefix in all files of the context and
   --  call Callback for each. Name could contain a stale reference if the File
   --  was updated since last indexing operation.

   function Charset (Self : Context) return String;
   --  Return the charset for this context

private

   type Context (Trace : GNATCOLL.Traces.Trace_Handle) is tagged limited record
      Id             : VSS.Strings.Virtual_String;
      Unit_Provider  : Libadalang.Analysis.Unit_Provider_Reference;
      LAL_Context    : Libadalang.Analysis.Analysis_Context;
      Charset        : Ada.Strings.Unbounded.Unbounded_String;

      Is_Fallback_Context : Boolean := False;
      --  Indicate that this is a "fallback" context, ie the context
      --  holding any file, in the case no valid project was loaded.

      Source_Files   : LSP.Ada_File_Sets.Indexed_File_Set;
      --  Cache for the list of Ada source files in the loaded project tree.

      Source_Dirs    : LSP.Ada_File_Sets.File_Sets.Set;
      --  All the source dirs in the loaded project, not including
      --  the externally built projects

      PP_Options : Utils.Command_Lines.Command_Line
                    (Pp.Command_Lines.Descriptor'Access);
      --  Object to keep gnatpp options

      Follow_Symlinks : Boolean := True;
      --  See LSP.Ada_Handlers for description

      Reader_Reference : Langkit_Support.File_Readers.File_Reader_Reference;
      --  A reference to the file reader created for this context
   end record;

   function LAL_Context
     (Self : Context) return Libadalang.Analysis.Analysis_Context is
     (Self.LAL_Context);

   function List_Files (Self : Context)
     return File_Sets.Set_Iterator_Interfaces.Reversible_Iterator'Class
       is (Self.Source_Files.Iterate);

   function File_Count (Self : Context) return Natural
       is (Self.Source_Files.Length);

end LSP.Ada_Contexts;

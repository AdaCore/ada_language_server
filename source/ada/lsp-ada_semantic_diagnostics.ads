------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2026, AdaCore                     --
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

--  This package provides background jobs for computing Libadalang semantic
--  diagnostics on open documents.

with LSP.Ada_Documents;
with LSP.Ada_Handlers;
with LSP.Server_Jobs;
with LSP.Structures;
with LSP.Text_Documents;
limited with LSP.Servers;
private with Ada.Unchecked_Deallocation;
private with GNATCOLL.VFS;
private with Libadalang.Iterators;
private with LSP.Client_Message_Receivers;
private with LSP.Server_Messages;

package LSP.Ada_Semantic_Diagnostics is

   procedure Schedule_Semantic_Diagnostics_For_Change
     (Server   : not null access LSP.Servers.Server'Class;
      Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Document : not null LSP.Ada_Documents.Document_Access;
      Ranges   : LSP.Structures.Range_Vector := []);
   --  Queue a background check for semantic errors in Document.
   --  If Ranges is empty, check the whole file and replace all saved errors.
   --  Otherwise, check a subtree containing the ranges and keep saved errors
   --  outside that subtree. For example, checking one procedure keeps errors
   --  in other procedures visible until a full check updates them.

   type Semantic_Diagnostics_Job (<>) is
     new LSP.Server_Jobs.Server_Job with private;
   type Semantic_Diagnostics_Job_Access is
     access all Semantic_Diagnostics_Job'Class;

private

   type Traverse_Iterator_Access is
     access Libadalang.Iterators.Traverse_Iterator'Class;

   procedure Free is new Ada.Unchecked_Deallocation
     (Libadalang.Iterators.Traverse_Iterator'Class, Traverse_Iterator_Access);

   type Semantic_Diagnostics_Job
     (Handler : not null access LSP.Ada_Handlers.Message_Handler'Class)
   is new LSP.Server_Jobs.Server_Job with record
      Project_Stamp : LSP.Ada_Handlers.Project_Stamp;
      --  Stop the job if the project has been reloaded since it was queued.

      Document_URI : LSP.Structures.DocumentUri;
      --  Used to look up the document again on each Execute call.

      Document_Signature : LSP.Text_Documents.Document_Signature;
      --  The document's URI, version and opening time when the job was queued.

      File_Name : GNATCOLL.VFS.Virtual_File;
      --  The document's file name, kept for trace messages.

      Ranges : LSP.Structures.Range_Vector;
      --  The edited ranges. Empty means check the whole file.

      Cursor : Traverse_Iterator_Access := null;
      --  Keeps our place in the tree between Execute calls.

      Errors : LSP.Structures.Diagnostic_Vector;
      --  Errors found so far, kept between Execute calls.

      Traversal_Range : LSP.Structures.A_Range_Optional;
      --  The range of the subtree checked by a partial job. Replace saved
      --  errors in this range and keep the others. If unset, replace all
      --  saved errors after checking the whole file.
   end record;

   overriding function Priority
     (Self : Semantic_Diagnostics_Job) return LSP.Server_Jobs.Job_Priority is
       (LSP.Server_Jobs.Lowest);

   overriding procedure Execute
     (Self   : in out Semantic_Diagnostics_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   overriding function Message (Self : Semantic_Diagnostics_Job)
     return LSP.Server_Messages.Server_Message_Access is (null);

end LSP.Ada_Semantic_Diagnostics;

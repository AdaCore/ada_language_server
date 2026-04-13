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
limited with LSP.Servers;
private with Ada.Unchecked_Deallocation;
private with Libadalang.Iterators;
private with LSP.Client_Message_Receivers;
private with LSP.Server_Messages;

package LSP.Ada_Semantic_Diagnostics is

   procedure Schedule_Semantic_Diagnostics_For_Change
     (Server   : not null access LSP.Servers.Server'Class;
      Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Document : not null LSP.Ada_Documents.Document_Access;
      Ranges   : LSP.Structures.Range_Vector);
   --  Create and enqueue a semantic diagnostics job for Document.
   --  When Ranges is empty the entire document is (re)computed and the cache
   --  is replaced. Otherwise only nodes overlapping Ranges are visited and
   --  results are merged into the existing cache.

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
      --  Used to detect if the project was reloaded during execution,
      --  in which case results are discarded and the job is terminated.

      Document      : LSP.Ada_Documents.Document_Access;
      --  The document to analyze.

<<<<<<< Updated upstream
=======
      Document_Version : LSP.Structures.Integer_Or_Null;
      --  The document version at the time the job was enqueued.

>>>>>>> Stashed changes
      Ranges        : LSP.Structures.Range_Vector;
      --  The set of changed ranges to process.

      Cursor        : Traverse_Iterator_Access := null;
      --  Iterator tracking the current node being processed.

      Errors        : LSP.Structures.Diagnostic_Vector;
      --  Diagnostics accumulated across Execute calls.
   end record;

   overriding function Priority
     (Self : Semantic_Diagnostics_Job) return LSP.Server_Jobs.Job_Priority is
       (LSP.Server_Jobs.Low);

   overriding procedure Execute
     (Self   : in out Semantic_Diagnostics_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status);

   overriding function Message (Self : Semantic_Diagnostics_Job)
     return LSP.Server_Messages.Server_Message_Access is (null);

end LSP.Ada_Semantic_Diagnostics;

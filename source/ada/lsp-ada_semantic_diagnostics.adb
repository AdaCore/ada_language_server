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

with Langkit_Support.Slocs;
with Langkit_Support.Text;
with Langkit_Support.Errors;

with Libadalang.Analysis;
with Libadalang.Semantic_Diagnostics;

with VSS.Strings.Conversions;

with GNATCOLL.VFS;

with LSP.Ada_Context_Sets;
with LSP.Ada_Documents.Semantic_Diagnostics;
with LSP.Servers;

package body LSP.Ada_Semantic_Diagnostics is

   Max_Nodes_Per_Batch : constant := 300;
   --  Number of AST nodes processed per Execute call before yielding.
   --  Matches the batch size used by the folding-range and tokens-full jobs.

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute
     (Self   : in out Semantic_Diagnostics_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is
      pragma Unreferenced (Client);
      use Langkit_Support.Slocs;
      use Libadalang.Analysis;
      use Libadalang.Iterators;
      use LSP.Structures;
      use type LSP.Ada_Handlers.Project_Stamp;

      procedure Process_Node (Node : Ada_Node);
      --  Process a single node during traversal: if it is an xref entry point and
      --  overlaps changed ranges (when applicable), extract diagnostics and add
      --  them to Self.Errors.

      ------------------
      -- Process_Node --
      ------------------

      procedure Process_Node (Node : Ada_Node) is
      begin
         --  Skip nodes that are not xref entry points: only those
         --  can have semantic diagnostics attached.
         if not Node.P_Xref_Entry_Point then
            return;
         end if;

         --  Process this xref entry point.
         declare
            Raw_Diags : constant Solver_Diagnostic_Array :=
              Node.P_Nameres_Diagnostics;
         begin
            if Raw_Diags'Length > 0 then
               declare
                  Contextual :
                    constant Libadalang
                               .Semantic_Diagnostics
                               .Contextual_Diagnostic :=
                      Libadalang
                        .Semantic_Diagnostics
                        .Build_Contextual_Diagnostics
                           (Node        => Node,
                            Sem_Diags   => Raw_Diags,
                            Aggregate   =>
                              Libadalang
                                .Semantic_Diagnostics
                                .Basic_Aggregator'Access,
                            Render_Node =>
                              Libadalang
                                .Semantic_Diagnostics
                                .Basic_Node_Renderer'Access);
                  Error_Loc  : constant Ada_Node := Contextual.Error.Location;
                  Diag_Range : constant LSP.Structures.A_Range :=
                    (if Error_Loc.Is_Null
                     then Self.Document.To_A_Range (Node.Sloc_Range)
                     else Self.Document.To_A_Range (Error_Loc.Sloc_Range));
                  Item       : LSP.Structures.Diagnostic;
               begin
                  Item.source := "libadalang";
                  Item.a_range := Diag_Range;
                  Item.message :=
                    VSS.Strings.Conversions.To_Virtual_String
                      (Langkit_Support.Text.To_UTF8
                         (Langkit_Support.Text.To_Text
                            (Contextual.Error.Message)));
                  Self.Errors.Append (Item);
               end;
            end if;
         end;
      end Process_Node;

   begin
      if Self.Handler.Get_Project_Stamp /= Self.Project_Stamp
        or else Self.Handler.Is_Shutdown
      then
         --  Project was reloaded or the server is shutting down.
         Free (Self.Cursor);
         Status := LSP.Server_Jobs.Done;
         return;
      end if;

      if Self.Ranges.Is_Empty
        and then Self.Document.Identifier.version /= Self.Document_Version
      then
         --  This is a full-document traversal job, but the document has been
         --  edited since it was enqueued.  A newer full-document job will
         --  already be (or will soon be) in the queue, so discard this one to
         --  avoid wasting CPU on a result that will be immediately superseded.
         --  Per-range jobs are not cancelled here: each covers a distinct
         --  changed region and should always run, even if other edits arrived
         --  at different locations in the meantime.
         Free (Self.Cursor);
         Status := LSP.Server_Jobs.Done;
         return;
      end if;

      --  First Execute call: create the iterator.
      if Self.Cursor = null then
         declare
            Context : constant LSP.Ada_Context_Sets.Context_Access :=
              Self.Handler.Get_Best_Context (Self.Document.URI);
            File    : constant GNATCOLL.VFS.Virtual_File :=
              Self.Handler.To_File (Self.Document.URI);
            Unit    : constant Analysis_Unit := Context.Get_AU (File);
            Start   : Ada_Node := Unit.Root;
         begin
            --  When processing only a specific set of ranges, start the
            --  traversal from the xref entry-point ancestor of the first
            --  changed position rather than from Unit.Root. This avoids
            --  visiting nodes that are entirely before the first change.
            if not Self.Ranges.Is_Empty then
               declare
                  First_Pos : constant LSP.Structures.Position :=
                    Self.Ranges.First_Element.start;
                  Sloc      : constant Source_Location :=
                    (Line => Line_Number (First_Pos.line + 1), Column => 1);
                  Node      : Ada_Node := Unit.Root.Lookup (Sloc);
               begin
                  --  Walk up to the nearest xref entry-point ancestor.
                  while not Node.Is_Null and then not Node.P_Xref_Entry_Point
                  loop
                     Node := Node.Parent;
                  end loop;

                  --  Start from the parent so that sibling entry points
                  --  after this one are also visited.
                  if not Node.Is_Null and then not Node.Parent.Is_Null then
                     Start := Node.Parent;
                  end if;
               end;

            end if;

            Self.Cursor := new Traverse_Iterator'Class'(Traverse (Start));
         end;
      end if;

      Status := LSP.Server_Jobs.Continue;

      for J in 1 .. Max_Nodes_Per_Batch loop
         declare
            Node : Ada_Node;
         begin
            if not Self.Cursor.Next (Node) then
               --  Iterator exhausted: deposit accumulated results and publish.
               --  Pass Self.Traversal_Range as the eviction boundary:
               --  - not set (full-document job) → the cache is fully replaced;
               --  - set (per-range job) → only diagnostics inside the
               --    traversal root's extent are evicted and replaced, which
               --    correctly covers nodes larger than the original didChange
               --    Ranges.
               declare
                  Semantic_Diags_Source :
                    constant LSP
                               .Ada_Documents
                               .Semantic_Diagnostics
                               .Semantic_Diagnostic_Source_Access :=
                      LSP
                        .Ada_Documents
                        .Semantic_Diagnostics
                        .Semantic_Diagnostic_Source_Access
                           (Self.Document.Semantic_Diagnostic_Source);
               begin
                  Semantic_Diags_Source.Update_Diagnostics
                    (Errors => Self.Errors,
                     Eviction_Range => Self.Traversal_Range);
                  Self.Handler.Publish_Diagnostics (Self.Document);
               end;

               Free (Self.Cursor);
               Status := LSP.Server_Jobs.Done;
               return;
            end if;

            Process_Node (Node => Node);
         exception
            when
              Langkit_Support.Errors.Property_Error
              | Langkit_Support.Errors.Stale_Reference_Error
            =>
               --  A Libadalang property (P_Xref_Entry_Point, P_Nameres_
               --  Diagnostics, ...) was called on a node whose unit was
               --  reparsed mid-batch (a didChange arrived while this job
               --  was running).  Skip the node and continue in this case.
               null;
         end;
      end loop;
   exception
      when E : others =>
         Free (Self.Cursor);
         Self.Handler.Trace_Exception (E);
         Status := LSP.Server_Jobs.Done;
   end Execute;

   -------------------------------------------------
   -- Schedule_Semantic_Diagnostics_For_Change --
   -------------------------------------------------

   procedure Schedule_Semantic_Diagnostics_For_Change
     (Server   : not null access LSP.Servers.Server'Class;
      Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Document : not null LSP.Ada_Documents.Document_Access;
      Ranges   : LSP.Structures.Range_Vector := []) is
   begin
      if not Handler.Semantic_Diagnostics_Enabled or else Handler.Is_Shutdown
      then
         return;
      end if;

      declare
         Job        : constant Semantic_Diagnostics_Job_Access :=
           new Semantic_Diagnostics_Job (Handler => Handler);
         Job_Access : LSP.Server_Jobs.Server_Job_Access;
      begin
         Job.Project_Stamp    := Handler.Get_Project_Stamp;
         Job.Document         := Document;
         Job.Document_Version := Document.Identifier.version;
         Job.Ranges           := Ranges;
         Job_Access           := LSP.Server_Jobs.Server_Job_Access (Job);
         Server.Enqueue (Job_Access);
      end;
   end Schedule_Semantic_Diagnostics_For_Change;

end LSP.Ada_Semantic_Diagnostics;

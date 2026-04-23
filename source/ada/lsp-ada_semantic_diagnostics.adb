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

with GNATCOLL.Traces; use GNATCOLL.Traces;
with GNATCOLL.VFS;

with LSP.Ada_Context_Sets;
with LSP.Ada_Documents.Semantic_Diagnostics;
with LSP.Servers;

package body LSP.Ada_Semantic_Diagnostics is

   Me_Debug : constant Trace_Handle :=
     Create ("ALS.DIAGNOSTICS.SEMANTIC", Off);

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
      use type LSP.Ada_Documents.Document_Access;

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

         --  For per-range jobs, skip entry points whose sloc range does not
         --  overlap any of the changed ranges.  The traversal still visits all
         --  nodes in the containing entry-point ancestor (which may be the
         --  whole package body), but this guard avoids calling the expensive
         --  P_Nameres_Diagnostics on unrelated entry points.
         if not Self.Ranges.Is_Empty then
            declare
               Node_Start : constant Source_Location :=
                 Start_Sloc (Node.Sloc_Range);
               Node_End   : constant Source_Location :=
                 End_Sloc (Node.Sloc_Range);
               Overlaps   : Boolean := False;
            begin
               for R of Self.Ranges loop
                  declare
                     R_Start : constant Source_Location :=
                       (Line   => Line_Number (R.start.line + 1),
                        Column => Column_Number (R.start.character + 1));
                     R_End   : constant Source_Location :=
                       (Line   => Line_Number (R.an_end.line + 1),
                        Column => Column_Number (R.an_end.character + 1));
                  begin
                     if Node_Start <= R_End and then Node_End >= R_Start then
                        Overlaps := True;
                        exit;
                     end if;
                  end;
               end loop;

               if not Overlaps then
                  return;
               end if;
            end;
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
        or else Self.Document = null
      then
          --  Project was reloaded, server is shutting down, or document was
          --  closed while this job was in the queue: discard it.
         Me_Debug.Trace
           ("Cancelling semantic diagnostics job for "
            & Self.Handler.To_File (Self.Document.URI).Display_Base_Name
            & " because the project was reloaded, server is shutting down, or document was closed");
         Free (Self.Cursor);
         Status := LSP.Server_Jobs.Done;
         return;
      end if;

      if Self.Document.Identifier.version /= Self.Document_Version then
         --  The document has been edited since this job was enqueued.  A
         --  newer job will already be (or will soon be) in the queue, so
         --  discard this one to avoid wasting CPU on a result that will be
         --  immediately superseded.  This applies to both full-document and
         --  per-range jobs: the newer didChange will schedule fresh jobs
         --  covering the up-to-date content.

         Me_Debug.Trace
           ("Cancelling semantic diagnostics job for "
            & Self.Handler.To_File (Self.Document.URI).Display_Base_Name
            & " because the document was edited since it was enqueued");
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

               Me_Debug.Trace ("Executing a per-range semantic diagnostics job for "
                               & Self.Handler.To_File (Self.Document.URI).Display_Base_Name
                  & " (version "
                  & Self.Document_Version.Value'Image
                  & ")");
               Me_Debug.Trace
                 ("Changed ranges: "
                  & Self.Ranges'Image);

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

                  --  Start directly from the xref entry-point that contains
                  --  the change.  The full-document job covers every other
                  --  entry point in the file, so there is no need to start
                  --  from Node.Parent (which could be a CompilationUnit and
                  --  would cause the traversal to span the whole file).
                  if not Node.Is_Null then
                     Start := Node;
                  end if;
               end;

               Me_Debug.Trace
                 ("Starting semantic diagnostics traversal from: "
                  & Start.Image);
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
                  Me_Debug.Trace
                    ("Semantic diagnostics traversal completed for "
                     & Self.Handler.To_File (Self.Document.URI)
                         .Display_Base_Name
                     & " (version "
                     & Self.Document_Version.Value'Image
                     & ")");

                  Semantic_Diags_Source.Update_Diagnostics
                    (Errors => Self.Errors);
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

      Me_Debug.Trace
        ("Yielding semantic diagnostics job after processing "
         & Max_Nodes_Per_Batch'Image
         & " nodes"
         & " (version "
         & Self.Document_Version.Value'Image
         & ")");

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

      Me_Debug.Trace
        ("Scheduling "
         & (if Ranges.Is_Empty then "full-document" else "per-range")
         & "semantic diagnostics job for "
         & Handler.To_File (Document.URI).Display_Base_Name
         & " (version "
         & Document.Identifier.version.Value'Image
         & ")");

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

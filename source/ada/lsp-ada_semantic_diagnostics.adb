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

with LSP.Ada_Context_Sets;
with LSP.Ada_Documents.Semantic_Diagnostics;
with LSP.Servers;
with LSP.Utils;

package body LSP.Ada_Semantic_Diagnostics is

   Me_Debug : constant Trace_Handle :=
     Create ("ALS.DIAGNOSTICS.SEMANTIC", Off);

   Max_Nodes_Per_Batch : constant := 300;
   --  Check at most this many tree nodes before letting other jobs run.
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

      Document : constant LSP.Ada_Documents.Document_Access :=
        Self.Handler.Get_Open_Document (Self.Document_URI);
      --  Look up the document on each call. It may have been closed while
      --  this job was waiting, so an older pointer could no longer be valid.

      procedure Process_Node (Node : Ada_Node);
      --  Collect errors in Self.Errors if Libadalang can check this node
      --  on its own (an xref entry point, such as a declaration).

      ------------------
      -- Process_Node --
      ------------------

      procedure Process_Node (Node : Ada_Node) is
      begin
         --  Other nodes cannot be checked for semantic errors on their own.
         if not Node.P_Xref_Entry_Point then
            return;
         end if;

         --  Check all entry points in the selected subtree, because the
         --  cache update will replace all errors in that subtree. For
         --  example, renaming a declaration in a package must not hide a
         --  type error elsewhere in the same package.
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
                     then Document.To_A_Range (Node.Sloc_Range)
                     else Document.To_A_Range (Error_Loc.Sloc_Range));
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
        or else not Self.Handler.Semantic_Diagnostics_Enabled
        or else Self.Handler.Is_Shutdown
        or else Document = null
        or else not Document.Is_Same (Self.Document_Signature)
      then
         --  Stop if semantic checks are disabled or the server is stopping.
         --  Also stop if the project or document changed, or the document
         --  was closed or reopened, since this job was queued.
         Me_Debug.Trace
           ("Cancelling semantic diagnostics job for "
            & Self.File_Name.Display_Base_Name
            & " because the project was reloaded, server is shutting down,"
            & " or document changed");
         Free (Self.Cursor);
         Status := LSP.Server_Jobs.Done;
         return;
      end if;

      --  On the first call, choose which part of the tree to check.
      if Self.Cursor = null then
         declare
            Context : constant LSP.Ada_Context_Sets.Context_Access :=
              Self.Handler.Get_Best_Context (Self.Document_URI);
            File    : constant GNATCOLL.VFS.Virtual_File :=
              Self.Handler.To_File (Self.Document_URI);
            Unit    : constant Analysis_Unit := Context.Get_AU (File);
            Start   : Ada_Node := Unit.Root;
         begin
            --  For a partial check, start at a node containing the first
            --  edit. This avoids walking the whole file just to check the
            --  changed declaration.
            if not Self.Ranges.Is_Empty then

               Me_Debug.Trace
                 ("Executing a per-range semantic diagnostics job for "
                  & Self.File_Name.Display_Base_Name
                  & " (version "
                  & Self.Document_Signature.Version'Image
                  & ")");
               Me_Debug.Trace ("Changed ranges: " & Self.Ranges'Image);

               declare
                  First_Pos : constant LSP.Structures.Position :=
                    Self.Ranges.First_Element.start;
                  Sloc      : constant Source_Location :=
                    Document.To_Source_Location (First_Pos);
                  Node      : Ada_Node := Unit.Root.Lookup (Sloc);
               begin
                  --  Move up the tree until Libadalang can check this node
                  --  on its own.
                  while not Node.Is_Null and then not Node.P_Xref_Entry_Point
                  loop
                     Node := Node.Parent;
                  end loop;

                  --  Start at this node, not its parent, which could cover
                  --  the whole file. A later full check handles the rest of
                  --  the file.
                  if not Node.Is_Null then
                     Start := Node;
                  end if;

                  Self.Traversal_Range :=
                    (Is_Set => True,
                     Value  => Document.To_A_Range (Start.Sloc_Range));

                  --  Check the whole file if an edit extends outside this
                  --  node. Compare LSP positions without looking up lines in
                  --  the text. For example, an edit can end on line 10 even
                  --  though deleting text makes the file end on line 8.
                  for R of Self.Ranges loop
                     if not LSP.Utils.In_Range
                       (R.start, Self.Traversal_Range.Value)
                       or else
                         not LSP.Utils.In_Range
                           (R.an_end, Self.Traversal_Range.Value)
                     then
                        Start := Unit.Root;
                        Self.Traversal_Range.Value :=
                          Document.To_A_Range (Start.Sloc_Range);
                        exit;
                     end if;
                  end loop;
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
               --  All selected nodes have been checked. Save the results
               --  and publish them. For a full check, replace all saved
               --  errors. For a partial check, replace errors in the whole
               --  subtree we checked and keep errors elsewhere in the file.
               --  Use the checked subtree's range, not just the edited text.
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
                           (Document.Semantic_Diagnostic_Source);
               begin
                  Me_Debug.Trace
                    ("Semantic diagnostics traversal completed for "
                     & Self.File_Name.Display_Base_Name
                     & " (version "
                     & Self.Document_Signature.Version'Image
                     & ")");

                  Semantic_Diags_Source.Update_Diagnostics
                    (New_Diagnostics  => Self.Errors,
                     Range_To_Replace => Self.Traversal_Range);
                  Self.Handler.Publish_Diagnostics (Document);
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
               --  Libadalang could not check this node, or the node became
               --  invalid after reparsing. Skip it and continue.
               null;
         end;
      end loop;

      Me_Debug.Trace
        ("Yielding semantic diagnostics job after processing "
         & Max_Nodes_Per_Batch'Image
         & " nodes"
         & " (version "
         & Self.Document_Signature.Version'Image
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
         & Document.Signature.Version'Image
         & ")");

      declare
         Job        : constant Semantic_Diagnostics_Job_Access :=
           new Semantic_Diagnostics_Job (Handler => Handler);
         Job_Access : LSP.Server_Jobs.Server_Job_Access;
      begin
         --  Remember this document version. Each Execute call checks that
         --  the text has not changed and the document has not been reopened.

         Job.Project_Stamp := Handler.Get_Project_Stamp;
         Job.Document_URI := Document.URI;
         Job.Document_Signature := Document.Signature;
         Job.File_Name := Handler.To_File (Document.URI);
         Job.Ranges := Ranges;
         Job_Access := LSP.Server_Jobs.Server_Job_Access (Job);
         Server.Enqueue (Job_Access);
      end;
   end Schedule_Semantic_Diagnostics_For_Change;

end LSP.Ada_Semantic_Diagnostics;

"""Partial publications retain errors at their current document positions."""

import os
from pathlib import Path
import queue
import re
import time

from drivers.lsp_python_driver import LSP, complex_test
from drivers.lsp_types import LSPMessage
from drivers.lsp_ada_requests import initialize, initialized


def utf16_length(text):
    # LSP counts UTF-16 units. Python counts an emoji as one character,
    # but an LSP position counts it as two.
    return len(text.encode("utf-16-le")) // 2


@complex_test
def check_cache(working_dir):
    client = LSP(os.environ["ALS"], working_dir)
    uri = (Path(working_dir) / "pkg.adb").as_uri()
    text = (Path(working_dir) / "pkg.adb").read_text()

    def notify(method, params):
        client.send(
            LSPMessage({"method": method, "params": params}, use_id=False),
            expect_response=False,
        )

    def diagnostics(timeout=None):
        delay = 10 if timeout is None else timeout
        deadline = time.monotonic() + delay * float(
            os.environ.get("ALS_WAIT_FACTOR", 1)
        )
        while time.monotonic() < deadline:
            try:
                message = client.queue.get(
                    timeout=max(0, deadline - time.monotonic())
                ).from_dict
            except queue.Empty:
                break
            if (
                message.get("method") == "textDocument/publishDiagnostics"
                and message["params"]["uri"] == uri
            ):
                return message["params"]["diagnostics"]
        if timeout is None:
            raise AssertionError("Timed out waiting for semantic diagnostics")
        return None

    def check_positions(result):
        expected = []
        for line_number, line in enumerate(text.splitlines()):
            # Every False is assigned to an Integer, so each must have one
            # type error. Derive positions from the edited text below.
            for match in re.finditer("False", line):
                column = utf16_length(line[: match.start()])
                expected.append((line_number, column, line_number, column + 5))
        actual = sorted(
            (
                d["range"]["start"]["line"],
                d["range"]["start"]["character"],
                d["range"]["end"]["line"],
                d["range"]["end"]["character"],
            )
            for d in result
        )
        assert actual == expected, ("Wrong diagnostic positions", actual, expected)

    def drain_publications():
        # Configuration and indexing can cause extra publications. Check
        # those too, before sending an edit with different source positions.
        while (result := diagnostics(timeout=0.25)) is not None:
            check_positions(result)

    version = 1

    def send_changes(changes):
        nonlocal version
        version += 1
        notify(
            "textDocument/didChange",
            {
                "textDocument": {"uri": uri, "version": version},
                "contentChanges": changes,
            },
        )
        # A single ranged edit publishes a local result, then a full result.
        # Check both: the full result could hide a broken cache in the first.
        # Multiple edits and full replacements get one full check.
        partial = len(changes) == 1 and "range" in changes[0]
        for _ in range(2 if partial else 1):
            check_positions(diagnostics())
        drain_publications()

    def change(edits):
        nonlocal text
        changes = []
        for start, end, replacement in edits:
            lines = text.splitlines(keepends=True)

            def offset(lines, position):
                line, column = position
                # Convert the LSP column to a Python string offset. Keep the
                # line endings when counting characters before this line.
                prefix = lines[line].encode("utf-16-le")[: column * 2]
                return sum(map(len, lines[:line])) + len(prefix.decode("utf-16-le"))

            first, last = offset(lines, start), offset(lines, end)
            # Each edit refers to the text left by the previous edit, even
            # when they are sent together in one notification.
            text = text[:first] + replacement + text[last:]
            changes.append(
                {
                    "range": {
                        "start": {"line": start[0], "character": start[1]},
                        "end": {"line": end[0], "character": end[1]},
                    },
                    "text": replacement,
                }
            )
        send_changes(changes)

    try:
        client.send(initialize())
        client.send(initialized(), expect_response=False)
        notify(
            "workspace/didChangeConfiguration",
            {
                "settings": {
                    "ada": {
                        "projectFile": str(Path(working_dir) / "default.gpr"),
                        "semanticDiagnostics": True,
                        "adaFileDiagnostics": False,
                        "sourceInfoDiagnostics": False,
                    }
                }
            },
        )
        notify(
            "textDocument/didOpen",
            {
                "textDocument": {
                    "uri": uri,
                    "languageId": "ada",
                    "version": 1,
                    "text": text,
                }
            },
        )
        # Opening the file may publish an empty result before the semantic
        # check finishes. Later edits must keep the errors visible throughout.
        while not (initial := diagnostics()):
            pass
        check_positions(initial)
        drain_publications()

        # Rename a declaration inside One. Both One's type error and the
        # unrelated error in Two must remain visible in the local result,
        # before the full-document follow-up publishes its result.
        change([((2, 6), (2, 7), "Z")])

        # After deleting trailing lines, the edit's old end position is
        # beyond the new end of the file. The local check must still finish.
        change([((12, 8), (12, 8), "\n\n")])
        change([((12, 8), (14, 0), "")])

        # Inserting lines in One moves the cached error in Two. Its old
        # position now falls inside One's replacement boundary.
        change([((3, 0), (3, 0), "\n" * 6)])
        change([((3, 0), (9, 0), "")])
        change([((3, 0), (3, 0), "\r\n\r\n")])
        change([((3, 0), (5, 0), "")])

        # Fix Two's error. The old cached error must disappear.
        change([((8, 21), (8, 26), "0")])

        # The first edit adds an error in Two. The next edit inserts lines
        # before it, moving Two beyond the original edit range. Checking
        # that old range against the final tree would select only One.
        change(
            [
                ((8, 21), (8, 22), "False"),
                ((3, 0), (3, 0), "\n" * 6),
            ]
        )
        change([((14, 6), (14, 7), "W"), ((3, 0), (9, 0), "")])

        # Put both procedures on one line. Removing the declaration in One
        # must move Two's error too; the emoji counts as two UTF-16 units.
        text = " ".join(text.splitlines()) + "\n"
        column = text.index("Z :")
        inserted = 'S : Wide_Wide_String := "😀"; '
        # Load the emoji with a full replacement: VSS currently raises an
        # exception when inserting it with a ranged edit.
        text = text[:column] + inserted + text[column:]
        send_changes([{"text": text}])
        width = utf16_length(inserted)
        change([((0, column), (0, column + width), "")])
        # An accented character takes two UTF-8 bytes but one UTF-16 unit.
        inserted = 'S : String := "é"; '
        change([((0, column), (0, column), inserted)])
        change([((0, column), (0, column + len(inserted)), "")])
    finally:
        client.shutdown()
    # A nonempty list makes the Python driver fail this test.
    return client.errors

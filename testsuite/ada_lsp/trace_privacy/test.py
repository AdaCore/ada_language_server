"""Check message summaries and opt-in debugging in both server modes."""

import os
from pathlib import Path

from drivers.lsp_python_driver import LSP, complex_test
from drivers.lsp_types import LSPMessage


@complex_test
def trace_privacy(wd):
    uri = "file:///private_trace_directory/private_trace_file.adb"
    source = "procedure Private_Trace_Source is begin null; end;"

    for language in ("ada", "gpr"):
        for debug in (False, True):
            run_dir = Path(wd) / f"{language}_{debug}"
            run_dir.mkdir()
            (run_dir / "traces.cfg").write_text(
                f">{run_dir / 'trace.log'}:buffer_size=0\n"
                f"ALS.MAIN=yes >{run_dir / 'main.log'}:buffer_size=0\n"
                "ALS.IN=no\n"
                "ALS.OUT=no\n" + ("ALS.MAIN.DEBUG=yes\n" if debug else "")
            )
            command = [os.environ["ALS"], "--tracefile=traces.cfg"]
            if language == "gpr":
                command.append("--language-gpr")
            lsp = LSP(command, str(run_dir), env={"ALS_HOME": str(run_dir)})
            try:
                initialize = LSPMessage(
                    {
                        "method": "initialize",
                        "params": {"capabilities": {}, "rootUri": uri},
                    }
                )
                initialize.id = "private-initialize-id"
                response = lsp.send(initialize)
                assert response.id == initialize.id
                assert "capabilities" in response.from_dict

                # An unknown request exercises the raw-message debug trace
                # without requiring any project or source files on disk.
                request = LSPMessage(
                    {
                        "method": "test/tracePrivacy",
                        "params": {"uri": uri, "text": source},
                    }
                )
                request.id = "private-request-id"
                response = lsp.send(request)
                assert response.id == request.id
                response.assertField("error.code", -32601)
            finally:
                lsp.shutdown()
                lsp.process.wait()

            assert not lsp.errors, lsp.errors
            main = (run_dir / "main.log").read_text()
            details = (run_dir / "trace.log").read_text()
            assert "Received request initialize id=<string>" in main
            assert "Sending response" in main
            assert "Unknown method." in main
            for private in (
                uri,
                "private_trace_directory",
                "private_trace_file",
                source,
                "Private_Trace_Source",
                request.id,
            ):
                assert private not in main, (language, debug, private)
                assert (private in details) == debug, (language, debug, private)
            assert initialize.id not in main + details
            assert ("[ALS.MAIN.DEBUG]" in details) == debug

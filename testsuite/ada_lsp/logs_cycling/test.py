""" This test verifies the proper cycling of log files.
"""

from drivers.lsp_python_driver import LSP, complex_test
from drivers.lsp_ada_requests import initialize
import os
import glob
import time

# Use a complex_test rather than a simple_test
# since we need to launch several servers.
@complex_test
def test_cycle_logs(wd) -> list[str] | None:
    # Create a .als directory local to this test
    os.makedirs(os.path.join(wd, ".als"))
    env = {"ALS_HOME": wd}

    # Create 20 fake old log files for gpr_ls and ada_ls
    for i in range(20):
        for name in ["gpr_ls", "ada_ls"]:
            with open(os.path.join(wd, ".als", f"{name}_log.1978-02-24T0000{i:02}.log"), "w") as f:
                f.write("This is a log file")
    time.sleep(1)

    # Launch 2 Ada servers and 2 GPR servers and shut them down
    for i in range(2):
        lsp = LSP("ada_language_server", wd, env=env)
        lsp.send(initialize())
        lsp.shutdown()
        time.sleep(1)
        lsp = LSP(["ada_language_server", "--language-gpr"], wd, env=env)
        lsp.send(initialize())
        lsp.shutdown()
        time.sleep(1)

    # At this point we should have 20 log files remaining:
    # 10 log files for gpr_ls and 10 for ada_ls
    log_files = [os.path.basename(f) for f in glob.glob(os.path.join(wd, ".als", "*.log"))]
    assert len(log_files) == 20
    assert len([f for f in log_files if "gpr_ls" in f]) == 10
    assert len([f for f in log_files if "ada_ls" in f]) == 10

    # There should also be 16 log files from "1978" remaining
    assert len([f for f in log_files if "log.1978" in f]) == 16

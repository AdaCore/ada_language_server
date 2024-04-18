"""
The vscode-test tool has a bug on Windows. There is a workaround of spawning the node
process with a CWD that has a lowercase drive letter. This script implements that
workaround.

See https://github.com/microsoft/vscode-test/issues/136#issuecomment-2049714028
"""

import os
from pathlib import Path
import subprocess
import sys


cwd = Path().absolute()

windows = os.environ.get("OS") == "Windows_NT"
if windows:
    # Convert the path to a lowercase drive letter
    cwd = Path(f"{cwd.drive.lower()}\\", *cwd.parts[1:])

# Use shell=True on Windows to resolve the first argument based on PATH
exit(subprocess.call(sys.argv[1:], cwd=cwd, shell=windows))

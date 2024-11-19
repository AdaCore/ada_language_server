"""This file wraps the lsp-devtools entry point that is used for recording LSP message
into a replay file.

Its purpose is to catch the CancelledError exception which occurs systematically in the
nominal termination of the lsp-devtools agent. These exceptions appear even in
successful tests and cause confusion, hence creating this wrapper to mask them.
"""

import asyncio
import sys
from lsp_devtools.cli import main

if __name__ == "__main__":
    try:
        sys.exit(main())
    except asyncio.exceptions.CancelledError:
        # Silence these exceptions
        sys.exit(1)

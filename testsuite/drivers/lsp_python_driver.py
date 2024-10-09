#!/usr/bin/env python

"""Provide a Python class to drive an LSP server for testing purposes."""

import subprocess
import argparse
import json
import threading
import queue
import time
import sys

from lsp_ada_requests import initialize
from lsp_types import LSPMessage, LSPResponse

# This is a class that allows writing tests in a "test.py" file.
# When the execution is complete, a file "/tmp/replay.txt" is written that
# contains the steps used to replay the test.
#
# If the test.py prints anything on stdout, this will be considered an error in the test.
# You can add prints on stderr to help debugging, and the test will not fail.
#
# Example test.py:
#
#    from lsp_python_driver import LSP
#    from lsp_ada_requests import initialize
#
#    # Initialize the LSP server
#    lsp = LSP()
#
#    # Insert a debug point at any place with this:
#    lsp.debug_here()
#
#    # Send a request and check the response
#    result = lsp.send(initialize())
#
#    # Inspect the response in various ways: here we check the value of one field...
#    result.assertField("capabilities.callHierarchyProvider", True)
#    # ... and here we check that the response contains a specific dictionary
#    result.assertContains({"capabilities": {"callHierarchyProvider": True}})
#
#    # Shut down the LSP server at the end
#    lsp.shutdown()


class LSP(object):

    def __init__(self, cl: list = ["ada_language_server"]):
        """Launch an LSP server and provide a way to send messages to it.
        cl is the command line to launch the LSP server.
        """
        self.pid = None

        # Launch the lsp server, with pipes ready for input/output
        self.process = subprocess.Popen(
            cl,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )

        # Launch a task that will receive messages from the LSP server
        # and store them in a queue
        self.task = threading.Thread(target=self.receive_task)
        self.task.start()

        # The queue receiving the messages from the reading task
        self.queue = queue.Queue()

        # A list of bytes containing everything we sent to the server
        self.replay = []

        # Set a global trap - in case of exception, shut down the server
        def shutdown_on_exception(exctype, value, tb):
            self.shutdown()
            sys.__excepthook__(exctype, value, tb)

        sys.excepthook = shutdown_on_exception

    def receive_task(self):
        """Receive messages from the LSP server."""
        while True:
            # Read the header
            header = self.process.stdout.readline().decode("utf-8")
            if not header:
                break
            if not header.startswith("Content-Length:"):
                continue
            length = int(header[len("Content-Length:") :])
            # Read the JSON message
            # (adding +2 to account of \r\n)
            content = self.process.stdout.read(length + 2).decode("utf-8")
            message_dict = json.loads(content)

            # Create a LSPResponse object
            message = LSPResponse(message_dict)

            # Store the message in a queue
            self.queue.put(message)

            # If the process is terminated, exit the thread
            if self.process.poll() is not None:
                break

    def send(
        self, message: LSPMessage, expect_response=True, timeout: float = 2.0
    ) -> LSPResponse | None:
        """Send a message to the LSP server.
        If expect_response is True, wait for a response for at most timeout seconds.
        Return the response if any, or None if no response
        """
        encoded = message.encode()
        self.replay.append(encoded)

        self.process.stdin.write(encoded)
        self.process.stdin.flush()

        start_time = time.time()

        if expect_response:
            while time.time() - start_time <= timeout:
                # Check if there is something in the queue
                if self.queue.empty():
                    time.sleep(0.1)
                    continue
                # Get the response
                response = self.queue.get()
                if response.id == message.id:
                    return response
            return LSPResponse(
                {
                    "error": f"response to {message.id} ({message.method}) not received in {timeout} seconds"
                }
            )

    def debug_here(self):
        """Insert a debug point in the test."""
        print("## Debug point reached. Attach with:", file=sys.stderr)
        print(f"    gdb --pid={self.process.pid}", file=sys.stderr)
        print("", file=sys.stderr)
        print("## Press Enter to continue", file=sys.stderr)
        input()

    def shutdown(self):
        """Shutdown the LSP server."""
        self.send(LSPMessage({"method": "shutdown"}))
        self.process.kill()
        self.process.wait()

        # Write a "replay.txt" replay file
        with open("/tmp/replay.txt", "wb") as f:
            for message in self.replay:
                f.write(message)


def main():
    # Define a main for testing/development purposes
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "program", nargs="?", default="ada_language_server", help="The program to run"
    )
    args = parser.parse_args()

    lsp = LSP([args.program])
    result = lsp.send(initialize())
    result.assertField("capabilities.callHierarchyProvider", True)
    result.assertContains({"capabilities": {"callHierarchyProvider": True}})
    lsp.shutdown()


if __name__ == "__main__":
    main()

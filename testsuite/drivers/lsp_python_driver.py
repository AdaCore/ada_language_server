#!/usr/bin/env python

"""Provide a Python class to drive an LSP server for testing purposes."""

import subprocess
import argparse
import json
import threading
import queue
import os
import time
import sys

from drivers.lsp_ada_requests import initialize, initialized, didChangeConfiguration
from drivers.lsp_types import LSPMessage, LSPResponse, ResponseAssertionError
import traceback

# This is a class that allows writing tests in a "test.py" file.
# When the execution is complete, a file "/tmp/replay.txt" is written that
# contains the steps used to replay the test.
#
# If the test.py prints anything on stdout, this will be considered an error in the
# test. You can add prints on stderr to help debugging, and the test will not fail.
#
# Example test.py with a full test:
#
#    from lsp_python_driver import LSP
#    from lsp_ada_requests import initialize, initialized, didChangeConfiguration
#
#    @complex_test
#    def test_some_functionality(working_dir) -> list[str] | None:
#        # Figure out the program to run
#        program = os.environ.get("ALS", "ada_language_server")
#        # Initialize the LSP server
#        lsp = LSP(program, working_dir)
#
#        # Send the initialize request, the initialized notification and the
#        # didChangeConfiguration notification
#        _ = lsp.send(initialize())
#        lsp.send(initialized())
#        lsp.send(didChangeConfiguration())
#
#        # Insert here whatever you want to test
#        # For example, you can send a didOpen notification:
#        # lsp.send(didOpen_from_disk("main.adb"))
#
#        # Note: you can insert a debug point at any place with this:
#        lsp.debug_here()
#
#        # Shut down the LSP server at the end
#        lsp.shutdown()
#
# The example above is a complex test which has a lot of boilerplate
# to start the LSP server, send the initialize request, etc.
# If you have a simple test that doesn't need all this boilerplate,
# you can use the simple_test decorator instead:
#
#    from lsp_python_driver import LSP, simple_test
#
#    @simple_test
#    def test_some_functionality(lsp, working_dir):
#        # Insert here whatever you want to test
#        # That's it! the LSP server is started and shut down for you,
#        # and the initialize/initialized/didChangeConfiguration requests
#        # have been sent by the framework.


# Global configuration options
# Time limit for the test globally
RLIMIT_SECONDS = 60.0
# Timeout for a response to a request
RESPONSE_TIMEOUT = 2.0
DEBUG_MODE = False


def set_debug_mode(mode: bool = True):
    """Set the debug mode."""
    global DEBUG_MODE
    DEBUG_MODE = mode


def set_wait_factor(factor: float):
    """Set the wait factor."""
    global RLIMIT_SECONDS
    global RESPONSE_TIMEOUT
    RLIMIT_SECONDS = RLIMIT_SECONDS * factor
    RESPONSE_TIMEOUT = RESPONSE_TIMEOUT * factor


class LSP(object):

    def __init__(
        self,
        cl: str | list[str] | None = "ada_language_server",
        working_dir: str = ".",
        env: dict | None = None,
    ):
        """Launch an LSP server and provide a way to send messages to it.
        cl is the command line to launch the LSP server.
        """
        self.pid = None

        if env is not None:
            env = os.environ.copy() | env

        if cl is None:
            cl = os.environ.get("ALS", "ada_language_server")

        # Launch the lsp server, with pipes ready for input/output
        self.process = subprocess.Popen(
            cl,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            cwd=working_dir,
            env=env,
        )

        self.wd = working_dir

        # Kill the server when we reach this time
        self.kill_me_at = time.time() + int(
            RLIMIT_SECONDS * os.environ.get("ALS_WAIT_FACTOR", 1.0)
        )

        # This contains either None or a timestamp. If a timestamp,
        # then the process should be killed after 2 seconds after this.
        self.license_to_kill = None

        # The queue receiving the messages from the reading task
        self.queue = queue.Queue()

        # Launch a task that will receive messages from the LSP server
        # and store them in a queue
        self.task = threading.Thread(target=self.receive_task)
        self.k_task = threading.Thread(target=self.kill_task)

        # Error messages encountered
        self.errors = []

        # A list of bytes containing everything we sent to the server
        self.replay = []

        # Start the tasks after all the attributes have been declared
        self.task.start()
        self.k_task.start()

        # If we are in debug mode, insert a debug point
        if DEBUG_MODE:
            self.debug_here()

    def kill_task(self):
        while True:
            time.sleep(0.2)
            now = time.time()

            # If we have reached the time limit, kill the process,
            # unless we are debugging.
            if not DEBUG_MODE:
                if now > self.kill_me_at:
                    self.errors.append("rlimit time limit reached")
                    self.process.kill()
                    break

            # If we have received a license to kill, check if it is time
            # to kill the process!
            if self.license_to_kill is not None:
                if now - self.license_to_kill > RESPONSE_TIMEOUT:
                    self.process.kill()
                    break

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

            # TODO: Add support for "Content-Type" header

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

            time.sleep(0.01)

    def send(self, message: LSPMessage, expect_response=True) -> LSPResponse | None:
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
            while time.time() - start_time <= RESPONSE_TIMEOUT:
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
                    "error": f"response to {message.id} ({message.method})"
                    f" not received in {RESPONSE_TIMEOUT} seconds"
                }
            )

    def error(self, message):
        """Log an error message."""
        self.errors.append(message)

    def debug_here(self):
        """Insert a debug point in the test."""
        print("## Debug point reached. Attach with:", file=sys.stderr)
        print(f"    gdb -p {self.process.pid}", file=sys.stderr)
        print("", file=sys.stderr)
        print("## Press Enter to continue", file=sys.stderr)
        input()

    def shutdown(self):
        """Shutdown the LSP server."""
        self.send(LSPMessage({"method": "shutdown"}))
        # Detach the input pipe
        self.process.stdin.close()
        # Set the license to kill
        self.license_to_kill = time.time()

        if not self.errors:
            return

        # If errors were found, capture a replay file.
        # Compute the replay dir based on this file
        replay_dir = os.path.join(
            os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "replays"
        )
        # Create the directory if it doesn't exist
        os.makedirs(replay_dir, exist_ok=True)
        replay_file = os.path.join(
            replay_dir, os.path.basename(self.wd) + "_replay.txt"
        )
        self.errors.append(f"Replay file written to {replay_file}")

        # Write a "replay.txt" replay file
        with open(replay_file, "wb") as f:
            for message in self.replay:
                f.write(message)


def run_complex_test(test_function, working_dir) -> list[str]:
    """Run a test function, catch exceptions and return any errors found."""
    try:
        errors = test_function(working_dir)
        if errors is None:
            return []
        return errors
    except Exception as e:
        errors = [str(e)]
        # If the exception is a ResponseAssertionError, no need for the traceback
        if not isinstance(e, ResponseAssertionError):
            errors.append(traceback.format_exc())
        return errors


def run_simple_test(test_function, working_dir) -> list[str]:
    """Run a test function, catch exceptions and return any errors found."""
    try:
        program = os.environ.get("ALS", "ada_language_server")
        lsp = LSP(program, working_dir)
        _ = lsp.send(initialize())
        lsp.send(initialized())
        lsp.send(didChangeConfiguration())
        test_function(lsp, working_dir)
    except Exception as e:
        lsp.errors += [str(e)]
        # If the exception is an AssertionError, no need for the traceback
        if not isinstance(e, ResponseAssertionError):
            lsp.errors.append(traceback.format_exc())
    finally:
        lsp.shutdown()
        return lsp.errors


# Make run_simple_test available as a decorator
def simple_test(test_function):
    def wrapper(working_dir):
        return run_simple_test(test_function, working_dir)

    wrapper.simple_test = True
    return wrapper


# Make run_complex_test available as a decorator
def complex_test(test_function):
    def wrapper(working_dir):
        return run_complex_test(test_function, working_dir)

    wrapper.complex_test = True
    return wrapper


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

"""Describes some types used in the LSP testing."""

from typing import Any
import json
import os

current_id = 0


class LSPMessage(object):
    """Represents a message to be sent to an LSP server."""

    def __init__(self, from_dict, use_id=True):
        self.from_dict = from_dict
        self.id = None
        if use_id:
            global current_id
            current_id += 1
            self.id = "ada-" + str(current_id)

    def encode(self):
        computed_dict = self.from_dict.copy()
        computed_dict["jsonrpc"] = "2.0"
        if self.id is not None:
            computed_dict["id"] = self.id
        # make a compact JSON string
        as_str = json.dumps(computed_dict)
        s = "Content-Length:{}\r\n\r\n{}".format(len(as_str), as_str)
        return s.encode("utf-8")

    @property
    def method(self):
        return self.from_dict.get("method", "no method")


class LSPResponse(object):
    """Represents a response from an LSP server."""

    def __init__(self, from_dict):
        if "result" in from_dict:
            self.from_dict = from_dict["result"]
        else:
            self.from_dict = from_dict
        self.id = from_dict.get("id", "no id")
        # remove "jsonrpc" and "id" from the dictionary
        if self.from_dict is not None:
            if "jsonrpc" in self.from_dict:
                self.from_dict.pop("jsonrpc")
            if "id" in self.from_dict:
                self.from_dict.pop("id")

    def __str__(self):
        # Print the response in a readable way
        return json.dumps(self.from_dict, indent=4)

    def assertEquals(self, expected):
        """Assert that the response is equal to the expected dictionary."""
        if self.from_dict != expected:
            print(
                f"### RECEIVED ###\n{self.from_dict}\n### EXPECTED ####\n{expected}\n"
            )
            raise AssertionError()

    def assertField(self, field, expected):
        """Assert that the response field is equal to the expected value.

        Field can contain "." to access nested fields.
        """
        fields = field.split(".")
        value = self.from_dict
        for f in fields:
            value = value.get(f)
        if value != expected:
            raise AssertionError(f"{field} is #{value}#, expected #{expected}#")

    def assertContains(self, expected_dict):
        """Assert that the response contains the expected dictionary."""

        def contains(source, expected):
            if isinstance(expected, dict):
                for key, value in expected.items():
                    if key not in source:
                        return False
                    if isinstance(value, dict):
                        if not contains(source[key], value):
                            return False
                    elif source[key] != value:
                        return False
            elif isinstance(expected, list):
                for item in expected:
                    if item not in source:
                        return False
            else:
                if source != expected:
                    return False
            return True

        if not contains(self.from_dict, expected_dict):
            raise AssertionError(f"Response does not contain {expected_dict}")


def URI(filename: str) -> str:
    """Return a URI for the given filename."""
    # Get the absolute path for filename
    abs_path = os.path.abspath(filename)
    # Convert it to a URI
    return "file://" + abs_path

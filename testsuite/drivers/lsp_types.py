"""Describes some types used in the LSP testing."""

from typing import Any
import json
import os

import traceback

current_id = 0


class ResponseAssertionError(Exception):
    """An exception that we manage - to avoid printing long traceback."""
    pass


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
            if f not in value:
                raise ResponseAssertionError(
                    self._line_info() + f"Field {field} not found in {value}"
                )
            value = value.get(f)
        if value != expected:
            raise ResponseAssertionError(
                self._line_info() + f"{field} is #{value}#, expected #{expected}#"
            )

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
            raise ResponseAssertionError(f"Response does not contain {expected_dict}")

    def _line_info(self) -> str:
        """Return the line number and file name of the caller of the parent."""
        # Use traceback to get the previous frame
        (filename, line_number, fn_name, _) = traceback.extract_stack()[-3]
        splits = filename.split(os.path.sep)
        test_name = splits[-2].replace("__", os.path.sep)
        dir_and_file = os.path.join("testsuite", test_name, splits[-1])
        message = f"At {dir_and_file}:{line_number} in {fn_name}:\n"
        return message

    def assertLocationsList(self, expected: list[(str, int)]):
        """Compare the response to an expected list of locations.
        The expected list is a list of tuples (file base name, line number),
        with line_number being 1-based.
        """
        if not isinstance(self.from_dict, list):
            raise AssertionError("The response does not contain a list")

        # Extract the locations from the response
        locations = []
        for item in self.from_dict:
            filename = ""
            line = -1
            if "uri" in item and "range" in item:
                # This is a location in the style returned by "prepareCallHierarchy"
                filename = os.path.basename(item["uri"])
                line = item["range"]["start"]["line"] + 1
                locations.append((filename, line))
            elif "from" in item and "uri" in item["from"] and "range" in item["from"]:
                # This is a location in the style returned by "incomingCalls"
                filename = os.path.basename(item["from"]["uri"])
                line = item["from"]["range"]["start"]["line"] + 1
                locations.append((filename, line))

        # Compare the locations to the expected list
        if locations != expected:
            message = self._line_info()
            message += "Expected locations:\n"
            for loc in expected:
                message += f"   {loc[0]}:{loc[1]}\n"
            message += "Received locations:\n"
            for loc in locations:
                message += f"   {loc[0]}:{loc[1]}\n"
            raise ResponseAssertionError(message)


def URI(filename: str) -> str:
    """Return a URI for the given filename."""
    # Get the absolute path for filename
    abs_path = os.path.abspath(filename)
    # Convert it to a URI
    return "file://" + abs_path

#!/usr/bin/env python3
"""
NOTE: this script has been developed with AI assistance.

Generate an Ada package with embedded tool switches database.

This script runs various tools with their help options and extracts the
switches and their associated documentation into a structured JSON database,
which is then embedded into an Ada package specification as string
constants.
"""

import argparse
import json
import re
import subprocess
import sys
from typing import Dict, List, Optional


class HelpParser:
    """Base class for parsing tool help output."""

    def parse(self, help_text: str) -> Dict[str, str]:
        """Parse help text and return a dictionary of switches to docs.

        Args:
            help_text: The raw help output from the tool

        Returns:
            Dictionary mapping switch names to their documentation
        """
        raise NotImplementedError("Subclasses must implement parse()")


class GnatHelpParser(HelpParser):
    """Parser for GNAT compiler help output."""

    def parse(self, help_text: str) -> Dict[str, str]:
        """Parse GNAT help output format.

        GNAT help has lines like:
          -switch   Documentation text that may span
                    multiple lines
        or:
         -switch   Documentation text
             --long-switch  More documentation
        """
        switches = {}
        current_switch = None
        current_doc = []

        lines = help_text.split("\n")

        for line in lines:
            # Check if line starts a new switch (starts with 1-5 spaces
            # and a dash). Handles "  -switch", " -switch", "--switch"
            match = re.match(r"^ {1,5}(-+\S+(?:,\s*-+\S+)*)\s+(.*)$", line)
            if match:
                # Save previous switch if any
                if current_switch:
                    switches[current_switch] = " ".join(current_doc).strip()

                # Start new switch
                current_switch = match.group(1)
                current_doc = [match.group(2)] if match.group(2).strip() else []
            elif current_switch:
                # Continuation line - check if indented documentation
                stripped = line.strip()
                if stripped and not line.startswith(
                    "   ."
                ):  # Skip mode value descriptions
                    # Only add if doesn't look like new section header
                    if not re.match(r"^[A-Z][a-z].*:$", stripped):
                        current_doc.append(stripped)
                elif stripped == "":
                    # Empty line might indicate end of switch's docs
                    pass

        # Don't forget the last switch
        if current_switch:
            switches[current_switch] = " ".join(current_doc).strip()

        return switches


class GenericHelpParser(HelpParser):
    """Generic parser for standard help output formats."""

    def parse(self, help_text: str) -> Dict[str, str]:
        """Parse generic help output.

        Tries to identify lines that look like:
          -switch, --long-switch    Documentation
        or:
          -switch    Documentation
        """
        switches = {}
        current_switch = None
        current_doc = []

        lines = help_text.split("\n")

        for line in lines:
            # Try to match common switch patterns
            match = re.match(r"^\s{0,4}(-+\S+(?:,\s*-+\S+)*)\s{2,}(.*)$", line)
            if match:
                # Save previous switch if any
                if current_switch:
                    switches[current_switch] = " ".join(current_doc).strip()

                # Start new switch (may be multiple comma-separated)
                switch_list = match.group(1)
                current_switch = switch_list
                current_doc = [match.group(2)]
            elif current_switch and line.startswith(" " * 6) and line.strip():
                # Continuation line with significant indentation
                current_doc.append(line.strip())
            elif line.strip() == "":
                # Empty line might end current switch
                if current_switch and current_doc:
                    switches[current_switch] = " ".join(current_doc).strip()
                    current_switch = None
                    current_doc = []

        # Don't forget the last switch
        if current_switch and current_doc:
            switches[current_switch] = " ".join(current_doc).strip()

        return switches


class GprbuildHelpParser(HelpParser):
    """Parser for gprbuild and gprclean style help output."""

    def parse(self, help_text: str) -> Dict[str, str]:
        """Parse gprbuild/gprclean help output format.

        Help output has lines like:
          -switch   Documentation text
         -switch   Documentation text (gprclean uses 1 space)
          --long-switch
                   Documentation text that may span
                   multiple lines
          --db dir  Docs with space-separated argument
        """
        switches = {}
        current_switch = None
        current_doc = []

        lines = help_text.split("\n")

        for line in lines:
            # Check if line starts new switch (starts with 1-2 spaces
            # and a dash). First try switches with space-separated args
            match_with_arg = re.match(
                r"^ {1,2}(-+[^\s]+)\s+([a-z<][^\s]*)\s+(.+)$", line
            )
            # Then try regular switches with or without doc on same line
            # Pattern captures switch names with optional
            # equals/bracket modifiers and comma-separated alternatives
            match_simple = re.match(
                r"^ {1,2}((?:-+[^\s,]+(?:=\S+|\[=\S+\])?(?:,\s*-+[^\s,]+"
                r"(?:=\S+|\[=\S+\])?)*,?))(?:\s+(.*))?$",
                line,
            )

            if match_with_arg:
                # Switch with space-separated arg, like "--db dir"
                # Save previous switch if any
                if current_switch:
                    switches[current_switch] = " ".join(current_doc).strip()

                switch_name = match_with_arg.group(1)
                arg_name = match_with_arg.group(2)
                doc_text = match_with_arg.group(3).strip()

                # Include argument in switch name
                current_switch = f"{switch_name} {arg_name}"
                current_doc = [doc_text] if doc_text else []
            elif match_simple:
                # Save previous switch if any
                if current_switch:
                    switches[current_switch] = " ".join(current_doc).strip()

                # Regular switch, possibly with doc on same line
                current_switch = match_simple.group(1)
                doc_text = match_simple.group(2)
                current_doc = (
                    [doc_text.strip()] if doc_text and doc_text.strip() else []
                )
            elif current_switch:
                # Check if continuation line (starts with more spaces
                # than switch lines)
                stripped = line.strip()
                if stripped and line.startswith("   "):
                    # Continuation line with indentation (at least 3
                    # spaces, more than switch lines)
                    current_doc.append(stripped)
                elif not line.strip():
                    # Empty line ends the current switch
                    if current_doc:
                        switches[current_switch] = " ".join(current_doc).strip()
                    current_switch = None
                    current_doc = []
                elif not line.startswith(" -"):
                    # Line part of section headers or other text, end
                    # current
                    if current_doc:
                        switches[current_switch] = " ".join(current_doc).strip()
                    current_switch = None
                    current_doc = []

        # Don't forget the last switch
        if current_switch and current_doc:
            switches[current_switch] = " ".join(current_doc).strip()

        # Clean up docs: remove leading "-" followed by spaces
        # (used by tools like gnatcheck)
        for switch in switches:
            doc = switches[switch]
            if doc.startswith("-"):
                # Remove the leading dash and any spaces after it
                switches[switch] = re.sub(r"^-\s*", "", doc)

        return switches


def run_tool_help(tool_command: str) -> Optional[str]:
    """Run a tool with its help option and capture output.

    Args:
        tool_command: The full command to run (e.g., "gnat --help-ada")

    Returns:
        The help output as a string, or None if the command failed
    """
    try:
        result = subprocess.run(
            tool_command.split(), capture_output=True, text=True, timeout=30
        )
        # Many tools output help to stderr, so combine both
        output = result.stdout + result.stderr
        return output if output.strip() else None
    except (
        subprocess.TimeoutExpired,
        subprocess.CalledProcessError,
        FileNotFoundError,
    ) as e:
        print(f"Error running '{tool_command}': {e}", file=sys.stderr)
        return None


def select_parser(tool_name: str) -> HelpParser:
    """Select the appropriate parser for a given tool.

    Args:
        tool_name: Name of the tool (extracted from command)

    Returns:
        An appropriate HelpParser instance
    """
    tool_name_lower = tool_name.lower()
    if tool_name_lower in ("gnat", "gnatprove"):
        return GnatHelpParser()
    elif tool_name_lower in (
        "gprbuild",
        "gprclean",
        "gprinstall",
        "gnatcheck",
        "arm-eabi-gnatemu",
    ):
        return GprbuildHelpParser()
    else:
        return GenericHelpParser()


def extract_tool_name(tool_command: str) -> str:
    """Extract the tool name from a command string.

    Args:
        tool_command: Full command like "gnat --help-ada" or
                      "/path/to/tool --help"

    Returns:
        Just the tool name, e.g., "gnat"
    """
    import os

    tool_path = tool_command.split()[0]
    return os.path.basename(tool_path)


def normalize_switch_name(switch_name: str) -> str:
    """Normalize switch names to prefer long over short versions.

    When a switch has both long and short versions separated by comma
    (e.g., "--width, -w" or "-v, --verbose"), extract only the long
    version. If no long version exists, use the part before the comma.

    Args:
        switch_name: The switch name, possibly with multiple versions

    Returns:
        The normalized switch name (preferring long version)
    """
    # Check if there's a comma indicating multiple versions
    if "," not in switch_name:
        return switch_name

    # Split by comma to get individual versions
    parts = [part.strip() for part in switch_name.split(",")]

    # Look for a long version (starts with --)
    long_versions = [p for p in parts if p.startswith("--")]
    if long_versions:
        return long_versions[0]

    # No long version found, use the first part (before comma)
    return parts[0]


def escape_ada_string(s: str) -> str:
    """Escape a string for use in an Ada string literal.

    Args:
        s: The string to escape

    Returns:
        The escaped string suitable for Ada string literals
    """
    # In Ada, quotes are doubled to escape them
    return s.replace('"', '""')


def split_string_for_ada(json_str: str, max_length: int = 1000) -> List[str]:
    """Split a JSON string into chunks suitable for Ada string constants.

    Args:
        json_str: The JSON string to split
        max_length: Maximum length of each chunk (to avoid Ada line
                    length limits)

    Returns:
        List of string chunks
    """
    chunks = []
    i = 0
    while i < len(json_str):
        # Try to find a good breaking point (after comma or closing brace)
        end = min(i + max_length, len(json_str))
        if end < len(json_str):
            # Look back for a good break point
            for j in range(end, max(i, end - 100), -1):
                if json_str[j] in ",}]":
                    end = j + 1
                    break

        chunks.append(json_str[i:end])
        i = end

    return chunks


def generate_ada_package(database: dict, output_dir: str):
    """Generate an Ada package spec with the JSON database embedded.

    Args:
        database: The tool database dictionary
        output_dir: Directory where to write the Ada package spec
    """
    import os

    # Convert database to compact JSON string
    json_str = json.dumps(database, ensure_ascii=False, separators=(",", ":"))

    # Escape for Ada
    escaped_json = escape_ada_string(json_str)

    # Split into manageable chunks
    chunks = split_string_for_ada(escaped_json, max_length=2000)

    # Generate Ada package spec
    ada_code = []
    ada_code.append("--  Automatically generated, do not edit.")
    ada_code.append("")
    ada_code.append("pragma Style_Checks (Off);")
    ada_code.append("")
    ada_code.append("package LSP.GPR_Completions.Tools.Database is")
    ada_code.append("")

    # Generate string constants for each chunk
    for i, chunk in enumerate(chunks, 1):
        ada_code.append(f'   Db{i} : constant String := "{chunk}";')
        ada_code.append("")

    # Generate the main concatenated constant
    db_parts = " & ".join(f"Db{i}" for i in range(1, len(chunks) + 1))
    ada_code.append(f"   Db : constant String := {db_parts};")
    ada_code.append("")
    ada_code.append("end LSP.GPR_Completions.Tools.Database;")

    # Write to file
    output_file = os.path.join(output_dir, "lsp-gpr_completions-tools-database.ads")
    with open(output_file, "w", encoding="utf-8") as f:
        f.write("\n".join(ada_code))

    print(f"\nAda package written to: {output_file}")
    print(f"Total string chunks: {len(chunks)}")
    print(f"Total JSON size: {len(json_str)} characters")


def generate_database(tool_configs: List[str], output_dir: str, pretty: bool = True):
    """Generate the Ada package database from tool help outputs.

    Args:
        tool_configs: List of tool commands (e.g., ["gnat --help-ada"])
        output_dir: Directory where to write the Ada package
        pretty: Unused (kept for compatibility)
    """
    database = {}

    for tool_command in tool_configs:
        print(f"Processing: {tool_command}")

        # Extract tool name for the database key
        tool_name = extract_tool_name(tool_command)

        # Run the tool and get help output
        help_text = run_tool_help(tool_command)
        if not help_text:
            print(
                f"  Warning: No help output received for '{tool_command}'",
                file=sys.stderr,
            )
            continue

        # Select appropriate parser
        parser = select_parser(tool_name)

        # Parse the help text
        switches = parser.parse(help_text)

        if not switches:
            print(
                f"  Warning: No switches parsed from '{tool_command}'",
                file=sys.stderr,
            )
            continue

        # Normalize switch names to prefer long versions
        normalized_switches = {
            normalize_switch_name(switch): doc for switch, doc in switches.items()
        }

        print(f"  Found {len(normalized_switches)} switches")

        # Store in database (use tool name as key)
        database[tool_name] = {
            "command": tool_command,
            "switches": normalized_switches,
        }

    # Generate Ada package
    generate_ada_package(database, output_dir)

    print(f"Total tools processed: {len(database)}")


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Generate Ada package with embedded tool switches database",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s -o source/gpr/generated "gnat --help-ada"
  %(prog)s -o source/gpr/generated "gnat --help-ada" "gnatprove --help"
  %(prog)s --default -o source/gpr/generated
        """,
    )

    parser.add_argument(
        "tools",
        nargs="*",
        help='Tool commands to process (e.g., "gnat --help-ada")',
    )

    parser.add_argument(
        "-o",
        "--output",
        default="source/gpr/generated",
        help="Output directory for Ada package " "(default: source/gpr/generated)",
    )

    parser.add_argument(
        "--compact",
        action="store_true",
        help="Unused (kept for compatibility)",
    )

    parser.add_argument(
        "--default",
        action="store_true",
        help="Process default GNAT tools " "(gnat --help-ada, gnatprove --help)",
    )

    args = parser.parse_args()

    # Use default tools if --default is specified or no tools provided
    if args.default or not args.tools:
        default_tools = [
            "gnat --help-ada",
            "gnatprove --help",
            "gprbuild --help",
        ]
        tools_to_process = default_tools
    else:
        tools_to_process = args.tools

    generate_database(tools_to_process, args.output, pretty=not args.compact)


if __name__ == "__main__":
    main()

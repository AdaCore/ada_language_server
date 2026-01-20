"""
Compare dependencies and devDependencies keys between two package.json files.

This script reads two JSON files (typically package.json files) and compares
the keys under 'dependencies' and 'devDependencies' properties. It reports
any missing or extra keys and exits with status 0 if all keys match, or
status 1 if there are mismatches.

Usage:
    python compare.py <file1.json> <file2.json>
"""

import sys
import json


def read_json_file(filepath):
    with open(filepath, "r", encoding="utf-8") as f:
        return json.load(f)


def compare_keys(keys1, keys2):
    missing = keys1 - keys2
    extra = keys2 - keys1

    has_errors = False

    if missing:
        print("\n❌ Missing:", file=sys.stderr)
        for key in sorted(missing):
            print(f"   - {key}", file=sys.stderr)
        has_errors = True

    if extra:
        print("\n❌ Extra:", file=sys.stderr)
        for key in sorted(extra):
            print(f"   - {key}", file=sys.stderr)
        has_errors = True

    return has_errors


def main():
    if len(sys.argv) != 3:
        print("Usage: python compare.py <file1.json> <file2.json>", file=sys.stderr)
        sys.exit(1)

    # Read JSON files
    json1 = read_json_file(sys.argv[1])
    json2 = read_json_file(sys.argv[2])

    has_errors = False

    # Compare dependencies
    for prop in ["devDependencies", "dependencies"]:
        has_errors = compare_keys(json1[prop].keys(), json2[prop].keys()) or has_errors

    if has_errors:
        print("\n❌ Dependency documentation mismatches found!", file=sys.stderr)
        print("\n  Please update: " + sys.argv[2], file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()

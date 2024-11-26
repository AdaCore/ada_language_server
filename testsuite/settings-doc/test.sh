#!/bin/sh

# Fail at the first error
set -e

# Collect settings from package.json and remove 'ada.' prefix
python3 get-extension-settings.py <"$1/integration/vscode/ada/package.json" | grep ^ada. | sed -e 's/ada\.//' | sort >pkg.txt

# Collect settings from documentation
grep '^### ' "$1/doc/settings.md" | sed -e 's/....//' | sort >doc.txt

# Collect settings from the JSON schema for .als.json files
python3 get-schema-properties.py <"$1/integration/vscode/ada/schemas/als-settings-schema.json" >schema.txt

# Collect settings read in the ALS implementation
grep 'if Name = "[^"]\+"' "$1/source/ada/lsp-ada_configurations.adb" | sed -e 's/.*"\([^"]\+\)"/\1/' >impl.txt
# Remove the following settings from the implementation list because they are
# either hidden, or nested
exclude="logThreshold onTypeFormatting indentOnly"
for exc in $exclude; do
    echo "$(grep -v "$exc" <impl.txt)" >impl.txt
done
# Add the following properties because they are nested
add="onTypeFormatting.indentOnly"
for a in $add; do
    echo "$a" >>impl.txt
done
# Sort the list
echo "$(sort <impl.txt)" >impl.txt

# Check that all VS Code settings are documented
diff -u pkg.txt doc.txt

# The ada.trace.server setting exists only in VS Code and not in .als.json
# files. So remove it before the comparison.
#
# We need to use a subshell because it's not allowed to read and write the same
# file in one pipeline
# echo "$(grep -v trace.server <doc.txt)" >doc.txt
echo "$(grep -v trace.server <doc.txt)" >doc.txt

# Check that all implemented settings are documented
diff -u impl.txt doc.txt

# Check that all implemented settings are defined in the JSON Schema
diff -u impl.txt schema.txt

# Check that all settings defined in the JSON Schema are documented
diff -u schema.txt doc.txt

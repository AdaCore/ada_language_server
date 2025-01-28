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
python3 get-implementation-settings.py <"$1/source/ada/lsp-ada_configurations.adb" >impl.txt

# Remove the following settings from the implementation list because they are
# either hidden, or nested
exclude="onTypeFormatting indentOnly trace"
for exc in $exclude; do
    echo "$(grep -v "$exc" <impl.txt)" >impl.txt
done
# Add the following properties because they are nested
add="onTypeFormatting.indentOnly trace.server"
for a in $add; do
    echo "$a" >>impl.txt
done
# Sort the list
echo "$(sort <impl.txt)" >impl.txt

# Add the following properties in schema, they are obsolete
add="enableDiagnostics"
for a in $add; do
    echo "$a" >>schema.txt
    echo "$a" >>pkg.txt
done
# Sort the lists
echo "$(sort <schema.txt)" >schema.txt
echo "$(sort <pkg.txt)" >pkg.txt

# Check that all VS Code settings are documented
diff -u pkg.txt doc.txt

# Check that all implemented settings are documented
diff -u impl.txt doc.txt

# Before comparing with the schema, we need to filter out VSCode-only settings
# which are not defined in the schema and shouldn't be because we don't want to
# allow such values in .als.json files.
vscode_only="trace.server showNotificationsOnErrors"
for s in $vscode_only; do
    echo "$(grep -v "$s" impl.txt)" >impl.txt
    echo "$(grep -v "$s" doc.txt)" >doc.txt
done

# Check that all implemented settings are defined in the JSON Schema
diff -u impl.txt schema.txt

# Check that all settings defined in the JSON Schema are documented
diff -u schema.txt doc.txt

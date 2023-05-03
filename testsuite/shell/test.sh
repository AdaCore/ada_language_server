#!/bin/sh
jq -r '.contributes.configuration[].properties|keys[]' < $1/integration/vscode/ada/package.json | grep ^ada. > pkg.txt
grep '^## ' $1/doc/settings.md |sed -e 's/.../ada./' | sort > doc.txt
diff -u pkg.txt doc.txt

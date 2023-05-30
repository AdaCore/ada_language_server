#!/bin/sh
python3 get-extension-settings.py < $1/integration/vscode/ada/package.json | grep ^ada. | sort > pkg.txt
grep '^## ' $1/doc/settings.md |sed -e 's/.../ada./' | sort > doc.txt
diff -u pkg.txt doc.txt

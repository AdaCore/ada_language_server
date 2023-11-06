import sys
import json

pkg = json.load(sys.stdin)
settings = []

for group in pkg["contributes"]["configuration"]:
    settings.extend(group["properties"].keys())

print("\n".join(settings))

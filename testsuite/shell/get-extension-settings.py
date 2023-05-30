import sys
import json

pkg = json.load(sys.stdin)
settings = pkg["contributes"]["configuration"][0]["properties"].keys()
print("\n".join(settings))

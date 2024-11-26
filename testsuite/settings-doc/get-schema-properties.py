import json
import sys

schema = json.load(sys.stdin)
properties: dict[str, dict] = schema["properties"]
settings: list[str] = []


def process_property(name: str, value: dict):
    """We want to print nested properties as <name1>.<name2>.<leafname>. This function
    recurses into nested properties to achieve that.
    """
    if value.get("type", None) == "object" and "properties" in value:
        for k, v in value["properties"].items():
            process_property(f"{name}.{k}", v)
    else:
        settings.append(name)


for k, v in properties.items():
    process_property(k, v)

settings = sorted(settings)
print("\n".join(settings))

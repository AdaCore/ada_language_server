import re
import sys

settings = []


text = sys.stdin.read()

# Settings are defined both by `Name = "Foo"` or a call to the function
# `Check_Variable`
simple_setting = re.compile(' Name = "([^"]+)"')
complex_setting = re.compile('^.*Check_Variable\n.*\n?.*"([^"]+)",', re.MULTILINE)

for match in simple_setting.findall(text):
    settings.append(match)

for match in complex_setting.findall(text):
    settings.append(match)


settings = sorted(settings)
print("\n".join(settings))

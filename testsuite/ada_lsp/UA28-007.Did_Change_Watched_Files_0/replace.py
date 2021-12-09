#!/bin/python

import sys
import os

# Arguments: 1) Old Name 2) New Name 3) Target File
if len(sys.argv) != 4:
    raise Program_Error ("Invalid arguments")

file_path=os.path.dirname(sys.argv[3])
if not os.path.exists(file_path):
    raise Program_Error (f"Invalid file path: $filepath")

with open(sys.argv[3], 'r') as file :
  filedata = file.read()

filedata = filedata.replace(sys.argv[1], sys.argv[2])

with open(sys.argv[3], 'w') as file:
  file.write(filedata)

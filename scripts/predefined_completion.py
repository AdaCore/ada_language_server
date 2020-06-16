"""
This script is used to generate an Ada file containing an hard-coded
JSON string that contains all the information for predefined completion
items (pragmas, attributes and aspects).

The script should be laucnhed like this:

   python predefined_completion.py <json_filename>

Where <json_filename> points to a JSON file with the following format:

{
"PREDEFINED_ADA": {
  "ASPECT": [
    {
      "DOC": "This aspect is equivalent to *note pragma Abstract_State: 1c.",
      "_id": "0",
      "_name": "Abstract_State",
      "_origin": "GNAT RM",
      "_category": "unknown"
   },
   ...
  "ATTRIBUTE": [
    {
      "DOC": "Doc for Abort_Signal pragma",
      "_id": "0",
      "_name": "Abort_Signal",
      "_origin": "GNAT RM",
      "_category": "variable"
   },
   ...
   "PRAGMA": [
     {
       "DOC": "Doc for Abort_Defer pragma",
       "_id": "0",
       "_name": "Abort_Defer",
       "_origin": "GNAT RM"
   },
   ...
}
"""

import json
import sys
import os


spec_header = """--  Automatically generated, do not edit.

pragma Style_Checks (Off);

package LSP.Predefined_Completion.%s is

"""

spec_footer = """

end LSP.Predefined_Completion.%s;
"""

def convert_to_ada(json_filename, output_file):
    """
    Convert the JSON file content into an hard-coded Ada string that
    will contain it.
    """
    with open(json_filename, "r") as json_file:
        output = ""

        output = '   Db : constant String := '
        for line in json_file.readlines():
            output += '"' + line.strip().replace('"', '""') + \
                '" & ASCII.LF\n   & '

        output = output[:-6] + ';'
        output_file.write(output)

json_filename = sys.argv[1]
ada_version = os.path.basename(json_filename).replace(".json", "")
ads_filename = "source/ada/generated/lsp-predefined_completion-%s.ads" \
    % ada_version

ads = open(ads_filename, "wb")
ads.write(spec_header % ada_version.title())
convert_to_ada(json_filename, ads)
ads.write(spec_footer % ada_version.title())

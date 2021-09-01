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

MAX_LINES_PER_BIT = 50


def convert_to_ada(json_filename, output_file):
    """
    Convert the JSON file content into an hard-coded Ada string that
    will contain it.
    """

    line_counter = 0
    statement_counter = 1
    with open(json_filename, "r") as json_file:
        output = ""

        for line in json_file.readlines():
            if line_counter % MAX_LINES_PER_BIT == 0:
                if statement_counter != 1:
                    output = output[:-6] + ";"
                output += f"\n\n   Db{statement_counter} : constant String := "
                statement_counter += 1

            output += '"' + line.strip().replace('"', '""') + '"\n   & '

            line_counter += 1

        output = (
            output[:-6]
            + f';\n\n   Db : constant String := {" & ".join([f"Db{x}" for x in range(1, statement_counter)])};'
        )

        output_file.write(output)


json_filename = sys.argv[1]
ada_version = os.path.basename(json_filename).replace(".json", "")
ads_filename = "source/ada/generated/lsp-predefined_completion-%s.ads" % ada_version

ads = open(ads_filename, "w")
ads.write(spec_header % ada_version.title())
convert_to_ada(json_filename, ads)
ads.write(spec_footer % ada_version.title())

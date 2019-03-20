""" Command line program that runs a single test.
    The test should be passed as parameter, as the path to a test file.
"""
import sys
import runner

test_file = sys.argv[1]

errors = runner.run_single_test(test_file)

# Print the errors, if any
if errors:
    for x in errors:
        print x
        print

else:
    print "SUCCESS"

# Developing on the Ada Language Server

## Debugging

You can activate traces that show all the server input/output. This is done
by creating a file `$HOME/.als/traces.cfg` with the following contents:

```
ALS.IN=yes > inout.txt:buffer_size=0
ALS.OUT=yes > inout.txt:buffer_size=0
```

When this is present, the ALS will generate a file `$HOME/.als/inout.txt`
which logs the input received and the output sent by the language server.

## Writing tests

To write a functional test for Ada Language Server:

  * Choose a meaninful name for your test, for instance `completion_inside_generics`. 
     We'll refer to this as `<testname>` below
  * Create a new directory `testsuite/ada_lsp/<testname>` containing your test data
  * Activate full in/out language server traces. See the Debugging section above.
  * From the base directory in this repository, run:
     ```
     python scripts/traces_to_test.py <testname> <path_to_the_als_traces_file>
     ```

## Running tests

 * run `make check` to run the entire testsuite
 * to run an individual test, go to `testsuite` and run `sh run.sh ada_lsp/<testname>`
    (you will need `https://github.com/AdaCore/e3-testsuite` installed to do this)

## Other tests

See more about the project testsuite [here](../testsuite/README.md).

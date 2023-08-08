# Requirements

Have a working Ada environment (with `gnatchop`, `gnatpp` binaries) and
install the program with `alire`:

```shell
git clone https://github.com/AdaCore/VSS.git
cd VSS/tools/json_schema
alr build
```

# How to use

One have to be in the `lsp_gen` directory and and have `gnatpp`, `gnatchop`, `gen_json` in the `PATH`.

From there one can generate the code with a single `make` command.

# Building Ada Language Server from sources

1. Install the following tools and add them to the `PATH` environment variable:
   - Alire >= 2.0.2
   - Python >= 3.11
   - NodeJS >= 20.18.3

1. Disable dependency sharing in Alire

       alr settings --global --set dependencies.shared false

1. Clone repository

       git clone https://github.com/AdaCore/ada_language_server.git
       cd ada_language_server

1. If you know dependency commits numbers then create `deps.txt` file
   with lines `<repo_name>=commit`. Otherwise checkout `master` branch.

1. Use `alr toolchain --select` to install the latest available versions of `gnat` and `gprbuild`.

1. Run `build_als.sh` script

       ./scripts/build_als.sh

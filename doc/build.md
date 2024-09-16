# Building Ada Language Server from sources

1. Install Alire, Python 3, NodeJS. Add them to `PATH`.

2. Disable dependency sharing in Alire

       alr settings --global --set dependencies.shared false

3. Clone repository

       git clone https://github.com/AdaCore/ada_language_server.git
       cd ada_language_server

4. If you know dependency commits numbers then create `deps.txt` file
   with lines `<repo_name>=commit`. Otherwise checkout `edge` branch.

5. Build scripts installs Python modules. So it's better to activate
   `venv` for Python:

       python -m venv venv
       source venv/bin/activate

5. Run `build_als.sh` script

       ./scripts/build_als.sh

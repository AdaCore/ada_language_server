# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = "Ada & SPARK extension for VS Code"
project_copyright = "2024-2025, AdaCore"
author = "AdaCore"

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = [
    "myst_parser",
]

exclude_patterns = [
    "_build",
    "Thumbs.db",
    ".DS_Store",
    # 'ada_language_server.wiki/Custom-colors-in-VS-Code.md',
    # 'ada_language_server.wiki/Getting-Started.md',
    "ada_language_server.wiki/Home.md",
    # 'ada_language_server.wiki/Project-file-editing.md',
    # 'ada_language_server.wiki/Set-workspace-specific-environment-variables.md',
    "ada_language_server.wiki/Supported-LSP-requests.md",
    # 'ada_language_server.wiki/Working-on-a-remote-machine.md',
    # 'build.md',
    "gitpod.md",
    "protocol.md",
    "specification-3-14.md",
    "specification-3-15.md",
    "specification-3-16.md",
]

# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = "sphinx_rtd_theme"

myst_heading_anchors = 3

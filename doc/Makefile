# Minimal makefile for Sphinx documentation
#
prefix = $(PWD)
docdir = $(prefix)/share/doc/als
MKDIR = mkdir -p
CP = cp -p

# You can set these variables from the command line, and also
# from the environment for the first two.
SPHINXOPTS    ?=
SPHINXBUILD   ?= sphinx-build
SOURCEDIR     = .
BUILDDIR      = _build

# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help Makefile

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

install:
	$(MKDIR) $(docdir)/html

	${MKDIR} $(docdir)/html/users_guide/
	-${CP} -r _build/html/* $(docdir)/html/users_guide/


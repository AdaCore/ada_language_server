# Server executable file:
ROOTDIR=$(shell pwd)

# ALS version
VERSION ?=

# ALS build date
BUILD_DATE ?=

# Define platform-specific variables
ifeq ($(OS),Windows_NT)
   PYTHON=python.exe
   EXE=.exe
else
   UNAME_S := $(shell uname -s)
   ifeq ($(UNAME_S),Linux)
      OS=unix
   else ifeq ($(UNAME_S),Darwin)
      OS=osx
   endif
   PYTHON=python3
   EXE=
endif

# Location of home dir for tests
export ALS_HOME=$(ROOTDIR)/testsuite

# Command to run for tests
export ALS=$(ROOTDIR)/.obj/server/ada_language_server$(EXE)

# Tester files
TESTER=$(ROOTDIR)/.obj/tester/tester-run$(EXE)

# Env variable to set for update VS Code test references
MOCHA_ALS_UPDATE=

GPRBUILD_EXTRA=
GPRBUILD_FLAGS=-j0 $(GPRBUILD_EXTRA)
GPRBUILD=gprbuild $(GPRBUILD_FLAGS) -XSUPERPROJECT=
GPRCLEAN_EXTRA=
GPRCLEAN=gprclean -XSUPERPROJECT= $(GPRCLEAN_EXTRA)

# Installation directory
prefix ?= /usr/local
ifeq ($(DESTDIR),)
  DESTDIR=$(prefix)
endif

# Library type
LIBRARY_TYPE?=relocatable

# Build mode (dev or prod)
BUILD_MODE?=dev

# Whether to enable coverage (empty for no, any other value for yes)
COVERAGE=
COVERAGE_INSTR=gnatcov instrument --level stmt $(LIBRARY_FLAGS) \
	--dump-trigger=atexit --no-subprojects

ifeq (,$(shell which node))
   # Node is not available so do not define node-related variables
else
   # Set NODE variable as an indicator that node is available
   NODE=node
   # Obtain architecture and platform from the node executable. This information
   # is used to deposit the ALS executable at the location expected by the VS
   # Code Ada extension.
   NODE_ARCH=$(shell node -e "console.log(process.arch)")
   NODE_PLATFORM=$(shell node -e "console.log(process.platform)")
endif

VSCE=npx vsce

LIBRARY_FLAGS=-XBUILD_MODE=$(BUILD_MODE)	\
              -XOS=$(OS)			\
              -XLIBRARY_TYPE=$(LIBRARY_TYPE)	\
              -XXMLADA_BUILD=$(LIBRARY_TYPE)	\
	      -XGPR_BUILD=$(LIBRARY_TYPE)

BUILD_FLAGS=$(LIBRARY_FLAGS)

ifeq ($(COVERAGE),)
	COVERAGE_BUILD_FLAGS= $(LIBRARY_FLAGS)
else
	COVERAGE_BUILD_FLAGS= $(LIBRARY_FLAGS) \
		--implicit-with=gnatcov_rts \
		--src-subdirs=gnatcov-instr \
		-XALS_WARN_ERRORS=false \
		-XSPAWN_WARN_ERRORS=false \
		-gnatyN
endif

all: coverage-instrument
	$(GPRBUILD) -P gnat/lsp_3_17.gpr -p $(COVERAGE_BUILD_FLAGS) -c lsp-inputs.adb
	$(GPRBUILD) -P gnat/lsp_3_17.gpr -p $(COVERAGE_BUILD_FLAGS)
	$(GPRBUILD) -P gnat/tester.gpr -p $(BUILD_FLAGS)
	$(GPRBUILD) -d -ws -c -u -P gnat/lsp_server.gpr -p $(BUILD_FLAGS) s-memory.adb
	$(GPRBUILD) -P gnat/lsp_server.gpr -p $(COVERAGE_BUILD_FLAGS) \
		-XVERSION=$(VERSION) -XBUILD_DATE=$(BUILD_DATE)
	$(GPRBUILD) -P gnat/lsp_client.gpr -p $(COVERAGE_BUILD_FLAGS)
ifdef NODE
	mkdir -p integration/vscode/ada/$(NODE_ARCH)/$(NODE_PLATFORM)
	cp -f $(ALS) integration/vscode/ada/$(NODE_ARCH)/$(NODE_PLATFORM)
endif

generate:
	python scripts/generate.py

generate_io:
	python scripts/io_gen.py

coverage-instrument:
ifneq ($(COVERAGE),)
	# Remove artifacts from previous instrumentations, so that stale units that
	# are not overriden by new ones don't get in our way.
	rm -rf .obj/*/gnatcov-instr
	$(COVERAGE_INSTR) -XVERSION=$(VERSION) -XBUILD_DATE=$(BUILD_DATE) \
		-Pgnat/lsp_server.gpr --projects lsp_server --projects lsp_3_17
endif

install:
	gprinstall -f -P gnat/lsp_server.gpr -p -r --mode=usage \
		--prefix=$(DESTDIR) $(LIBRARY_FLAGS)
	gprinstall -f -P gnat/tester.gpr -p --prefix=$(DESTDIR) $(LIBRARY_FLAGS)
	gprinstall -f -P gnat/lsp_client.gpr -p -r	\
		--mode=dev				\
		--prefix=$(DESTDIR)			\
		$(LIBRARY_FLAGS)
ifneq ($(COVERAGE),)
	mkdir -p $(DESTDIR)/share/als/sids || true
	cp .obj/*/*.sid $(DESTDIR)/share/als/sids/
endif

clean:
	-$(GPRCLEAN) -P gnat/lsp.gpr $(LIBRARY_FLAGS)
	-$(GPRCLEAN) -P gnat/lsp_3_17.gpr $(LIBRARY_FLAGS)
	-$(GPRCLEAN) -P gnat/lsp_server.gpr $(LIBRARY_FLAGS)
	-$(GPRCLEAN) -P gnat/tester.gpr $(LIBRARY_FLAGS)
	-rm -rf integration/vscode/ada/$(NODE_ARCH)/$(NODE_PLATFORM)

vscode:
ifneq ($(npm_config_offline),true)
# These commands may try to contact remote servers so if npm is configured to
# run in offline mode, don't bother running them
	cd integration/vscode/ada; LD_LIBRARY_PATH= npm install --no-audit
	cd integration/vscode/ada; LD_LIBRARY_PATH= npm run check-licenses
endif
	cd integration/vscode/ada; LD_LIBRARY_PATH= npm run cilint
	cd integration/vscode/ada; LD_LIBRARY_PATH= npm run compile
	@echo Now run:
	@echo code --extensionDevelopmentPath=`pwd`/integration/vscode/ada/ `pwd`

vscode-test:
	# Run the VS Code integration testsuite.
	MOCHA_ALS_UPDATE=$(MOCHA_ALS_UPDATE) cd integration/vscode/ada; LD_LIBRARY_PATH= npm run test

vscode-package:
	cd integration/vscode/ada; LD_LIBRARY_PATH= $(VSCE) package

check: all
	set -e; \
        export PYTHON=$(PYTHON); \
        if [ `$(PYTHON) -c "import sys;print('e3' in sys.modules)"` = "True" ]; then\
           (cd testsuite ; sh run.sh $(test)) ; \
        else \
           for a in testsuite/*_lsp/*/*.json; do \
              echo $$a ; \
              (cd `dirname $$a ` ; $(TESTER) `basename $$a`) ;\
           done; \
        fi

deploy: check
	integration/$(USER)/deploy.sh $(NODE_PLATFORM)

# Instantiates the VS Code workspace with default settings
configure:
	cp .vscode/settings.json.tmpl .vscode/settings.json

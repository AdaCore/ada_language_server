# Server executable file:
export ALS=$(shell pwd)/.obj/server/ada_language_server

# Tester files
TESTER=$(shell pwd)/.obj/tester/tester-run
CODEC_TEST=.obj/codec_test/codec_test

# Testsuite directory
TD=testsuite/ada_lsp

GPRBUILD=gprbuild -j0

# Installation directory
DESTDIR=

# Library type
LIBRARY_TYPE=relocatable

# Build mode (dev or prod)
BUILD_MODE=dev

# Whether to enable coverage (empty for no, any other value for yes)
COVERAGE=
COVERAGE_INSTR=gnatcov instrument --level stmt $(LIBRARY_FLAGS) \
	--dump-method=atexit

# Target platform as nodejs reports it
ifeq ($(OS),Windows_NT)
   PLATFORM=win32
else
   UNAME_S := $(shell uname -s)
   ifeq ($(UNAME_S),Linux)
      PLATFORM=linux
      OS=unix
   endif
   ifeq ($(UNAME_S),Darwin)
      PLATFORM=darwin
      OS=osx
   endif
endif

ifneq ($(COVERAGE),)
	COVERAGE_BUILD_FLAGS=\
		--implicit-with=gnatcov_rts_full \
		--src-subdirs=gnatcov-instr \
		-XALS_WARN_ERRORS=false \
		-XSPAWN_WARN_ERRORS=false \
		-gnatyN
endif

ifeq ($(LIBRARY_TYPE), static)
    LIBRARY_FLAGS=-XBUILD_MODE=$(BUILD_MODE) \
		  -XLIBRARY_TYPE=static \
		  -XXMLADA_BUILD=static \
		  -XGPR_BUILD=static \
		  -XOS=$(OS)
else
    LIBRARY_FLAGS=-XBUILD_MODE=$(BUILD_MODE) -XOS=$(OS)
endif

BUILD_FLAGS=$(LIBRARY_FLAGS) $(COVERAGE_BUILD_FLAGS)

all: coverage-instrument
	$(GPRBUILD) -P gnat/lsp.gpr -p $(BUILD_FLAGS)
	$(GPRBUILD) -P gnat/lsp_server.gpr -p $(BUILD_FLAGS) -XVERSION=$(TRAVIS_TAG)
	$(GPRBUILD) -P gnat/lsp_client.gpr -p $(BUILD_FLAGS)
	$(GPRBUILD) -P gnat/spawn_tests.gpr -p $(BUILD_FLAGS)
	$(GPRBUILD) -P gnat/tester.gpr -p $(BUILD_FLAGS)
	$(GPRBUILD) -P gnat/codec_test.gpr -p $(BUILD_FLAGS)

	mkdir -p integration/vscode/ada/$(PLATFORM)
	cp -f .obj/server/ada_language_server integration/vscode/ada/$(PLATFORM) ||\
	  cp -f .obj/server/ada_language_server.exe integration/vscode/ada/$(PLATFORM)

generate:
	python scripts/generate.py

coverage-instrument:
ifneq ($(COVERAGE),)
	# Remove artifacts from previous instrumentations, so that stale units that
	# are not overriden by new ones don't get in our way.
	rm -rf .obj/*/gnatcov-instr
	$(COVERAGE_INSTR) -Pgnat/lsp.gpr
	$(COVERAGE_INSTR) -XVERSION=$(TRAVIS_TAG) \
		-Pgnat/lsp_server.gpr --projects lsp_server --projects lsp
	$(COVERAGE_INSTR) -Pgnat/lsp_client.gpr --projects lsp
	$(COVERAGE_INSTR) -Pgnat/tester.gpr --projects lsp
	$(COVERAGE_INSTR) -Pgnat/codec_test.gpr --projects lsp
endif

install:
	gprinstall -f -P gnat/lsp_server.gpr -p -r --prefix=$(DESTDIR) $(LIBRARY_FLAGS)
	gprinstall -f -P gnat/tester.gpr -p --prefix=$(DESTDIR) $(LIBRARY_FLAGS)
	gprinstall -f -P gnat/codec_test.gpr -p --prefix=$(DESTDIR) $(LIBRARY_FLAGS)
ifneq ($(COVERAGE),)
	mkdir -p $(DESTDIR)/share/als/sids || true
	cp .obj/*/*.sid $(DESTDIR)/share/als/sids/
endif

clean:
	gprclean -P gnat/lsp.gpr $(LIBRARY_FLAGS)
	gprclean -P gnat/lsp_server.gpr $(LIBRARY_FLAGS)
	gprclean -P gnat/spawn_tests.gpr $(LIBRARY_FLAGS)
	gprclean -P gnat/tester.gpr $(LIBRARY_FLAGS)
	gprclean -P gnat/codec_test.gpr $(LIBRARY_FLAGS)
	rm -rf integration/vscode/ada/$(PLATFORM)

vscode:
	cd integration/vscode/ada; npm install
	@echo Now run:
	@echo code --extensionDevelopmentPath=`pwd`/integration/vscode/ada/ `pwd`

check: all
	set -e; \
        if [ `python -c "import sys;print 'e3' in sys.modules"` = "True" ]; then\
           (cd testsuite ; sh run.sh ) ; \
        else \
           for a in $(TD)/*/*.json; do \
              echo $$a ; \
              (cd `dirname $$a ` ; $(TESTER) `basename $$a`) ;\
           done; \
        fi
	${CODEC_TEST} < testsuite/codecs/index.txt

deploy: check
	integration/$(USER)/deploy.sh $(PLATFORM)

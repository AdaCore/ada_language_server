# Server executable file:
export ALS=.obj/server/ada_language_server

# Tester file
TESTER=.obj/tester/tester-run

# Testsuite directory
TD=testsuite/ada_lsp

GPRBUILD=gprbuild -j0

# Installation directory
DESTDIR=

# Target platform as nodejs reports it
ifeq ($(OS),Windows_NT)
   PLATFORM=win32
else
   UNAME_S := $(shell uname -s)
   ifeq ($(UNAME_S),Linux)
      PLATFORM=linux
   endif
   ifeq ($(UNAME_S),Darwin)
      PLATFORM=darwin
   endif
endif

LIBRARY_FLAGS=-XLIBRARY_TYPE=static -XXMLADA_BUILD=static -XGPR_BUILD=static

all:
	$(GPRBUILD) -P gnat/lsp.gpr -p $(LIBRARY_FLAGS)
	$(GPRBUILD) -P gnat/lsp_server.gpr -p $(LIBRARY_FLAGS)
	$(GPRBUILD) -P gnat/spawn_tests.gpr -p $(LIBRARY_FLAGS)
	$(GPRBUILD) -P gnat/tester.gpr -p $(LIBRARY_FLAGS)
	mkdir -p integration/vscode/ada/$(PLATFORM)
	cp -f .obj/server/ada_language_server integration/vscode/ada/$(PLATFORM) ||\
	  cp -f .obj/server/ada_language_server.exe integration/vscode/ada/$(PLATFORM)

install:
	gprinstall -P gnat/lsp_server.gpr -p -r --prefix=$(DESTDIR)

clean:
	rm -rf .obj/*.* .obj/server/* .obj/lsp/* integration/vscode/ada/$(PLATFORM)

vscode:
	cd integration/vscode/ada; npm install
	@echo Now run:
	@echo code --extensionDevelopmentPath=`pwd`/integration/vscode/ada/ `pwd`

check: all
	set -e; for a in $(TD)/*/*.json; do echo $$a ; $(TESTER) $$a ; done

deploy: check
	integration/$(USER)/deploy.sh $(PLATFORM)

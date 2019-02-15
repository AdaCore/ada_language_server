TESTER=.obj/tester/tester-run
TD=testsuite/ada_lsp
GPRBUILD=gprbuild -j0
DESTDIR=
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

all:
	$(GPRBUILD) -P gnat/lsp.gpr -p
	$(GPRBUILD) -P gnat/lsp_server.gpr -p
	$(GPRBUILD) -P gnat/spawn_tests.gpr -p
	$(GPRBUILD) -P gnat/tester.gpr -p
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
	for a in $(TD)/*/*.json; do echo $$a ; $(TESTER) $$a ; done

deploy: check
	integration/$(USER)/deploy.sh $(PLATFORM)

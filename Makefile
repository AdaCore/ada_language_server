TESTER=.obj/tester/tester-run
TD=testsuite/ada_lsp
GPRBUILD=gprbuild -j0
DESTDIR=

all:
	$(GPRBUILD) -P gnat/lsp.gpr -p
	$(GPRBUILD) -P gnat/lsp_server.gpr -p
	$(GPRBUILD) -P gnat/spawn_tests.gpr -p
	$(GPRBUILD) -P gnat/tester.gpr -p

install:
	gprinstall -P gnat/lsp_server.gpr -p -r --prefix=$(DESTDIR)

clean:
	rm -rf .obj/*.* .obj/server/* .obj/lsp/* integration/vscode/ada/$(PLATFORM)

vscode:
	cd integration/vscode/ada; npm install
	@echo Now run:
	@echo code --extensionDevelopmentPath=`pwd`/integration/vscode/ada/ `pwd`
	PLATFORM=$(shell node -e 'console.log(require("process").platform)') ;\
          mkdir -p integration/vscode/ada/$(PLATFORM) && \
          cp -f .obj/server/ada_language_server integration/vscode/ada/$(PLATFORM) ||\
          cp -f .obj/server/ada_language_server.exe integration/vscode/ada/$(PLATFORM)

check: all
	$(TESTER) $(TD)/0001-start_stop.json
	$(TESTER) $(TD)/0002-shutdown.json
	$(TESTER) $(TD)/0003-get_symbols.json
	$(TESTER) $(TD)/def_name.json
	$(TESTER) $(TD)/project_search.json
	$(TESTER) $(TD)/project_config.json
	$(TESTER) $(TD)/project_config_2.json
	$(TESTER) $(TD)/uri_with_slash.json
	@echo All test passed!

deploy: check
	integration/$(USER)/deploy.sh $(PLATFORM)

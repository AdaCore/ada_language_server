TESTER=.obj/tester/tester-run
TD=testsuite/ada_lsp
GPRBUILD=gprbuild -j0

all:
	$(GPRBUILD) -P gnat/lsp.gpr -p
	$(GPRBUILD) -P gnat/lsp_server.gpr -p
	$(GPRBUILD) -P gnat/spawn_tests.gpr -p
	$(GPRBUILD) -P gnat/tester.gpr -p
	rm -rf integration/vscode/ada/server
	ln -s ../../../.obj/server/lsp-ada_driver integration/vscode/ada/server

clean:
	rm -rf .obj/*.* .obj/server/* .obj/lsp/* integration/vscode/ada/server

vscode:
	cd integration/vscode/ada; npm install
	@echo Now run:
	@echo code --extensionDevelopmentPath=`pwd`/integration/vscode/ada/ `pwd`

check: all
	$(TESTER) $(TD)/0001-start_stop.json
	$(TESTER) $(TD)/0002-shutdown.json
	$(TESTER) $(TD)/0003-get_symbols.json
	@echo All test passed!

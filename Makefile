TESTER=.obj/tester/tester-run
TD=testsuite/ada_lsp

all:
	gprbuild -P gnat/lsp.gpr -p
	gprbuild -P gnat/lsp_server.gpr -p
	gprbuild -P gnat/spawn_tests.gpr -p
	gprbuild -P gnat/tester.gpr -p
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
	@echo All test passed!

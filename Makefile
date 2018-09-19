all:
	gprbuild -P gnat/lsp.gpr -p
	gprbuild -P gnat/lsp_server.gpr -p
	ln -s ../../../.obj/server/lsp-ada_driver integration/vscode/ada/server

clean:
	rm -rf .obj/*.* .obj/server/* .obj/lsp/* integration/vscode/ada/server

vscode:
	cd integration/vscode/ada; npm install
	@echo Now run:
	@echo code --extensionDevelopmentPath=`pwd`/integration/vscode/ada/ `pwd`


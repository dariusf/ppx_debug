
export OCAMLRUNPARAM=b

.PHONY: all
all:
	dune test --display=short
	rm /tmp/ppx_debug.txt > /dev/null 2>&1 || true
	rm debug.trace > /dev/null 2>&1 || true
	rm chrome.json > /dev/null 2>&1 || true
	dune exec --display=short ./demo/app/app.exe
	dune exec --display=short ./demo/debug/debug.exe > chrome.json

.PHONY: w
w:
	dune exec --watch --display=short --terminal-persistence=clear-on-rebuild ./demo/debug/debug.exe

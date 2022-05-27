
export OCAMLRUNPARAM=b

.PHONY: all
all:
	dune test --display=short
	# dune build @install
	rm /tmp/ppx_debug.txt || true
	rm debug.trace || true
	dune exec --display=short ./demo/app/app.exe
	# dune build @check
	dune exec --display=short ./demo/debug/debug.exe | tee chrome.json | jq .

.PHONY: w
w:
	dune exec --watch --display=short --terminal-persistence=clear-on-rebuild ./demo/debug/debug.exe

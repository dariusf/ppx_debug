
export OCAMLRUNPARAM=b

.PHONY: all
all:
	dune test --display=short
	# dune build @install
	dune exec --display=short ./demo/app/app.exe
	dune build @check
	dune exec --display=short ./demo/debug/debug.exe | tee chrome.json | jq .

.PHONY: w
w:
	dune exec ./demo/debug/debug.exe
	# --watch --display=short --terminal-persistence=clear-on-rebuild

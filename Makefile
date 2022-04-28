
export OCAMLRUNPARAM=b

.PHONY: all
all:
	dune test --display=short
# integration test
	rm out.bin || true
	dune exec --display=short --instrument-with ppx_debug ./demo/app/app.exe
	dune build @check
	dune exec --display=short ./demo/debug/debug.exe | tee chrome.json | jq .

.PHONY: w
w:
	dune exec ./demo/debug/debug.exe
	# --watch --display=short --terminal-persistence=clear-on-rebuild

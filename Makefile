
export OCAMLRUNPARAM=b
export PPX_DEBUG=$(shell jq -c < config.json)

.PHONY: all
all:
	dune test --display=short
	rm /tmp/ppx_debug* debugger.json chrome.json *.trace > /dev/null 2>&1 || true

	dune exec ./demo/app/app.exe
	dune exec ./demo/debug/debug.exe -- debug.trace > chrome.json
	dune exec ./demo/debug/debug.exe -- debug.trace -f debugger > debugger.json
	dune exec ./demo/debug/debug.exe -- debug.trace -f calls | head -n 2

	ls chrome.json debugger.json *.trace /tmp/ppx_debug* || true

.PHONY: debug
debug: all
	dune exec --display=short ./demo/debug/repl.bc
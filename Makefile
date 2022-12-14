
export OCAMLRUNPARAM=b
export PPX_DEBUG=$(shell cat config.json)
#export PPX_DEBUG=$(shell jq -c < config.json)

.PHONY: all
all:
	dune test
	rm /tmp/ppx_debug* debugger.json chrome.json *.trace > /dev/null 2>&1 || true

	dune exec ./demo/app/app.exe
	dune exec ./demo/debug/debug.exe -- trace debug.trace > chrome.json
	dune exec ./demo/debug/debug.exe -- trace debug.trace -f debugger > debugger.json
	dune exec ./demo/debug/debug.exe -- trace debug.trace -f calls | head -n 6
	dune exec ./demo/debug/debug.exe -- trace debug.trace -f tree | head -n 6

	ls chrome.json debugger.json *.trace /tmp/ppx_debug* || true

.PHONY: debug
debug: all
	dune exec ./demo/debug/repl.bc -- repl debug.trace -i 6

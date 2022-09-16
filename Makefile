
export OCAMLRUNPARAM=b
# export PPX_DEBUG=$(shell jq -c < config.json)

.PHONY: all
all:
	dune exec ./demo/app/app.exe
	dune exec ./demo/debug/debug.exe debug.trace > chrome.json
	dune exec ./demo/debug/debug.exe -- debug.trace -f debugger > debugger.json

.PHONY: all1
all1:
	dune test --display=short
	rm /tmp/ppx_debug.txt chrome.json debugger.json debug.trace > /dev/null 2>&1 || true
	dune exec --display=short ./demo/app/app.exe
	dune exec --display=short ./demo/debug/debug.exe debug.trace > chrome.json
	dune exec --display=short ./demo/debug/debug.exe -- debug.trace -f debugger > debugger.json
	ls chrome.json debugger.json debug.trace

.PHONY: debug
debug:
	dune exec --display=short ./demo/debug/debug.bc

.PHONY: w
w:
	dune exec --watch --display=short --terminal-persistence=clear-on-rebuild ./demo/debug/debug.exe

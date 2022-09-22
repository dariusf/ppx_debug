
# ppx_debug

![](docs/insertion-sort.png)

A collection of tools for record-and-replay debugging.

This works by instrumenting a program using ppx, running the program to record an execution, then analyzing the execution using editor plugins (which provide an interface like that of an interactive debugger), [Perfetto](https://ui.perfetto.dev/)/[magic-trace](https://magic-trace.org/), the OCaml toplevel, or CLI tools.

- [Docs](docs/docs.md) (start here)
- [Demo project](demo)
- [OCaml 2022](https://icfp22.sigplan.org/home/ocaml-2022) [talk](youtube), [abstract](todo)

**This is an early prototype. Feel free to try it on your projects, but expect rough edges. Contributions are very welcome!**
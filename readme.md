
# ppx_debug

A small framework for application-level tracing and omniscient debugging, intended for exploring code with complex functions and datatypes (e.g. PL tools).

## How does it work?

1. A library to be explored is instrumented with a ppx to log function arguments, return values, and other program events to a file using Marshal.
2.  A separate executable is then compiled using a _second_ ppx (which reads the cmt files of the library at compile time) to interpret the traces.
3. Running this executable produces trace files in many formats, which can be inspected in tools like [magic-trace](https://magic-trace.org/) and our VSCode plugin for [omniscient debugging](https://www.computer.org/csdl/magazine/so/2009/06/mso2009060078/13rRUwvBy70).

## More info

- Short paper
- [OCaml workshop talk](https://icfp22.sigplan.org/home/ocaml-2022)
- [Demo](demo)
- [Docs](docs.md)

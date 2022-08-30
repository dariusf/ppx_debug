
- lib: user code to be debugged
- app: frontend for lib, assumed bug-free
- ppx_debug: used when compiling lib to generate tracing code
- ppx_debug_runtime: used at runtime in lib by code generated by ppx_debug
- debug: frontend for reading traces, depends on lib
- ppx_debug_tool: used by debug to read type info produced by compilation of lib

Fills the niche an interactive debugger would fill in other languages

A small framework for _application-level tracing_, intended for exploring and debugging code with complex recursive functions (i.e. PL tools).

Annotate code like this:

```ocaml
let[@trace] f xs =
  List.length xs + 1

let () =
  f ([1] @ [2]) |> ignore
```

and in the resulting trace, you'll see that the function has the argument `[1; 2]` and result `3`.

```bash
echo todo
```

This would be unsurprising in any language other than OCaml, where there is no polymorphic show or runtime type metadata, and because staged_pps isn't being used.

See the [demo project](demo) for the required setup and assumptions.

# How it works:

1. User code is first compiled with a (non-staged) ppx to instrument functions. This dumps values using [Marshal](https://ocaml.org/api/Stdlib.Marshal.html).
2. A separate executable is created using a _second_ ppx, which reads the cmt files of the code from step 1 during compilation to figure out how to interpret the dumped values. This is similar to how trace formats like [CTF](https://diamon.org/ctf/) separate trace content from metadata. Specializing this to OCaml, _types_ are not needed to dump values, only to interpret them.
3. The executable from step 2 is run on the trace to either query it or convert it into a form that can be explored using [magic-trace](https://magic-trace.org/) or [Perfetto](https://ui.perfetto.dev/).

# Why do it this way?

- Separating trace content from schema reduces runtime overhead, i.e. there are reasons to do this in other languages, and even with modular implicits.
- Not using staged_pps reduces compilation time when the trace isn't read.
- A form of staging is still used (compiling a dependent second executable), but with an explicit separation of phases that's arguably easier to understand. From the current [docs](https://dune.readthedocs.io/en/stable/concepts.html#preprocessing-specification), it's not clear if reading the cmt file of a compilation unit during preprocessing is allowed.
- The typechecker is not run twice. This seems hard to avoid with staged_pps as ppx-generated code has to be validated.
- It's unclear if there is sufficient information available at runtime to dump data structures with something like an [instrumented bytecode interpreter](https://ocaml.org/releases/4.14/htmlman/instrumented-runtime.html) or [gdb](https://mshinwell.github.io/libmonda/). The maintenance burden seems higher, though.
- We don't use Dune's instrumentation because it's not meant to remain in the final executable, so the generated code doesn't end up in the cmt files as we need. Instead tracing may be disabled via environment variable.

# How is this different from...

- [magic-trace](https://github.com/janestreet/magic-trace): performance-oriented, so no support for interpreting app data structures. Intended to be used without recompiling and will show low-level function calls using Intel PT, whereas we require instrumentation in order to be able to display values faithfully and are interested in user code, not libraries.
- [landmarks](https://github.com/LexiFi/landmarks): performance-oriented. Contains a binding to x86/x64 [TSC](https://github.com/LexiFi/landmarks#the-clock-function). We also measure timing but don't focus too much on performance, as magic-trace fills that niche.
- [genprintlib](https://github.com/progman1/genprintlib): uses staged_pps to implement a polymorphic print function, so is an exemplar of that approach.
- [ppx_polyprint](https://github.com/dariusf/ppx_polyprint): precursor to this project.

<!--
https://github.com/EduardoRFS/typedppxlib
https://github.com/chetmurthy/typpx

https://github.com/krohrer/caml-inspect
https://github.com/ocaml-batteries-team/batteries-included/blob/master/src/batPervasives.ml
-->

# Bootstrapping

Running ppx_debug on itself is possible but rather involved:

- Clone the main repo locally. Apply the patch in branch `bootstrap1` to rename the ppx (appending 1 to the end of all conflicting names, as programmers do).
- Symlink the clone into the main repo under `debug1`.
- Development in the main repo happens on the `bootstrap` branch, which is ahead of its `master` in two ways. First is a patch for enabling bootstrapping (enabling the ppx on itself). Next are local changes.
- To test a change, make a commit on (`bootstrap`, then cherry pick it onto) `master`, then rebase `bootstrap` onto master. Have the clone pull changes from `master`, which will rebase its patch. Run tests.

# FAQ

- Unbound module during compilation, after tool ppx has run, with puzzling line number: our heuristics are probably not good enough to figure out how to access a type from outside its use site. Manually configure how to access the type from outside the library.

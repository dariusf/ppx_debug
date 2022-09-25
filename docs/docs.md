
- [Getting started](#getting-started)
- [The demo project](#the-demo-project)
- [How do I use this in my project?](#how-do-i-use-this-in-my-project)
- [Tips, FAQs, and known issues](#tips-faqs-and-known-issues)
- [Other approaches](#other-approaches)
- [Project structure](#project-structure)
- [Bootstrapping](#bootstrapping)

# Getting started

Check out the links in the [readme](../readme.md) if you haven't already.

We'll start by building ppx_debug, running tests, then running it on the demo project.

```sh
git clone git@github.com:dariusf/ppx_debug.git
cd ppx_debug
opam install . --deps-only
make
```

What just happened? We built, instrumented, and ran the demo project, and a record of its execution was written to the file debug.trace. If you peek inside it, you'll see that it contains binary data -- marshalled OCaml values.

Several commands were then run to interpret this binary file and export it to various formats.

Two of them, *calls* and *tree*, are human-readable.

*calls* shows one function invocation per line, with inputs and outputs, ordering calls before their children.

```sh
dune exec ./demo/debug/debug.exe -- trace debug.trace -f calls | head -n 6
    1 demo/lib/lib.ml:40 main = ()
    6 demo/lib/lib.ml:25 consume (t: (Misc 1)) = 1
   13 demo/lib/lib.ml:11 depth (t: (Node [(Node [(Leaf 1)]); (Leaf 2)])) = 2
   18 demo/lib/lib.ml:14 _lambda (c: (Leaf 2)), (t: 0) = 0
   21 demo/lib/lib.ml:11 depth (t: (Leaf 2)) = 0
   26 demo/lib/lib.ml:14 _lambda (c: (Node [(Leaf 1)])), (t: 0) = 1
```

*tree* is a call tree, using indentation to indicate nesting

```sh
dune exec ./demo/debug/debug.exe -- trace debug.trace -f tree | head -n 6
(1) demo/lib/lib.ml:40 main
  (6) demo/lib/lib.ml:25 consume (t: (Misc 1))
  (6) demo/lib/lib.ml:25 consume = 1
  (13) demo/lib/lib.ml:11 depth (t: (Node [(Node [(Leaf 1)]); (Leaf 2)]))
    (18) demo/lib/lib.ml:14 _lambda (c: (Leaf 2)), (t: 0)
      (21) demo/lib/lib.ml:11 depth (t: (Leaf 2))
```

The numbers are _timestamps_, which identify points in the execution and can be used to navigate to them.

The other two files are the inputs to tools.

```sh
dune exec ./demo/debug/debug.exe -- trace debug.trace > chrome.json
dune exec ./demo/debug/debug.exe -- trace debug.trace -f debugger > debugger.json
```

chrome.json is the execution in [Chrome Trace Format](https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/preview), which can be read by [chrome://tracing](chrome://tracing), [Perfetto](https://ui.perfetto.dev/), or [magic-trace](https://magic-trace.org/).
Try opening it in one of these tools!

debugger.json can be read by an [editor plugin](https://github.com/dariusf/ppx_debug-vscode) to enable an experience like that of interactive debugger, where you can navigate freely in time through the execution.

Try installing the VSCode plugin and stepping through the execution!

A toplevel can also be opened at a given point in the execution, allowing interaction with values in context. This requires a bit of additional setup for now:

```sh
git clone git@github.com:dariusf/ppx_interact.git
make debug
```

which gives us

```
dune exec ./demo/debug/repl.bc -- repl debug.trace -i 6
val t : Lib.Other.misc = Lib.Other.Misc 1
val _res : int = 1
> open Lib.Other
> let (Misc x) = t
val x : int = 1
```

The event at timestamp 6 is the call to the `consume` function.
We're able to destructure the argument like any value, and even call `consume` with a modified argument.

```
> open Lib
> consume (Misc 2)
- : int = 2
```

# The demo project

To understand how all of this is set up, we'll now walk through the demo project.

```
$ tree demo
demo
├── app
│   ├── app.ml
│   └── dune
├── debug
│   ├── debug.ml
│   ├── dune
│   └── repl.ml
└── lib
    ├── dune
    ├── lib.ml
    └── other.ml

3 directories, 8 files
```

It consists of a library (`lib`) and three executables (`app`, `debug`, `repl`).

`lib` is where interesting user code would live and contains the modules that will be [instrumented](demo/lib/dune) via the ppx `ppx_debug`, which reads configuration from the environment variable PPX_DEBUG.

`app` is the [entry point](demo/app/app.ml) of the program and [depends on lib](demo/app/dune). It's the means of running the instrumented `lib`, which will produce a binary trace. No special setup is required here.

`debug` is an executable we'll add which can read binary trace files. First, we'll need an ml file with [two lines of boilerplate](demo/debug/debug.ml) to serve as its entry point. Next is the [build setup](demo/debug/dune), which sees `debug` depending on `lib` both at compile-time and runtime, and using a _second_ ppx `ppx_debug_tool`.

Why is it set up this way? `debug` reads the cmt files of `lib` during compilation via the ppx, to figure out which types and printers to use at runtime to unmarshal values in recorded executions.

The final executable, `repl`, is the entry point for opening a toplevel.
The only difference in setup from `debug` is that it is built in bytecode mode.
When the native toplevel is released, we'll no longer need it to be separate.

Finally, the Makefile demonstrates how to build the demo project.
Notably it also reads the config file config.json and makes its contents available in the environment variable PPX_DEBUG. Configuration options are documented [here](ppx_debug_runtime/config.ml).

Now you're ready to use this! Next is (a recap on) how to set up your own project with this.

# How do I use this in my project?

1. Structure your project so the code to be instrumented lives in one or more [libraries](demo/lib). If your project is a ppx, create [a library that contains the AST transformations](ppx_debug_common) and use it in another [executable that contains the driver](ppx_debug).
2. Ensure your libraries satisfy the assumptions detailed below.
3. [Add the ppx](demo/lib/dune) to any libraries you want to instrument.
4. Create [an executable](demo/debug) for interpreting executions.
5. Try to [build](Makefile) your project with an initial configuration (e.g. only instrument function definitions, and only for one simple module), record a trace, and see if you are able to see the values of arguments.
6. Once this works, tweak the configuration until you're able to instrument all the important modules and get the data you need.

In order for instrumentation to work well, a number of things are assumed:

- Your project uses dune.
- Only libraries need to be instrumented/debugged.
- There are no top-level side effects in libraries. This is because the debug executable loads libraries to call their printers, and any unexpected side effects at this point could generate trace events, clobbering debug.trace before it can be read. This is also [good practice in general](https://erratique.ch/software/logs/doc/Logs/index.html#setupreporter). If you have modules which require initialization, a workaround is to initialize lazily in the library's entry point.
- Printers for types are defined following ppx_deriving's conventions: `pp` for a type named `t`, and `pp_type` otherwise.
- The printers of all types used in instrumented modules must be accessible from the library entry point.

# Tips, FAQs, and known issues

**How scalable is this?**

Logging every single event that happens in a nontrivial program under large input likely will not work -- the program will run too slowly and the trace will be massive.

However, doing this is unnecessary for practical use cases.
When debugging, having an inkling of where a bug might be allows one to be selective about which modules are instrumented. Minimizing the input to reproduce a bug also will contribute to a reduction in trace size.
When exploring new code, what matters is control flow, so one can disable the printing of values entirely but instrument calls and matches.

In principle it should be possible to instrument _any_ project, so knobs are provided for all these purposes.

Also, different views of executions scale differently. Interactive stepping and grepping for specific arguments can still work well even if an execution is too large to render in magic-trace.

**Concurrency?**

There is no support for this at the moment, but it would be nice to add.

**Why not use the Common Trace Format?**

That would be ideal. The present trace format is a simple prototype.

**Why does the REPL not take calling context into account and give access to all variables lexically in scope, instead of just those in the current frame?**

This is also planned.

**Unbound module during compilation of the debug executable, with puzzling line number**

Our heuristics are probably not good enough to figure out how to access a type from outside a library in your case. Improvements are being worked on, but for now, to see the types involved, check the generated code using `dune describe pp <file>`, or "Show Preprocessed Document" in VSCode. It may be possible to move modules around so the heuristics kick in. If all else fails, manually specify how to access the type from outside the library using `mappings`. Contacting us about your problem would also help us improve the heuristics.

**Stack overflow when interpreting large (200 MB) executions**

This is due to the use of scanf and is being worked on.

<!-- # How does this work? -->

<!-- why not typecheck twice? -->
<!-- https://github.com/ocaml/RFCs/pull/24 -->

# Other approaches

**How does this compare to...**

- **... `#trace`, printf?** Both of these are subsumed, though with a more heavyweight build pipeline.
- **... ocamldebug?** Reverse execution is really nice, but like other interactive debuggers, interactions are limited to what the debugger can actually make the running program do. For example, ocamldebug cannot evaluate arbitrary code, and users are constrained to navigation along the single timeline of the program's execution, instead of being able to get an overview like with magic-trace.
- **... logging, testing, tracing ([Runtime Events](https://github.com/sadiqj/runtime_events_tools))?** The crucial distinction between what these provide and the needs of users when debugging is that in the latter case, **the user does not know precisely which parts of the program are relevant**.
- **... dtrace, rr, lldb, gdb via [libmonda](https://github.com/mshinwell/libmonda)?** It would be ideal if these tools understood native OCaml code, as they are fully-fledged and mature, but they don't today, and it is a significant amount of work to get them there. The main advantage of source-level instrumentation is that it is easier to convince ourselves of the fidelity of recorded traces under compiler optimizations, compared to the mapping of native code back to source-level constructs that these tools would require.
- **... [ocamli](https://github.com/johnwhitington/ocamli), [Furukawa's stepper](https://arxiv.org/abs/1906.11422)?** Custom interpreters are another means of understanding executions, by being able to show actual sequences of reductions performed. They are a large undertaking, however, and both of these only support a subset of OCaml. This is fine for teaching, but not for debugging and exploring arbitrary projects.
- **... [magic-trace](https://github.com/janestreet/magic-trace), [Landmarks](https://github.com/LexiFi/landmarks)?** Landmarks does similar instrumentation, and both provide tools for viewing executions. They are more oriented towards performance bugs and do not show the values of arguments and such.
- **... reflection?** There are several libraries for runtime type representations, e.g. [dyntype](https://github.com/samoht/dyntype) (2013), [lrt](https://github.com/LexiFi/lrt) (2020), [repr](https://github.com/mirage/repr), [typerep](https://github.com/janestreet/typerep), [refl](https://github.com/thierry-martinez/refl) (2022). Perhaps the only problem is that none of these are standard. Nevertheless, if you are able to use one of these in your project, all ppx_debug offers is the tools to view executions with.

# Project structure

- ppx_debug_runtime: like in ppx_deriving, is required at runtime and should contain minimal dependencies
- ppx_debug_common: depends on runtime, contains side-effect-free AST transformation code for the ppxes which can be instrumented when bootstrapping
- ppx_debug, ppx_debug_tool: depend on common and build ppx drivers
- ppx_debug_interact: integration with ppx_interact for opening toplevels, also kept separate instead of being combined with ppx_debug_runtime because then the latter would fail to build in native mode
- test: tests for instrumentation

# Bootstrapping

ppx_debug can be run on itself:

- Clone the main repo locally. Apply the patch in branch `bootstrap1` to rename the ppx (appending 1 to the end of all conflicting names, as programmers do).
- Symlink the clone into the main repo under `debug1`.
- Develop in the main repo's `master`
- To test a change, check out `bootstrap` (which contains a patch to enable bootstrapping) and rebase it onto master. Have the clone pull changes from `master`, which will rebase its patch. Run tests.

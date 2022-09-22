
- [Getting started](#getting-started)
- [How does it work?](#how-does-it-work)
- [Bootstrapping](#bootstrapping)
- [Other approaches](#other-approaches)
- [Project structure](#project-structure)
- [FAQs and known issues](#faqs-and-known-issues)

# Getting started

# How does it work?

1. A library to be explored is instrumented with a ppx to log function arguments, return values, and other program events to a file using Marshal.
2. A separate executable is then compiled using a _second_ ppx (which reads the cmt files of the library at compile time) to interpret the traces.
3. Running this executable produces trace files in many formats, which can be inspected in tools like [magic-trace](https://magic-trace.org/) and our VSCode plugin for [omniscient debugging](https://www.computer.org/csdl/magazine/so/2009/06/mso2009060078/13rRUwvBy70).

# Bootstrapping

ppx_debug can be run on itself:

- Clone the main repo locally. Apply the patch in branch `bootstrap1` to rename the ppx (appending 1 to the end of all conflicting names, as programmers do).
- Symlink the clone into the main repo under `debug1`.
- Develop in the main repo's `master`
- To test a change, check out `bootstrap` (which contains a patch to enable bootstrapping) and rebase it onto master. Have the clone pull changes from `master`, which will rebase its patch. Run tests.

# Other approaches

# Project structure

- runtime, like in ppx_deriving, is required at runtime and should contain minimal dependencies
- common depends on runtime mostly for convenience, and also because at compilation time we're not as concerned with minimising dependencies. It contains side-effect-free ppx code which can be instrumented when bootstrapping.
- The two ppx packages depend on common and build ppx drivers
- tool is the second entrypoint when bootstrapping

# FAQs and known issues

**Unbound module during compilation, after tool ppx has run, with puzzling line number?**

Our heuristics are probably not good enough to figure out how to access a type from outside its use site. Improvements are being worked on, but for now, to see the types involved, check the generated code using `dune describe pp <file>`, or "Show Preprocessed Document" in VSCode. Manually configure how to access the type from outside the library.

**I get a stack overflow on large traces (200 MB)**

This is due to the use of scanf and is being worked on.
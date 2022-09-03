
# Bootstrapping

ppx_debug can be run on itself:

- Clone the main repo locally. Apply the patch in branch `bootstrap1` to rename the ppx (appending 1 to the end of all conflicting names, as programmers do).
- Symlink the clone into the main repo under `debug1`.
- Develop in the main repo's `master`
- To test a change, check out `bootstrap` (which contains a patch to enable bootstrapping) and rebase it onto master. Have the clone pull changes from `master`, which will rebase its patch. Run tests.

# FAQ

**Unbound module during compilation, after tool ppx has run, with puzzling line number?**

Our heuristics are probably not good enough to figure out how to access a type from outside its use site. Improvements are being worked on, but for now, to see the types involved, check the generated code using `dune describe pp <file>`, or "Show Preprocessed Document" in the VSCode extension. Manually configure how to access the type from outside the library.

**Why all the modules?**

- runtime, like in ppx_deriving, is required at runtime and should contain minimal dependencies
- common depends on runtime mostly for convenience, and also because at compilation time we're not as concerned with minimising dependencies. It contains side-effect-free ppx code which can be instrumented when bootstrapping.
- The two ppx packages depend on common and build ppx drivers
- tool is the second entrypoint when bootstrapping
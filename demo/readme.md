
# Assumptions

1. The project uses dune and is split into executables and libraries.
2. Only libraries are traced.
3. There are no side effects at the top level in libraries. This is because the debug executable loads libraries to use their types, and any side effects would generate trace events, clobbering the trace file before it can be read. To get around this, top-level side effects may be blacklisted.
4. Printers for types are defined following ppx_deriving's conventions.
5. All printed types (including private ones) must be accessible from the library.

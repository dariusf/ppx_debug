(lang dune 3.0)

(name ppx_debug)
(generate_opam_files true)

(source
 (github dariusf/ppx_debug))

(authors "Darius Foo")

(maintainers "darius.foo.tw@gmail.com")

(package
 (name ppx_debug)
 (version 0.1)
 (synopsis "A ppx for debugging")
 (description "Debug away")
 (depends
   (ppx_debug_runtime (= 0.1))))

(package
 (name ppx_debug_tool)
 (version 0.1)
 (synopsis "A ppx for debugging")
 (description "Debug away")
 (depends
   (ppx_debug_runtime (= 0.1))))

(package
 (name ppx_debug_runtime)
 (version 0.1)
 (synopsis "A ppx for debugging")
 (description "Debug away"))

(package
 (name ppx_debug_common)
 (version 0.1)
 (synopsis "A ppx for debugging")
 (description "Debug away"))

(package
 (name ppx_debug_interact)
 (version 0.1)
 (synopsis "A ppx for debugging")
 (description "Debug away")
 (depends
   (ppx_debug_runtime (= 0.1))))

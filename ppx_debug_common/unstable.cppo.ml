
module Load_path = struct
  let init p =
    Ocaml_common.Load_path.init
      #if OCAML_VERSION < (5,0,0)
      #else
        ~auto_include:Load_path.no_auto_include
      #endif
      p
end
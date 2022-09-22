open Ppxlib

let rule =
  Ppxlib.Context_free.Rule.extension
    (Extension.declare "generate" Structure_item
       Ast_pattern.(__)
       Ppx_debug_common.Interpret_cmt.handle_si)

let () =
  let config = Ppx_debug_runtime.Config.read () in
  Ppx_debug_common.Interpret_cmt.log "%a" Ppx_debug_runtime.Config.pp config;
  Driver.register_transformation ~rules:[rule] "ppx_debug_tool"

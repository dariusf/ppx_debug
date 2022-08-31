open Ppxlib

let rule =
  Ppxlib.Context_free.Rule.extension
    (Extension.declare "generate" Structure_item
       (* Ast_pattern.(pstr (__ ^:: nil)) *)
       (* Ast_pattern.(pstr __) *)
       Ast_pattern.(__)
       Ppx_debug_common.Interpret_cmt.handle_si)

let () =
  Driver.register_transformation ~rules:[rule (* ; rule_expr *)]
    "ppx_debug_tool"

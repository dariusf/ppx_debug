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

(* let () =
   Driver.register_transformation
     ~instrument:
       (Driver.Instrument.V2.make
          (fun _ctxt s ->
            (* let cp = Expansion_context.Base.code_path ctxt in *)
            (* let filename = Code_path.file_path cp in *)
            (* let modname = Code_path.main_module_name cp in *)
            (* let file =
                 match s with
                 | [] -> failwith "nothing to translate"
                 | { pstr_loc; _ } :: _ -> pstr_loc.loc_start.pos_fname
               in *)
            (* file  *)
            List.concat_map
              (fun si ->
                Ppx_debug_common.Interpret_cmt.handle_si ~loc:si.pstr_loc si)
              s
            (* Ppx_debug_common.Interpret_cmt.handle_si *)
            (* Ppx_debug_common.Instrument.process filename modname config s *))
          ~position:After)
     "ppx_debug" *)

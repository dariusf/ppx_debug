open Ppxlib
module Config = Ppx_debug_runtime.Config

let log = Ppx_debug_common.Instrument.log

let () =
  let config = Config.read () in
  if not config.Config.enabled then log "not transforming: disabled via config"
  else
    Driver.register_transformation
      ~instrument:
        (Driver.Instrument.V2.make
           (fun ctxt s ->
             let cp = Expansion_context.Base.code_path ctxt in
             let filename = Code_path.file_path cp in
             let modname = Code_path.main_module_name cp in
             (* let file =
                  match s with
                  | [] -> failwith "nothing to translate"
                  | { pstr_loc; _ } :: _ -> pstr_loc.loc_start.pos_fname
                in *)
             (* file  *)
             Ppx_debug_common.Instrument.process filename modname config s)
           ~position:After)
      "ppx_debug"

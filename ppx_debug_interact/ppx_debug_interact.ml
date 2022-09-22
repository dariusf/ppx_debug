let backslash = Str.regexp {|\\|}
let content_marker = Str.regexp "_content"

open Ppx_debug_runtime.Trace

let repl ~print_value filename find_i =
  let res = read ~print_value filename in
  let tree = to_call_tree res in
  let find f which =
    traverse (fun t i _c -> if which = i then f t else ()) tree
  in
  let z =
    let strs = ref [] in
    find
      (fun t ->
        match t with
        | Call { unmarshal; _ } ->
          strs :=
            List.map
              (fun (k, (Bytestr r, v)) ->
                let content =
                  "\""
                  ^ Str.global_replace backslash {|\\\\|} (String.escaped r)
                  ^ "\""
                in
                Format.asprintf "let %s = %s;;" k
                  (Str.global_replace content_marker content v))
              unmarshal
            @ !strs
        | Event { unmarshal; raw = Bytestr r; _ } ->
          let content =
            "\""
            ^ Str.global_replace backslash {|\\\\|} (String.escaped r)
            ^ "\""
          in
          strs :=
            Format.asprintf "let v = %s;;"
              (Str.global_replace content_marker content unmarshal)
            :: !strs)
      find_i;
    !strs
  in
  let w = 10 in
  Ppx_interact_runtime.(
    interact ~init:z ~unit:__MODULE__ ~loc:__POS__
      ~values:
        [
          V ("y", 7);
          V ("w", w + 1);
          V ("res", res);
          V ("tree", tree);
          V ("find", find);
        ]
      ())

module Cli = struct
  open Cmdliner

  let repl_cmd print_value =
    let i =
      Arg.(
        value & opt int 0
        & info ["i"] ~docv:"timestamp" ~doc:"Timestamp to go to")
    in
    let file =
      let doc = "file" in
      Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)
    in
    let info =
      Cmd.info "repl" ~doc:"start toplevel"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "Analyze a trace using the toplevel by loading the arguments and \
               return value of the call at the given point in time";
          ]
    in
    Cmd.v info Term.(const (repl ~print_value) $ file $ i)

  let main_cmd print_value =
    let info =
      Cmd.info "debug" ~version:"v0.1"
        ~doc:"Extracts information from ppx_debug traces"
    in
    let default = Term.(ret (const (`Ok ()))) in
    Cmd.group info ~default [repl_cmd print_value]
end

let main ~print_value () = exit (Cmdliner.Cmd.eval (Cli.main_cmd print_value))
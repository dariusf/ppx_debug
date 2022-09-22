let backslash = Str.regexp {|\\|}
let content_marker = Str.regexp "_content"

open Ppx_debug_runtime.Trace

let repl ~print_value filename =
  let res = read ~print_value filename in
  let tree = to_call_tree res in
  let find_i = 2 in
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
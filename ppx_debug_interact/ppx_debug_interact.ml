let backslash = Str.regexp {|\\|}
let content_marker = Str.regexp "_content"

open Ppx_debug_runtime.Trace

let repl ~print_value filename =
  let res = read ~print_value filename in
  (* res *)
  let tree = to_call_tree res in
  let rec traverse f tree =
    match tree with
    | Call { i; calls; args; _ } ->
      f tree i args;
      List.iter (traverse f) calls
    | Event { i; content; _ } -> f tree i [("val", content)]
  in
  (* traverse
     (fun _t i c -> match mapping i with None -> () | Some r -> r := c)
     tree; *)
  let find_i = 2 in
  let find f which =
    traverse (fun t i _c -> if which = i then f t else ()) tree
  in
  let z =
    let strs = ref [] in
    find
      (fun t ->
        (* print_endline (show_call t); *)
        match t with
        | Call { unmarshal; _ } ->
          strs :=
            List.map
              (fun (k, (Bytestr r, v)) ->
                (* Format.printf "v: %s@." v; *)
                let content =
                  "\""
                  ^ Str.global_replace backslash {|\\\\|} (String.escaped r)
                  ^ "\""
                in
                (* Format.printf "content: %s@." content; *)
                Format.asprintf "let %s = %s;;" k
                  (Str.global_replace content_marker content v))
              unmarshal
            @ !strs
        | Event { unmarshal; _ } ->
          strs := Format.asprintf "let v = %s;;" unmarshal :: !strs)
      find_i;
    !strs
  in
  (* List.iter print_endline z; *)
  (* Ppx_interact_runtime.interact ~read_and_print_value () *)
  (* print_endline "haha"; *)
  let w = 10 in
  Ppx_interact_runtime.(
    interact ~init:z ~unit:__MODULE__ ~loc:__POS__
      ~values:
        [
          V ("y", 7);
          (* lol more hacks *)
          (* V ("s", "helo"); *)
          V ("w", w + 1);
          V ("res", res);
          V ("tree", tree);
          V ("find", find)
          (* V ("v0", !v0); *)
          (* V ("v62", !v62); *);
        ]
      ())
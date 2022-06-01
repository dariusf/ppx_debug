let rec print_tree d trace =
  match trace with
  | [] -> ()
  | Trace.FrameStart f :: ts ->
    Format.printf "%s%s@." (String.init d (fun _ -> ' ')) f.func;
    print_tree (d + 2) ts
  | FrameEnd _ :: ts -> print_tree (d - 2) ts
  | _ :: ts -> print_tree d ts

let print_calls trace =
  List.filter_map
    (function Trace.FrameStart f -> Some f.func | _ -> None)
    trace
  |> List.sort_uniq String.compare
  |> List.iter print_endline

let print_call call trace =
  let tree = Trace.to_call_tree trace in
  let rec collect_calls t =
    match t with
    | Trace.Event _ -> []
    | Call c ->
      let contrib =
        if String.equal c.name call then
          [
            `Assoc
              ([("call", `String c.name)]
              @ List.filter_map
                  (function
                    | Trace.Event e -> Some (e.name, `String e.content)
                    | _ -> None)
                  c.calls);
          ]
        else []
      in
      contrib @ List.concat_map collect_calls c.calls
  in
  let calls = collect_calls tree in
  Yojson.Safe.to_string (`List calls)

let main ~read_and_print_value file =
  let trace = Trace.read ~read_and_print_value file in
  if Array.length Sys.argv < 2 then
    Chrome_trace.trace_to_chrome trace |> print_endline
  else
    match Sys.argv.(1) with
    | "tree" -> print_tree 0 trace
    | "calls" -> print_calls trace
    | "call" ->
      let name = Sys.argv.(2) in
      print_endline (print_call name trace)
    | _ -> print_endline "unknown action"
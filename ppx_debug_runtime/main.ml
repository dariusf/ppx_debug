let rec print_tree d trace =
  match trace with
  | [] -> ()
  | Trace.FrameStart f :: ts ->
    Format.printf "%s%s %d@." (String.init d (fun _ -> ' ')) f.func f.id.id;
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
              [
                ("call", `String c.name);
                (* TODO line? *)
                ("args", `Assoc (List.map (fun (n, v) -> (n, `String v)) c.args));
                ( "events",
                  `List
                    (List.filter_map
                       (function
                         (* TODO line? *)
                         | Trace.Event e ->
                           Some (`Assoc [(e.name, `String e.content)])
                         | _ -> None)
                       c.calls) );
              ];
          ]
        else []
      in
      contrib @ List.concat_map collect_calls c.calls
  in
  let calls = collect_calls tree in
  Yojson.Safe.to_string (`List calls)

let linearize tree =
  let rec collect t =
    match t with
    | Trace.Event { id; _ } -> [id]
    | Call { id; calls; _ } ->
      List.concat [[id]; List.concat_map collect calls; [id]]
  in
  (* drop the first dummy node *)
  let events = collect tree |> List.tl in
  `List (List.map Trace.Id.to_yojson events)

let main_old ~read_and_print_value file =
  let trace = Trace.read ~read_and_print_value file in
  if Array.length Sys.argv < 2 then
    Chrome_trace.trace_to_chrome trace |> print_endline
  else
    match Sys.argv.(1) with
    | "tree" ->
      (* a call tree, without arguments *)
      (* TODO a means to print a specific call *)
      print_tree 0 trace
    | "calls" ->
      (* a list of all calls *)
      print_calls trace
    | "call" ->
      (* find calls by (string) name *)
      let name = Sys.argv.(2) in
      print_endline (print_call name trace)
    | "raw" ->
      (* the raw data in the trace *)
      let tree = Trace.to_call_tree trace in
      print_endline (Yojson.Safe.to_string (Trace.call_to_yojson tree))
    | "linear" ->
      (* probably useless *)
      let tree = Trace.to_call_tree trace in
      let linear = linearize tree in
      print_endline (Yojson.Safe.to_string linear)
    | "debug" ->
      (* a preprocessed trace for navigation by an interactive debugger *)
      let tree = Trace.to_call_tree trace in
      let json = Trace.preprocess_for_debugging tree in
      print_endline (Yojson.Safe.to_string json)
    | _ -> print_endline "unknown action"

let input_files = ref None
let file_args filename = input_files := Some filename

type output =
  | Chrome
  | Debugger

let format : output option ref = ref None

let parse_args () =
  Arg.parse
    [
      ( "-f",
        Arg.String
          (fun s ->
            format :=
              match s with
              | "debugger" -> Some Debugger
              | "chrome" | _ -> Some Chrome),
        "Print output in the given trace format" );
    ]
    file_args "tool <file1> -f <chrome|debugger>"

let act_on fmt trace =
  match fmt with
  | Some Debugger ->
    let tree = Trace.to_call_tree trace in
    let json = Trace.preprocess_for_debugging tree in
    print_endline (Yojson.Safe.to_string json)
  | Some Chrome | None -> Chrome_trace.trace_to_chrome trace |> print_endline

let main ~read_and_print_value () =
  parse_args ();
  match (!input_files, !format) with
  | Some file, fmt ->
    let trace = Trace.read ~read_and_print_value file in
    act_on fmt trace
  | None, _ -> print_endline "expected a file"

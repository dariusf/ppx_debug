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

let main_old ~print_value file =
  let trace = Trace.read ~print_value file in
  if Array.length Sys.argv < 2 then print_endline "chrome"
    (* Chrome_trace.trace_to_chrome trace |> print_endline *)
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
      (* let tree = Trace.to_call_tree trace in *)
      (* print_endline (Yojson.Safe.to_string (Trace.call_to_yojson tree)) *)
      failwith "incurs exponential blowup"
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
  | Calls

let format : output option ref = ref None
let function_name : string option ref = ref None

let parse_args () =
  Arg.parse
    [
      ("-n", Arg.String (fun s -> function_name := Some s), "Function name");
      ( "-f",
        Arg.String
          (fun s ->
            format :=
              match s with
              | "calls" -> Some Calls
              | "debugger" -> Some Debugger
              | "chrome" | _ -> Some Chrome),
        "Print output in the given trace format" );
    ]
    file_args "tool <file1> -f <chrome|debugger>"

let strip_newlines =
  let nl = Str.regexp "\n" in
  fun v -> Str.global_replace nl " " v

let act_on fmt trace =
  match fmt with
  | Some Calls ->
    let tree = Trace.to_call_tree trace in
    let rec f t =
      match (t, !function_name) with
      | Trace.Call { calls; args = []; _ }, _ -> List.iter f calls
      | Trace.Call { calls; name; _ }, Some fn when not (String.equal name fn)
        ->
        List.iter f calls
      | ( Trace.Call
            { i; calls; args; name; id = { file; loc = (line, _), _; _ }; _ },
          _ ) ->
        let argss =
          args
          |> List.filter (fun (n, _) -> not (String.starts_with ~prefix:"_" n))
          |> List.map (fun (k, v) ->
                 Format.asprintf "(%s: %s)" k (strip_newlines v))
          |> String.concat ", "
        in
        let res = List.assoc "_res" args |> strip_newlines in
        Format.printf "%5d %s:%d %s %s = %s@." i file line name argss res;
        List.iter f calls
      | Event _, _ -> ()
    in
    f tree
  | Some Debugger ->
    let tree = Trace.to_call_tree trace in
    let json = Trace.preprocess_for_debugging tree in
    print_endline (Yojson.Safe.to_string json)
  | Some Chrome | None ->
    let tree = Trace.to_call_tree trace in
    Chrome_trace.call_tree_to_chrome tree |> fun e ->
    `List e |> Yojson.Safe.to_string |> print_endline

let main ~print_value () =
  parse_args ();
  match (!input_files, !format) with
  | Some file, fmt ->
    let trace = Trace.read ~print_value file in
    act_on fmt trace
  | None, _ -> print_endline "expected a file"

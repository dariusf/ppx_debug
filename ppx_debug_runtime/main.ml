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

let strip_newlines =
  let nl = Str.regexp "\n" in
  fun v -> Str.global_replace nl " " v

let act_on ~print_value file fmt =
  let trace = Trace.read ~print_value file in
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
        let res = List.assoc "_res" args |> strip_newlines in
        let args =
          args
          |> List.filter (fun (n, _) -> not (String.starts_with ~prefix:"_" n))
        in
        let argss =
          match args with
          | [] -> ""
          | _ ->
            args
            |> List.map (fun (k, v) ->
                   Format.asprintf "(%s: %s)" k (strip_newlines v))
            |> String.concat ", " |> ( ^ ) " "
        in
        Format.printf "%5d %s:%d %s%s = %s@." i file line name argss res;
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

module Cli = struct
  open Cmdliner

  let trace_cmd print_value =
    let format =
      Arg.(
        value
        & opt
            (some
               (Arg.enum
                  [("chrome", Chrome); ("debugger", Debugger); ("calls", Calls)]))
            (Some Chrome)
        & info ["f"; "format"] ~docv:"FORMAT"
            ~doc:
              "Specify output format. Allowed values: chrome, debugger, calls")
    in
    let file =
      let doc = "file" in
      Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)
      (* non_empty *)
    in
    let info =
      Cmd.info "trace" ~doc:"render traces in various formats"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "Prints a trace in Chrome trace format, as a list of calls, or \
               as JSON consumed by editor plugins.";
          ]
    in
    Cmd.v info Term.(const (act_on ~print_value) $ file $ format)

  let main_cmd print_value =
    let info =
      Cmd.info "debug" ~version:"v0.1"
        ~doc:"Extracts information from ppx_debug traces"
    in
    let default = Term.(ret (const (`Ok ()))) in
    Cmd.group info ~default [trace_cmd print_value]
end

let main ~print_value () = exit (Cmdliner.Cmd.eval (Cli.main_cmd print_value))
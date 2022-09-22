type output =
  | Chrome
  | Debugger
  | Calls
  | Tree

let strip_newlines =
  let nl = Str.regexp "\n" in
  fun v -> Str.global_replace nl " " v

let rec print_tree data f tree =
  match tree with
  | Trace.Call { calls; name; args = []; _ }
    when String.equal name Trace.top_level_node ->
    List.iter (print_tree data f) calls
  | Trace.Call { i; calls; args; name; id = { file; loc = (line, _), _; _ }; _ }
    ->
    let res = List.assoc_opt "_res" args |> Option.map strip_newlines in
    let args =
      args |> List.filter (fun (n, _) -> not (String.starts_with ~prefix:"_" n))
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
    let res = match res with None -> "" | Some r -> " = " ^ r in
    f
      (fun data -> List.iter (print_tree data f) calls)
      data i file line name argss res
  | Event _ -> ()

let act_on ~print_value file fmt =
  let trace = Trace.read ~print_value file in
  match fmt with
  | Some Calls ->
    let tree = Trace.to_call_tree trace in
    print_tree ()
      (fun self data i file line name args res ->
        Format.printf "%5d %s:%d %s%s%s@." i file line name args res;
        self data)
      tree
  | Some Tree ->
    let tree = Trace.to_call_tree trace in
    print_tree 0
      (fun self data i file line name args res ->
        let indent = String.init data (fun _ -> ' ') in
        Format.printf "%s(%d) %s:%d %s%s@." indent i file line name args;
        self (data + 2);
        Format.printf "%s(%d) %s:%d %s%s@." indent i file line name res)
      tree
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
                  [
                    ("chrome", Chrome);
                    ("debugger", Debugger);
                    ("calls", Calls);
                    ("tree", Tree);
                  ]))
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
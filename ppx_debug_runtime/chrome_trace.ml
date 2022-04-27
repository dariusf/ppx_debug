(* Incomplete but tiny implementation of

   https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/preview#heading=h.yr4qxyxotyw

   there are at least two other options for writing trace data in a way that allows one of https://magic-trace.org/, https://ui.perfetto.dev/#!/, or chrome://tracing to be used; see https://github.com/ocaml/dune/pull/5618 *)

let merge_arguments trace =
  let rec loop pending res trace =
    match trace with
    | [] -> List.rev res
    | t :: ts ->
      (match (t, pending) with
      | Trace.FrameStart _, _ ->
        (* the start, arguments, stuff after *)
        loop ((t, [], []) :: pending) res ts
      | Argument _, (t1, a, s) :: ps -> loop ((t1, t :: a, s) :: ps) res ts
      | Argument _, [] -> failwith "argument cannot appear outside a function"
      | Value _, (t1, a, s) :: ps -> loop ((t1, a, t :: s) :: ps) res ts
      | Value _, [] -> failwith "value cannot appear outside a function"
      | FrameEnd _, (t1, a, s) :: ps ->
        let convert_start e args =
          match e with
          | Trace.FrameStart { time; func } ->
            `Assoc
              [
                ("name", `String func);
                ("ph", `String "B");
                ("ts", `Float (float_of_int time));
                ("pid", `Float 1.);
                ("tid", `Float 1.);
                ( "args",
                  `Assoc
                    (List.map
                       (function
                         | Trace.Argument a -> (a.name, `String a.content)
                         | _ -> failwith "invalid")
                       args) );
              ]
          | _ -> failwith "expected start"
        in
        let convert_end e =
          match e with
          | Trace.FrameEnd { time; _ } ->
            `Assoc
              [
                ("ph", `String "E");
                ("ts", `Float (float_of_int time));
                ("pid", `Float 1.);
                ("tid", `Float 1.);
              ]
          | _ -> failwith "invalid"
        in
        let convert_value e =
          match e with
          | Trace.Value { time; name; content; _ } ->
            (* `Assoc
               [
                 ("ph", `String "E");
                 ("ts", `Float (float_of_int time));
                 ("pid", `Float 1.);
                 ("tid", `Float 1.);
               ] *)
            `Assoc
              [
                ("ph", `String "X");
                ("ts", `Float (float_of_int time));
                ("pid", `Float 1.);
                ("tid", `Float 1.);
                ("name", `String name);
                ("args", `Assoc [("content", `String content)]);
              ]
          | _ -> failwith "invalid"
        in

        (* | Value { content; id; time } ->
           | FrameEnd { time; func } ->
             `Assoc
               [
                 ("ph", `String "E");
                 ("ts", `Float (float_of_int i));
                 ("pid", `Float 1.);
                 ("tid", `Float 1.);
               ] *)
        loop ps
          ((convert_end t :: List.map convert_value (List.rev s))
          @ (convert_start t1 (List.rev a) :: res))
          ts
      | Trace.FrameEnd _, [] -> failwith "nothing to pop")
  in
  loop [] [] trace

let trace_to_chrome trace =
  (* `Assoc
     [
       ("traceEvents", `List (merge_arguments trace));
       ("displayTimeUnit", `String "ns");
     ] *)
  `List (merge_arguments trace) |> Yojson.Basic.to_string

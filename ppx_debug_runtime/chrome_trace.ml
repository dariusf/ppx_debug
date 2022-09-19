(* Incomplete but tiny implementation of

   https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/preview#heading=h.yr4qxyxotyw

   there are at least two other options for writing trace data in a way that allows one of https://magic-trace.org/, https://ui.perfetto.dev/#!/, or chrome://tracing to be used; see https://github.com/ocaml/dune/pull/5618 *)
let rec call_tree_to_chrome (call : Trace.call) : Yojson.Safe.t list =
  match call with
  | Event { time; name; content; i; id = { file; loc; _ }; _ } ->
    ignore
      [
        `Assoc
          [
            ("ph", `String "i");
            ("ts", `Float (float_of_int time));
            ("pid", `Float 1.);
            ("tid", `Float 1.);
            ("name", `String name);
            ( "args",
              `Assoc
                [
                  ("_i", `Int i);
                  ("_file", `String file);
                  ("_loc", `String (Trace.Id.show_loc loc));
                  ("content", `String content);
                ] );
          ];
      ];
    []
  | Call
      {
        name = func;
        start_time;
        end_time;
        args;
        calls;
        i;
        id = { file; loc; _ };
        _;
      } ->
    let start, end_ =
      match func with
      | _ when String.equal func Trace.top_level_node -> ([], [])
      | _ ->
        ( [
            `Assoc
              [
                ("name", `String func);
                ("ph", `String "B");
                ("ts", `Float (float_of_int start_time));
                ("pid", `Float 1.);
                ("tid", `Float 1.);
                ( "args",
                  `Assoc
                    ([
                       ("_i", `Int i);
                       ("_file", `String file);
                       ("_loc", `String (Trace.Id.show_loc loc));
                     ]
                    @ List.map (fun (k, v) -> (k, `String v)) args) );
              ];
          ],
          [
            `Assoc
              [
                ("ph", `String "E");
                ("ts", `Float (float_of_int end_time));
                ("pid", `Float 1.);
                ("tid", `Float 1.);
              ];
          ] )
    in
    let rest = List.concat_map call_tree_to_chrome calls in
    List.concat [start; rest; end_]
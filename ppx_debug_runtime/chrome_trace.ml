(* Incomplete but tiny implementation of

   https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/preview#heading=h.yr4qxyxotyw

   there are at least two other options for writing trace data in a way that allows one of https://magic-trace.org/, https://ui.perfetto.dev/#!/, or chrome://tracing to be used; see https://github.com/ocaml/dune/pull/5618 *)

let trace_to_chrome trace =
  List.mapi
    (fun i e ->
      match e with
      | Trace.FrameStart f ->
        `Assoc
          [
            ("name", `String f);
            ("ph", `String "B");
            ("ts", `Float (float_of_int i));
            ("pid", `Float 1.);
            ("tid", `Float 1.);
          ]
      | Value ((_file, _func, _id), s) ->
        `Assoc
          [
            ("ph", `String "X");
            ("ts", `Float (float_of_int i));
            ("pid", `Float 1.);
            ("tid", `Float 1.);
            ("args", `Assoc [("content", `String s)]);
          ]
      | FrameEnd _ ->
        `Assoc
          [
            ("ph", `String "E");
            ("ts", `Float (float_of_int i));
            ("pid", `Float 1.);
            ("tid", `Float 1.);
          ])
    trace
  |> List.map Yojson.Basic.to_string
  |> String.concat ","
  |> fun s -> "[" ^ s

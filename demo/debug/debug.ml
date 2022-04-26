[%%generate read_and_print_value]

let () =
  let trace = Ppx_debug_runtime.Trace.read ~read_and_print_value "out.bin" in

  (* trace
     |> List.iter (fun e ->
            Format.printf "%a@." Ppx_debug_runtime.Trace.pp_event e); *)

  (* Ppx_debug_runtime.Viewer.view trace *)
  Ppx_debug_runtime.Chrome_trace.trace_to_chrome trace |> print_endline

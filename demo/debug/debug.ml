[%%generate read_and_print_value]

let () =
  let trace =
    Ppx_debug_runtime.Trace.read ~read_and_print_value "debug.trace"
  in

  (* trace
     |> List.iter (fun e ->
            Format.printf "%a@." Ppx_debug_runtime.Trace.pp_event e); *)

  (* Ppx_debug_runtime.Viewer.view trace *)
  Ppx_debug_runtime.Chrome_trace.trace_to_chrome trace |> print_endline

(* type 'a tree =
     | Leaf
     | Node of 'a tree list

   let rec depth t =
     match t with
     | Leaf -> 0
     | Node sub -> List.fold_right (fun c t -> max (depth c) t) sub 0 + 1

   let () =
     (* let s = Marshal.to_string [1; 2] [] in
        let xs = (Marshal.from_string s 0 : 'a list) in
        Format.printf "%d@." (List.length xs) *)
     let s tr = Marshal.to_string tr [] in
     let z = Node [Leaf; Node [Leaf; Node [Leaf]]] in
     let xs = (Marshal.from_string (s z) 0 : 'a tree) in
     Format.printf "%d@." (depth xs) *)

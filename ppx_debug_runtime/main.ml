let rec spaces n = match n with 0 -> "" | _ -> " " ^ spaces (n - 1)

let rec print_tree d trace =
  match trace with
  | [] -> print_endline "end"
  | Trace.FrameStart f :: ts ->
    Format.printf "%s%s@." (spaces d) f.func;
    print_tree (d + 2) ts
  | FrameEnd _ :: ts -> print_tree (d - 2) ts
  | _ :: ts -> print_tree d ts

let main ~read_and_print_value file =
  let trace = Trace.read ~read_and_print_value file in
  if Array.length Sys.argv < 2 then
    Chrome_trace.trace_to_chrome trace |> print_endline
  else
    match Sys.argv.(1) with
    | "tree" -> print_tree 0 trace
    | _ -> print_endline "unknown action"
(* trace
   |> List.iter (fun e ->
          Format.printf "%a@." Ppx_debug_runtime.Trace.pp_event e); *)

(* Ppx_debug_runtime.Viewer.view trace *)

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
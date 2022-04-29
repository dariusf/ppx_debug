(* type a = B of int list (* | C of string *)
   [@@deriving show { with_path = false }] *)

type b =
  | X of int list
  | C of string * int
[@@deriving show { with_path = false }]

(* let f x = x + 1 *)

(* let show_int = string_of_int *)
(* let show_unit () = "()" *)

(* type nonrec int = int *)
(* type nonrec unit = unit *)

(* let%trace f x = x + 1 *)
(* let f x =
   let f_original x = x + 1 in
   Ppx_debug_runtime.Trace.emit_value ~ppx_debug_file
     ~ppx_debug_id:("demo/lib/lib.ml", "func", 0)
     x;
   print_endline "arg1";
   let res = f_original x in
   Ppx_debug_runtime.Trace.emit_value ~ppx_debug_file
     ~ppx_debug_id:("demo/lib/lib.ml", "func", 1)
     res;
   print_endline "res";
   res *)

(* let%trace rec fact n = match n with _ when n <= 0 -> 1 | _ -> n * fact (n - 1) *)
(* let%trace rec fib n =
   match n with _ when n <= 0 -> 1 | _ -> fib (n - 1) + fib (n - 2) *)
let rec fib n =
  match n with _ when n <= 0 -> 1 | _ -> fib (n - 1) + fib (n - 2)

let f x =
  fib 3 |> ignore;
  (* fact 3 |> ignore; *)
  x + 1

let f1 (x : b) (y : b) =
  fib 3 |> ignore;
  (* fact 3 |> ignore; *)
  ignore x;
  ignore y

(* let%trace f x =
     fib 3 |> ignore;
     (* fact 3 |> ignore; *)
     x + 1

   let%trace f1 (x : b) (y : b) =
     fib 3 |> ignore;
     (* fact 3 |> ignore; *)
     ignore x;
     ignore y *)

(* let f1 x y =
   let f1_original (x : b) (y : b) =
     ignore x;
     ignore y
   in
   Ppx_debug_runtime.Trace.emit_value ~ppx_debug_file
     ~ppx_debug_id:("demo/lib/lib.ml", "func", 3)
     y;
   Ppx_debug_runtime.Trace.emit_value ~ppx_debug_file
     ~ppx_debug_id:("demo/lib/lib.ml", "func", 2)
     x;
   let res = f1_original x y in
   Ppx_debug_runtime.Trace.emit_value ~ppx_debug_file
     ~ppx_debug_id:("demo/lib/lib.ml", "func", 4)
     res;
   res *)

let main () =
  let x = X [2] in
  let y = C ("test", 7) in

  (* let open Ppx_debug_runtime in *)
  (* Trace.to_file "debug.trace" (fun ppx_debug_file -> *)
  (* Trace.emit_value ~ppx_debug_file
       ~ppx_debug_id:(__FILE__, __FUNCTION__, 3)
       x;
     Trace.emit_value ~ppx_debug_file
       ~ppx_debug_id:(__FILE__, __FUNCTION__, 4)
       y
       ; *)
  (* ); *)
  (* print_endline "before write"; *)
  (* Ppx_debug_runtime.Trace.emit_value ~ppx_debug_file
     ~ppx_debug_id:("demo/lib/lib.ml", "func", 3)
     "y" y; *)
  (* print_endline "after write"; *)
  (* flush ppx_debug_file; *)
  (* print_endline "after flush"; *)
  (* close_out ppx_debug_file; *)
  (* print_endline "after close"; *)
  (* output_string ppx_debug_file "lol\n"; *)
  (* exit 0 |> ignore; *)
  f 1 |> ignore;
  (* close_out ppx_debug_file; *)
  (* flush ppx_debug_file; *)
  f1 x y |> ignore
(* flush ppx_debug_file; *)
(* print_endline "end" *)
(* let a : a = B [2] in
   let bytes = Marshal.to_bytes a [] in
   let x = (Marshal.from_bytes bytes 0 : a) in
   print_endline (show_a a);
   print_endline (show_a x);
   let y = (Marshal.from_bytes bytes 0 : b) in
   print_endline (show_b y);
   let z = (Marshal.from_bytes bytes 0 : Obj.t) in
   let z = (Obj.magic z : b) in
   print_endline (show_b z) *)

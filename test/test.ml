(* open Containers *)
(* module Tracing = Ppx_debug.Tracing_simple *)

let ppx_debug_file = Ppx_debug_runtime.Trace.new_channel "test"

(* let fact n =
   let fact_original self n = match n with 0 -> 1 | n -> n * self (n - 1) in
   let rec aux n =
     Ppx_debug_runtime.Trace.emit_value ~ppx_debug_file
       ~ppx_debug_id:("test/test.ml", "func", 0)
       n;
     let res = fact_original aux n in
     Ppx_debug_runtime.Trace.emit_value ~ppx_debug_file
       ~ppx_debug_id:("test/test.ml", "func", 1)
       res;
     res
   in
   aux n *)

let%trace rec fact n = match n with 0 -> 1 | n -> n * fact (n - 1)

(*
   let a n =
     let%trace rec aux : 'a -> int -> int =
      fun a n -> match n with 0 -> 1 | n -> n + aux a (n - 1)
     in
     aux 1 n

   let%trace b : int -> 'a list =
    fun n -> match n with 0 -> raise (Failure "a") | _ -> []

   (* let rec ping : int -> int =
      fun n ->
        let ping_original self n = match n with | 0 -> 1 | _ -> pong (n - 1) in
        let rec aux n =
          Tracing.print1 __MODULE__ "ping" (ping_original aux) "n" Int.pp n
            Int.pp in
        aux n
        and pong : int -> int =
      fun n ->
        let pong_original self n = match n with | 0 -> 1 | _ -> ping (n - 1) in
        let rec aux n =
          Tracing.print1 __MODULE__ "pong" (pong_original aux) "n" Int.pp n
            Int.pp in
        aux n *)

   let%trace rec ping : int -> int =
    fun n -> match n with 0 -> 1 | _ -> pong (n - 1)

   and pong : int -> int = fun n -> match n with 0 -> 1 | _ -> ping (n - 1)

   module G = struct
     type t = int

     let pp fmt i = Format.fprintf fmt "%d!" i
   end

   let%trace t : int -> G.t = fun _ -> 1
   (* labels are not fully supported due to needing to pass them into Tracing.printn *)
   (* let%trace t1 : a:int -> int = fun ~a -> a *)
   let%trace t2 : (string * string) list -> int = fun _ -> 1

   let main () =
     a 1 |> ignore;
     Format.printf "%d@." (fact 5);
     Format.printf "%d@." (ping 2)

   let () = Tracing.wrap main *)

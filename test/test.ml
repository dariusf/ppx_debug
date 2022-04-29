let ppx_debug_file = Ppx_debug_runtime.Trace.new_channel "test"
let rec fact n = match n with 0 -> 1 | n -> n * fact (n - 1)
let _ = fun () -> ()

type v = Root of { value : int }

(* TODO not capture-avoiding *)
(* let value (Root { value }) = value *)

let labelled ~l () = l
let optional ?(l = 1) () = l
let optional_opt ?l () = l

let rec ping : int -> int = fun n -> match n with 0 -> 1 | _ -> pong (n - 1)
and pong : int -> int = fun n -> match n with 0 -> 1 | _ -> ping (n - 1)

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

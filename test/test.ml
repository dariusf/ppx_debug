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

let a f =
  let b f =
    let c f = f in
    c f
  in
  b f
(* let sum =
    List.fold_right (fun c t -> c + t) [1;2;3] 0 *)
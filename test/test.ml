[@@@warning "-32-34-37"]

let value = 1
let rec fact n = match n with 0 -> 1 | n -> n * fact (n - 1)
let _ = fun () -> ()

type v = Root of { value : int }

(* TODO not capture-avoiding *)
(* let value (Root { value }) = value *)

let labelled ~l () = l
let optional ?(l = 1) () = l
let optional_opt ?l () = l
let succ : int -> int = fun n -> n + 1

let rec ping : int -> int = fun n -> match n with 0 -> 1 | _ -> pong (n - 1)
and pong : int -> int = fun n -> match n with 0 -> 1 | _ -> ping (n - 1)

let ext =
  let rec ping1 : int -> int =
   fun n -> match n with 0 -> 1 | _ -> pong1 (n - 1)
  and pong1 : int -> int =
   fun n -> match n with 0 -> 1 | _ -> ping1 (n - 1)
  in
  ping1 2

let a f =
  let b f =
    let c f = f in
    c f
  in
  b f

let sum = List.fold_right (fun c t -> c + t) [1; 2; 3] 0

let z =
  List.map
    (fun a ->
      let f x = x in
      f a)
    [1; 2; 3]

let a () =
  let x = 1 in
  [%trace x]

let lambda_unhandled () = List.map (fun (a, b) -> a + b) [(1, 2)]
let keep_type_annotations (t : int) = t

let obj n =
  let a =
    object
      method f x = x + 1
    end
  in
  a#f (2 + n)

class virtual clz =
  object
    method g x = x * 2
  end
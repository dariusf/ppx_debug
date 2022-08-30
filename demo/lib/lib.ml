(* Top-level side effects are not allowed here (see assumptions) *)

let rec fib n =
  match n with _ when n <= 0 -> 1 | _ -> fib (n - 1) + fib (n - 2)

let rec fact n = match n with _ when n <= 0 -> 1 | _ -> n * fact (n - 1)

type 'a tree =
  | Leaf of int
  | Node of 'a tree list
[@@deriving show { with_path = false }]

let rec depth t =
  match t with
  | Leaf _ -> 0
  | Node sub -> List.fold_right (fun c t -> max (depth c) t) sub 0 + 1

let c f = f 1
let sum xs = List.fold_right (fun c t -> c + t) xs 0
let rec tail xs acc = match xs with [] -> acc | x :: xs -> tail xs (x + acc)

module Int = struct
  type t = int

  let of_int x = x
  let to_int x = x
end

module Priv : sig
  type t = private int

  val of_int : int -> t
  val to_int : t -> int
end =
  Int

module Abstr : sig
  type t

  val of_int : int -> t
  val to_int : t -> int
end =
  Int

let abstr_type (t : Abstr.t) = t
let prv_type (t : Priv.t) = t

open Mod
module Mod = Mod

let aaa (t : Qual.t) = match t with Qual a -> a

let main () =
  let z = Node [Node [Leaf 1]; Leaf 2] in
  aaa (Qual 1) |> ignore;
  depth z |> ignore;
  fact 5 |> ignore;
  fib 3 |> ignore;
  c (fun x -> x + 1) |> ignore;
  sum [1; 2; 3] |> ignore;
  tail [1; 2; 3] 0 |> ignore;
  abstr_type (Abstr.of_int 1) |> ignore;
  prv_type (Priv.of_int 1) |> ignore

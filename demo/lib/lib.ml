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

let main () =
  let z = Node [Node [Leaf 1]; Leaf 2] in
  depth z |> ignore;
  fact 5 |> ignore;
  fib 3 |> ignore;
  c (fun x -> x + 1) |> ignore;
  sum [1; 2; 3] |> ignore;
  tail [1; 2; 3] 0 |> ignore

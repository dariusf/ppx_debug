let rec fib n =
  match n with _ when n <= 0 -> 1 | _ -> fib (n - 1) + fib (n - 2)

let rec fact n = match n with _ when n <= 0 -> 1 | _ -> n * fact (n - 1)

type 'a tree =
  | Leaf
  | Node of 'a tree list
[@@deriving show { with_path = false }]

let rec depth t =
  match t with
  | Leaf -> 0
  | Node sub -> List.fold_right (fun c t -> max (depth c) t) sub 0 + 1

let main () =
  let z = Node [Node [Leaf]; Leaf] in
  depth z |> ignore;
  fact 5 |> ignore;
  fib 3 |> ignore
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
let rec sum2 xs acc = match xs with [] -> acc | x :: xs -> sum2 xs (x + acc)

(* this is so Other can be referenced through Lib *)
module Other = Other

let abstr_type (t : Other.Abstr.t) = t
let prv_type (t : Other.Priv.t) = t
let consume (t : Other.misc) = match t with Misc a -> a

let rec insert x xs =
  match xs with
  | [] -> [(x : int)]
  | y :: ys -> if x < y then x :: y :: ys else y :: insert x ys

let rec sort xs =
  match xs with [] -> ([] : int list) | x :: xs -> insert x (sort xs)

let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  (List.map snd sond : int list)

let main () =
  Random.self_init ();
  let z = Node [Node [Leaf 1]; Leaf 2] in
  consume (Misc 1) |> ignore;
  depth z |> ignore;
  fact 5 |> ignore;
  fib 3 |> ignore;
  c (fun x -> x + 1) |> ignore;
  sum [1; 2; 3] |> ignore;
  sum2 [1; 2; 3] 0 |> ignore;
  sort (List.init 10 (fun i -> i) |> shuffle) |> ignore;
  abstr_type (Other.Abstr.of_int 1) |> ignore;
  prv_type (Other.Priv.of_int 1) |> ignore

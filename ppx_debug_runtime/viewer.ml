module Id = Trace.Id

(* let print ~file ~ppx_debug_id:id v =
   (* Format.printf "%s, v: %s@." (Id.show id) (Marshal.to_string v []) *)
   let s = Marshal.to_string v [] in
   Printf.fprintf file "%s\n%s\n" (Id.show id) s *)

(* let print ~file ~func ~ppx_debug_id v =
   Format.printf "file: %s, func: %s, id: %d, v: %s@." file func ppx_debug_id
     (Marshal.to_string v []) *)

module Tree = struct
  type 'a t =
    | Leaf of 'a
    | Empty
    | Rose of string * 'a t list * 'a t * 'a t list
  [@@deriving show { with_path = false }]

  type 'a path =
    | Top
    | Node of string * 'a path * 'a t list * 'a t list
  [@@deriving show { with_path = false }]

  type 'a pos = {
    focus : 'a t;
    path : 'a path;
  }
  [@@deriving show { with_path = false }]

  let right { focus; path } =
    match (focus, path) with
    | _, Top -> None
    | _, Node (_, _, _, []) -> None
    | t, Node (n, p, ls, r :: rs) ->
      Some { focus = r; path = Node (n, p, t :: ls, rs) }

  let left { focus; path } =
    match (focus, path) with
    | _, Top -> None
    | _, Node (_, _, [], _) -> None
    | t, Node (n, p, l :: ls, rs) ->
      Some { focus = l; path = Node (n, p, ls, t :: rs) }

  let up { focus; path } =
    match (focus, path) with
    | _, Top -> None
    | t, Node (n, p, l, r) -> Some { focus = Rose (n, l, t, r); path = p }

  let down { focus; path } =
    match (focus, path) with
    | Leaf _, _ -> None
    | Empty, _ -> None
    (* don't allow going into focused leaves, just not useful *)
    (* | Rose (_, _, Leaf _, _), _ -> None *)
    | Rose (n, l, t, r), p -> Some { focus = t; path = Node (n, p, l, r) }

  let focus t = Some { focus = t; path = Top }
  let ( >>= ) = Option.bind
end

let list_to_tree _i func _id _args trees _start_time _end_time =
  match trees with
  | [] -> failwith "empty tree?"
  (* | [t] -> Tree.Rose (func, [], t, []) *)
  | t :: ts -> Tree.Rose (func, [], t, ts)

(* produce a rose tree suitable for traversal via zipper *)
let to_tree =
  Trace.to_tree
    ~leaf:(fun _i e id _name c _time -> Tree.Leaf (e, (id, c)))
    ~node:list_to_tree

let pp_pair pp_a pp_b fmt (a, b) = Format.fprintf fmt "(%a, %a)" pp_a a pp_b b

let view trace =
  let tree = to_tree trace in
  let pp_tree =
    Tree.pp (pp_pair Trace.pp_event (pp_pair Id.pp Format.pp_print_string))
  in
  let debug_tree tree = Format.printf "%a@." pp_tree tree in
  if true then debug_tree tree;

  let rec user_input prompt cb =
    match LNoise.linenoise prompt with
    | None -> ()
    | Some v ->
      cb v;
      user_input prompt cb
  in
  (* LNoise.set_multiline true; *)
  (* LNoise.set_hints_callback (fun line ->
       if line <> "git remote add " then None
       else Some (" <this is the remote name> <this is the remote URL>",
                  LNoise.Yellow,
                  true)
     ); *)
  (* LNoise.history_load ~filename:"history.txt" |> ignore; *)
  (* LNoise.history_set ~max_length:100 |> ignore; *)
  (* LNoise.set_completion_callback begin fun line_so_far ln_completions ->
       if line_so_far <> "" && line_so_far.[0] = 'h' then
         ["Hey"; "Howard"; "Hughes";"Hocus"]
         |> List.iter (LNoise.add_completion ln_completions);
     end; *)
  (* ["These are OCaml bindings to linenoise";
      "get tab completion with <TAB>, type h then hit <TAB>";
      "type quit to exit gracefully";
      "By Edgar Aroutiounian\n"]
     |> List.iter print_endline; *)
  let open Tree in
  let preview_tree tree =
    match tree with
    | Leaf (e, (_id, _s)) -> Trace.show_event e
    | Empty -> "empty"
    | Rose (n, _l, _t, _r) -> Format.asprintf "call %s" n
  in
  (* prints what's inside the focused *)
  (* let show_current_tree ctx tree =
       match tree with
       | Rose (n, l, t, r) ->
         Format.printf "at %s@." n;
         List.map (fun t -> "   " ^ preview_tree t) l
         @ [Format.asprintf "-> %s" (preview_tree t)]
         @ List.map (fun t -> "   " ^ preview_tree t) r
         |> String.concat "\n"
       | Leaf (e, (_id, _s)) -> Ppx_debug_runtime.Trace.show_event e
       | Empty -> "empty"
     in *)
  let show_current_tree { focus; path } =
    let ctx, l, r =
      match path with Top -> ("top", [], []) | Node (s, _, l, r) -> (s, l, r)
    in
    (* | Top *)
    (* | Node of string * 'a path * 'a t list * 'a t list *)
    Format.printf "at %s@." ctx;
    let current =
      match focus with
      | Rose (_n, l, t, r) ->
        (* Format.printf "at %s@." n; *)
        [
          List.map (fun t -> "     " ^ preview_tree t) l
          @ [Format.asprintf "   > %s" (preview_tree t)]
          @ List.map (fun t -> "     " ^ preview_tree t) r
          |> String.concat "\n";
        ]
      | Leaf (_, (_id, _s)) -> []
      | Empty -> ["empty"]
    in
    List.concat
      [
        List.map (fun t -> "   " ^ preview_tree t) l;
        [Format.asprintf "-> %s" (preview_tree focus)];
        current;
        List.map (fun t -> "   " ^ preview_tree t) r;
      ]
    |> String.concat "\n"
    (* debug_tree tree; *)
  in
  let ( or ) a b = match a with None -> b | Some _ -> a in
  let move_step pos =
    (* let pos = match pos with None -> failwith "invalid" | Some pos -> pos in *)
    match pos with
    | { focus = _; path = Top } -> failwith "cannot move?"
    | { focus = Leaf _; path = Node (_n, _u, _l, _ :: _) } ->
      print_endline "leaf nonempty";
      Some pos >>= right
    | { focus = Rose _; path = Node (_n, _u, _l, _) } ->
      print_endline "node";
      Some pos >>= down
    | { focus = Leaf _; path = Node (_n, _u, _l, []) } ->
      print_endline "node empty";
      let parent = Some pos >>= up in
      let rec find_next p = Some p >>= right or Some p >>= up >>= find_next in
      parent >>= find_next
    | { focus = Empty; path = Node (_n, _u, _l, _) } -> failwith "unexpected"
    (* match pos.focus with
       | Leaf _ | Empty -> failwith "invalid"
       | Rose (_, _, _, []) -> Some pos >>= up >>= move_step
       | Rose (_, _, Leaf _, _ :: _) ->
         (* Some ({pos with focus = Rose (name, t :: l, n, ns)}) *)
         print_endline "leaf nonempty";
         Some pos >>= right
       | Rose (_, _, Rose _, _ :: _) ->
         print_endline "rose nonempty";
         Some pos >>= down
       | Rose (_, _, Empty, _ :: _) -> failwith "invalid, empty?" *)
    (* | _ -> failwith "nyi" *)
    (* If leaf, go right
       If node, go down
       If cannot, go right
       If cannot, go up, right
       If cannot, go up, ...
       If still fails, we are at the end of the program *)
  in

  (* get the top level ctx *)
  (* let ctx = ref "" in *)
  (* focus on the first child *)
  let tree = ref (focus tree >>= down) in
  (* let update_ctx () =
       match (Option.get !tree).focus with
       | Rose (n, _, _, _) -> ctx := n
       | _ -> failwith "asd"
     in *)
  (* update_ctx (); *)
  print_endline (show_current_tree (!tree |> Option.get));
  user_input "> " (fun inp ->
      let inp = String.trim inp in
      match inp with
      | "s" ->
        begin
          match !tree >>= move_step with
          | None -> print_endline "invalid move"
          | Some pos ->
            tree := Some pos;
            (* update_ctx (); *)
            print_endline (show_current_tree pos)
        end
      | "n" ->
        begin
          match !tree >>= right with
          | None -> print_endline "invalid move"
          | Some pos ->
            tree := Some pos;
            (* update_ctx (); *)
            print_endline (show_current_tree pos)
        end
      | _ ->
        print_endline "unknown command";
        print_endline (show_current_tree (!tree |> Option.get))
      (* if from_user = "quit" then exit 0; *)
      (* LNoise.history_add from_user |> ignore; *)
      (* LNoise.history_save ~filename:"history.txt" |> ignore; *)
      (* Printf.sprintf "Got: %s" from_user |> print_endline *))
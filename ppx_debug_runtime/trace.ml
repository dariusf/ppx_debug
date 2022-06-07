let open_channels : (string * out_channel) list ref = ref []

module Id = struct
  type t = {
    (* the combination of file (compilation unit) and id is sufficiently unique *)
    file : string;
    id : int;
    (* extra fields *)
    (* func : string; *)
    (* modname : string; *)
    line : int;
  }
  [@@deriving show { with_path = false }, yojson]

  let dummy = { file = "_none"; id = -1; line = -1 }
  let serialize { file; id; line } = Format.sprintf "%s:%d:%d" file id line

  let deserialize file =
    Scanf.bscanf file "%s@:%d:%d\n" @@ fun file id line -> { file; id; line }

  let show { file; id; line } =
    Format.sprintf "(file: %s, id: %d, line: %d)" file id line
end

(* really simple internal trace format. this is written in binary mode (via the emit_* functions, which marshal ocaml values) and read together with type metadata to unmarshal values (producing text) *)
type event =
  | FrameStart of {
      time : int;
      id : Id.t;
      func : string;
    }
  | Value of {
      time : int;
      id : Id.t;
      name : string;
      content : string;
    }
  | Argument of {
      time : int;
      id : Id.t;
      name : string;
      content : string;
    }
  | Match of {
      time : int;
      id : Id.t;
      name : string;
      content : string;
    }
  | FrameEnd of {
      time : int;
      id : Id.t;
      func : string;
    }
[@@deriving show { with_path = false }]

type t = event list

let to_file filename f =
  let file = open_out_bin filename in
  f file;
  close_out file

let new_channel c = c

let lazy_init c =
  if not (List.mem_assoc c !open_channels) then (
    let f = open_out_bin c in
    at_exit (fun () -> close_out f);
    open_channels := (c, f) :: !open_channels;
    f)
  else List.assoc c !open_channels

let get_time =
  (* hack to ensure time monotonically increases, so every event takes at least 1 ms *)
  let time_i = ref 0 in
  let last_time = ref 0 in
  fun () ->
    let get () =
      (* millisecond *)
      Float.to_int (Float.round (Unix.gettimeofday () *. 1000.))
    in
    let t = get () in
    if t <= !last_time then begin
      incr time_i;
      last_time := !last_time + !time_i;
      !last_time
    end
    else begin
      time_i := 0;
      last_time := t;
      t
    end

let emit_start ~ppx_debug_file ~ppx_debug_id:id ~func =
  Printf.fprintf (lazy_init ppx_debug_file) "start\n%s\n%d\n%s\n"
    (Id.serialize id) (get_time ()) func

let emit_end ~ppx_debug_file ~ppx_debug_id:id ~func =
  Printf.fprintf (lazy_init ppx_debug_file) "end\n%s\n%d\n%s\n"
    (Id.serialize id) (get_time ()) func

let emit_raw ~ppx_debug_file ~ppx_debug_id:id typ what v =
  (* if a function is given, instead of throwing an exception, output a string.
     this is okay because the printer for functions ignores its argument. *)
  let[@warning "-52"] s =
    try Marshal.to_string v []
    with Invalid_argument "output_value: functional value" ->
      Marshal.to_string "<fn>" []
  in
  Printf.fprintf (lazy_init ppx_debug_file) "%s\n%s\n%s\n%d\n%d%s\n" typ
    (Id.serialize id) what (get_time ()) (String.length s) s

let emit_argument ~ppx_debug_file ~ppx_debug_id:id what v =
  emit_raw ~ppx_debug_file ~ppx_debug_id:id "arg" what v

let emit_value ~ppx_debug_file ~ppx_debug_id:id what v =
  emit_raw ~ppx_debug_file ~ppx_debug_id:id "value" what v

let emit_match ~ppx_debug_file ~ppx_debug_id:id what v =
  emit_raw ~ppx_debug_file ~ppx_debug_id:id "match" what v

let read ~read_and_print_value filename =
  let file = Scanf.Scanning.open_in_bin filename in
  let rec loop all =
    let typ = Scanf.bscanf file "%s@\n" (fun typ -> typ) in
    match typ with
    | "start" ->
      let id = Id.deserialize file in
      let time = Scanf.bscanf file "%d\n" (fun t -> t) in
      let func = Scanf.bscanf file "%s@\n" (fun id -> id) in
      loop (FrameStart { id; time; func } :: all)
    | "end" ->
      let id = Id.deserialize file in
      let time = Scanf.bscanf file "%d\n" (fun t -> t) in
      let func = Scanf.bscanf file "%s@\n" (fun id -> id) in
      loop (FrameEnd { id; time; func } :: all)
    | "arg" | "value" | "match" ->
      let id = Id.deserialize file in
      let what = Scanf.bscanf file "%s@\n" (fun what -> what) in
      let time = Scanf.bscanf file "%d\n" (fun t -> t) in
      let len = Scanf.bscanf file "%d" (fun t -> t) in
      let v = read_and_print_value len file id in
      let next =
        match typ with
        | "arg" -> Argument { time; id; name = what; content = v }
        | "value" -> Value { time; id; name = what; content = v }
        | "match" -> Match { time; id; name = what; content = v }
        | _ -> failwith "invalid"
      in
      loop (next :: all)
    | "" -> List.rev all
    | _ -> failwith ("invalid " ^ typ)
  in
  let res = loop [] in
  Scanf.Scanning.close_in file;
  res

let to_tree ?(toplevel = "top level") ~leaf ~node trace =
  let rec build_tree trace =
    match trace with
    | FrameStart { func; id; _ } :: es ->
      let trees, trace = look_for_end es [] in
      (node func id trees, trace)
    | _ :: _ -> failwith "expected FrameStart"
    | [] -> failwith "empty trace"
  and look_for_end trace res =
    match trace with
    | e :: es ->
      begin
        match e with
        | FrameEnd _ -> (List.rev res, es)
        | FrameStart _ ->
          (* note that we recurse on trace, not es *)
          let tree, trace = build_tree (e :: es) in
          look_for_end trace (tree :: res)
        | Match { id; content; name; _ }
        | Value { id; content; name; _ }
        | Argument { id; content; name; _ } ->
          look_for_end es (leaf e id name content :: res)
      end
    | [] -> (List.rev res, [])
  in
  (* build_tree returns the collected trees and the remaining trace, so we have to iterate it *)
  let rec collect_trees trace =
    match trace with
    | [] -> []
    | _ :: _ ->
      let tree, trace = build_tree trace in
      tree :: collect_trees trace
  in
  node toplevel Id.dummy (collect_trees trace)

(* a tree representation of traces that makes many operations easier *)
type call =
  | Event of {
      name : string;
      content : string;
      id : Id.t;
    }
  | Call of {
      name : string;
      calls : call list;
      id : Id.t;
    }
[@@deriving yojson]

let to_call_tree trace =
  to_tree
    ~node:(fun f id trees -> Call { name = f; calls = trees; id })
    ~leaf:(fun _e id name content -> Event { name; content; id })
    trace

let group_sorted f xs =
  let rec loop res xs =
    match (xs, res) with
    | [], _ -> List.rev res
    | x :: xs, [] -> loop ([x] :: res) xs
    | x :: xs, (r :: rs) :: rs1 ->
      if f x = f r then loop ((x :: r :: rs) :: rs1) xs
      else loop ([x] :: (r :: rs) :: rs1) xs
    | _ :: _, [] :: _ ->
      (* this isn't possible as we can't create an empty group *)
      failwith "invalid"
  in
  loop [] xs

let%expect_test _ =
  let show a = a |> [%derive.show: int list list] |> print_endline in
  group_sorted Fun.id [1; 1; 2; 3; 3; 4] |> show;
  [%expect {| [[1; 1]; [2]; [3; 3]; [4]] |}]

(**
  Walks the call tree as if it were being stepped through (preorder, depth-first),
  numbering each node and computing a few relations between nodes.

  Many relations are inverses (next/back, out/in, next_sibling/prev_sibling).
  Not all are useful, e.g. next subsumes in and would probably be used to implement a debugger's "step in" command.
  Not all correspond directly to debugger actions, e.g. "step out" would be done by composing out and next_sibling.

  Outputs JSON. Could be used for the zipper viewer but we would have to separate the JSON encoding phase.
*)
let preprocess_for_debugging tree : Yojson.Safe.t =
  let fresh =
    let i = ref 0 in
    fun () ->
      let r = !i in
      incr i;
      r
  in
  let rec loop prev lineage nodes edges t =
    (* this node id *)
    let nid = fresh () in
    (* compute all the relations *)
    let step_out, step_in =
      match lineage with
      | [] -> ([], [])
      | n :: _ ->
        (* don't add an in node twice *)
        let in_ = [(n, ("in", nid))] in
        (* return the id of the current node, so it may be used as the previous node of the next one *)
        let out = [(nid, ("out", n))] in
        (out, in_)
    in
    let step_back, step_next =
      match prev with
      | None -> ([], [])
      | Some p ->
        let back = [(nid, ("back", p))] in
        let next = [(p, ("next", nid))] in
        (back, next)
    in
    let new_edges = step_in @ step_back @ step_out @ step_next in
    (* decide what to present for each kind of node *)
    match t with
    | Event { id; name; _ } ->
      let this = `Assoc [("id", Id.to_yojson id); ("name", `String name)] in
      (nid, nid, (nid, this) :: nodes, new_edges @ edges)
    | Call { id; name; calls; _ } ->
      let this = `Assoc [("id", Id.to_yojson id); ("name", `String name)] in
      let nodes, edges = ((nid, this) :: nodes, new_edges @ edges) in
      (* extend lineage once for all children *)
      let lin = nid :: lineage in
      (* pre-order traversal, left to right *)
      let _last_sib, cpid, ns, es =
        List.fold_left
          (fun (prev_sib, prv, ns, es) c ->
            (* previous node updates on each iteration *)
            let sid, cid, ns1, es1 = loop (Some prv) lin ns es c in
            (* track siblings *)
            let extra =
              match prev_sib with
              | None -> []
              | Some s ->
                [(s, ("next_sibling", sid)); (sid, ("prev_sibling", s))]
            in
            (Some sid, cid, ns1, extra @ es1))
          (None, nid, nodes, edges) calls
      in
      (* nid is our id. cpid is the id of the last thing that executed, which may be a child *)
      (nid, cpid, ns, es)
  in
  let _sid, _nid, nodes, edges = loop None [] [] [] tree in
  (* encode into json *)
  let nodes =
    nodes |> List.rev |> List.map (fun (id, n) -> (string_of_int id, n))
  in
  let edges =
    List.sort (fun (e1, _) (e2, _) -> compare e1 e2) edges
    |> group_sorted fst
    |> List.map (fun g ->
           let kvs = List.map (fun (_gid, (k, v)) -> (k, v)) g in
           let kvs =
             (* fix in keys. there will be many because we keep track of all. keep only the minimum, which is correct because we do a pre-order traversal and generate fresh ids in that order *)
             let in_keys, non_in =
               List.partition (fun (k, _) -> k = "in") kvs
             in
             match in_keys with
             | [] -> non_in
             | _ ->
               let min_val =
                 List.fold_right (fun (_, c) t -> min c t) in_keys Int.max_int
               in
               ("in", min_val) :: non_in
           in
           let kvs = List.map (fun (k, v) -> (k, `Int v)) kvs in
           let gid =
             match g with
             | (gid, _) :: _ -> gid
             | [] -> failwith "invalid, empty group"
           in
           (string_of_int gid, `Assoc kvs))
  in
  let edges : Yojson.Safe.t = `Assoc edges in
  `Assoc
    [
      ("nodes", `Assoc nodes);
      ("edges", edges);
      ("last", `Int (List.length nodes - 1));
    ]

let%expect_test _ =
  let tree =
    Call
      {
        (* 0 *)
        name = "f";
        id = Id.dummy;
        calls =
          [
            Event { (* 1 *)
                    name = "a"; content = "x"; id = Id.dummy };
            Event { (* 2 *)
                    name = "b"; content = "y"; id = Id.dummy };
            Call
              {
                (* 3 *)
                name = "g";
                id = Id.dummy;
                calls =
                  [Event { (* 4 *)
                           name = "c"; content = "z"; id = Id.dummy }];
              };
            Event { (* 5 *)
                    name = "d"; content = "w"; id = Id.dummy };
          ];
      }
  in
  let debug = preprocess_for_debugging tree in
  Yojson.Safe.pretty_to_string debug |> print_endline;
  [%expect
    {|
    {
      "nodes": {
        "0": { "id": { "file": "_none", "id": -1, "line": -1 }, "name": "f" },
        "1": { "id": { "file": "_none", "id": -1, "line": -1 }, "name": "a" },
        "2": { "id": { "file": "_none", "id": -1, "line": -1 }, "name": "b" },
        "3": { "id": { "file": "_none", "id": -1, "line": -1 }, "name": "g" },
        "4": { "id": { "file": "_none", "id": -1, "line": -1 }, "name": "c" },
        "5": { "id": { "file": "_none", "id": -1, "line": -1 }, "name": "d" }
      },
      "edges": {
        "0": { "in": 1, "next": 1 },
        "1": { "out": 0, "back": 0, "next": 2, "next_sibling": 2 },
        "2": {
          "out": 0,
          "back": 1,
          "prev_sibling": 1,
          "next": 3,
          "next_sibling": 3
        },
        "3": {
          "in": 4,
          "out": 0,
          "back": 2,
          "next": 4,
          "prev_sibling": 2,
          "next_sibling": 5
        },
        "4": { "out": 3, "back": 3, "next": 5 },
        "5": { "out": 0, "back": 4, "prev_sibling": 3 }
      },
      "last": 5
    }
  |}]
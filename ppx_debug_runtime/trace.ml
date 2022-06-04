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
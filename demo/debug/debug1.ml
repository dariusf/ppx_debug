let rec dump r =
  if Obj.is_int r then string_of_int (Obj.magic r : int)
  else
    (* Block. *)
    let rec get_fields acc = function
      | 0 -> acc
      | n ->
        let n = n - 1 in
        get_fields (Obj.field r n :: acc) n
    in
    let rec is_list r =
      if Obj.is_int r then r = Obj.repr 0 (* [] *)
      else
        let s = Obj.size r and t = Obj.tag r in
        t = 0 && s = 2 && is_list (Obj.field r 1)
      (* h :: t *)
    in
    let rec get_list r =
      if Obj.is_int r then []
      else
        let h = Obj.field r 0 and t = get_list (Obj.field r 1) in
        h :: t
    in
    let opaque name =
      (* XXX In future, print the address of value 'r'.  Not possible
       * in pure OCaml at the moment. *)
      "<" ^ name ^ ">"
    in
    let s = Obj.size r and t = Obj.tag r in
    (* From the tag, determine the type of block. *)
    match t with
    | _ when is_list r ->
      let fields = get_list r in
      "[" ^ String.concat "; " (List.map dump fields) ^ "]"
    | 0 ->
      let fields = get_fields [] s in
      "(" ^ String.concat ", " (List.map dump fields) ^ ")"
    | x when x = Obj.lazy_tag ->
      (* Note that [lazy_tag .. forward_tag] are < no_scan_tag.  Not
         * clear if very large constructed values could have the same
         * tag. XXX *)
      opaque "lazy"
    | x when x = Obj.closure_tag -> opaque "closure"
    | x when x = Obj.object_tag ->
      let fields = get_fields [] s in
      let _clasz, id, slots =
        match fields with h :: h' :: t -> (h, h', t) | _ -> assert false
      in
      (* No information on decoding the class (first field).  So just print
         * out the ID and the slots. *)
      "Object #" ^ dump id ^ " ("
      ^ String.concat ", " (List.map dump slots)
      ^ ")"
    | x when x = Obj.infix_tag -> opaque "infix"
    | x when x = Obj.forward_tag -> opaque "forward"
    | x when x < Obj.no_scan_tag ->
      let fields = get_fields [] s in
      "Tag" ^ string_of_int t ^ " ("
      ^ String.concat ", " (List.map dump fields)
      ^ ")"
    | x when x = Obj.string_tag ->
      "\"" ^ String.escaped (Obj.magic r : string) ^ "\""
    | x when x = Obj.double_tag -> string_of_float (Obj.magic r : float)
    | x when x = Obj.abstract_tag -> opaque "abstract"
    | x when x = Obj.custom_tag -> opaque "custom"
    | x when x = Obj.final_tag -> opaque "final"
    | x when x = Obj.double_array_tag ->
      "array"
      (* BatIO.to_string (BatArray.print BatFloat.print) (Obj.magic r : float array) *)
    | _ -> opaque (Printf.sprintf "unknown: tag %d size %d" t s)

let dump v = dump (Obj.repr v)

(* [%%generate load_value] *)

(* res *)

(* let () = Ppx_debug_runtime.Main.main ~read_and_print_value () *)

(* let read filename =
   let file = Scanf.Scanning.open_in_bin filename in
   let rec loop all =
     let typ = Scanf.bscanf file "%s@\n" (fun typ -> typ) in
     (* print_endline typ;
        print_endline (string_of_int (String.length typ)); *)
     let open Ppx_debug_runtime in
     let open Trace in
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
       let v = load_value len file id in
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
   (* i and id don't match. read trace, convert into tree, traverse tree and generate bindings
      doesn't work here as number of bindings is dynamic...
         (with types using i to lookup), traverse tree to set bindings, then put bindings in the repl *)
   let res = loop [] in
   Scanf.Scanning.close_in file;
   let tree = Ppx_debug_runtime.Trace.to_call_tree res in
   let rec traverse f tree =
     match tree with
     | Ppx_debug_runtime.Trace.Call { i; calls; args; _ } ->
       f tree i args;
       List.iter (traverse f) calls
     | Event { i; content; _ } -> f tree i [("val", content)]
   in
   traverse
     (fun _t i c -> match mapping i with None -> () | Some r -> r := c)
     tree;
   let x = 1 in
   let find p =
     traverse
       (fun t i _c ->
         if p = i then print_endline (Ppx_debug_runtime.Trace.show_call t)
         else ())
       tree
   in
   (* Ppx_interact_runtime.interact ~read_and_print_value () *)
   Ppx_debug_interact.(
     interact ~unit:__MODULE__ ~loc:__POS__
       ~values:
         [
           V ("x", x);
           V ("res", res);
           V ("tree", tree);
           V ("find", find)
           (* V ("v0", !v0); *)
           (* V ("v62", !v62); *);
         ]
       ())] *)

open Ppx_debug_runtime.Trace

[%%generate print_value]

(* let print_value id _content =
   match id with
   | Ppx_debug_runtime.Id.{ file = "demo/lib/other.ml"; id = 14; _ } ->
     Format.asprintf "%a"
       (fun fmt _ -> Format.fprintf fmt "<poly>")
       (Marshal.from_string _content 0)
   | Ppx_debug_runtime.Id.{ file = "demo/lib/other.ml"; id = 13; _ } ->
     Format.asprintf "%a"
       (fun fmt _ -> Format.fprintf fmt "<poly>")
       (Marshal.from_string _content 0)
   | Ppx_debug_runtime.Id.{ file = "demo/lib/other.ml"; id = 10; _ } ->
     Format.asprintf "%a"
       (fun fmt _ -> Format.fprintf fmt "<poly>")
       (Marshal.from_string _content 0)
   | Ppx_debug_runtime.Id.{ file = "demo/lib/other.ml"; id = 9; _ } ->
     Format.asprintf "%a"
       (fun fmt _ -> Format.fprintf fmt "<poly>")
       (Marshal.from_string _content 0)
   | Ppx_debug_runtime.Id.{ file = "demo/lib/other.ml"; id = 6; _ } ->
     Format.asprintf "%a" Lib.Other.pp_misc (Marshal.from_string _content 0)
   | Ppx_debug_runtime.Id.{ file = "demo/lib/other.ml"; id = 3; _ } ->
     Format.asprintf "%a" Format.pp_print_int
       (Marshal.from_string _content 0 : int)
   | Ppx_debug_runtime.Id.{ file = "demo/lib/other.ml"; id = 2; _ } ->
     Format.asprintf "%a" Lib.Other.pp_misc (Marshal.from_string _content 0)
     (* just these three *)
   | Ppx_debug_runtime.Id.{ file = "demo/lib/lib.ml"; id = 6; _ } ->
     Format.asprintf "%a"
       (fun fmt () -> Format.fprintf fmt "()")
       (Marshal.from_string _content 0 : unit)
   | Ppx_debug_runtime.Id.{ file = "demo/lib/lib.ml"; id = 3; _ } ->
     Format.asprintf "%a" Format.pp_print_int
       (Marshal.from_string _content 0 : int)
   | Ppx_debug_runtime.Id.{ file = "demo/lib/lib.ml"; id = 2; _ } ->
     Format.asprintf "%a" Format.pp_print_int
       (Marshal.from_string _content 0 : int)
   | Ppx_debug_runtime.Id.{ file; id; _ } ->
     failwith (Format.sprintf "unknown type %s %d" file id) *)

type 'a ppf = Format.formatter -> 'a -> unit
type 'a ppv = 'a * 'a ppf
type t = string * string [@@deriving show { with_path = false }]

type dyn =
  | Int of int ppv
  | String of string ppv
  | T of t ppv

let pp_dyn fmt dyn =
  match dyn with
  | Int (a, pp) -> Format.fprintf fmt "int %a" pp a
  | String (a, pp) -> Format.fprintf fmt "string %a" pp a
  | T (a, pp) -> Format.fprintf fmt "t %a" pp a

(* # let a = String ("aaa", Format.pp_print_string);;
   val a : dyn = String ("aaa", <fun>)
   # Format.asprintf "%a" pp_dyn a;;
   - : string = "string aaa"
   # (match a with |String (s, _) -> s | _ -> failwith "asd") ^ "!";;
   - : string = "aaa!" *)

let backslash = Str.regexp {|\\|}
let content_marker = Str.regexp "_content"

let read ~print_value filename =
  (* let read_n n file =
       Bytes.init n (fun _i -> Scanf.bscanf file "%c" Fun.id) |> Bytes.to_string
     in
     let file = Scanf.Scanning.open_in_bin filename in
     let rec loop all =
       let typ = Scanf.bscanf file "%s@\n" (fun typ -> typ) in
       (* print_endline typ;
          print_endline (string_of_int (String.length typ)); *)
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
       | "arg" | "value" | "match" | "matchb" | "acall" | "bcall" ->
         let id = Id.deserialize file in
         let what = Scanf.bscanf file "%s@\n" (fun what -> what) in
         let time = Scanf.bscanf file "%d\n" (fun t -> t) in
         let len = Scanf.bscanf file "%d" (fun t -> t) in
         let raw = Bytestr (read_n len file) in
         Scanf.bscanf file "\n" ();
         let v, unmarshal = print_value id raw in
         let next =
           match typ with
           | "arg" -> Argument { time; id; name = what; content = v; unmarshal }
           | "value" -> Value { time; id; name = what; content = v; unmarshal }
           | "match" ->
             MatchScrutinee { time; id; name = what; content = v; unmarshal }
           | "matchb" ->
             MatchBranch { time; id; name = what; content = v; unmarshal }
           | "acall" -> AfterCall { time; id; name = what; content = v; unmarshal }
           | "bcall" ->
             BeforeCall { time; id; name = what; content = v; unmarshal }
           | _ -> failwith "invalid"
         in
         loop (next :: all)
       | "" -> List.rev all
       | _ -> failwith ("invalid " ^ typ)
     in
     let res = loop [] in
     Scanf.Scanning.close_in file; *)
  let res = Ppx_debug_runtime.Trace.read ~print_value filename in
  (* res *)
  let tree = Ppx_debug_runtime.Trace.to_call_tree res in
  let rec traverse f tree =
    match tree with
    | Ppx_debug_runtime.Trace.Call { i; calls; args; _ } ->
      f tree i args;
      List.iter (traverse f) calls
    | Event { i; content; _ } -> f tree i [("val", content)]
  in
  (* traverse
     (fun _t i c -> match mapping i with None -> () | Some r -> r := c)
     tree; *)
  let find_i = 2 in
  let find f which =
    traverse (fun t i _c -> if which = i then f t else ()) tree
  in
  let z =
    let strs = ref [] in
    find
      (fun t ->
        (* print_endline (Ppx_debug_runtime.Trace.show_call t); *)
        match t with
        | Ppx_debug_runtime.Trace.Call { unmarshal; _ } ->
          strs :=
            List.map
              (fun (k, (Bytestr r, v)) ->
                (* Format.printf "v: %s@." v; *)
                let content =
                  "\""
                  ^ Str.global_replace backslash {|\\\\|} (String.escaped r)
                  ^ "\""
                in
                (* Format.printf "content: %s@." content; *)
                Format.asprintf "let %s = %s;;" k
                  (Str.global_replace content_marker content v))
              unmarshal
            @ !strs
        | Event { unmarshal; _ } ->
          strs := Format.asprintf "let v = %s;;" unmarshal :: !strs)
      find_i;
    !strs
  in
  (* List.iter print_endline z; *)
  (* Ppx_interact_runtime.interact ~read_and_print_value () *)
  (* print_endline "haha"; *)
  let w = 10 in
  Ppx_debug_interact.(
    interact ~init:z ~unit:__MODULE__ ~loc:__POS__
      ~values:
        [
          V ("y", 7);
          (* lol more hacks *)
          (* V ("s", "helo"); *)
          V ("w", w + 1);
          V ("res", res);
          V ("tree", tree);
          V ("find", find)
          (* V ("v0", !v0); *)
          (* V ("v62", !v62); *);
        ]
      ())

(* let () = read1 "debug.trace" *)
let () =
  (* let y = 1 in
     Ppx_debug_interact.(
       interact ~unit:__MODULE__ ~loc:__POS__ ~values:[V ("y", y)] ()) *)
  read ~print_value "debug.trace"

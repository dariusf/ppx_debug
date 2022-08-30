type mode =
  (* blacklist *)
  | All of string list
  (* whitelist *)
  | Some of string list
  | Modules of string list
[@@deriving yojson]

type variant =
  | Containers
  | Stdlib
[@@deriving yojson]

module SMap = struct
  module M = Map.Make (String)
  include M

  let pp pp_v fmt map =
    Format.fprintf fmt "@[<v 0>{@;<0 2>@[<v 0>%a@]@,}@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt (k, v) -> Format.fprintf fmt "%s: %a" k pp_v v))
      (bindings map)

  let update_ k f m =
    update k (function None -> failwith "invalid" | Some v -> Some (f v)) m

  let create xs = M.of_seq (List.to_seq xs)
  (* type 'a elements = (string * 'a) list [@@deriving yojson] *)

  (* let to_yojson af set = set |> bindings |> elements_to_yojson af

     let of_yojson af json =
       json |> elements_of_yojson af
       |> Result.map (fun a -> a |> List.to_seq |> of_seq) *)
end

type action =
  | Opaque
  | Rewrite of string
[@@deriving yojson, show]

type rewrites = action SMap.t SMap.t [@@deriving show]

let rewrites_to_yojson m =
  `Assoc
    (m |> SMap.bindings
    |> List.map (fun (k, v) ->
           ( k,
             `Assoc
               (v |> SMap.bindings
               |> List.map (fun (k, v) -> (k, action_to_yojson v))) )))

let rewrites_of_yojson json =
  try
    match json with
    | `Assoc kvs ->
      Ok
        (kvs
        |> List.map (fun (k, v) ->
               match v with
               | `Assoc kvs1 ->
                 let v =
                   kvs1
                   |> List.map (fun (k, v) ->
                          match action_of_yojson v with
                          | Ok v -> (k, v)
                          | Error s ->
                            failwith ("contents should be strings: " ^ s))
                   |> SMap.create
                 in
                 (k, v)
               | _ -> failwith "expected assoc")
        |> SMap.create)
    | _ -> failwith "not an assoc"
  with Failure s -> Error s

type t = {
  (* whether to run the ppx *)
  enabled : bool;
  (* control which functions/modules to log *)
  mode : mode;
  (* the file the raw trace should be written to *)
  file : string;
  (* whether to enable debug logging in the ppx, and the locations of those logs *)
  ppx_logging : bool;
  internal_log : string;
  internal_tool_log : string;
  (* whether to trace anonymous functions *)
  lambdas : bool;
  (* whether to trace match discriminees *)
  matches : bool;
  (* what sorts of printers to generate *)
  variant : variant;
  (* mappings *)
  (* file -> from_name -> to_name *)
  (* mappings : string SMap.t SMap.t; *)
  (* mappings : (string * (string * [ `Opaque | `Rewrite of string ]) list) list; *)
  (* string SMap.t SMap.t *)
  mappings : rewrites;
      [@to_yojson rewrites_to_yojson] [@of_yojson rewrites_of_yojson]
  treat_as_opaque : string list;
}
[@@deriving yojson]

let default =
  {
    enabled = true;
    mode = All [];
    file = "debug.trace";
    ppx_logging = true;
    internal_log = "/tmp/ppx_debug.txt";
    internal_tool_log = "/tmp/ppx_debug_tool.txt";
    lambdas = true;
    matches = true;
    variant = Stdlib;
    (* mappings = [] *)
    (* mappings = [] SMap.empty;; *)
    mappings = SMap.empty;
    treat_as_opaque =
      []
      (* mappings =
           [
             ( "ppx_debug/ppx_debug.ml",
               [
                 ("C.t", `Opaque);
                 ("C.mode", `Opaque);
                 ("param", `Opaque);
                 ("func", `Opaque);
               ] );
             ( "ppx_debug_runtime/trace.ml",
               [
                 ("Id.t", `Rewrite "Ppx_debug_runtime.Trace.Id.t");
                 (* ("t", "Ppx_debug_runtime.Trace.t"); *)
                 ("t", `Opaque);
               ] );
             ( "ppx_debug_runtime/config.ml",
               [
                 (* this is weird *)
                 ("key", `Opaque);
                 ("t", `Opaque);
                 ("mode", `Opaque);
                 ("variant", `Opaque);
               ] );
           ]
           (* SMap.of_seq (List.to_seq [, SMap.]) *)
           (* [; [k]] *);
         treat_as_opaque =
           [
             "Astlib__.Ast_412.Parsetree.expression";
             "Astlib__.Ast_412.Parsetree.structure";
             "Astlib__.Ast_412.Parsetree.structure_item";
             "Astlib__.Ast_412.Parsetree.pattern";
             "Astlib__.Ast_412.Parsetree.core_type";
             "Stdlib.format4";
             "Ppxlib.pattern_desc";
             "Ppxlib.pattern";
             "Ppxlib.attribute";
             "Ppxlib.longident";
             "Ppxlib_ast__Ast_helper_lite.loc";
             "Ppxlib.core_type_desc";
             "Astlib.Ast_412.Parsetree.expression";
             "Ppxlib__.Import.Ast.expression";
             "Ppxlib__.Import.Ast.structure_item";
             "Ppxlib.value_binding";
             "Ppxlib.expression";
             "Ppxlib.location";
             "Ppxlib.core_type";
             "Ppxlib_ast__Ast_helper_lite.pp_loc";
             "Ppxlib__.Import.expression";
             "Ppxlib.expression_desc";
             "Ppxlib.structure_item";
             "Ppxlib.case";
             "Ppxlib.rec_flag";
             "Astlib.Ast_412.Parsetree.structure_item";
             "Ppx_deriving_yojson_runtime.error_or";
             "Ppx_deriving_yojson_runtime.Result.result";
             "Stdlib.Scanf.Scanning.file_name";
             "Stdlib.Scanf.Scanning.in_channel";
             "Stdlib.in_channel";
             "Stdlib.out_channel";
             (* this should be ignored as a builtin *)
             "Stdlib.String.t";
           ]; *);
  }

(* mode=All,f,g+blah=a,b,c *)
let parse s =
  let kvps = s |> String.split_on_char '+' in
  List.fold_right
    (fun c t ->
      match c |> String.trim |> String.split_on_char '=' with
      | ["enabled"; "true"] -> { t with enabled = true }
      | ["enabled"; "false"] -> { t with enabled = false }
      | ["lambdas"; "true"] -> { t with lambdas = true }
      | ["lambdas"; "false"] -> { t with lambdas = false }
      | ["matches"; "true"] -> { t with matches = true }
      | ["matches"; "false"] -> { t with matches = false }
      | ["file"; f] -> { t with file = f }
      | ["log"; "true"] -> { t with ppx_logging = true }
      | ["log"; "false"] -> { t with ppx_logging = false }
      | ["variant"; "containers"] -> { t with variant = Containers }
      | ["variant"; "stdlib"] -> { t with variant = Stdlib }
      | ["mode"; v] ->
        begin
          match String.split_on_char ',' v with
          | "All" :: blacklist -> { t with mode = All blacklist }
          | "Some" :: whitelist -> { t with mode = Some whitelist }
          | "Modules" :: whitelist -> { t with mode = Modules whitelist }
          | m -> failwith ("unable to parse mode " ^ String.concat "," m)
        end
      | _ -> failwith ("unable to parse config: " ^ s))
    kvps default

let parse s =
  let x = s |> Yojson.Safe.from_string |> of_yojson in
  match x with
  | Ok y -> y
  | Error s ->
    print_endline s;
    failwith s
(* Result.get_ok x *)

(* memoize because this may be called many times and environment variables don't change *)
let read =
  let config = ref None in
  fun () ->
    (* print_endline (default |> to_yojson |> Yojson.Safe.to_string); *)
    match !config with
    | None ->
      let c =
        match Sys.getenv_opt "PPX_DEBUG" with
        | None -> default
        | Some s ->
          (* default *)
          parse s
      in
      config := Some c;
      c
    | Some c -> c

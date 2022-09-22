type variant =
  | Containers
  | Stdlib
[@@deriving yojson, show { with_path = false }]

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
  (* whether to enable debug logging in the ppx itself, and the locations of its log files *)
  ppx_logging : bool; [@default true]
  internal_log : string; [@default "/tmp/ppx_debug.txt"]
  internal_tool_log : string; [@default "/tmp/ppx_debug_tool.txt"]
  (* whether to run the instrumentation ppx *)
  should_instrument : bool; [@default true]
  (* which functions/modules should be instrumented. *)
  instrument_modules : string; [@default ".*"]
      (* the function blacklist is applied on top of the whitelist *)
  instrument_functions : string; [@default ".*"]
  do_not_instrument_functions : string; [@default " "]
  (* file raw trace should be written to *)
  file : string; [@default "debug.trace"]
  (* TODO this needs more testing *)
  randomize_filename : bool; [@default false]
  (* whether to instrument the given syntactic constructs *)
  should_instrument_lambdas : bool; [@default true]
  should_instrument_matches : bool; [@default true]
  should_instrument_calls : bool; [@default true]
  (* again the blacklist is applied on top of the whitelist *)
  should_instrument_definitions : bool; [@default true]
  (* (module name, function name) pairs *)
  should_not_instrument_definitions : (string * string) list; [@default []]
  (* what sort of printers to generate *)
  variant : variant; [@default Stdlib]
  (* do not search these directories for cmt files *)
  cmt_ignored_directories : string list; [@default ["test/"]]
  (* if (dune) libraries are nested more than one directory deep, provide their entrypoint modules here, e.g. demo/lib *)
  libraries : string list; [@default []]
  (* do not search cmt files for printers of types whose names match these regexes *)
  opaque_type_names : string list; [@default []]
  (* mappings *)
  (* TODO needs more testing *)
  (* file -> from_name -> to_name *)
  mappings : rewrites;
      [@to_yojson rewrites_to_yojson]
      [@of_yojson rewrites_of_yojson]
      [@default SMap.empty]
}
[@@deriving yojson { strict = false }, show { with_path = false }]

let get_file d =
  if d.randomize_filename then
    Format.asprintf "/tmp/%d_%s" (int_of_float (Unix.gettimeofday ())) d.file
  else d.file

let default = of_yojson (`Assoc []) |> Result.get_ok

let parse s =
  let x = s |> Yojson.Safe.from_string |> of_yojson in
  match x with
  | Ok y -> y
  | Error s -> failwith (Format.asprintf "failed to parse config: %s" s)

(* memoize because this may be called many times and environment variables don't change *)
let read =
  let config = ref None in
  fun () ->
    match !config with
    | None ->
      let c =
        match Sys.getenv_opt "PPX_DEBUG" with
        | None -> default
        | Some s -> parse s
      in
      config := Some c;
      c
    | Some c -> c

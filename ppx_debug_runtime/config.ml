type mode =
  (* blacklist *)
  | All of string list
  (* whitelist *)
  | Some of string list
  | Modules of string list
[@@deriving yojson, show { with_path = false }]

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
  (* whether to run the ppx *)
  enabled : bool; [@default true]
  (* control which functions/modules to log *)
  (* deprecated *)
  mode : mode; [@default All []]
  (* control which functions/modules to log via regexes *)
  instrument_modules : string; [@default ".*"]
  instrument_functions : string; [@default ".*"]
  function_blacklist : string; [@default " "]
  light_logging : (string * string) list; [@default []]
  (* the file the raw trace should be written to *)
  file1 : string; [@default "debug.trace"]
  randomize_filename : bool; [@default false]
  (* whether to enable debug logging in the ppx, and the locations of those logs *)
  ppx_logging : bool; [@default true]
  internal_log : string; [@default "/tmp/ppx_debug.txt"]
  internal_tool_log : string; [@default "/tmp/ppx_debug_tool.txt"]
  (* whether to trace anonymous functions *)
  lambdas : bool; [@default true]
  (* whether to trace match discriminees *)
  matches : bool; [@default true]
  (* whether to trace function calls *)
  calls : bool; [@default true]
  (* what sorts of printers to generate *)
  variant : variant; [@default Stdlib]
  (* mappings *)
  (* file -> from_name -> to_name *)
  (* mappings : string SMap.t SMap.t; *)
  (* mappings : (string * (string * [ `Opaque | `Rewrite of string ]) list) list; *)
  (* string SMap.t SMap.t *)
  mappings : rewrites;
      [@to_yojson rewrites_to_yojson]
      [@of_yojson rewrites_of_yojson]
      [@default SMap.empty]
  treat_as_opaque : string list; [@default []]
  libraries : string list; [@default []]
}
[@@deriving yojson { strict = false }, show { with_path = false }]

let get_file d =
  if d.randomize_filename then
    Format.asprintf "/tmp/%d_%s" (int_of_float (Unix.gettimeofday ())) d.file1
  else d.file1

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

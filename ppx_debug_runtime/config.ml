type mode =
  (* blacklist *)
  | All of string list
  (* whitelist *)
  | Some of string list
  | Modules of string list

type variant =
  | Containers
  | Stdlib

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
}

let default =
  {
    enabled = true;
    mode = All [];
    file = "debug.trace";
    ppx_logging = false;
    internal_log = "/tmp/ppx_debug.txt";
    internal_tool_log = "/tmp/ppx_debug_tool.txt";
    lambdas = true;
    matches = true;
    variant = Stdlib;
  }

(* mode=All,f,g; blah=a,b,c *)
let parse s =
  let kvps = s |> String.split_on_char ';' in
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
          | _ -> failwith "unable to parse"
        end
      | _ -> failwith "unable to parse")
    kvps default

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

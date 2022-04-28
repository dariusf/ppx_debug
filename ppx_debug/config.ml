type mode =
  (* blacklist *)
  | All of string list
  (* whitelist *)
  | Some of string list
  | Modules of string list

type t = {
  enabled : bool;
  mode : mode;
}

let default = { enabled = true; mode = All [] }

(* mode=All,f,g; blah=a,b,c *)
let parse s =
  let kvps = s |> String.split_on_char ';' in
  List.fold_right
    (fun c t ->
      match c |> String.trim |> String.split_on_char '=' with
      | ["enabled"; "true"] -> { t with enabled = true }
      | ["enabled"; "false"] -> { t with enabled = false }
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

let read () =
  match Sys.getenv_opt "PPX_DEBUG" with None -> default | Some s -> parse s

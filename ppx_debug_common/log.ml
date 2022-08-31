open Containers

(* logging the low-tech way *)
(* IO.File.remove_noerr filename; *)

(* IO.File.make (Sys.getcwd () ^ "/ppx_debug.txt") *)
module Config = Ppx_debug_runtime.Config

let logfile = ref None

let log fmt =
  let lf =
    match !logfile with
    | None ->
      let l = IO.File.make (Config.read ()).internal_log in
      logfile := Some l;
      l
    | Some l -> l
  in
  Format.kasprintf
    (fun s ->
      if (Config.read ()).ppx_logging then IO.File.append_exn lf (s ^ "\n"))
    fmt
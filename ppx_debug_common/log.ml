open Containers

(* logging the low-tech way *)

module Config = Ppx_debug_runtime.Config

module Make (S : sig
  val name : string
end) =
struct
  let logfile = ref None

  let log fmt =
    let lf =
      match !logfile with
      | None ->
        let l = IO.File.make S.name in
        logfile := Some l;
        l
      | Some l -> l
    in
    Format.kasprintf
      (fun s ->
        if (Config.read ()).ppx_logging then IO.File.append_exn lf (s ^ "\n"))
      fmt
end

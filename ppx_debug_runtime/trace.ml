(* let channels : string list ref = ref [] *)
let open_channels : (string * out_channel) list ref = ref []

module Id = struct
  type t = string * string * int [@@deriving show { with_path = false }]

  let serialize (fl, fu, id) = Format.sprintf "%s:%s:%d" fl fu id

  let deserialize file =
    Scanf.bscanf file "%s@:%s@:%d\n" @@ fun fl fu id -> (fl, fu, id)

  let show (fl, fu, id) = Format.sprintf "(file: %s, func: %s, id: %d)" fl fu id
end

(* really simple internal trace format. this is written in binary mode (via the emit_* functions, which marshal ocaml values) and read together with type metadata to unmarshal values (producing text) *)
type event =
  | FrameStart of string
  | Value of Id.t * string
  | FrameEnd of string
[@@deriving show { with_path = false }]

type t = event list

let to_file filename f =
  let file = open_out_bin filename in
  f file;
  close_out file

let new_channel c =
  (* channels := c :: !channels; *)
  c

let lazy_init c =
  if not (List.mem_assoc c !open_channels) then (
    let f = open_out_bin (c ^ ".bin") in
    at_exit (fun () -> close_out f);
    open_channels := (c, f) :: !open_channels;
    f)
  else List.assoc c !open_channels

(* if !disable_logs || Sys.getenv "LOG" = "off" then begin
     print_endline "disabled";
     stdout
   end
   else begin
     print_endline "run";
     let file = open_out_bin filename in
     at_exit (fun () -> close_out file);
     file
   end *)

let emit_start ~ppx_debug_file ~func =
  Printf.fprintf (lazy_init ppx_debug_file) "start\n%s\n" func

let emit_value ~ppx_debug_file ~ppx_debug_id:id what v =
  (* Format.printf "%s, v: %s@." (Id.show id) (Marshal.to_string v []) *)
  let s = Marshal.to_string v [] in
  Printf.fprintf (lazy_init ppx_debug_file) "value\n%s\n%s\n%s\n"
    (Id.serialize id) what s

let emit_end ~ppx_debug_file ~func =
  Printf.fprintf (lazy_init ppx_debug_file) "end\n%s\n" func

let read ~read_and_print_value filename =
  let file = Scanf.Scanning.open_in_bin filename in
  let rec loop all =
    (* try *)
    let typ = Scanf.bscanf file "%s@\n" (fun typ -> typ) in
    (* print_endline typ; *)
    (* ( *)
    match typ with
    | "start" ->
      (* let id = Scanf.bscanf file "%s@\n" (fun id -> id) in *)
      (* let id = Id.deserialize file in *)
      let func = Scanf.bscanf file "%s@\n" (fun id -> id) in
      loop (FrameStart func :: all)
    | "end" ->
      (* let id = Scanf.bscanf file "%s@\n" (fun id -> id) in *)
      (* let id = Id.deserialize file in *)
      let func = Scanf.bscanf file "%s@\n" (fun id -> id) in
      loop (FrameEnd func :: all)
    | "value" ->
      let id = Id.deserialize file in
      let what = Scanf.bscanf file "%s@\n" (fun what -> what) in
      let v = read_and_print_value file id in
      (* print_endline v; *)
      loop (Value (id, Format.sprintf "%s: %s" what v) :: all)
    | "" -> List.rev all
    | _ -> failwith ("invalid " ^ typ)
    (* ) *)
    (* |> loop *)
    (* with End_of_file -> List.rev all *)
  in
  let res = loop [] in
  Scanf.Scanning.close_in file;
  res

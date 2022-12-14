type misc = Misc of int [@@deriving show { with_path = false }]

let func a = match a with Misc b -> b
let get () = Misc 1

module Int = struct
  type t = int

  let of_int x = x
  let to_int x = x
  let pp = Format.pp_print_int
end

module Priv : sig
  type t = private int

  val of_int : int -> t
  val to_int : t -> int
  val pp : Format.formatter -> t -> unit
end =
  Int

module Abstr : sig
  type t

  val of_int : int -> t
  val to_int : t -> int
  val pp : Format.formatter -> t -> unit
end =
  Int

module Trace = Trace
module Id = Trace.Id
module Viewer = Viewer
module Chrome_trace = Chrome_trace
module Config = Config

let read_n n file =
  Bytes.init n (fun _i -> Scanf.bscanf file "%c" Fun.id) |> Bytes.to_string

type misc = Qual of int [@@deriving show { with_path = false }]

let func a = match a with Qual b -> b

open Ppxlib

let pexpr = Pprintast.expression
let pstr = Pprintast.structure

let debug_pexpr fmt { pexp_desc; _ } =
  let name =
    match pexp_desc with
    | Pexp_ident _ -> "Pexp_ident"
    | Pexp_constant _ -> "Pexp_constant"
    | Pexp_let (_, _, _) -> "Pexp_let"
    | Pexp_function _ -> "Pexp_function"
    | Pexp_fun (_, _, _, _) -> "Pexp_fun"
    | Pexp_apply (_, _) -> "Pexp_apply"
    | Pexp_match (_, _) -> "Pexp_match"
    | Pexp_try (_, _) -> "Pexp_try"
    | Pexp_tuple _ -> "Pexp_tuple"
    | Pexp_construct (_, _) -> "Pexp_construct"
    | Pexp_variant (_, _) -> "Pexp_variant"
    | Pexp_record (_, _) -> "Pexp_record"
    | Pexp_field (_, _) -> "Pexp_field"
    | Pexp_setfield (_, _, _) -> "Pexp_setfield"
    | Pexp_array _ -> "Pexp_array"
    | Pexp_ifthenelse (_, _, _) -> "Pexp_ifthenelse"
    | Pexp_sequence (_, _) -> "Pexp_sequence"
    | Pexp_while (_, _) -> "Pexp_while"
    | Pexp_for (_, _, _, _, _) -> "Pexp_for"
    | Pexp_constraint (_, _) -> "Pexp_constraint"
    | Pexp_coerce (_, _, _) -> "Pexp_coerce"
    | Pexp_send (_, _) -> "Pexp_send"
    | Pexp_new _ -> "Pexp_new"
    | Pexp_setinstvar (_, _) -> "Pexp_setinstvar"
    | Pexp_override _ -> "Pexp_override"
    | Pexp_letmodule (_, _, _) -> "Pexp_letmodule"
    | Pexp_letexception (_, _) -> "Pexp_letexception"
    | Pexp_assert _ -> "Pexp_assert"
    | Pexp_lazy _ -> "Pexp_lazy"
    | Pexp_poly (_, _) -> "Pexp_poly"
    | Pexp_object _ -> "Pexp_object"
    | Pexp_newtype (_, _) -> "Pexp_newtype"
    | Pexp_pack _ -> "Pexp_pack"
    | Pexp_open (_, _) -> "Pexp_open"
    | Pexp_letop _ -> "Pexp_letop"
    | Pexp_extension _ -> "Pexp_extension"
    | Pexp_unreachable -> "Pexp_unreachable"
  in
  Format.fprintf fmt "%s" name

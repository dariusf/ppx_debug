open Containers
open Ppxlib

let p_si si = Format.printf "structure_item %a@." Pprintast.structure_item si
let p_s s = Format.printf "structure %a@." Pprintast.structure s
let p_e e = Format.printf "expression %a@." Pprintast.expression e
let p_p p = Format.printf "pattern %a@." Pprintast.pattern p
let p_t t = Format.printf "type %a@." Pprintast.core_type t

let dummy_loc =
  {
    Location.loc_start = Lexing.dummy_pos;
    Location.loc_end = Lexing.dummy_pos;
    Location.loc_ghost = true;
  }

let get_fn_name pat =
  match pat with
  | { ppat_desc = Ppat_var { txt = fn_name; _ }; _ } -> fn_name
  | {
   ppat_desc =
     Ppat_constraint ({ ppat_desc = Ppat_var { txt = fn_name; _ }; _ }, _);
   _;
  } ->
    fn_name
  | _ ->
    p_p pat;
    failwith "not a function pattern"

let fresh =
  let n = ref 0 in
  fun () ->
    let r = !n in
    incr n;
    Format.sprintf "v%d" r

type label = arg_label * expression option

type param =
  | Param of {
      label : label;
      param : string loc;
    }
  | Unit of { label : label }

let rec collect_params f =
  match f with
  | { pexp_desc = Pexp_fun (lbl, lbl_e, { ppat_desc = desc; _ }, rest); _ } ->
    let label = (lbl, lbl_e) in
    begin
      match desc with
      | Ppat_var { txt = param; _ }
      | Ppat_constraint ({ ppat_desc = Ppat_var { txt = param; _ }; _ }, _) ->
        let param = { txt = param; loc = dummy_loc } in
        Param { param; label } :: collect_params rest
      | Ppat_construct ({ txt = Lident "()"; _ }, None) ->
        Unit { label } :: collect_params rest
      | Ppat_any ->
        let param = { txt = fresh (); loc = dummy_loc } in
        Param { param; label } :: collect_params rest
      | _ -> []
    end
  | { pexp_desc = Pexp_constraint (e, _); _ } -> collect_params e
  | _ -> []

let clamp l h x = max l (min h x)

let has_attr name attr =
  match attr with
  | { attr_name = Loc.{ txt = n; _ }; _ } when String.equal n name -> true
  | _ -> false

let is_function_binding b =
  match b with
  | { pvb_expr = { pexp_desc = Pexp_fun _; _ }; _ } -> true
  | _ -> false

let interesting_expr_binding rec_flag attrs binding =
  let has_attribute =
    match rec_flag with
    | Nonrecursive ->
      if List.exists (has_attr "tracerec") attrs then
        failwith "tracerec used on nonrecursive binding"
      else List.exists (has_attr "trace") attrs
    | Recursive ->
      List.exists (has_attr "trace") attrs
      || List.exists (has_attr "tracerec") attrs
  in
  has_attribute && is_function_binding binding

let interesting_str_binding rec_flag binding =
  let attrs = binding.pvb_attributes in
  interesting_expr_binding rec_flag attrs binding

let extract_binding_info b =
  let { pvb_pat = original_lhs; pvb_expr = original_rhs; _ } = b in
  let fn_name = get_fn_name original_lhs in
  let params = collect_params original_rhs in

  (original_rhs, fn_name, params)

(** Recurses down a curried lambda to get the body *)
let rec get_constrained_fn ?(loc = dummy_loc) pexp =
  match pexp with
  | { pexp_desc = Pexp_constraint (e, _); pexp_loc; _ } ->
    get_constrained_fn ~loc:pexp_loc e
  | _ -> (pexp, loc)

let app_traverse find replace =
  let open Ast_helper in
  object
    inherit Ast_traverse.map as super

    method! expression expr =
      let expr = super#expression expr in
      match expr with
      (* | {
          pexp_desc =
            Pexp_apply
              ({ pexp_desc = Pexp_ident { txt = Lident fn_name; loc }; _ }, args);
          _;
         }
           when String.equal fn_name find ->
           Exp.apply ~loc (Exp.ident ~loc { txt = Lident replace; loc }) args
         | {
          pexp_desc =
            Pexp_apply
              ( { pexp_desc = Pexp_ident { txt = Ldot (initial, fn_name); loc }; _ },
                args );
          _;
         }
           when String.equal fn_name find ->
           Exp.apply ~loc
             (Exp.ident ~loc { txt = Ldot (initial, replace); loc })
             args *)
      | { pexp_desc = Pexp_ident { txt = Lident fn_name; loc }; _ }
        when String.equal fn_name find ->
        Exp.ident ~loc { txt = Lident replace; loc }
      | { pexp_desc = Pexp_ident { txt = Ldot (initial, fn_name); loc }; _ }
        when String.equal fn_name find ->
        Exp.ident ~loc { txt = Ldot (initial, replace); loc }
      | _ -> expr
  end

let mangle fn_name = fn_name ^ "_original"

let rec fun_with_params ?(loc = dummy_loc) params body =
  let open Ast_helper in
  match params with
  | [] -> body
  | p :: ps ->
    let p', (lbl, lbl_e) =
      match p with
      | Param { param = v; label } -> (Pat.var v, label)
      | Unit { label } -> (Pat.construct { txt = Lident "()"; loc } None, label)
    in
    Exp.fun_ ~loc lbl lbl_e p' (fun_with_params ~loc ps body)

let ident ?(loc = dummy_loc) s =
  let open Ast_helper in
  Exp.ident ~loc { txt = Lident s; loc }

let qualified_ident ?(loc = dummy_loc) ss =
  let open Ast_helper in
  match ss with
  | [] -> failwith "qualified_ident requires a non-empty list"
  | [s] -> ident ~loc s
  | s :: ss ->
    let res = List.fold_left (fun t c -> Ldot (t, c)) (Lident s) ss in
    Exp.ident ~loc { txt = res; loc }

let rec fun_wildcards ?(loc = dummy_loc) n body =
  let open Ast_helper in
  match n with
  | 0 -> body
  | _ ->
    Exp.fun_ ~loc Nolabel None (Pat.any ()) (fun_wildcards ~loc (n - 1) body)

let app ?(loc = dummy_loc) f args =
  let open Ast_helper in
  Exp.apply ~loc f (List.map (fun a -> (Nolabel, a)) args)

let str ?(loc = dummy_loc) s =
  let open Ast_helper in
  Exp.constant ~loc (Pconst_string (s, loc, None))

let param_to_expr ?(loc = dummy_loc) p =
  let open Ast_helper in
  match p with
  | Unit _ -> Exp.construct ~loc { txt = Lident "()"; loc } None
  | Param { param; _ } -> Exp.ident ~loc { txt = Lident param.txt; loc }

let param_to_str p =
  match p with Unit _ -> str "()" | Param { param; _ } -> str param.txt

let show_param p = match p with `Unit -> "()" | `Param x -> x

let rec show_longident l =
  match l with
  | Lident s -> s
  | Ldot (l, s) -> show_longident l ^ s
  | Lapply (a, b) ->
    Format.sprintf "apply %s %s" (show_longident a) (show_longident b)

module Longident = struct
  include Longident

  let pp fmt l = Format.fprintf fmt "%s" (show_longident l)
end

type ptype =
  | PType of Longident.t * ptype list
  | PTuple of ptype list
  | PPoly of string
  | PPolyVariant
[@@deriving show]

let rec arrow_to_list t =
  match t.ptyp_desc with
  | Ptyp_poly (_, t) -> arrow_to_list t
  | Ptyp_constr ({ txt = name; _ }, params) ->
    [PType (name, List.concat_map arrow_to_list params)]
  | Ptyp_var name -> [PPoly name]
  (* | Ptyp_arrow (_, { ptyp_desc = Ptyp_constr ({ txt = name; _ }, params); _ }, b)
     ->
     PType (name, List.concat_map arrow_to_list params) :: arrow_to_list b *)
  (* | Ptyp_arrow (_, { ptyp_desc = Ptyp_var name; _ }, b) ->
     PPoly name :: arrow_to_list b *)
  | Ptyp_arrow (_, a, b) -> arrow_to_list a @ arrow_to_list b
  (* | Ptyp_constr ({ txt = name; _ }, params) ->
     [PType (name, List.concat_map arrow_to_list params)] *)
  | Ptyp_tuple params -> [PTuple (params |> List.concat_map arrow_to_list)]
  | Ptyp_variant (_, _, _) -> [PPolyVariant]
  | _ ->
    p_t t;
    failwith "could not convert arrow to list"

let interpret_type t =
  let a = arrow_to_list t in
  let l = List.length a in
  match l with
  | 0 -> failwith "empty type"
  | 1 -> failwith "not a function"
  | _ ->
    (* List.sub.arrow_to_list t *)
    let a, b = List.take_drop (l - 1) a in
    (a, List.hd b)

(* let rec generate_printer_typ t =
   let containers = true in
   let open Ast_helper in
   let loc = dummy_loc in
   match t with
   | PType (Lident "int", []) ->
     (match containers with
     | true -> [%expr CCInt.pp]
     | _ -> [%expr Format.pp_print_int])
   | PType (Lident "float", []) ->
     (match containers with
     | true -> [%expr CCFloat.pp]
     | _ -> [%expr Format.pp_print_float])
   | PType (Lident "string", []) ->
     (match containers with
     | true -> [%expr CCString.pp]
     | _ -> [%expr Format.pp_print_string])
   | PType (Lident "option", [a]) ->
     (match containers with
     | true -> [%expr CCOpt.pp [%e generate_printer_typ a]]
     | _ -> [%expr Format.pp_print_option [%e generate_printer_typ a]])
   | PType (Lident "result", [a; b]) ->
     (* can't use this as it specializes the second arg to string *)
     (* (match containers with
        | true ->
          [%expr Result.pp [%e generate_printer_typ a] [%e generate_printer_typ b]]
        | _ -> *)
     [%expr
       Format.pp_print_result ~ok:[%e generate_printer_typ a]
         ~error:[%e generate_printer_typ b]]
     (* ) *)
   | PType (Lident "list", [a]) ->
     (match containers with
     | true ->
       [%expr
         CCList.pp
           ~pp_start:(fun fmt () -> Format.fprintf fmt "[")
           ~pp_stop:(fun fmt () -> Format.fprintf fmt "]")
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";")
           [%e generate_printer_typ a]]
     | _ -> [%expr Format.pp_print_list [%e generate_printer_typ a]])
   | PType (Lident name, args) ->
     let fn = Exp.ident { txt = Lident ("pp_" ^ name); loc } in
     let args =
       args |> List.map generate_printer_typ |> List.map (fun e -> (Nolabel, e))
     in
     (match args with [] -> fn | _ -> Exp.apply fn args)
   | PType (Ldot (m, "t"), args) ->
     let fn = Exp.ident { txt = Ldot (m, "pp"); loc } in
     let args =
       args |> List.map generate_printer_typ |> List.map (fun e -> (Nolabel, e))
     in
     (* ensure we don't break parsetree invariants. this happens with a type like G.t, without args *)
     (match args with [] -> fn | _ -> Exp.apply fn args)
   | PType (Ldot (m, name), args) ->
     let fn = Exp.ident { txt = Ldot (m, "pp_" ^ name); loc } in
     let args =
       args |> List.map generate_printer_typ |> List.map (fun e -> (Nolabel, e))
     in
     (match args with [] -> fn | _ -> Exp.apply fn args)
     (* factor out *)
   | PTuple args ->
     let fn =
       match List.length args with
       | 2 ->
         [%expr
           Ppx_debug.Pair.pp
             ~pp_start:(fun fmt () -> Format.fprintf fmt "(")
             ~pp_stop:(fun fmt () -> Format.fprintf fmt ")")]
       | 3 ->
         [%expr
           Ppx_debug.Triple.pp
             ~pp_start:(fun fmt () -> Format.fprintf fmt "(")
             ~pp_stop:(fun fmt () -> Format.fprintf fmt ")")]
       | 4 ->
         [%expr
           Ppx_debug.Quad.pp
             ~pp_start:(fun fmt () -> Format.fprintf fmt "(")
             ~pp_stop:(fun fmt () -> Format.fprintf fmt ")")]
       | 5 ->
         [%expr
           Ppx_debug.Quint.pp
             ~pp_start:(fun fmt () -> Format.fprintf fmt "(")
             ~pp_stop:(fun fmt () -> Format.fprintf fmt ")")]
       | _ ->
         failwith ("unsupported tuple length " ^ string_of_int (List.length args))
     in
     let args =
       args |> List.map generate_printer_typ |> List.map (fun e -> (Nolabel, e))
     in
     let labelled =
       [ (* (Labelled "start", [%expr fun fmt () -> Format.fprintf fmt "("]); *)
         (* (Labelled "stop", [%expr fun fmt () -> Format.fprintf fmt ")"]); *) ]
     in
     Exp.apply fn (labelled @ args)
     (* factor out *)
   | PPoly _ ->
     (* [%expr fun fmt _ -> Format.fprintf fmt "%s" "_"] *)
     [%expr Ppx_debug.pp_const "_polymorphic"]
   | PPolyVariant -> [%expr Ppx_debug.pp_const "_poly_variant"]
   | _ ->
     Format.printf "%s@." (show_ptype t);
     failwith "generate_printer_typ: unrecognized type" *)

(* let pppp fmt _ = Format.fprintf fmt "%s" "<abstr>" *)

let ids = ref 0

let fresh () =
  let r = !ids in
  incr ids;
  r

module A = Ast_builder.Default

let generate_value ~loc cu what v =
  [%expr
    Ppx_debug_runtime.Trace.emit_value ~ppx_debug_file
      ~ppx_debug_id:([%e A.estring ~loc cu], "func", [%e A.eint ~loc (fresh ())])
      [%e A.estring ~loc what]
      [%e A.pexp_ident ~loc { loc; txt = Lident v }]]

let generate_arg ~loc cu what v =
  [%expr
    Ppx_debug_runtime.Trace.emit_argument ~ppx_debug_file
      ~ppx_debug_id:([%e A.estring ~loc cu], "func", [%e A.eint ~loc (fresh ())])
      [%e A.estring ~loc what]
      [%e A.pexp_ident ~loc { loc; txt = Lident v }]]

let generate_start ~loc what =
  [%expr
    Ppx_debug_runtime.Trace.emit_start ~ppx_debug_file
      ~func:[%e A.estring ~loc what]]

let generate_end ~loc what =
  [%expr
    Ppx_debug_runtime.Trace.emit_end ~ppx_debug_file
      ~func:[%e A.estring ~loc what]]

let run_invoc ~loc cu fn_expr fn_name params =
  (* TODO should fn_name be given to func in generate_printer? *)
  let start = generate_start ~loc fn_name in
  let stop = generate_end ~loc fn_name in
  let print_params =
    params
    |> List.filter_map (function
         | Param { param = { txt = s; _ }; _ } ->
           Some (generate_arg ~loc cu (Format.sprintf "%s %s" fn_name s) s)
         | Unit _ -> None)
  in
  let print_params =
    (* match print_params with
       | [] -> [%expr ()]
       | [e] -> e
       | f :: r ->
         List.fold_left (fun t c -> A.pexp_sequence ~loc c t) f r *)
    List.fold_right (A.pexp_sequence ~loc) print_params [%expr ()]
  in
  let call_fn =
    A.pexp_apply ~loc fn_expr
      (params
      |> List.map (function
           | Param { param = { txt = s; _ }; _ } ->
             (Nolabel, A.pexp_ident ~loc { loc; txt = Lident s })
           | Unit _ ->
             (Nolabel, A.pexp_construct ~loc { loc; txt = Lident "()" } None)))
  in
  let print_res =
    generate_arg ~loc cu (Format.sprintf "%s res" fn_name) "res"
  in
  [%expr
    [%e start];
    [%e print_params];
    let res = [%e call_fn] in
    [%e print_res];
    [%e stop];
    res]

(* (app ~loc run_fn_ident
   (List.concat
      [
        [[%expr __MODULE__]];
        [str ~loc fn_name; fn_expr];
        List.concat
          (List.map2
             (fun c (a, b) -> [c; a; b])
             (params |> List.map param_to_str)
             (List.map2
                (fun a b -> (a, b))
                param_printers
                (params |> List.map param_to_expr)));
        [result_printer];
      ]) [@metaloc loc]) *)

let transform_binding_nonrecursively cu b =
  let original_rhs, fn_name, params = extract_binding_info b in
  let original_fn_body, loc =
    let body, loc = get_constrained_fn original_rhs in
    let tr = app_traverse fn_name (mangle fn_name) in
    (* this is needed in the nonrecursive case because this may be used for recursive fns *)
    let new_body = tr#expression body in
    (new_body, loc)
  in

  (* let arg_types, result_type = interpret_type typ in *)
  (* let arg_printers = *)
  (* failwith "nyi" *)
  (* List.map generate_printer_typ arg_types *)
  (* List.map *)
  (* in *)
  (* let result_printer = failwith "nyi" generate_printer_typ result_type in *)
  (* the entire new rhs *)
  let new_rhs1 =
    let open Ast_helper in
    let ps =
      params
      (* |> List.map (fun p ->
             match p with
             | Param {param; label} -> `Param {label; param={ txt = s; loc = dummy_loc }}
             | Unit _ -> `Unit) *)
    in
    fun_with_params ~loc ps
      (Exp.let_ Nonrecursive
         [
           Ast_builder.Default.value_binding ~loc
             ~pat:(Pat.var { txt = mangle fn_name; loc = dummy_loc })
             ~expr:original_fn_body;
         ]
         (run_invoc ~loc cu (ident (mangle fn_name)) fn_name params))
  in
  [{ b with pvb_expr = new_rhs1 }]

let transform_binding_recursively cu b =
  let original_rhs, fn_name, params = extract_binding_info b in
  let original_fn_body, loc =
    let body, loc = get_constrained_fn original_rhs in
    let tr = app_traverse fn_name "self" in
    let new_body = tr#expression body in

    ( fun_with_params ~loc
        [
          Param
            {
              param = { txt = "self"; loc = dummy_loc };
              label = (Nolabel, None);
            };
        ]
        new_body,
      loc )
  in

  (* let arg_types, result_type = interpret_type typ in *)
  (* let arg_printers =
       failwith "nyi"
       (* List.map generate_printer_typ arg_types *)
     in *)
  (* let result_printer = failwith "nyi" generate_printer_typ result_type in *)
  (* the entire new rhs *)
  let new_rhs1 =
    let open Ast_helper in
    let run =
      run_invoc ~loc cu
        (* ~fn_expr: *)
        [%expr [%e ident (mangle fn_name)] aux]
        fn_name params
      (* arg_printers result_printer *)
    in
    let ps1 =
      params
      |> List.map (fun p ->
             match p with
             | Param { param; label = label, _ } ->
               (* optional args don't seem to be supported by Exp.apply? *)
               (label, ident ~loc param.txt)
             | Unit { label = label, _ } -> (label, [%expr ()]))
    in
    let aux = ident ~loc "aux" in
    fun_with_params ~loc params
      (Exp.let_ Nonrecursive
         [
           Ast_builder.Default.value_binding ~loc
             ~pat:(Pat.var { txt = mangle fn_name; loc = dummy_loc })
             ~expr:original_fn_body;
         ]
         (Exp.let_ Recursive
            [
              Ast_builder.Default.value_binding ~loc
                ~pat:(Pat.var { txt = "aux"; loc = dummy_loc })
                ~expr:(fun_with_params ~loc params run);
            ]
            (Exp.apply ~loc aux ps1)))
  in
  [{ b with pvb_expr = new_rhs1 }]

let transform_bindings comp_unit rec_flag bindings =
  match rec_flag with
  | Recursive ->
    List.concat_map
      (fun b -> transform_binding_recursively comp_unit b)
      bindings
  | Nonrecursive ->
    List.concat_map
      (fun b -> transform_binding_nonrecursively comp_unit b)
      bindings

let traverse comp_unit =
  object
    inherit Ast_traverse.map (* _with_expansion_context *) as super

    method! expression e =
      (* print_endline "expr";
         print_endline (Expansion_context.Base.tool_name ctx);
         print_endline (Expansion_context.Base.input_name ctx);
         let cp = Expansion_context.Base.code_path ctx in
         print_endline (Code_path.file_path cp);
         print_endline (Code_path.main_module_name cp);
         Format.printf "%a@." (List.pp String.pp) (Code_path.submodule_path cp);
         Format.printf "%a@." (Option.pp String.pp) (Code_path.value cp);
         print_endline (Code_path.fully_qualified_path cp);
         print_endline (Code_path.to_string_path cp); *)
      match e with
      | { pexp_desc = Pexp_let (rec_flag, bindings, body); _ } ->
        {
          e with
          pexp_desc =
            Pexp_let
              ( Nonrecursive,
                transform_bindings comp_unit rec_flag bindings,
                body );
        }
      | _ -> e

    method! structure_item si =
      (* print_endline "str"; *)
      (* print_endline path; *)
      let si = super#structure_item si in
      match si with
      | { pstr_desc = Pstr_value (rec_flag, bindings); _ } ->
        (* handle mutual recursion *)
        let flag = match bindings with [_] -> Nonrecursive | _ -> rec_flag in
        Ast_helper.Str.value flag
          (transform_bindings comp_unit rec_flag bindings)
      | _ -> si
  end

let handle_si ~ctxt str =
  (* Expansion_context.Extension.code_path ctxt *)
  let cp = Expansion_context.Extension.code_path ctxt in
  let comp_unit = Code_path.file_path cp in

  (* source file *)
  (* print_endline (Code_path.file_path cp); *)
  (* compilation unit *)
  (* print_endline (Code_path.main_module_name cp); *)
  (* prob useful *)
  (* Format.printf "%a@." (List.pp String.pp) (Code_path.submodule_path cp); *)
  (* unsure *)
  (* Format.printf "%a@." (Option.pp String.pp) (Code_path.value cp); *)
  (* same as compilation unit? *)
  (* print_endline (Code_path.fully_qualified_path cp); *)

  (* same as file path? *)
  (* print_endline (Code_path.to_string_path cp); *)

  (* print_endline "str"; *)
  (* path is the file path. which might be sufficient for uniqueness... *)
  (* TODO go up until git, since this will reside in dune build dir *)
  (* let config = Util.read_config () in *)
  (* if config.debug then *)
  (traverse comp_unit)#structure_item str
(* else *)
(* str *)

let handle_expr ~ctxt expr =
  let cp = Expansion_context.Extension.code_path ctxt in
  let comp_unit = Code_path.file_path cp in
  (* print_endline "expr"; *)
  (* let config = Util.read_config () in *)
  (* if config.debug then *)
  (traverse comp_unit)#expression expr
(* else *)
(* expr *)

(* let rule =
   Ppxlib.Context_free.Rule.extension
     (Extension.declare "trace" Structure_item
        Ast_pattern.(pstr (__ ^:: nil))
        handle_si) *)

let rule =
  Ppxlib.Context_free.Rule.extension
    (Extension.V3.declare "trace" Structure_item
       Ast_pattern.(pstr (__ ^:: nil))
       handle_si)

let rule_expr =
  Ppxlib.Context_free.Rule.extension
    (Extension.V3.declare "trace" Extension.Context.expression
       Ast_pattern.(single_expr_payload __)
       handle_expr)

let () = Driver.register_transformation ~rules:[rule; rule_expr] "ppx_debug"

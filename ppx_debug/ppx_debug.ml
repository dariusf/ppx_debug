module C = Ppx_debug_runtime.Config
open Containers
open Ppxlib

(* logging the low-tech way *)
let logfile =
  (* IO.File.remove_noerr filename; *)
  IO.File.make (C.read ()).internal_log
(* IO.File.make (Sys.getcwd () ^ "/ppx_debug.txt") *)

let log fmt =
  Format.kasprintf
    (fun s ->
      if (C.read ()).ppx_logging then IO.File.append_exn logfile (s ^ "\n"))
    fmt

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

module A = Ast_builder.Default

(* thrown when we can't transform this function for legitimate reasons *)
exception NotTransforming of string

let not_transforming fmt =
  Format.ksprintf ?margin:None ~f:(fun s -> raise (NotTransforming s)) fmt

let get_fn_name pat =
  match pat with
  | { ppat_desc = Ppat_var { txt = fn_name; _ }; _ } -> fn_name
  | {
   ppat_desc =
     Ppat_constraint ({ ppat_desc = Ppat_var { txt = fn_name; _ }; _ }, _);
   _;
  } ->
    fn_name
  | _ -> not_transforming "%a is not a function pattern" Pprintast.pattern pat

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

(* this repl *)
let replace_calls find replace =
  let open Ast_helper in
  object
    inherit Ast_traverse.map as super

    method! expression expr =
      let expr = super#expression expr in
      (* only replace unqualified names, as those are more likely to be calls *)
      (* TODO this is not capture-avoiding *)
      match expr with
      | { pexp_desc = Pexp_ident { txt = Lident fn_name; loc }; _ }
        when String.equal fn_name find ->
        Exp.ident ~loc { txt = Lident replace; loc }
      (* | { pexp_desc = Pexp_ident { txt = Ldot (initial, fn_name); loc }; _ }
         when String.equal fn_name find ->
         Exp.ident ~loc { txt = Ldot (initial, replace); loc } *)
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

let ids = ref 0

let fresh () =
  let r = !ids in
  incr ids;
  r

(* for now, we get ppx_debug_file by reading the environment, but removing that allows users to configure it through changing source *)
let generate_value ~loc cu what v =
  [%expr
    let Ppx_debug_runtime.Config.{ file = ppx_debug_file; _ } =
      Ppx_debug_runtime.Config.read ()
    in
    Ppx_debug_runtime.Trace.emit_value ~ppx_debug_file
      ~ppx_debug_id:([%e A.estring ~loc cu], "func", [%e A.eint ~loc (fresh ())])
      [%e A.estring ~loc what]
      [%e A.pexp_ident ~loc { loc; txt = Lident v }]]

let generate_arg ~loc cu arg =
  [%expr
    let Ppx_debug_runtime.Config.{ file = ppx_debug_file; _ } =
      Ppx_debug_runtime.Config.read ()
    in
    Ppx_debug_runtime.Trace.emit_argument ~ppx_debug_file
      ~ppx_debug_id:([%e A.estring ~loc cu], "func", [%e A.eint ~loc (fresh ())])
      [%e A.estring ~loc arg]
      [%e A.pexp_ident ~loc { loc; txt = Lident arg }]]

let generate_start ~loc what =
  [%expr
    let Ppx_debug_runtime.Config.{ file = ppx_debug_file; _ } =
      Ppx_debug_runtime.Config.read ()
    in
    Ppx_debug_runtime.Trace.emit_start ~ppx_debug_file
      ~func:[%e A.estring ~loc what]]

let generate_end ~loc what =
  [%expr
    let Ppx_debug_runtime.Config.{ file = ppx_debug_file; _ } =
      Ppx_debug_runtime.Config.read ()
    in
    Ppx_debug_runtime.Trace.emit_end ~ppx_debug_file
      ~func:[%e A.estring ~loc what]]

let run_invoc ~loc cu fn_expr fn_name params =
  (* TODO should fn_name be given to func in generate_printer? *)
  let start = generate_start ~loc fn_name in
  let stop = generate_end ~loc fn_name in
  let print_params =
    params
    |> List.filter_map (function
         | Param { param = { txt = s; _ }; _ } -> Some (generate_arg ~loc cu s)
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
  let print_res = generate_arg ~loc cu "res" in
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

let check_should_transform config modname fn =
  (* built-in blacklist. don't instrument printers and some generated code that can be identified syntactically *)
  if List.mem ~eq:String.equal fn ["pp"; "show"] then
    not_transforming "%s is a printer or generated" fn;
  if
    List.exists
      (fun prefix -> String.starts_with ~prefix fn)
      [
        "pp_";
        "show_";
        (* ppx_deriving generates functions like __0 for printer arguments *)
        "__";
      ]
  then not_transforming "%s is a printer or generated" fn;
  match config.C.mode with
  | All bs when List.mem ~eq:String.equal fn bs ->
    not_transforming "%s is in function blacklist" fn
  | Some bs when not (List.mem ~eq:String.equal fn bs) ->
    not_transforming "%s is not in function whitelist" fn
  | Modules m when not (List.mem ~eq:String.equal modname m) ->
    not_transforming "%s is not in module whitelist" modname
  | _ -> ()

let transform_binding_nonrecursively config filename modname b =
  let original_rhs, fn_name, params = extract_binding_info b in
  check_should_transform config modname fn_name;
  let original_fn_body, loc =
    let body, loc = get_constrained_fn original_rhs in
    let tr = replace_calls fn_name (mangle fn_name) in
    (* this is needed in the nonrecursive case because this may be used for recursive fns *)
    let new_body = tr#expression body in
    (new_body, loc)
  in

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
         (run_invoc ~loc filename (ident (mangle fn_name)) fn_name params))
  in
  [{ b with pvb_expr = new_rhs1 }]

let transform_binding_recursively config filename modname b =
  let original_rhs, fn_name, params = extract_binding_info b in
  check_should_transform config modname fn_name;
  let original_fn_body, loc =
    let body, loc = get_constrained_fn original_rhs in
    let tr = replace_calls fn_name "self" in
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

  (* the entire new rhs *)
  let new_rhs1 =
    let open Ast_helper in
    let run =
      run_invoc ~loc filename
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

let all_function_bindings bs =
  List.for_all
    (fun b ->
      match b.pvb_expr.pexp_desc with
      | Pexp_fun _ | Pexp_function _ -> true
      | _ -> false)
    bs

let transform_bindings filename modname config rec_flag bindings =
  if not (all_function_bindings bindings) then
    not_transforming "not all right sides are functions. left sides: %s"
      (List.map
         (fun b -> Format.asprintf "%a" Pprintast.pattern b.pvb_pat)
         bindings
      |> String.concat ",");
  match rec_flag with
  | Recursive ->
    List.concat_map
      (fun b -> transform_binding_recursively config filename modname b)
      bindings
  | Nonrecursive ->
    List.concat_map
      (fun b -> transform_binding_nonrecursively config filename modname b)
      bindings

let traverse filename modname config =
  object
    inherit Ast_traverse.map (* _with_expansion_context *) as super

    method! expression e =
      match e with
      | { pexp_desc = Pexp_let (rec_flag, bindings, body); pexp_loc = loc; _ }
        ->
        begin
          try
            {
              e with
              pexp_desc =
                Pexp_let
                  ( Nonrecursive,
                    transform_bindings filename modname config rec_flag bindings,
                    body );
            }
          with
          | NotTransforming s ->
            log "not transforming: %s" s;
            e
          | Failure s ->
            A.pexp_extension ~loc (Location.error_extensionf ~loc "%s" s)
        end
      | _ -> e

    method! structure_item si =
      let si = super#structure_item si in
      match si with
      | { pstr_desc = Pstr_value (rec_flag, bindings); pstr_loc = loc; _ } ->
        (* handle mutual recursion *)
        let flag = match bindings with [_] -> Nonrecursive | _ -> rec_flag in
        begin
          try
            Ast_helper.Str.value flag
              (transform_bindings filename modname config rec_flag bindings)
          with
          | NotTransforming s ->
            log "not transforming: %s" s;
            si
          | Failure s ->
            A.pstr_extension ~loc (Location.error_extensionf ~loc "%s" s) []
            (* A.pstr_value ~loc Nonrecursive
               [
                 A.value_binding ~loc ~pat:(A.ppat_any ~loc)
                   ~expr:;
               ] *)
        end
      | _ -> si
  end

let () =
  let config = C.read () in
  if not config.C.enabled then log "not transforming: disabled via config"
  else
    Driver.register_transformation
      ~instrument:
        (Driver.Instrument.V2.make
           (fun ctxt s ->
             let cp = Expansion_context.Base.code_path ctxt in
             let filename = Code_path.file_path cp in
             let modname = Code_path.main_module_name cp in
             (* let file =
                  match s with
                  | [] -> failwith "nothing to translate"
                  | { pstr_loc; _ } :: _ -> pstr_loc.loc_start.pos_fname
                in *)
             (* file  *)
             (traverse filename modname config)#structure s)
           ~position:After)
      "ppx_debug"

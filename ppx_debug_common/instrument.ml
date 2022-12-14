open Containers
open Ppxlib
module Config = Ppx_debug_runtime.Config

module L = Log.Make (struct
  let name = (Config.read ()).internal_log
end)

let log = L.log
let p_si si = Format.printf "structure_item %a@." Pprintast.structure_item si
let p_s s = Format.printf "structure %a@." Pprintast.structure s
let p_e e = Format.printf "expression %a@." Pprintast.expression e
let p_p p = Format.printf "pattern %a@." Pprintast.pattern p
let p_t t = Format.printf "type %a@." Pprintast.core_type t

module A = Ast_builder.Default

(* thrown when we can't transform this function for legitimate reasons *)
exception NotTransforming of string

let not_transforming fmt =
  Format.ksprintf ?margin:None ~f:(fun s -> raise (NotTransforming s)) fmt

let fresh =
  let n = ref 0 in
  fun () ->
    let r = !n in
    incr n;
    r

let fresh_v () = Format.sprintf "v%d" (fresh ())
let self_name = "_self"
let lambda_name = "_lambda"
let aux_fn_name = "aux__"

type label = arg_label * expression option

type param = {
  name : string;
  ignored : bool;
  label : label;
  pattern : pattern;
  call : arg_label * expression;
}

type func = {
  name : string;
  params : param list;
  body : expression;
  loc : location;
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
  | _ -> not_transforming "%a is not a function pattern" Pprintast.pattern pat

let normalize_fn f : func =
  let name = lambda_name in
  let loc = match f with { pexp_loc = loc; _ } -> loc in
  let rec aux f =
    match f with
    | { pexp_desc = Pexp_fun (lbl, lbl_e, arg_pat, rest); _ } ->
      let { ppat_desc = desc; ppat_loc = loc; _ } = arg_pat in
      let label = (lbl, lbl_e) in
      let func = aux rest in
      begin
        match desc with
        | Ppat_var { txt = name; _ }
        | Ppat_constraint ({ ppat_desc = Ppat_var { txt = name; _ }; _ }, _) ->
          (* the usual case *)
          let call =
            let lab =
              match (lbl, lbl_e) with
              | Optional s, Some _ -> Labelled s
              | _ -> lbl
            in
            (lab, A.pexp_ident ~loc { loc; txt = Lident name })
          in
          let param =
            { name; label; ignored = false; pattern = arg_pat; call }
          in
          { func with params = param :: func.params }
        | Ppat_construct ({ txt = Lident "()"; _ }, None) ->
          (* ignore unit *)
          let call =
            (Nolabel, A.pexp_construct ~loc { loc; txt = Lident "()" } None)
          in
          let param =
            { name = "()"; label; ignored = true; pattern = arg_pat; call }
          in
          { func with params = param :: func.params }
        | Ppat_any ->
          let name = "_" ^ fresh_v () in
          let call =
            let lab =
              match (lbl, lbl_e) with
              | Optional s, Some _ -> Labelled s
              | _ -> lbl
            in
            (lab, A.pexp_ident ~loc { loc; txt = Lident name })
          in
          let param =
            (* generate a name in the pattern *)
            {
              name;
              label;
              ignored = false;
              pattern = A.ppat_var ~loc { txt = name; loc };
              call;
            }
          in
          { func with params = param :: func.params }
        | _ -> { body = f; params = []; name; loc }
      end
    | { pexp_desc = Pexp_constraint (e, _); _ } ->
      (* this is lossy. losing the constraint may change types *)
      aux e
    | _ ->
      (* this is incomplete for cases with e.g. wildcards inside constructors that then have to be reconstructed *)
      log "did not fully normalize non-function %a" Pprintast.expression f;
      { body = f; params = []; name; loc }
  in
  aux f

(** somewhat-inverse of traverse_fn *)
let rec build_fn ({ loc; params; body; _ } as func) =
  match params with
  | [] -> body
  | { label = lbl, lbl_e; pattern; _ } :: ps ->
    A.pexp_fun ~loc lbl lbl_e pattern (build_fn { func with params = ps })

let transform_fn_body f e =
  let func = normalize_fn e in
  build_fn { func with body = f func.body }

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
  let func = normalize_fn original_rhs in
  { func with name = fn_name }

let replace_calls find replace =
  object
    inherit Ast_traverse.map as super

    method! expression expr =
      let expr = super#expression expr in
      (* only replace unqualified names, as those are more likely to be calls *)
      (* TODO this is not capture-avoiding *)
      match expr with
      | { pexp_desc = Pexp_ident { txt = Lident fn_name; loc }; _ }
        when String.equal fn_name find ->
        A.pexp_ident ~loc { txt = Lident replace; loc }
      | _ -> expr
  end

let mangle fn_name = fn_name ^ "_original"
let ident ~loc s = A.pexp_ident ~loc { txt = Lident s; loc }

let qualified_ident ~loc ss =
  match ss with
  | [] -> failwith "qualified_ident requires a non-empty list"
  | [s] -> ident ~loc s
  | s :: ss ->
    let res = List.fold_left (fun t c -> Ldot (t, c)) (Lident s) ss in
    A.pexp_ident ~loc { txt = res; loc }

let rec fun_wildcards ~loc n body =
  match n with
  | 0 -> body
  | _ ->
    A.pexp_fun ~loc Nolabel None (A.ppat_any ~loc)
      (fun_wildcards ~loc (n - 1) body)

let app ~loc f args =
  A.pexp_apply ~loc f (List.map (fun a -> (Nolabel, a)) args)

let str ~loc s = A.pexp_constant ~loc (Pconst_string (s, loc, None))

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
  | Ptyp_arrow (_, a, b) -> arrow_to_list a @ arrow_to_list b
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

(* for now, we get ppx_debug_file by reading the environment, but removing that allows users to configure it through changing source *)
let generate_value ~loc cu v =
  [%expr
    let ppx_debug_file = Ppx_debug_runtime.Config.(get_file (read ())) in
    Ppx_debug_runtime.Trace.emit_value ~ppx_debug_file
      ~ppx_debug_id:
        {
          file = [%e A.estring ~loc cu];
          id = [%e A.eint ~loc (fresh ())];
          loc =
            ( ( [%e A.eint ~loc loc.loc_start.pos_lnum],
                [%e
                  A.eint ~loc (loc.loc_start.pos_cnum - loc.loc_start.pos_bol)]
              ),
              ( [%e A.eint ~loc loc.loc_end.pos_lnum],
                [%e A.eint ~loc (loc.loc_end.pos_cnum - loc.loc_end.pos_bol)] )
            );
        }
      [%e A.estring ~loc v]
      [%e A.pexp_ident ~loc { loc; txt = Lident v }]]

let generate_event ~loc cu typ name v =
  [%expr
    let ppx_debug_file = Ppx_debug_runtime.Config.(get_file (read ())) in
    Ppx_debug_runtime.Trace.emit_raw ~ppx_debug_file
      ~ppx_debug_id:
        {
          file = [%e A.estring ~loc cu];
          id = [%e A.eint ~loc (fresh ())];
          loc =
            ( ( [%e A.eint ~loc loc.loc_start.pos_lnum],
                [%e
                  A.eint ~loc (loc.loc_start.pos_cnum - loc.loc_start.pos_bol)]
              ),
              ( [%e A.eint ~loc loc.loc_end.pos_lnum],
                [%e A.eint ~loc (loc.loc_end.pos_cnum - loc.loc_end.pos_bol)] )
            );
        }
      [%e A.estring ~loc typ] [%e A.estring ~loc name] [%e v]]

let generate_arg ~loc cu arg =
  [%expr
    let ppx_debug_file = Ppx_debug_runtime.Config.(get_file (read ())) in
    Ppx_debug_runtime.Trace.emit_argument ~ppx_debug_file
      ~ppx_debug_id:
        {
          file = [%e A.estring ~loc cu];
          id = [%e A.eint ~loc (fresh ())];
          loc =
            ( ( [%e A.eint ~loc loc.loc_start.pos_lnum],
                [%e
                  A.eint ~loc (loc.loc_start.pos_cnum - loc.loc_start.pos_bol)]
              ),
              ( [%e A.eint ~loc loc.loc_end.pos_lnum],
                [%e A.eint ~loc (loc.loc_end.pos_cnum - loc.loc_end.pos_bol)] )
            );
        }
      [%e A.estring ~loc arg]
      [%e A.pexp_ident ~loc { loc; txt = Lident arg }]]

let generate_start ~loc cu what =
  [%expr
    let ppx_debug_file = Ppx_debug_runtime.Config.(get_file (read ())) in
    Ppx_debug_runtime.Trace.emit_start ~ppx_debug_file
      ~ppx_debug_id:
        {
          file = [%e A.estring ~loc cu];
          id = [%e A.eint ~loc (fresh ())];
          loc =
            ( ( [%e A.eint ~loc loc.loc_start.pos_lnum],
                [%e
                  A.eint ~loc (loc.loc_start.pos_cnum - loc.loc_start.pos_bol)]
              ),
              ( [%e A.eint ~loc loc.loc_end.pos_lnum],
                [%e A.eint ~loc (loc.loc_end.pos_cnum - loc.loc_end.pos_bol)] )
            );
        }
      ~func:[%e A.estring ~loc what]]

let generate_end ~loc cu what =
  [%expr
    let ppx_debug_file = Ppx_debug_runtime.Config.(get_file (read ())) in
    Ppx_debug_runtime.Trace.emit_end ~ppx_debug_file
      ~ppx_debug_id:
        {
          file = [%e A.estring ~loc cu];
          id = [%e A.eint ~loc (fresh ())];
          loc =
            ( ( [%e A.eint ~loc loc.loc_start.pos_lnum],
                [%e
                  A.eint ~loc (loc.loc_start.pos_cnum - loc.loc_start.pos_bol)]
              ),
              ( [%e A.eint ~loc loc.loc_end.pos_lnum],
                [%e A.eint ~loc (loc.loc_end.pos_cnum - loc.loc_end.pos_bol)] )
            );
        }
      ~func:[%e A.estring ~loc what]]

let run_invoc modname (config : Config.t) filename fn_expr func =
  let loc = func.loc in
  let start = generate_start ~loc filename func.name in
  let stop = generate_end ~loc filename func.name in
  let print_params =
    func.params
    |> List.filter_map (fun { name; ignored; _ } ->
           if ignored then None else Some (generate_arg ~loc filename name))
  in
  let print_params =
    if
      (not config.should_instrument_definitions)
      || List.exists
           (fun (m, f) ->
             Str.string_match (Str.regexp m) modname 0
             && Str.string_match (Str.regexp f) func.name 0)
           config.should_not_instrument_definitions
    then [%expr ()]
    else List.fold_right (A.pexp_sequence ~loc) print_params [%expr ()]
  in
  let call_fn =
    A.pexp_apply ~loc fn_expr (func.params |> List.map (fun { call; _ } -> call))
  in
  let print_res =
    if
      (not config.should_instrument_definitions)
      || List.exists
           (fun (m, f) ->
             Str.string_match (Str.regexp m) modname 0
             && Str.string_match (Str.regexp f) func.name 0)
           config.should_not_instrument_definitions
    then [%expr ()]
    else generate_arg ~loc filename "_res"
  in
  [%expr
    [%e start];
    [%e print_params];
    let _res = [%e call_fn] in
    [%e print_res];
    [%e stop];
    _res]

let check_should_transform_module config modname =
  let mwl = Str.regexp config.Config.instrument_modules in
  if not (Str.string_match mwl modname 0) then
    not_transforming "%s is not in module whitelist" modname

let check_should_transform_fn config fn =
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

  let fwl = Str.regexp config.Config.instrument_functions in
  let fbl = Str.regexp config.Config.do_not_instrument_functions in

  if not (Str.string_match fwl fn 0) then
    not_transforming "%s is not in function whitelist" fn;
  if Str.string_match fbl fn 0 then
    not_transforming "%s is not in function blacklist" fn

let nonrecursive_rhs modname config filename func =
  let loc = func.loc in
  let func =
    {
      func with
      body =
        A.pexp_let ~loc Nonrecursive
          [
            Ast_builder.Default.value_binding ~loc
              ~pat:(A.ppat_var ~loc { txt = mangle func.name; loc })
              ~expr:(build_fn func);
          ]
          (run_invoc modname config filename
             (ident ~loc (mangle func.name))
             func);
    }
  in
  build_fn func

let transform_bound_func_nonrecursively config modname filename func =
  check_should_transform_fn config func.name;
  let func =
    {
      func with
      body = (replace_calls func.name (mangle func.name))#expression func.body;
    }
  in
  let new_rhs1 = nonrecursive_rhs modname config filename func in
  new_rhs1

let transform_binding_nonrecursively config modname filename b =
  let func = extract_binding_info b in
  let new_rhs1 =
    transform_bound_func_nonrecursively config modname filename func
  in
  [{ b with pvb_expr = new_rhs1 }]

let transform_binding_recursively modname config filename b =
  let loc = b.pvb_loc in
  let func = extract_binding_info b in
  check_should_transform_fn config func.name;
  let original_fn_body, loc =
    let body = (replace_calls func.name self_name)#expression func.body in
    let self =
      {
        name = self_name;
        label = (Nolabel, None);
        ignored = false;
        pattern = A.ppat_var ~loc { loc; txt = self_name };
        call = (Nolabel, A.pexp_ident ~loc { loc; txt = Lident self_name });
      }
    in
    (build_fn { func with body; params = self :: func.params }, func.loc)
  in

  (* the entire new rhs *)
  let new_rhs1 =
    let aux = ident ~loc aux_fn_name in
    let run =
      run_invoc modname config filename
        [%expr [%e ident ~loc (mangle func.name)] [%e aux]]
        func
    in
    let ps1 = func.params |> List.map (fun { call; _ } -> call) in
    build_fn
      {
        func with
        body =
          A.pexp_let ~loc Nonrecursive
            [
              Ast_builder.Default.value_binding ~loc
                ~pat:(A.ppat_var ~loc { txt = mangle func.name; loc })
                ~expr:original_fn_body;
            ]
            (A.pexp_let ~loc Recursive
               [
                 Ast_builder.Default.value_binding ~loc
                   ~pat:(A.ppat_var ~loc { txt = aux_fn_name; loc })
                   ~expr:(build_fn { func with body = run });
               ]
               (A.pexp_apply ~loc aux ps1));
      }
  in

  [{ b with pvb_expr = new_rhs1 }]

let all_function_bindings bs =
  let rec is_func e =
    match e.pexp_desc with
    | Pexp_fun _ | Pexp_function _ -> true
    | Pexp_constraint (e, _) ->
      (* (let f : t = fun x -> x) is actually (let f = ((fun x -> x) : t)) *)
      is_func e
    | _ -> false
  in
  List.for_all (fun b -> is_func b.pvb_expr) bs

let transform_bindings modname filename config rec_flag bindings =
  if not (all_function_bindings bindings) then
    not_transforming "not all right sides are functions. left sides: %s"
      (List.map
         (fun b -> Format.asprintf "%a" Pprintast.pattern b.pvb_pat)
         bindings
      |> String.concat ",");
  match rec_flag with
  | Recursive ->
    List.concat_map
      (fun b -> transform_binding_recursively modname config filename b)
      bindings
  | Nonrecursive ->
    List.concat_map
      (fun b -> transform_binding_nonrecursively config modname filename b)
      bindings

let truncate s = if String.length s > 10 then String.sub s 0 10 ^ "..." else s

(** This looks for bindings in expression and structure contexts:

      let f x = 1

      let f x = 1 in
      b

    When we find such bindings, we first recurse in the body (1) to handle
    nested let expressions and lambdas. Then we try to transform the current
    binding; this may fail if e.g. it doesn't bind a function.

*)
let traverse modname filename config =
  object (self)
    inherit Ast_traverse.map (* _with_expansion_context *) as super

    method handle_method cf =
      match cf with
      | { pcf_desc = Pcf_method ({ txt = name; _ }, _, Cfk_virtual _); _ } ->
        not_transforming "virtual method %s" name
      | {
       pcf_desc =
         Pcf_method
           (({ txt = name; _ } as ident), priv, Cfk_concrete (over, mrhs));
       _;
      } ->
        (* check_should_transform_fn config name; *)
        begin
          match mrhs with
          | { pexp_desc = Pexp_poly (fn, otyp); _ } ->
            (* figure out the function name from the method *)
            let func = { (normalize_fn fn) with name } in
            (* recursively transform only the body -- transforming the function itself would do the work twice *)
            let func = { func with body = self#expression func.body } in
            let e1 =
              transform_bound_func_nonrecursively config modname filename func
            in
            {
              cf with
              pcf_desc =
                Pcf_method
                  ( ident,
                    priv,
                    Cfk_concrete
                      (over, { mrhs with pexp_desc = Pexp_poly (e1, otyp) }) );
            }
          | { pexp_desc = Pexp_fun _; _ } ->
            let func = { (normalize_fn mrhs) with name } in
            let func = { func with body = self#expression func.body } in
            let e1 =
              transform_bound_func_nonrecursively config modname filename func
            in
            {
              cf with
              pcf_desc = Pcf_method (ident, priv, Cfk_concrete (over, e1));
            }
          | e ->
            log "unhandled %s: %a %a" name Pp.debug_pexpr e Pprintast.expression
              e;
            (* this is not exposed by ppxlib apparently *)
            (* log "unhandled Pcf_method: %a" (Ocaml_common.Printast.expression 0) e; *)
            cf
        end
      (* | { pcf_desc = Pcf_method (_, _, Cfk_concrete (_, e)); _ } -> *)
      | { pcf_desc = desc; _ } ->
        let name =
          match desc with
          | Pcf_inherit (_, _, _) -> "Pcf_inherit"
          | Pcf_val _ -> "Pcf_val"
          | Pcf_method _ ->
            failwith "Pcf_method should already have been handled"
          | Pcf_constraint _ -> "Pcf_constraint"
          | Pcf_initializer _ -> "Pcf_initializer"
          | Pcf_attribute _ -> "Pcf_attribute"
          | Pcf_extension _ -> "Pcf_extension"
        in
        log "unhandled: %s" name;
        cf

    method! expression e =
      match e with
      | {
       pexp_desc =
         Pexp_extension
           ( { txt = "trace"; _ },
             PStr
               [
                 {
                   pstr_desc =
                     Pstr_eval
                       ( { pexp_desc = Pexp_ident { txt = Lident id; _ }; _ },
                         _attrs );
                   _;
                 };
               ] );
       pexp_loc = loc;
       _;
      } ->
        generate_value ~loc filename id
      | { pexp_desc = Pexp_match (scr, cases); _ }
        when config.Config.should_instrument_matches ->
        let loc = scr.pexp_loc in
        let matched =
          truncate (Format.asprintf "%a" Pprintast.expression scr)
        in
        let scr = self#expression scr in
        let cases =
          List.map
            (fun c ->
              {
                c with
                pc_guard = Option.map self#expression c.pc_guard;
                pc_rhs =
                  (let rhs1 = self#expression c.pc_rhs in
                   let loc = rhs1.pexp_loc in
                   [%expr
                     [%e
                       generate_event ~loc filename "matchb" "matchb"
                         (str ~loc matched)];
                     [%e rhs1]]);
              })
            cases
        in
        let scr =
          A.pexp_sequence ~loc
            (generate_event ~loc filename "match"
               (* this can be extremely large, and also contain output *)
               matched scr)
            scr
        in
        { e with pexp_desc = Pexp_match (scr, cases) }
      | { pexp_desc = Pexp_apply (f, args); pexp_loc = loc; _ }
        when config.Config.should_instrument_calls ->
        (* TODO these aren't perfect as they may hit the beginnings/ends of lines *)
        (* they are also unintuitive *)
        (* let bloc =
             {
               loc with
               loc_end =
                 { loc.loc_start with pos_cnum = loc.loc_start.pos_cnum + 1 };
             }
           in
           let aloc =
             {
               loc with
               loc_start = { loc.loc_end with pos_cnum = loc.loc_end.pos_cnum - 1 };
             }
           in *)
        let before =
          generate_event ~loc filename "bcall" "bcall" [%expr "(before)"]
        in
        let after =
          generate_event ~loc filename "acall" "acall" [%expr "(after)"]
        in
        (* recurse *)
        let e =
          {
            e with
            pexp_desc =
              Pexp_apply
                ( self#expression f,
                  List.map (fun (l, a) -> (l, self#expression a)) args );
          }
        in
        [%expr
          [%e before];
          let result__ = [%e e] in
          [%e after];
          result__]
      | { pexp_desc = Pexp_fun _; _ }
        when config.Config.should_instrument_lambdas ->
        let func = normalize_fn e in
        (* TODO name more uniquely *)
        if CCEqual.physical func.body e then
          (* TODO skip transforming if we can't handle this, instead of going into a loop.
             the problem is the lossy param repr we use.
             normalize_fn should be guaranteed to return a smaller expression. *)
          e
        else
          let func = { func with body = self#expression func.body } in
          nonrecursive_rhs modname config filename func
      | { pexp_desc = Pexp_let (rec_flag, bindings, body); pexp_loc = loc; _ }
        ->
        let bindings =
          List.map
            (fun b ->
              try
                let func = extract_binding_info b in
                check_should_transform_fn config func.name;
                {
                  b with
                  pvb_expr = transform_fn_body self#expression b.pvb_expr;
                }
              with NotTransforming _ -> b)
            bindings
        in
        let body = self#expression body in
        (* rebuild this in case we end up not transforming the binding *)
        let e = { e with pexp_desc = Pexp_let (rec_flag, bindings, body) } in
        begin
          try
            {
              e with
              pexp_desc =
                Pexp_let
                  ( rec_flag,
                    transform_bindings modname filename config rec_flag bindings,
                    body );
            }
          with
          | NotTransforming s ->
            log "not transforming: %s" s;
            e
          | Failure s ->
            A.pexp_extension ~loc (Location.error_extensionf ~loc "%s" s)
        end
      | { pexp_desc = Pexp_object cls; _ } ->
        let ms = List.map self#handle_method cls.pcstr_fields in
        { e with pexp_desc = Pexp_object { cls with pcstr_fields = ms } }
      | _ -> super#expression e

    method! structure_item si =
      match si with
      | { pstr_desc = Pstr_value (rec_flag, bindings); pstr_loc = loc; _ } ->
        let bindings =
          List.map
            (fun b ->
              try
                let func = extract_binding_info b in
                check_should_transform_fn config func.name;
                {
                  b with
                  pvb_expr = transform_fn_body self#expression b.pvb_expr;
                }
              with NotTransforming _ -> b)
            bindings
        in
        (* rebuild this in case we end up not transforming the binding *)
        let si = { si with pstr_desc = Pstr_value (rec_flag, bindings) } in
        (* handle mutual recursion *)
        let flag = match bindings with [_] -> Nonrecursive | _ -> rec_flag in
        begin
          try
            let r =
              A.pstr_value ~loc flag
                (transform_bindings modname filename config rec_flag bindings)
            in
            r
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
      | { pstr_desc = Pstr_class cdecls; pstr_loc = loc; _ } ->
        let transform_class_structure cstr =
          match cstr with
          | Pcl_structure c_str ->
            Pcl_structure
              {
                c_str with
                pcstr_fields = List.map self#handle_method c_str.pcstr_fields;
              }
          | _ ->
            let constr =
              match cstr with
              | Pcl_structure _ ->
                failwith "Pcl_structure should already have been handled"
              | Pcl_constr (_, _) -> "Pcl_constr"
              | Pcl_fun (_, _, _, _) -> "Pcl_fun "
              | Pcl_apply (_, _) -> "Pcl_apply "
              | Pcl_let (_, _, _) -> "Pcl_let "
              | Pcl_constraint (_, _) -> "Pcl_constraint "
              | Pcl_extension _ -> "Pcl_extension "
              | Pcl_open (_, _) -> "_Pcl_open "
            in
            log "unhandled: %s" constr;
            cstr
        in
        let handle_si si =
          let cdecls =
            List.map
              (fun cdecl ->
                {
                  cdecl with
                  pci_expr =
                    {
                      cdecl.pci_expr with
                      pcl_desc =
                        transform_class_structure cdecl.pci_expr.pcl_desc;
                    };
                })
              cdecls
          in
          { si with pstr_desc = Pstr_class cdecls }
        in
        begin
          try handle_si si with
          | NotTransforming s ->
            log "not transforming: %s" s;
            si
          | Failure s ->
            A.pstr_extension ~loc (Location.error_extensionf ~loc "%s" s) []
        end
      | _ -> super#structure_item si
  end

let process file modname config str =
  try
    check_should_transform_module config modname;
    (traverse modname file config)#structure str
  with NotTransforming s ->
    log "not transforming structure: %s" s;
    str

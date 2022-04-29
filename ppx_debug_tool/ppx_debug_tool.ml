open Containers
module Config = Ppx_debug_runtime.Config

(* logging the low-tech way *)
let logfile = IO.File.make (Config.read ()).internal_tool_log

let log fmt =
  Format.kasprintf
    (fun s ->
      if (Config.read ()).ppx_logging then IO.File.append_exn logfile (s ^ "\n"))
    fmt

(* we're not in ppxlib, because we're accessing the typedtree, which likely is out of scope.
   we probably aren't protected against breakage as well. *)
open Ocaml_common
module A = Ppxlib.Ast_builder.Default
module C = Ppx_debug_runtime.Config

let p_t expr =
  Format.printf "%a@." Pprintast.expression (Untypeast.untype_expression expr)

module Id = Ppx_debug_runtime.Trace.Id

(* this is generated code, so it comes from ppxlib *)
type typ_info = {
  (* if none, it's a type we can't unmarshal *)
  typ : Ppxlib.core_type option;
  pp_fn : Ppxlib.expression;
}

let id_type_mappings : (Id.t * typ_info) list ref = ref []
let p_te t = Format.printf "type %a@." Printtyp.type_expr t

(* this could return 'a always and it should still be fine as long as the printer generated by show_fn matches the id numbers of emit statements *)
let create_typ_name ~loc _qual exp_type =
  (* let open Option.Infix in
     let+ typ_name in
     Ppxlib.
       {
         ptyp_desc = Ptyp_constr ({ loc; txt = typ_name }, []);
         ptyp_loc = loc;
         ptyp_loc_stack = [];
         ptyp_attributes = [];
       } *)
  match Types.get_desc exp_type with
  | Tconstr (Pident ident, [], _) when String.equal (Ident.name ident) "unit" ->
    (* Some (Ppxlib.Lident "unit") *)
    Some (A.ptyp_constr ~loc { txt = Ppxlib.Lident "unit"; loc } [])
  (* Ldot (qual, typ_name) *)
  | Tconstr (Pident ident, [], _) when String.equal (Ident.name ident) "int" ->
    (* Some (Lident "int") *)
    Some (A.ptyp_constr ~loc { txt = Ppxlib.Lident "int"; loc } [])
  | Tconstr (Pident _ident, _params, _) ->
    (* TODO params? *)
    (* Some (Ldot (qual, Ident.name ident)) *)
    (* failwith "nyi" *)
    None
    (* Some (A.ptyp_var ~loc "a") *)
    (* Some
       (A.ptyp_constr ~loc
          { txt = Ldot (qual, Ident.name ident); loc }
          List.map create_typ_name params) *)
  | Tvar _ ->
    (* can't unmarshal polymorphic types *)
    None
  (* "Lib." ^ Ident.name ident *)
  (* "x" *)
  (* | T (Pident ident, _, _) -> *)
  | _ -> None
(* Some (A.ptyp_var ~loc "nyi") *)

(* why pp? because show is non-compositional *)
let rec create_pp_fn ~loc qual exp_type =
  let variant = (C.read ()).variant in
  match Types.get_desc exp_type with
  | Tconstr (Pident ident, _, _) when String.equal (Ident.name ident) "int" ->
    Ppxlib.(
      (match variant with
      | Containers -> [%expr CCInt.pp]
      | Stdlib -> [%expr Format.pp_print_int]))
  | Tconstr (Pident ident, _, _) when String.equal (Ident.name ident) "float" ->
    (match variant with
    | Containers -> [%expr CCFloat.pp]
    | Stdlib -> [%expr Format.pp_print_float])
  | Tconstr (Pident ident, _, _) when String.equal (Ident.name ident) "bool" ->
    (match variant with
    | Containers -> [%expr CCString.pp]
    | Stdlib -> [%expr Format.pp_print_string])
  | Tconstr (Pident ident, _, _) when String.equal (Ident.name ident) "string"
    ->
    (match variant with
    | Containers -> [%expr CCString.pp]
    | Stdlib -> [%expr Format.pp_print_string])
  | Tconstr (Pident ident, [a], _) when String.equal (Ident.name ident) "option"
    ->
    (match variant with
    | Containers -> [%expr CCOpt.pp [%e create_pp_fn ~loc qual a]]
    | Stdlib -> [%expr Format.pp_print_option [%e create_pp_fn ~loc qual a]])
  | Tconstr (Pident ident, [a; b], _)
    when String.equal (Ident.name ident) "result" ->
    (* can't use this as it specializes the second arg to string *)
    (* (match containers with
       | true ->
         [%expr Result.pp [%e generate_printer_typ a] [%e generate_printer_typ b]]
       | _ -> *)
    [%expr
      Format.pp_print_result ~ok:[%e create_pp_fn ~loc qual a]
        ~error:[%e create_pp_fn ~loc qual b]]
    (* ) *)
  | Tconstr (Pident ident, [a], _) when String.equal (Ident.name ident) "list"
    ->
    (match variant with
    | Containers ->
      [%expr
        CCList.pp
          ~pp_start:(fun fmt () -> Format.fprintf fmt "[")
          ~pp_stop:(fun fmt () -> Format.fprintf fmt "]")
          ~pp_sep:(fun fmt () -> Format.fprintf fmt ";")
          [%e create_pp_fn ~loc qual a]]
    | Stdlib -> [%expr Format.pp_print_list [%e create_pp_fn ~loc qual a]])
  | Tconstr (Pident ident, [], _) when String.equal (Ident.name ident) "unit" ->
    [%expr fun fmt () -> Format.fprintf fmt "()"]
    (* the following two cases are the same except for the qualifiers *)
  | Tconstr (Pdot (_q, ident), args, _) ->
    let f =
      A.pexp_ident ~loc
        {
          loc;
          txt =
            Ldot
              ( qual,
                (* or q?*)
                match ident with "t" -> "pp" | _ -> "pp_" ^ ident );
        }
    in
    (match args with
    | [] -> f
    | _ :: _ ->
      let p_args =
        List.map (create_pp_fn ~loc qual) args
        |> List.map (fun a -> (Ppxlib.Nolabel, a))
      in
      A.pexp_apply ~loc f p_args)
  | Tconstr (Pident ident, args, _) ->
    let f =
      A.pexp_ident ~loc
        {
          loc;
          txt =
            Ldot
              ( qual,
                match Ident.name ident with
                | "t" -> "pp"
                | _ -> "pp_" ^ Ident.name ident );
        }
    in
    (match args with
    | [] -> f
    | _ :: _ ->
      let p_args =
        List.map (create_pp_fn ~loc qual) args
        |> List.map (fun a -> (Ppxlib.Nolabel, a))
      in
      A.pexp_apply ~loc f p_args)
  | Tvar _ -> [%expr fun fmt _ -> Format.fprintf fmt "<poly>"]
  | Tarrow _ -> [%expr fun fmt _ -> Format.fprintf fmt "<fn>"]
  (* "Lib." ^ Ident.name ident *)
  (* "x" *)
  (* | T (Pident ident, _, _) -> *)
  | _ ->
    p_te exp_type;
    failwith "nyi"

let handle_expr modname it expr =
  let loc = expr.Typedtree.exp_loc in
  match expr.Typedtree.exp_desc with
  | Texp_apply
      ( {
          exp_desc =
            Texp_ident
              ( _,
                {
                  txt = Ldot (Ldot (Lident "Ppx_debug_runtime", "Trace"), name);
                  _;
                },
                _ );
          _;
        },
        args )
    when String.equal name "emit_value" || String.equal name "emit_argument" ->
    let site_id =
      args
      |> List.filter_map (function
           | Asttypes.Labelled "ppx_debug_id", Some tuple ->
             begin
               match tuple.Typedtree.exp_desc with
               | Texp_tuple
                   [
                     (* we can't use __FILE__ and __FUNCTION__ as these have not been expanded at ppx-processing time *)
                     { exp_desc = Texp_constant (Const_string (file, _, _)); _ };
                     (* { exp_desc = Texp_constant (Const_string (func, _, _)); _; }; *)
                     _;
                     { exp_desc = Texp_constant (Const_int id); _ };
                   ] ->
                 Some (file, "func", id)
               | _ ->
                 (* this shouldn't happen as we're only expecting a triple here (generated by ppx_debug), but it's possible the pattern in the last branch is wrong/too strict *)
                 p_t tuple;
                 failwith "expecting a triple as argument to ppx_debug_id"
             end
           | _ -> None)
      |> List.head_opt
      |> Option.get_exn_or "no ppx_debug_id argument"
    in
    let exp_type =
      let e =
        args |> List.last_opt
        |> Option.get_exn_or "no arguments?"
        |> snd
        |> Option.get_exn_or "last argument had no value"
      in
      e.exp_type
    in
    (* undo the mangling dune does to get a path we can refer to values with *)
    let demangle mn = String.split ~by:"__" mn in
    let modname = List.concat_map demangle modname in
    let qual =
      match modname with
      | [] -> failwith "modname cannot be empty"
      | m :: ms -> List.fold_left Ppxlib.(fun t c -> Ldot (t, c)) (Lident m) ms
    in
    let pp_fn = create_pp_fn ~loc qual exp_type in
    let typ = create_typ_name ~loc qual exp_type in
    (* begin
       match typ with
       | Some typ -> *)
    id_type_mappings := (site_id, { pp_fn; typ }) :: !id_type_mappings
    (* | None -> ()
       end *)
    (* args
       |> List.iter (fun (label, a) ->
              match a with
              | Some a ->
                Format.printf "  arg %a %a@." Pprintast.expression
                  (Untypeast.untype_expression a)
                  Location.print_loc a.exp_loc
              | None -> Format.printf "  arg absent?@.");
       () *)
  | _ ->
    (* it.Tast_iterator.expr it expr *)
    Tast_iterator.default_iterator.expr it expr
(* Format.printf "typed expr %a@." Pprintast.expression
   (Untypeast.untype_expression expr) *)

let walk_build_dir () =
  (* let _ = *)
  (* print_endline ("walking " ^ Sys.getcwd ()); *)
  (* we're inside _build/default *)
  let should_ignore s =
    (* TODO heuristic. config? *)
    not (String.mem ~sub:"test/" s)
  in
  IO.File.walk_seq "."
  |> Seq.iter (function
       | `File, s when String.ends_with ~suffix:"cmt" s && should_ignore s ->
         (* print_endline s; *)
         let cmt = Cmt_format.read_cmt s in
         (* TODO does this do anything? *)
         let modname = cmt.cmt_modname |> String.split ~by:"." in
         (* let () =
              match cmt.cmt_sourcefile with
              | None -> print_endline "no source file"
              | Some s -> Format.printf "file %s@." s
            in *)
         let str =
           match cmt.cmt_annots with
           | Implementation str -> str
           | _ -> failwith "not a cmt file"
         in
         (* Format.printf "typed ast %a@." Printtyped.implementation str; *)
         (* print_endline "traversing cmt"; *)
         let iter_structure =
           Tast_iterator.(
             default_iterator.structure
               {
                 default_iterator with
                 (* value_binding = handle_vb; *)
                 expr = handle_expr modname;
               })
         in
         iter_structure str
       | _ -> ())
(* in *)
(* print_endline "ok" *)

open Ppxlib

let handle_si ~loc ~path:_ _payload =
  (* TODO use payload for name of identifier *)
  (* TODO go up until git, since this will reside in dune build dir *)
  (* let config = Util.read_config () in
     if config.debug then
       (traverse ())#structure_item str
     else
       str *)
  (* str *)
  (* [ *)
  walk_build_dir ();
  let cases =
    A.pexp_match ~loc [%expr id]
      ((!id_type_mappings
       |> List.map (fun ((file, _func, id), typ_info) ->
              (* let typ_ident = A.pexp_ident ~loc { loc; txt = Lident typ } in *)
              (* let typ_ident = A.pexp_ident ~loc { loc; txt = Lident typ } in *)
              let show_arg =
                match typ_info.typ with
                | None ->
                  (* [%expr ()] *)
                  (* i.e. always work *)
                  [%expr Marshal.from_string _content 0]
                | Some typ ->
                  [%expr (Marshal.from_string _content 0 : [%t typ])]
              in
              A.case
                ~lhs:
                  (A.ppat_tuple ~loc
                     [
                       A.pstring ~loc file;
                       A.ppat_any ~loc;
                       (* A.ppat_any ~loc; *)
                       (* A.pstring ~loc func; *)
                       A.pint ~loc id;
                     ])
                ~guard:None
                ~rhs:
                  [%expr
                    let _content = Ppx_debug_runtime.read_n _len _file in
                    Scanf.bscanf _file "\n" ();
                    Format.asprintf "%a" [%e typ_info.pp_fn] [%e show_arg]]))
      @ [
          A.case
            ~lhs:(A.ppat_tuple ~loc [[%pat? file]; A.ppat_any ~loc; [%pat? id]])
            ~guard:None
            ~rhs:[%expr failwith (Format.sprintf "unknown type %s %d" file id)];
        ])
  in
  (* unsure why we have to prefix file with an underscore, or the compiler thinks it's unused *)
  let read = [%expr fun _len _file id -> [%e cases]] in
  (* !id_type_mappings |> List.map (); *)
  [%stri let read_and_print_value = [%e read]]
(* failwith "" *)

(* Scanf.bscanf file "%s@\n%s@\n" (fun id content ->
    Format.printf "%s %s@." id
      (show_b (Marshal.from_string content 0 : b))) *)

let rule =
  Ppxlib.Context_free.Rule.extension
    (Extension.declare "generate" Structure_item
       (* Ast_pattern.(pstr (__ ^:: nil)) *)
       (* Ast_pattern.(pstr __) *)
       Ast_pattern.(__)
       handle_si)

let () =
  Driver.register_transformation ~rules:[rule (* ; rule_expr *)]
    "ppx_debug_tool"

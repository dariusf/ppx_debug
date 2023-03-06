open Containers
module Config = Ppx_debug_runtime.Config

module L = Log.Make (struct
  let name = (Config.read ()).internal_tool_log
end)

let log = L.log

(* we're not in ppxlib, because we're accessing the typedtree, which likely is out of scope.
   we probably aren't protected against breakage as well. *)
open Ocaml_common
module A = Ppxlib.Ast_builder.Default
module C = Ppx_debug_runtime.Config

let p_t expr =
  Format.printf "%a@." Pprintast.expression (Untypeast.untype_expression expr)

module Id = Ppx_debug_runtime.Trace.Id

type typ_info = {
  (* if none, it's a type we can't unmarshal *)
  typ : Ppxlib.core_type option;
  pp_fn : Ppxlib.expression;
}

let id_type_mappings : (Id.t * typ_info) list ref = ref []
let p_te t = Format.printf "type %a@." Printtyp.type_expr t

open Result.Infix

let with_ctx m res =
  match res with Error s -> Error (Format.asprintf "%s: %s" m s) | Ok _ -> res

module LR = List.Traverse (struct
  type 'a t = ('a, string) result

  let return = Result.return
  let ( >>= ) = Result.( >>= )
end)

let relativize ~against path =
  let regexp = Str.regexp (Format.asprintf {|^\(%s\)/?\(.*\)|} against) in
  Str.string_match regexp path 0 |> ignore;
  try Some (Str.matched_group 2 path) with Invalid_argument _ -> None

let file_to_module =
  let dot_ml = Str.regexp {|\.ml|} in
  let slash = Str.regexp {|/|} in
  fun path ->
    let res =
      List.map String.capitalize_ascii
        (path |> Str.global_replace dot_ml "" |> Str.split slash)
    in
    match List.rev res with
    | a :: b :: c when String.equal a b -> List.rev (b :: c)
    | _ -> res

let%expect_test _ =
  let show a = a |> [%derive.show: string option] |> print_endline in
  relativize ~against:"demo/lib" "demo/lib/lib.ml" |> show;
  relativize ~against:"a" "demo/lib/lib.ml" |> show;
  relativize ~against:"demo/lib/" "demo/lib/lib.ml" |> show;
  let show a = a |> [%derive.show: string list] |> print_endline in
  file_to_module "demo/lib/other.ml" |> show;
  file_to_module "demo/lib/lib.ml" |> show;
  file_to_module "lib.ml" |> show;
  [%expect
    {|
  (Some "lib.ml")
  None
  (Some "lib.ml")
  ["Demo"; "Lib"; "Other"]
  ["Demo"; "Lib"]
  ["Lib"]
  |}]

let rec path_to_lident p =
  match p with
  | Path.Pdot (p, s) -> Longident.Ldot (path_to_lident p, s)
  | Pident i -> Lident (Ident.name i)
  | Papply _ -> failwith "no correspondence"

let flatten_path p =
  match Path.flatten p with
  | `Ok (h, xs) -> Ident.name h :: xs
  | `Contains_apply -> failwith "does not work with apply"

let path_to_s p = flatten_path p |> String.concat "."
let demangle modname = List.concat_map (String.split ~by:"__") modname

let mnl_to_lident modname =
  match modname with
  | [] -> failwith "modname cannot be empty"
  | m :: ms -> List.fold_left Ppxlib.(fun t c -> Ldot (t, c)) (Lident m) ms

let first_matching f xs =
  List.fold_left (fun t c -> match t with None -> f c | Some _ -> t) None xs

(* pp because show is non-compositional.
   file and qual are for the current compilation unit, i.e. where exp_type is used.
*)
let rec printer_and_type =
  let variant = (C.read ()).variant in
  fun ~loc env file modname exp_type ->
    (* undo the mangling dune does to get a path we can refer to values with *)
    let normal_type ?(args = []) name =
      A.ptyp_constr ~loc { txt = Ppxlib.Lident name; loc } args
    in
    let handle_result a b =
      (* TODO *)
      (* can't use this as it specializes the second arg to string *)
      (* (match containers with
         | true ->
           [%expr Result.pp [%e generate_printer_typ a] [%e generate_printer_typ b]]
         | _ -> *)
      let pp_a, ta = printer_and_type ~loc env file modname a in
      let pp_b, tb = printer_and_type ~loc env file modname b in
      ( Ppxlib.([%expr Format.pp_print_result ~ok:[%e pp_a] ~error:[%e pp_b]]),
        let* ta =
          with_ctx
            (Format.asprintf "type for %a failed" Ppxlib.Pprintast.expression
               pp_a)
            ta
        in
        let+ tb =
          with_ctx
            (Format.asprintf "type for %a failed" Ppxlib.Pprintast.expression
               pp_b)
            tb
        in
        normal_type ~args:[ta; tb] "result" )
      (* ) *)
    in
    match Types.get_desc exp_type with
    | Tconstr (Pident ident, _, _) when String.equal (Ident.name ident) "int" ->
      ( Ppxlib.(
          match variant with
          | Containers -> [%expr CCInt.pp]
          | Stdlib -> [%expr Format.pp_print_int]),
        Ok (normal_type "int") )
    | Tconstr (Pident ident, _, _) when String.equal (Ident.name ident) "char"
      ->
      ( Ppxlib.(
          match variant with
          | Containers -> [%expr CCChar.pp]
          | Stdlib -> [%expr Format.pp_print_char]),
        Ok (normal_type "char") )
    | Tconstr (Pident ident, _, _) when String.equal (Ident.name ident) "float"
      ->
      ( (match variant with
        | Containers -> [%expr CCFloat.pp]
        | Stdlib -> [%expr Format.pp_print_float]),
        Ok (normal_type "float") )
    | Tconstr (Pident ident, _, _) when String.equal (Ident.name ident) "bool"
      ->
      ( (match variant with
        | Containers -> [%expr CCString.pp]
        | Stdlib -> [%expr Format.pp_print_bool]),
        Ok (normal_type "bool") )
    | Tconstr (Pident ident, _, _) when String.equal (Ident.name ident) "string"
      ->
      ( (match variant with
        | Containers -> [%expr CCString.pp]
        | Stdlib -> [%expr Format.pp_print_string]),
        Ok (normal_type "string") )
    | Tconstr (Pident ident, [a], _)
      when String.equal (Ident.name ident) "option" ->
      let pp_a, ta = printer_and_type ~loc env file modname a in
      ( (match variant with
        | Containers -> [%expr CCOpt.pp [%e pp_a]]
        | Stdlib -> [%expr Format.pp_print_option [%e pp_a]]),
        let+ ta in
        normal_type ~args:[ta] "option" )
    | Tconstr (Pident ident, [a; b], _)
      when String.equal (Ident.name ident) "result" ->
      handle_result a b
    | Tconstr (Pdot (Pident q, "result"), [a; b], _)
      when String.equal (Ident.name q) "Stdlib" ->
      handle_result a b
    | Tconstr (Pident ident, [a], _) when String.equal (Ident.name ident) "list"
      ->
      let pp_a, ta = printer_and_type ~loc env file modname a in
      ( (match variant with
        | Containers ->
          [%expr
            CCList.pp
              ~pp_start:(fun fmt () -> Format.fprintf fmt "[")
              ~pp_stop:(fun fmt () -> Format.fprintf fmt "]")
              ~pp_sep:(fun fmt () -> Format.fprintf fmt ";")
              [%e pp_a]]
        | Stdlib ->
          [%expr
            fun fmt xs ->
              Format.fprintf fmt "[";
              Format.pp_print_list
                ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
                [%e pp_a] fmt xs;
              Format.fprintf fmt "]"]),
        let+ ta in
        normal_type ~args:[ta] "list" )
    | Tconstr (Pident ident, [], _) when String.equal (Ident.name ident) "unit"
      ->
      ([%expr fun fmt () -> Format.fprintf fmt "()"], Ok (normal_type "unit"))
    | Tconstr (id, args, _) -> guess_named_type loc env file modname id args
    | Tvar v ->
      ( [%expr fun fmt _ -> Format.fprintf fmt "<poly>"],
        Ok (A.ptyp_var ~loc (v |> Option.get_or ~default:"a")) )
    | Tarrow _ ->
      ([%expr fun fmt _ -> Format.fprintf fmt "<fn>"], Error "function")
    | Ttuple _ ->
      (* TODO *)
      ([%expr fun fmt _ -> Format.fprintf fmt "<tuple>"], Error "tuple")
    | _ ->
      ( [%expr fun fmt _ -> Format.fprintf fmt "<unimplemented>"],
        Error "unimplemented" )

and guess_named_type =
  let library_entrypoints = (C.read ()).libraries in
  fun loc env use_file use_modname id args ->
    let mappings =
      C.SMap.find_opt use_file (C.read ()).mappings
      |> Option.get_or ~default:C.SMap.empty
    in
    let opaque_regexes = (C.read ()).opaque_type_names |> List.map Str.regexp in
    match id with
    | _
      when List.exists
             (fun r -> Str.string_match r (path_to_s id) 0)
             opaque_regexes
           ||
           match C.SMap.find_opt (path_to_s id) mappings with
           | Some Opaque -> true
           | _ -> false ->
      log "opaque %s %b %b" (path_to_s id)
        (List.exists
           (fun r -> Str.string_match r (path_to_s id) 0)
           opaque_regexes)
        (match C.SMap.find_opt (path_to_s id) mappings with
        | Some Opaque -> true
        | _ -> false);
      ([%expr fun fmt _ -> Format.fprintf fmt "<opaque>"], Error "opaque type")
    | _ ->
      let get_pp_name typ = match typ with "t" -> "pp" | _ -> "pp_" ^ typ in
      let qualifier, ident_name =
        (* figure out where some type is defined, then use that to point to the right printer *)
        let typdecl = Env.find_type id env in
        let decl_filename = typdecl.type_loc.loc_start.pos_fname in
        let def_prefix =
          match
            library_entrypoints
            |> first_matching (fun le ->
                   match relativize ~against:le decl_filename with
                   | None -> None
                   | Some def_file ->
                     (* assume that all path segments but the last are the directory hierarchy *)
                     let e = file_to_module (Filename.basename le) in
                     let m = file_to_module def_file in
                     (* example: le = demo/lib, def_file = other.ml, e = Lib, m = Other *)
                     (* if these are equal, e.g. Lib = Lib, we assume the use and def are in the same module,
                        which must also be the entrypoint *)
                     if List.equal String.equal e m then Some e else Some (e @ m))
          with
          | None -> file_to_module use_file
          | Some p -> p
        in
        log "def_prefix: %a, id: %a" (List.pp String.pp) def_prefix Path.print
          id;
        let qual = mnl_to_lident def_prefix in
        let new1 =
          match id with
          | Path.Pdot (q, ident) ->
            ( List.fold_left
                (fun t c ->
                  (* TODO heuristic. say a type is in demo/lib/other.ml,
                     and we refer to the type as Other.Abstr.t.
                     this drops Other, so we keep the internal module ref,
                     which isn't apparent from filename. *)
                  (* TODO can the cmt give us this? *)
                  if not (List.mem ~eq:String.equal c def_prefix) then
                    Longident.Ldot (t, c)
                  else t)
                qual (flatten_path q),
              ident )
          | Pident ident -> (qual, Ident.name ident)
          | _ -> failwith "not applicable"
        in
        log "new1: %a" (Pair.pp Pprintast.longident String.pp) new1;
        new1
      in
      log
        "printer name: (use_file = %s, use_modname = %a, id = %a) -guess-> \
         (qualifier = %a, ident_name = %s)"
        use_file (List.pp String.pp) use_modname Path.print id
        Pprintast.longident qualifier ident_name;
      let printer =
        A.pexp_ident ~loc
          { loc; txt = Ldot (qualifier, get_pp_name ident_name) }
      in
      let tident args =
        A.ptyp_constr ~loc { loc; txt = Ldot (qualifier, ident_name) } args
      in
      (match args with
      | [] -> (printer, Ok (tident []))
      | _ :: _ ->
        let p_args =
          List.map
            (fun a -> printer_and_type ~loc env use_file use_modname a)
            args
        in
        let p_args, types = List.split p_args in
        let p_args = p_args |> List.map (fun a -> (Ppxlib.Nolabel, a)) in
        let types = LR.sequence_m types in
        ( A.pexp_apply ~loc printer p_args,
          let* types in
          Ok (tident types) ))

let handle_expr use_modname it expr =
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
    when String.equal name "emit_value"
         || String.equal name "emit_argument"
         || String.equal name "emit_raw" ->
    let site_id =
      args
      |> List.filter_map (function
           | Asttypes.Labelled "ppx_debug_id", Some tuple ->
             begin
               match tuple.Typedtree.exp_desc with
               | Texp_record
                   {
                     fields =
                       [|
                         ( _,
                           Overridden
                             ( _file,
                               {
                                 exp_desc =
                                   Texp_constant (Const_string (file, _, _));
                                 _;
                               } ) );
                         ( _,
                           Overridden
                             ( _id,
                               { exp_desc = Texp_constant (Const_int id); _ } )
                         );
                         ( _,
                           Overridden
                             ( _loc,
                               {
                                 exp_desc =
                                   Texp_tuple
                                     [
                                       {
                                         exp_desc =
                                           Texp_tuple
                                             [
                                               {
                                                 exp_desc =
                                                   Texp_constant (Const_int sl);
                                                 _;
                                               };
                                               {
                                                 exp_desc =
                                                   Texp_constant (Const_int sc);
                                                 _;
                                               };
                                             ];
                                         _;
                                       };
                                       {
                                         exp_desc =
                                           Texp_tuple
                                             [
                                               {
                                                 exp_desc =
                                                   Texp_constant (Const_int el);
                                                 _;
                                               };
                                               {
                                                 exp_desc =
                                                   Texp_constant (Const_int ec);
                                                 _;
                                               };
                                             ];
                                         _;
                                       };
                                     ];
                                 _;
                               } ) );
                       |];
                     _;
                   } ->
                 Some Id.{ file; id; loc = ((sl, sc), (el, ec)) }
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
    let env = expr.exp_env in
    let pp_fn, typ =
      printer_and_type ~loc env site_id.file use_modname exp_type
    in
    log "file = %s, id = %d -> exp_type = %a | pp_fn = %a | typ = %a"
      site_id.file site_id.id Printtyp.type_expr exp_type
      Ppxlib.Pprintast.expression pp_fn
      (Result.pp Ppxlib.Pprintast.core_type)
      typ;
    let typ = Result.to_opt typ in
    id_type_mappings := (site_id, { pp_fn; typ }) :: !id_type_mappings
  | _ -> Tast_iterator.default_iterator.expr it expr

let walk_build_dir () =
  assert (Filename.check_suffix (Sys.getcwd ()) "_build/default");
  let should_process s =
    List.for_all
      (fun dir -> not (String.mem ~sub:dir s))
      (C.read ()).cmt_ignored_directories
  in
  IO.File.walk_seq "."
  |> Seq.iter (function
       | `File, s when String.ends_with ~suffix:"cmt" s && should_process s ->
         let cmt = Cmt_format.read_cmt s in
         Unstable.Load_path.init cmt.cmt_loadpath;
         let modname = cmt.cmt_modname |> String.split ~by:"." in
         let str =
           match cmt.cmt_annots with
           | Implementation str ->
             let map =
               {
                 Tast_mapper.default with
                 env = (fun _ env -> Envaux.env_of_only_summary env);
               }
             in
             let str = map.structure map str in
             str
           | _ -> failwith "not a cmt file"
         in

         let iter_structure =
           Tast_iterator.(
             default_iterator.structure
               { default_iterator with expr = handle_expr modname })
         in
         iter_structure str
       | `File, f when String.ends_with ~suffix:"cmt" f ->
         log "did not process %s" f
       | `File, _ -> ()
       | `Dir, _ -> ())

open Ppxlib

let str ~loc s = A.pexp_constant ~loc (Pconst_string (s, loc, None))

let g_print_value loc =
  let cases =
    A.pexp_match ~loc [%expr id]
      ((!id_type_mappings
       |> List.map (fun (Id.{ file; id; _ }, typ_info) ->
              let show_arg =
                match typ_info.typ with
                | None ->
                  (* i.e. always work *)
                  [%expr Marshal.from_string _content 0]
                | Some typ ->
                  [%expr (Marshal.from_string _content 0 : [%t typ])]
              in
              A.case
                ~lhs:
                  [%pat?
                    Ppx_debug_runtime.Id.
                      {
                        file = [%p A.pstring ~loc file];
                        id = [%p A.pint ~loc id];
                        _;
                      }]
                ~guard:None
                ~rhs:
                  [%expr
                    Format.asprintf "%a" [%e typ_info.pp_fn] [%e show_arg],
                      [%e
                        str ~loc
                          (Format.asprintf "%a" Pprintast.expression show_arg)]])
       )
      @ [
          A.case
            ~lhs:[%pat? Ppx_debug_runtime.Id.{ file; id; _ }]
            ~guard:None
            ~rhs:[%expr failwith (Format.sprintf "unknown type %s %d" file id)];
        ])
  in
  (* not sure why we have to prefix file with an underscore, or the compiler thinks it's unused *)
  let read =
    [%expr fun id (Ppx_debug_runtime.Trace.Bytestr _content) -> [%e cases]]
  in
  [%stri let print_value = [%e read]]

let handle_si ~loc ~path:_ payload =
  walk_build_dir ();
  match payload with
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ({ pexp_desc = Pexp_ident { txt = Lident s; _ }; _ }, _attrs);
          _;
        };
      ] ->
    begin
      match s with
      | "print_value" -> g_print_value loc
      | _ -> failwith ("no such generator: " ^ s)
    end
  | _ -> failwith "invalid payload"

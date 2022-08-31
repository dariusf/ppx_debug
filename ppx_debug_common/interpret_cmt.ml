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

(* let contains s1 s2 =
    let re = Str.regexp_string s2
    in
        try ignore (Str.search_forward re s1 0); true
        with Not_found -> false *)

(* pp because show is non-compositional.
   file and qual are for the current compilation unit, i.e. where exp_type is used.
*)
let rec create_pp_fn ~loc file qual exp_type =
  let variant = (C.read ()).variant in
  let handle_result a b =
    (* can't use this as it specializes the second arg to string *)
    (* (match containers with
       | true ->
         [%expr Result.pp [%e generate_printer_typ a] [%e generate_printer_typ b]]
       | _ -> *)
    Ppxlib.(
      [%expr
        Format.pp_print_result
          ~ok:[%e create_pp_fn ~loc file qual a]
          ~error:[%e create_pp_fn ~loc file qual b]])
    (* ) *)
  in
  match Types.get_desc exp_type with
  | Tconstr (Pident ident, _, _) when String.equal (Ident.name ident) "int" ->
    Ppxlib.(
      (match variant with
      | Containers -> [%expr CCInt.pp]
      | Stdlib -> [%expr Format.pp_print_int]))
  | Tconstr (Pident ident, _, _) when String.equal (Ident.name ident) "char" ->
    Ppxlib.(
      (match variant with
      | Containers -> [%expr CCChar.pp]
      | Stdlib -> [%expr Format.pp_print_char]))
  | Tconstr (Pident ident, _, _) when String.equal (Ident.name ident) "float" ->
    (match variant with
    | Containers -> [%expr CCFloat.pp]
    | Stdlib -> [%expr Format.pp_print_float])
  | Tconstr (Pident ident, _, _) when String.equal (Ident.name ident) "bool" ->
    (match variant with
    | Containers -> [%expr CCString.pp]
    | Stdlib -> [%expr Format.pp_print_bool])
  | Tconstr (Pident ident, _, _) when String.equal (Ident.name ident) "string"
    ->
    (match variant with
    | Containers -> [%expr CCString.pp]
    | Stdlib -> [%expr Format.pp_print_string])
  | Tconstr (Pident ident, [a], _) when String.equal (Ident.name ident) "option"
    ->
    (match variant with
    | Containers -> [%expr CCOpt.pp [%e create_pp_fn ~loc file qual a]]
    | Stdlib ->
      [%expr Format.pp_print_option [%e create_pp_fn ~loc file qual a]])
  | Tconstr (Pident ident, [a; b], _)
    when String.equal (Ident.name ident) "result" ->
    handle_result a b
  | Tconstr (Pdot (Pident q, "result"), [a; b], _)
    when String.equal (Ident.name q) "Stdlib" ->
    handle_result a b
  | Tconstr (Pident ident, [a], _) when String.equal (Ident.name ident) "list"
    ->
    (match variant with
    | Containers ->
      [%expr
        CCList.pp
          ~pp_start:(fun fmt () -> Format.fprintf fmt "[")
          ~pp_stop:(fun fmt () -> Format.fprintf fmt "]")
          ~pp_sep:(fun fmt () -> Format.fprintf fmt ";")
          [%e create_pp_fn ~loc file qual a]]
    | Stdlib ->
      [%expr
        fun fmt xs ->
          Format.fprintf fmt "[";
          Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
            [%e create_pp_fn ~loc file qual a]
            fmt xs;
          Format.fprintf fmt "]"])
  | Tconstr (Pident ident, [], _) when String.equal (Ident.name ident) "unit" ->
    [%expr fun fmt () -> Format.fprintf fmt "()"]
    (* the following two cases are the same except for the qualifiers *)
  | Tconstr (id, args, _) ->
    (* log "qualified ident %a" Path.print id; *)
    (* Pdot (q, ident) *)
    (* qual is where the use of the type is found, q is where the type is defined *)
    (* TOOD existing uses of qual are probably wrong *)
    let rec path_to_lident p =
      match p with
      | Path.Pdot (p, s) -> Longident.Ldot (path_to_lident p, s)
      | Pident i -> Lident (Ident.name i)
      | Papply _ -> failwith "no correspondence"
    in
    let flatten_path p =
      match Path.flatten p with
      | `Ok (h, xs) -> Ident.name h :: xs
      | `Contains_apply -> failwith "adklj"
    in
    let path_to_s p = flatten_path p |> String.concat "." in
    let mappings =
      (* List.assoc_opt ~eq:String.equal file (C.read ()).mappings
      *)
      C.SMap.find_opt file (C.read ()).mappings
      |> Option.get_or ~default:C.SMap.empty
    in
    begin
      match id with
      (* Path.Pdot (Pident _i, _ident) *)
      | _
        when List.mem ~eq:String.equal (path_to_s id)
               (C.read ()).treat_as_opaque
             ||
             match
               (* List.assoc_opt ~eq:String.equal (path_to_s id) mappings *)
               C.SMap.find_opt (path_to_s id) mappings
             with
             | Some Opaque -> true
             | _ -> false
             (* when String.equal (Ident.name i) "Stdlib" *)
             (* && List.mem ~eq:String.equal ident ["in_channel"] *) ->
        log "opaque %s" (path_to_s id);
        [%expr fun fmt _ -> Format.fprintf fmt "<opaque>"]
      | _ ->
        (* we have to figure out where some type is defined from where it is used in order to point to the right printer. *)
        let get_pp_name typ = match typ with "t" -> "pp" | _ -> "pp_" ^ typ in
        (* log "mappings %a" (List.pp (Pair.pp String.pp String.pp)) mappings;
           log "%b" (List.mem_assoc ~eq:String.equal (path_to_s id) mappings);
           log "%s" (path_to_s id); *)
        let qualifier, ident_name =
          match id with
          (* sometimes a type is qualified because it is defined in a module, but it is used in the same compilation unit. a heuristic to figure this out is to check if the qualifier is not a library entrypoint... *)
          (* | Path.Pdot (q, ident)
             when String.find ~sub:"__" (Format.asprintf "%a" Path.print q) = -1
             ->
             let qq =
               Longident.unflatten
                 (Longident.flatten qual @ Longident.flatten (path_to_lident q))
               |> Option.get_exn_or "qualifiers cannot be concatenated"
             in
             (qq, ident) *)
          (* rather than use fragile heuristics, take user input for how to access qualified types *)

          (* Path.Pdot (q, ident) *)
          | _
            when (* List.mem_assoc ~eq:String.equal (path_to_s id) mappings *)
                 C.SMap.mem (path_to_s id) mappings
                 (* C.SMap.mem file (C.read ()).mappings *)
                 (* && C.SMap.mem (path_to_s q)
                      (C.SMap.find file (C.read ()).mappings) *) ->
            (* print_endline (path_to_s q); *)
            log "found mapping for %s" (path_to_s id);
            let zz =
              (* C.SMap.find (path_to_s q) (C.SMap.find file (C.read ()).mappings) *)
              match
                (* List.assoc ~eq:String.equal (path_to_s id) mappings *)
                C.SMap.find (path_to_s id) mappings
              with
              | Rewrite s -> s
              | _ -> failwith "invalid"
            in
            let li, ident =
              let li =
                Longident.unflatten (String.split_on_char '.' zz)
                |> Option.get_exn_or "cannot parse longident"
              in
              match li with
              | Longident.Ldot (li, ident) -> (li, ident)
              | _ -> failwith "asd"
            in
            (li, ident)
          (* if the type is used in a compilation unit other than where it is defined, this information is part of the type's name *)
          | Path.Pdot (q, ident) ->
            log "another comp unit %s" (path_to_s id);
            (path_to_lident q, ident)
          (* if it's unqualified, we assume it is used where it is defined *)
          | Pident ident ->
            log "unqualified %s" (path_to_s id);
            (qual, Ident.name ident)
          | Papply _ -> failwith "not applicable"
        in
        log "printer name: (%s, %a, %a) -> (%a, %s)" file Pprintast.longident
          qual Path.print id Pprintast.longident qualifier ident_name;
        let f =
          A.pexp_ident ~loc
            {
              loc;
              txt =
                Ldot
                  ( qualifier (* path_to_lident q *),
                    get_pp_name ident_name
                    (* match ident_name with
                       | "t" -> "pp"
                       | _ ->
                         "pp_"
                         ^ (* ident *)
                         ident_name *) );
            }
        in
        (match args with
        | [] -> f
        | _ :: _ ->
          let p_args =
            List.map (create_pp_fn ~loc file qual) args
            |> List.map (fun a -> (Ppxlib.Nolabel, a))
          in
          A.pexp_apply ~loc f p_args)
    end
  (* begin
       match q with
       | Pident i
         when String.equal (Ident.name i) "Stdlib"
              && List.mem ~eq:String.equal ident ["in_channel"] ->
         [%expr fun fmt _ -> Format.fprintf fmt "<opaque>"]
       | _ ->
         let f =
           A.pexp_ident ~loc
             {
               loc;
               txt =
                 Ldot
                   ( path_to_lident q,
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
     end *)
  (* | Tconstr (Pident ident, args, _) ->
     log "unqualified ident %s, qual = %a" (Ident.name ident) Pprintast.longident
       qual;
     let f =
       A.pexp_ident ~loc
         {
           loc;
           txt =
             Ldot
               (* assume that the type is defined where it is used *)
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
       A.pexp_apply ~loc f p_args) *)
  | Tvar _ -> [%expr fun fmt _ -> Format.fprintf fmt "<poly>"]
  | Tarrow _ -> [%expr fun fmt _ -> Format.fprintf fmt "<fn>"]
  | Ttuple _ ->
    (* TODO *)
    [%expr fun fmt _ -> Format.fprintf fmt "<tuple>"]
  (* "Lib." ^ Ident.name ident *)
  (* "x" *)
  (* | T (Pident ident, _, _) -> *)
  | _ ->
    (* p_te exp_type; *)
    (* failwith "nyi" *)
    [%expr fun fmt _ -> Format.fprintf fmt "<unimplemented>"]

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
               (* | Texp_tuple
                   [
                     (* we can't use __FILE__ and __FUNCTION__ as these have not been expanded at ppx-processing time *)
                     { exp_desc = Texp_constant (Const_string (file, _, _)); _ };
                     (* { exp_desc = Texp_constant (Const_string (func, _, _)); _; }; *)
                     _;
                     { exp_desc = Texp_constant (Const_int id); _ };
                   ] -> *)
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
                             ( _line,
                               { exp_desc = Texp_constant (Const_int line); _ }
                             ) );
                       |];
                     _;
                   } ->
                 Some Id.{ file; id; line }
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
    let pp_fn = create_pp_fn ~loc site_id.file qual exp_type in
    let typ = create_typ_name ~loc qual exp_type in
    (* begin
       match typ with
       | Some typ -> *)
    let () =
      log "%s %d -> %a | %a" site_id.file site_id.id Printtyp.type_expr exp_type
        Ppxlib.Pprintast.expression pp_fn
      (* | %a *)
      (* (Option.pp Ppxlib.Pprintast.core_type) *)
      (* typ *)
    in
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
       |> List.map (fun (Id.{ file; id; _ }, typ_info) ->
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
                  (* (A.ppat_tuple ~loc
                     [
                       A.pstring ~loc file;
                       A.ppat_any ~loc;
                       (* A.ppat_any ~loc; *)
                       (* A.pstring ~loc func; *)
                       A.pint ~loc id;
                     ]) *)
                  (* (A.ppat_record ~loc [] Closed) *)
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
                    let _content = Ppx_debug_runtime.read_n _len _file in
                    Scanf.bscanf _file "\n" ();
                    Format.asprintf "%a" [%e typ_info.pp_fn] [%e show_arg]]))
      @ [
          A.case
            ~lhs:
              (* (A.ppat_tuple ~loc [[%pat? file]; A.ppat_any ~loc; [%pat? id]]) *)
              [%pat? Ppx_debug_runtime.Id.{ file; id; _ }]
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

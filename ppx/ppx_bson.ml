open Ppxlib

module A = Ast_builder.Default

let loc_of_type_decl td = td.ptype_loc

let lid_of_parts = function
  | [] -> invalid_arg "lid_of_parts"
  | part :: parts -> List.fold_left (fun acc part -> Longident.Ldot (acc, part)) (Longident.Lident part) parts

let lid ~loc parts = { loc; txt = lid_of_parts parts }
let ident ~loc parts = A.pexp_ident ~loc (lid ~loc parts)
let str ~loc value = A.estring ~loc value
let var ~loc name = A.evar ~loc name
let pat_var ~loc name = A.ppat_var ~loc { loc; txt = name }

let app ~loc fn args =
  A.pexp_apply ~loc fn (List.map (fun arg -> (Nolabel, arg)) args)

let type_path_to_parts path =
  let rec loop acc = function
    | Longident.Lident name -> name :: acc
    | Longident.Ldot (prefix, name) -> loop (name :: acc) prefix
    | Longident.Lapply _ -> Location.raise_errorf "BSON deriving does not support applicative type paths"
  in
  loop [] path

let bson_key_attr =
  Attribute.declare "bson.key" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (estring __))
    (fun key -> key)

type codec = {
  to_bson : expression;
  of_bson : expression;
  option_inner_of_bson : expression option;
}

let builtin_codec ~loc name =
  {
    to_bson = ident ~loc [ "Bson_ext"; name ^ "_to_bson" ];
    of_bson = ident ~loc [ "Bson_ext"; name ^ "_of_bson" ];
    option_inner_of_bson = None;
  }

let rec codec_of_type typ =
  let loc = typ.ptyp_loc in
  match typ.ptyp_desc with
  | Ptyp_constr ({ txt = Longident.Lident "string"; _ }, []) -> builtin_codec ~loc "string"
  | Ptyp_constr ({ txt = Longident.Lident "int"; _ }, []) -> builtin_codec ~loc "int"
  | Ptyp_constr ({ txt = Longident.Lident "int32"; _ }, []) -> builtin_codec ~loc "int32"
  | Ptyp_constr ({ txt = Longident.Ldot (Longident.Lident "Int32", "t"); _ }, []) ->
      builtin_codec ~loc "int32"
  | Ptyp_constr ({ txt = Longident.Lident "int64"; _ }, []) -> builtin_codec ~loc "int64"
  | Ptyp_constr ({ txt = Longident.Ldot (Longident.Lident "Int64", "t"); _ }, []) ->
      builtin_codec ~loc "int64"
  | Ptyp_constr ({ txt = Longident.Lident "bool"; _ }, []) -> builtin_codec ~loc "bool"
  | Ptyp_constr ({ txt = Longident.Lident "float"; _ }, []) -> builtin_codec ~loc "float"
  | Ptyp_constr ({ txt = Longident.Lident "list"; _ }, [ inner ])
  | Ptyp_constr ({ txt = Longident.Ldot (Longident.Lident "List", "t"); _ }, [ inner ]) ->
      let inner = codec_of_type inner in
      {
        to_bson = app ~loc (ident ~loc [ "Bson_ext"; "list_to_bson" ]) [ inner.to_bson ];
        of_bson = app ~loc (ident ~loc [ "Bson_ext"; "list_of_bson" ]) [ inner.of_bson ];
        option_inner_of_bson = None;
      }
  | Ptyp_constr ({ txt = Longident.Lident "array"; _ }, [ inner ])
  | Ptyp_constr ({ txt = Longident.Ldot (Longident.Lident "Array", "t"); _ }, [ inner ]) ->
      let inner = codec_of_type inner in
      {
        to_bson = app ~loc (ident ~loc [ "Bson_ext"; "array_to_bson" ]) [ inner.to_bson ];
        of_bson = app ~loc (ident ~loc [ "Bson_ext"; "array_of_bson" ]) [ inner.of_bson ];
        option_inner_of_bson = None;
      }
  | Ptyp_constr ({ txt = Longident.Lident "option"; _ }, [ inner ])
  | Ptyp_constr ({ txt = Longident.Ldot (Longident.Lident "Option", "t"); _ }, [ inner ]) ->
      let inner = codec_of_type inner in
      {
        to_bson = app ~loc (ident ~loc [ "Bson_ext"; "option_to_bson" ]) [ inner.to_bson ];
        of_bson = app ~loc (ident ~loc [ "Bson_ext"; "option_of_bson" ]) [ inner.of_bson ];
        option_inner_of_bson = Some inner.of_bson;
      }
  | Ptyp_constr ({ txt = path; _ }, []) ->
      let parts = type_path_to_parts path in
      let codec_parts suffix =
        match List.rev parts with
        | "t" :: module_parts -> List.rev (suffix :: module_parts)
        | name :: module_parts -> List.rev ((name ^ suffix) :: module_parts)
        | [] -> assert false
      in
      {
        to_bson = ident ~loc (codec_parts "_to_bson");
        of_bson = ident ~loc (codec_parts "_of_bson");
        option_inner_of_bson = None;
      }
  | _ ->
      Location.raise_errorf ~loc
        "BSON deriving supports records whose fields are primitive values, lists, arrays, options, \
         or other BSON-derived types"

let field_key field =
  match Attribute.get bson_key_attr field with
  | Some key -> key
  | None -> field.pld_name.txt

let ensure_supported_type td =
  if td.ptype_params <> [] then
    Location.raise_errorf ~loc:td.ptype_loc "BSON deriving does not yet support parameterized types";
  match td.ptype_kind with
  | Ptype_record fields -> fields
  | _ -> Location.raise_errorf ~loc:td.ptype_loc "BSON deriving currently supports record types only"

let ensure_immutable field =
  match field.pld_mutable with
  | Immutable -> ()
  | Mutable -> Location.raise_errorf ~loc:field.pld_loc "BSON deriving does not support mutable record fields"

let type_expr ~loc type_name =
  A.ptyp_constr ~loc { loc; txt = Longident.Lident type_name } []

let gen_to_doc td fields =
  let loc = loc_of_type_decl td in
  let type_name = td.ptype_name.txt in
  let value = var ~loc "value" in
  let field_pair field =
    ensure_immutable field;
    let codec = codec_of_type field.pld_type in
    let field_loc = field.pld_loc in
    let field_name = field.pld_name.txt in
    let field_value =
      A.pexp_field ~loc:field_loc value { loc = field_loc; txt = Longident.Lident field_name }
    in
    A.pexp_tuple ~loc:field_loc [ str ~loc:field_loc (field_key field); app ~loc:field_loc codec.to_bson [ field_value ] ]
  in
  let body =
    app ~loc (ident ~loc [ "Bson_ext"; "document" ]) [ A.elist ~loc (List.map field_pair fields) ]
  in
  let pat =
    A.ppat_constraint ~loc (pat_var ~loc "value") (type_expr ~loc type_name)
  in
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc
        ~pat:(pat_var ~loc (type_name ^ "_to_bson_doc"))
        ~expr:(A.pexp_fun ~loc Nolabel None pat body);
    ]

let gen_of_doc td fields =
  let loc = loc_of_type_decl td in
  let type_name = td.ptype_name.txt in
  let doc = var ~loc "doc" in
  let field_expr field =
    ensure_immutable field;
    let codec = codec_of_type field.pld_type in
    let field_loc = field.pld_loc in
    let key = field_key field in
    let value =
      match codec.option_inner_of_bson with
      | Some of_bson ->
          app ~loc:field_loc (ident ~loc:field_loc [ "Bson_ext"; "optional_field" ])
            [ str ~loc:field_loc key; of_bson; doc ]
      | None ->
          app ~loc:field_loc (ident ~loc:field_loc [ "Bson_ext"; "required_field" ])
            [ str ~loc:field_loc key; codec.of_bson; doc ]
    in
    ({ loc = field_loc; txt = Longident.Lident field.pld_name.txt }, value)
  in
  let body = A.pexp_record ~loc (List.map field_expr fields) None in
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc
        ~pat:(pat_var ~loc (type_name ^ "_of_bson_doc"))
        ~expr:(A.pexp_fun ~loc Nolabel None (pat_var ~loc "doc") body);
    ]

let gen_of_doc_result td fields =
  let loc = loc_of_type_decl td in
  let type_name = td.ptype_name.txt in
  let of_doc = ident ~loc [ type_name ^ "_of_bson_doc" ] in
  List.iter ensure_immutable fields;
  let body =
    app ~loc (ident ~loc [ "Bson_ext"; "result_of_exn" ])
      [ A.pexp_fun ~loc Nolabel None (A.ppat_construct ~loc (lid ~loc [ "()" ]) None)
          (app ~loc of_doc [ var ~loc "doc" ]) ]
  in
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc
        ~pat:(pat_var ~loc (type_name ^ "_of_bson_doc_result"))
        ~expr:(A.pexp_fun ~loc Nolabel None (pat_var ~loc "doc") body);
    ]

let gen_element_codecs td =
  let loc = loc_of_type_decl td in
  let type_name = td.ptype_name.txt in
  let to_doc = ident ~loc [ type_name ^ "_to_bson_doc" ] in
  let of_doc = ident ~loc [ type_name ^ "_of_bson_doc" ] in
  let of_bson = ident ~loc [ type_name ^ "_of_bson" ] in
  let value = var ~loc "value" in
  let element = var ~loc "element" in
  [
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc
          ~pat:(pat_var ~loc (type_name ^ "_to_bson"))
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pat_var ~loc "value")
               (app ~loc (ident ~loc [ "Bson"; "create_doc_element" ]) [ app ~loc to_doc [ value ] ]));
      ];
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc
          ~pat:(pat_var ~loc (type_name ^ "_of_bson"))
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pat_var ~loc "element")
               (app ~loc of_doc [ app ~loc (ident ~loc [ "Bson"; "get_doc_element" ]) [ element ] ]));
      ];
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc
          ~pat:(pat_var ~loc (type_name ^ "_of_bson_result"))
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pat_var ~loc "element")
               (app ~loc (ident ~loc [ "Bson_ext"; "result_of_exn" ])
                  [ A.pexp_fun ~loc Nolabel None
                      (A.ppat_construct ~loc (lid ~loc [ "()" ]) None)
                      (app ~loc of_bson [ var ~loc "element" ]) ]));
      ];
  ]

let generate_str ~loc:_ ~path:_ (_rec_flag, tds) =
  List.concat_map
    (fun td ->
      let fields = ensure_supported_type td in
      [ gen_to_doc td fields; gen_of_doc td fields; gen_of_doc_result td fields ]
      @ gen_element_codecs td)
    tds

let gen_sig_for_type td =
  let loc = loc_of_type_decl td in
  let type_name = td.ptype_name.txt in
  if td.ptype_params <> [] then
    Location.raise_errorf ~loc "BSON deriving does not yet support parameterized types";
  let typ = type_expr ~loc type_name in
  let arrow left right = A.ptyp_arrow ~loc Nolabel left right in
  let value name typ =
    A.psig_value ~loc (A.value_description ~loc ~name:{ loc; txt = name } ~type_:typ ~prim:[])
  in
  [
    value (type_name ^ "_to_bson_doc") (arrow typ (A.ptyp_constr ~loc (lid ~loc [ "Bson"; "t" ]) []));
    value (type_name ^ "_of_bson_doc") (arrow (A.ptyp_constr ~loc (lid ~loc [ "Bson"; "t" ]) []) typ);
    value (type_name ^ "_of_bson_doc_result")
      (arrow (A.ptyp_constr ~loc (lid ~loc [ "Bson"; "t" ]) [])
         (A.ptyp_constr ~loc (lid ~loc [ "result" ])
            [ typ; A.ptyp_constr ~loc (lid ~loc [ "string" ]) [] ]));
    value (type_name ^ "_to_bson") (arrow typ (A.ptyp_constr ~loc (lid ~loc [ "Bson"; "element" ]) []));
    value (type_name ^ "_of_bson") (arrow (A.ptyp_constr ~loc (lid ~loc [ "Bson"; "element" ]) []) typ);
    value (type_name ^ "_of_bson_result")
      (arrow (A.ptyp_constr ~loc (lid ~loc [ "Bson"; "element" ]) [])
         (A.ptyp_constr ~loc (lid ~loc [ "result" ])
            [ typ; A.ptyp_constr ~loc (lid ~loc [ "string" ]) [] ]));
  ]

let generate_sig ~loc:_ ~path:_ (_rec_flag, tds) =
  List.concat_map
    (fun td ->
      ignore (ensure_supported_type td);
      gen_sig_for_type td)
    tds

let str_type_decl = Deriving.Generator.make_noarg generate_str
let sig_type_decl = Deriving.Generator.make_noarg generate_sig

let (_ : Deriving.t) =
  Deriving.add "bson" ~str_type_decl ~sig_type_decl

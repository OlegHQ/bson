let assert_equal label expected actual =
  if expected <> actual then failwith ("FAIL " ^ label)

let assert_true label condition =
  if not condition then failwith ("FAIL " ^ label)

let expect_malformed label encoded =
  try
    ignore (Bson.decode encoded);
    failwith ("FAIL " ^ label ^ ": decoded malformed BSON")
  with
  | Bson.Malformed_bson -> ()
  | Invalid_argument message ->
      failwith ("FAIL " ^ label ^ ": invalid_argument " ^ message)
  | exn -> failwith ("FAIL " ^ label ^ ": " ^ Printexc.to_string exn)

let bytes values =
  String.init (List.length values) (fun index -> Char.chr (List.nth values index))

let int32_le value =
  bytes
    [
      value land 0xFF;
      (value lsr 8) land 0xFF;
      (value lsr 16) land 0xFF;
      (value lsr 24) land 0xFF;
    ]

let cstring value = value ^ "\x00"

let document_body elements =
  let body = String.concat "" elements in
  int32_le (String.length body + 5) ^ body ^ "\x00"

let element tag name payload =
  String.make 1 (Char.chr tag) ^ cstring name ^ payload

let string_payload value =
  int32_le (String.length value + 1) ^ value ^ "\x00"

let doc fields =
  List.fold_right
    (fun (name, element) acc -> Bson.add_element name element acc)
    fields Bson.empty

let roundtrip_supported_types () =
  let nested = doc [ ("n", Bson.create_int32 7l) ] in
  let original =
    doc
      [
        ("double", Bson.create_double 1.25);
        ("string", Bson.create_string "hello");
        ("document", Bson.create_doc_element nested);
        ( "array",
          Bson.create_list
            [
              Bson.create_string "first";
              Bson.create_int32 Int32.max_int;
              Bson.create_boolean true;
            ] );
        ("binary", Bson.create_generic_binary "abc");
        ("objectId", Bson.create_objectId "52780d55c21477f7aa5b9108");
        ("boolean", Bson.create_boolean false);
        ("utc", Bson.create_utc 42L);
        ("null", Bson.create_null ());
        ("regex", Bson.create_regex "^a" "i");
        ("jscode", Bson.create_jscode "return 1;");
        ("int32_min", Bson.create_int32 Int32.min_int);
        ("int32_max", Bson.create_int32 Int32.max_int);
        ("int64_min", Bson.create_int64 Int64.min_int);
        ("int64_max", Bson.create_int64 Int64.max_int);
        ("minkey", Bson.create_minkey ());
        ("maxkey", Bson.create_maxkey ());
      ]
  in
  let decoded = Bson.decode (Bson.encode original) in
  assert_equal "string roundtrip" "hello"
    (Bson.get_string (Bson.get_element "string" decoded));
  assert_equal "nested int32" 7l
    (Bson.get_int32
       (Bson.get_element "n"
          (Bson.get_doc_element (Bson.get_element "document" decoded))));
  assert_equal "array order"
    [ "first"; "2147483647"; "true" ]
    (Bson.get_list (Bson.get_element "array" decoded)
    |> List.map (fun item ->
           try Bson.get_string item
           with Bson.Wrong_bson_type -> (
             try Int32.to_string (Bson.get_int32 item)
             with Bson.Wrong_bson_type ->
               string_of_bool (Bson.get_boolean item))));
  assert_equal "objectId hex" "\x52\x78\x0d\x55\xc2\x14\x77\xf7\xaa\x5b\x91\x08"
    (Bson.get_objectId (Bson.get_element "objectId" decoded));
  assert_equal "int64 min" Int64.min_int
    (Bson.get_int64 (Bson.get_element "int64_min" decoded));
  assert_equal "int64 max" Int64.max_int
    (Bson.get_int64 (Bson.get_element "int64_max" decoded))

let preserves_field_order_on_decode () =
  let encoded =
    document_body
      [
        element 0x10 "first" (int32_le 1);
        element 0x10 "second" (int32_le 2);
        element 0x10 "third" (int32_le 3);
      ]
  in
  assert_equal "decoded field order"
    [ "first"; "second"; "third" ]
    (Bson.decode encoded |> Bson.all_elements |> List.map fst)

let rejects_malformed_documents () =
  expect_malformed "too short document" (int32_le 4 ^ "\x00");
  expect_malformed "declared length longer than buffer" (int32_le 12 ^ "\x00");
  expect_malformed "declared length shorter than buffer"
    (int32_le 5 ^ "\x00\x00");
  expect_malformed "missing document terminator"
    (int32_le 12 ^ "\x10x\x00" ^ int32_le 1);
  expect_malformed "unknown type tag"
    (document_body [ element 0x42 "bad" "" ]);
  expect_malformed "missing element name terminator"
    (int32_le 11 ^ "\x10name" ^ int32_le 1 ^ "\x00");
  expect_malformed "string length shorter than nul"
    (document_body [ element 0x02 "s" (int32_le 0) ]);
  expect_malformed "string missing trailing nul"
    (document_body [ element 0x02 "s" (int32_le 2 ^ "ab") ]);
  expect_malformed "invalid boolean byte"
    (document_body [ element 0x08 "b" "\x02" ]);
  expect_malformed "array nonnumeric key"
    (document_body
       [
         element 0x04 "items"
           (document_body [ element 0x02 "name" (string_payload "bad") ]);
       ])

let object_id_validation () =
  assert_true "12 byte objectId accepted"
    (Bson.get_objectId (Bson.create_objectId "123456789012") = "123456789012");
  assert_true "24 hex objectId accepted"
    (String.length (Bson.get_objectId (Bson.create_objectId "52780d55c21477f7aa5b9108")) = 12);
  (try
     ignore (Bson.create_objectId "not-valid-object-id-value");
     failwith "FAIL invalid objectId accepted"
   with Bson.Invalid_objectId -> ())

let () =
  roundtrip_supported_types ();
  preserves_field_order_on_decode ();
  rejects_malformed_documents ();
  object_id_validation ()

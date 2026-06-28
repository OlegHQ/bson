type state_doc = {
  kind : string;
  external_id : string option;
}
[@@deriving bson]

type post_doc = {
  id : string [@bson.key "_id"];
  body : string;
  media_ids : string list;
  state : state_doc;
  created_at_ms : int64;
}
[@@deriving bson]

let assert_equal label left right =
  if left <> right then failwith ("expected equal " ^ label)

let () =
  let state = { kind = "draft"; external_id = None } in
  let post =
    {
      id = "post_1";
      body = "hello";
      media_ids = [ "media_1"; "media_2" ];
      state;
      created_at_ms = 42L;
    }
  in
  let doc = post_doc_to_bson_doc post in
  assert_equal "_id" "post_1" (Bson.get_string (Bson.get_element "_id" doc));
  assert_equal "roundtrip" post (post_doc_of_bson_doc doc);
  assert_equal "nested" state (state_doc_of_bson (Bson.get_element "state" doc))

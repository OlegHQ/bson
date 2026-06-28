(lang dune 3.16)

(name bson)

(package
 (name bson)
 (synopsis "BSON data structure with encoding and decoding")
 (description "Modern Dune packaging for the MassD BSON API used by mongo-eio.")
 (depends
  (ocaml (>= 5.0))
  (dune (>= 3.16))
  ppxlib))

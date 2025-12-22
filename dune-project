(lang dune 3.20)

(package
 (name musi)
 (allow_empty)
 (synopsis "Musi")
 (description
  "Systems programming language for clarity, safety, and performance")
 (depends
  dune
  alcotest
  ctypes-foreign
  fmt
  base-unix
  cmdliner
  yojson
  lsp
  jsonrpc))

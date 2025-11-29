open Basic
open Lex

type state = {
    tokens : (Token.t * Span.t) array
  ; pos : int
  ; interner : Interner.t
  ; diags : Diagnostic.bag
}

let mk_state tokens interner =
  {
    tokens = Array.of_list tokens
  ; pos = 0
  ; interner
  ; diags = Diagnostic.empty_bag
  }

let peek st =
  if st.pos >= Array.length st.tokens then (Token.EOF, Span.dummy)
  else st.tokens.(st.pos)

let peek_nth st n =
  let idx = st.pos + n in
  if idx >= Array.length st.tokens then (Token.EOF, Span.dummy)
  else st.tokens.(idx)

let advance st = { st with pos = st.pos + 1 }

let curr_span st =
  if st.pos >= Array.length st.tokens then Span.dummy
  else snd st.tokens.(st.pos)

let add_error st msg span =
  { st with diags = Diagnostic.add st.diags (Diagnostic.error msg span) }

let add_error_code st code span args =
  { st with diags = Diagnostic.add st.diags (Parse.Errors.diag code span args) }

let rec skip_trivia st =
  match fst (peek st) with
  | Token.Whitespace | Token.Newline | Token.Comment _ ->
    skip_trivia (advance st)
  | _ -> st

let expect st tok =
  let st = skip_trivia st in
  let curr, span = peek st in
  if curr = tok then (advance st, span)
  else
    let st' =
      add_error_code
        st
        Parse.Errors.E1003
        span
        [ Token.to_string tok; Token.to_string curr ]
    in
    (st', span)

let match_token st tok =
  let st = skip_trivia st in
  let curr, _ = peek st in
  curr = tok

let consume_if st tok =
  let st = skip_trivia st in
  let curr, _ = peek st in
  if curr = tok then (advance st, true) else (st, false)

let parse tokens interner =
  let st = mk_state tokens interner in
  let st = skip_trivia st in
  (* TODO: parse_program *)
  ([], st.diags)

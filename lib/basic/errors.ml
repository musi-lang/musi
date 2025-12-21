type level = Error | Warning | Note

type numeric_error =
  | InvalidUnderscore of string (* kind: "decimal", "hex", etc. *)
  | NoDigits of string

type lexical_error =
  | UnclosedString
  | UnclosedTemplate
  | UnclosedEscapedIdent
  | UnclosedRune
  | UnclosedComment
  | InvalidIdent
  | UnknownChar of char
  | MalformedUnderscore of string
  | EmptyRune
  | MultiCharRune
  | InvalidNumeric of numeric_error

type t = Lexical of lexical_error
type info = { msg : string; hint : string option; level : level }

let numeric_info = function
  | InvalidUnderscore kind ->
    {
      msg = Printf.sprintf "malformed underscore in %s literal" kind
    ; hint = Some "underscores must separate digits"
    ; level = Error
    }
  | NoDigits kind ->
    {
      msg = Printf.sprintf "invalid %s literal" kind
    ; hint = None
    ; level = Error
    }

let lexical_info = function
  | UnclosedString ->
    {
      msg = "unclosed string literal"
    ; hint = Some "missing '\"'"
    ; level = Error
    }
  | UnclosedTemplate ->
    {
      msg = "unclosed template literal"
    ; hint = Some "missing '\"'"
    ; level = Error
    }
  | UnclosedEscapedIdent ->
    {
      msg = "unclosed escaped identifier"
    ; hint = Some "missing '`'"
    ; level = Error
    }
  | UnclosedRune ->
    { msg = "unclosed rune literal"; hint = Some "missing '\''"; level = Error }
  | UnclosedComment ->
    {
      msg = "unclosed block comment"
    ; hint = Some "missing '*/'"
    ; level = Error
    }
  | InvalidIdent -> { msg = "invalid identifier"; hint = None; level = Error }
  | UnknownChar c ->
    {
      msg = Printf.sprintf "unknown character '%c'" c
    ; hint = Some "remove this character"
    ; level = Error
    }
  | MalformedUnderscore kind ->
    {
      msg = Printf.sprintf "malformed underscore in %s literal" kind
    ; hint = Some "underscores must separate digits"
    ; level = Error
    }
  | EmptyRune ->
    { msg = "rune literal cannot be empty"; hint = None; level = Error }
  | MultiCharRune ->
    {
      msg = "rune literal must contain exactly one character"
    ; hint = None
    ; level = Error
    }
  | InvalidNumeric err -> numeric_info err

let info = function Lexical err -> lexical_info err

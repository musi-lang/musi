type severity = Error | Warning | Note
type category = Lex | Parse | Sema | Codegen | Runtime

type error_code =
  (* E0001..E0999 *)
  | Lex of string
  (* E1001..E1999 *)
  | Parse of string
  (* E2001..E2999 *)
  | Sema of string
  (* E3001..E3999 *)
  | Codegen of string
  (* E4001..E4999 *)
  | Runtime of string

type t = {
    severity : severity
  ; code : error_code option
  ; message : string
  ; span : Span.t
  ; notes : (string * Span.t) list
}

type bag = { diags : t list; errors : int; warnings : int }

val make : severity -> string -> Span.t -> t
val make_with_code : severity -> error_code -> string -> Span.t -> t
val error : string -> Span.t -> t
val warning : string -> Span.t -> t
val note : string -> Span.t -> t
val error_with_code : error_code -> string -> Span.t -> t
val warning_with_code : error_code -> string -> Span.t -> t
val note_with_code : error_code -> string -> Span.t -> t
val add_note : string -> Span.t -> t -> t
val empty_bag : bag
val add : bag -> t -> bag
val add_list : bag -> t list -> bag
val merge : bag -> bag -> bag
val get_errors : bag -> t list
val get_warnings : bag -> t list
val get_all : bag -> t list
val count_errors : bag -> int
val count_warnings : bag -> int
val is_empty : bag -> bool

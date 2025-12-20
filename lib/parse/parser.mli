open Basic
open Lex
open Ast
open Nodes

(** Parser state *)
type t

(** Operator precedence levels *)

(** Create new parser instance from token sequence *)
val create : (Token.t * Span.t) Seq.t -> Source.t -> int -> Interner.t -> t

(** Check if parser has errors *)
val has_errors : t -> bool

(** Get diagnostics bag *)
val diag : t -> Reporter.bag

(** Parse all statements in source *)
val try_parse :
     (Token.t * Span.t) Seq.t
  -> Source.t
  -> int
  -> Interner.t
  -> (stmt list, Reporter.bag) result

(** Parse single expression with given precedence *)
val parse_expr : t -> Prec.t -> expr

(** Parse single type definition *)
val parse_ty : t -> ty

(** Parse single pattern *)
val parse_pat : t -> pat

(** Parse single statement *)
val parse_stmt : t -> stmt

open Basic
open Lex
open Ast
open Nodes

type t

type prec =
  | PrecNone
  | PrecAssign
  | PrecPipe
  | PrecCoal
  | PrecOr
  | PrecAnd
  | PrecBitOr
  | PrecBitXor
  | PrecBitAnd
  | PrecEquality
  | PrecComparison
  | PrecRange
  | PrecCons
  | PrecTerm
  | PecFactor
  | PrecExponent
  | PrecUnary
  | PrecPostfix

val create : Token.t Seq.t -> Source.t -> int -> Interner.t -> t

val parse :
     Token.t Seq.t
  -> Source.t
  -> int
  -> Interner.t
  -> (stmt list, Reporter.bag) result

val parse_expr : t -> prec -> expr
val parse_ty : t -> ty
val parse_pat : t -> pat
val parse_stmt : t -> stmt

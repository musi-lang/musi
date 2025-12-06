open Basic
open Combinator

val parse_lit :
     stream
  -> ((Ast.Node.expr Ast.Node.node * Span.t) * stream, Diagnostic.bag) Result.t

val parse_ident :
     stream
  -> ((Ast.Node.expr Ast.Node.node * Span.t) * stream, Diagnostic.bag) Result.t

val parse_atom :
     stream
  -> ((Ast.Node.expr Ast.Node.node * Span.t) * stream, Diagnostic.bag) Result.t

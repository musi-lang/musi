open Basic
open Node

type context = { mutable diags : Diagnostic.bag }
type 'a result = ('a, Diagnostic.bag) Result.t

type 'a t = {
    expr : expr -> 'a result
  ; stmt : stmt -> 'a result
  ; pat : pat -> 'a result
  ; typ : typ -> 'a result
}

val mk_context : unit -> context
val add_error : context -> string -> Span.t -> unit
val add_warning : context -> string -> Span.t -> unit
val ret_ok : 'a -> 'a result
val ret_err : Diagnostic.bag -> 'a result
val bind_result : 'a result -> ('a -> 'b result) -> 'b result
val map : 'a t -> context -> prog -> unit result
val iter : 'a t -> context -> prog -> unit
val fold : ('a -> stmt -> 'a result) -> 'a -> prog -> 'a result
val count_nodes : prog -> int
val find_idents : prog -> ident list

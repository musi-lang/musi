open Nodes

type 'ctx visitor = {
    visit_expr : 'ctx visitor -> 'ctx -> expr -> unit
  ; visit_pat : 'ctx visitor -> 'ctx -> pat -> unit
  ; visit_ty : 'ctx visitor -> 'ctx -> ty -> unit
  ; visit_stmt : 'ctx visitor -> 'ctx -> stmt -> unit
  ; visit_ident : 'ctx visitor -> 'ctx -> ident -> unit
  ; visit_lit : 'ctx visitor -> 'ctx -> lit -> unit
}

val traverse_expr : 'ctx visitor -> 'ctx -> expr -> unit
val traverse_pat : 'ctx visitor -> 'ctx -> pat -> unit
val traverse_ty : 'ctx visitor -> 'ctx -> ty -> unit
val traverse_stmt : 'ctx visitor -> 'ctx -> stmt -> unit
val traverse_fn_sig : 'ctx visitor -> 'ctx -> fn_sig -> unit
val traverse_param : 'ctx visitor -> 'ctx -> param -> unit
val default_visitor : 'ctx visitor

open Nodes

(** Visitor context and traversal functions *)
type 'ctx visitor = {
    visit_expr : 'ctx visitor -> 'ctx -> expr -> unit
  ; visit_pat : 'ctx visitor -> 'ctx -> pat -> unit
  ; visit_ty : 'ctx visitor -> 'ctx -> ty -> unit
  ; visit_stmt : 'ctx visitor -> 'ctx -> stmt -> unit
  ; visit_ident : 'ctx visitor -> 'ctx -> ident -> unit
  ; visit_lit : 'ctx visitor -> 'ctx -> lit -> unit
}

(** Traverse expression node *)
val traverse_expr : 'ctx visitor -> 'ctx -> expr -> unit

(** Traverse pattern node *)
val traverse_pat : 'ctx visitor -> 'ctx -> pat -> unit

(** Traverse type node *)
val traverse_ty : 'ctx visitor -> 'ctx -> ty -> unit

(** Traverse statement node *)
val traverse_stmt : 'ctx visitor -> 'ctx -> stmt -> unit

(** Traverse function signature *)
val traverse_fn_sig : 'ctx visitor -> 'ctx -> fn_sig -> unit

(** Traverse function parameter *)
val traverse_param : 'ctx visitor -> 'ctx -> param -> unit

(** Default visitor implementation with no-op leaves *)
val default_visitor : 'ctx visitor

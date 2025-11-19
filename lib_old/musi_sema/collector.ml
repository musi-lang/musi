open Musi_basic

type constraint_t =
  | Unify of Types.t * Types.t * Span.t
  | Equal of Types.t * Types.t * Span.t

type constraint_set = {
    constraints : constraint_t list ref
  ; interner : Interner.t
}

let create interner = { constraints = ref []; interner }

let add_constraint set constraint_t =
  set.constraints := constraint_t :: !(set.constraints)

let unify_constraint set t1 t2 span = add_constraint set (Unify (t1, t2, span))
let equal_constraint set t1 t2 span = add_constraint set (Equal (t1, t2, span))

let solve_constraints set diags =
  let error msg span =
    diags := Diagnostic.add !diags (Diagnostic.error msg span)
  in
  List.iter
    (fun constraint_t ->
      match constraint_t with
      | Unify (t1, t2, span) -> (
        try Unifier.unify (Types.repr t1) (Types.repr t2) with
        | Failure msg when String.contains msg '\'' ->
          let t1_str = Types.show_with set.interner (Types.repr t1) in
          let t2_str = Types.show_with set.interner (Types.repr t2) in
          let fixed_msg =
            Printf.sprintf "expected type '%s', found type '%s'" t1_str t2_str
          in
          error fixed_msg span
        | Failure msg -> error msg span)
      | Equal (t1, t2, span) ->
        let t1_repr = Types.repr t1 in
        let t2_repr = Types.repr t2 in
        if not (t1_repr = t2_repr) then
          let t1_str = Types.show_with set.interner t1_repr in
          let t2_str = Types.show_with set.interner t2_repr in
          error
            (Printf.sprintf "type mismatch: '%s' vs '%s'" t1_str t2_str)
            span)
    !(set.constraints)

let clear_constraints set = set.constraints := []
let get_constraints set = !(set.constraints)

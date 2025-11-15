open Musi_basic
open Musi_parse

type t =
  | TyUnit
  | TyNamed of Interner.name
  | TyTuple of t list
  | TyArray of t
  | TyRecord of (Interner.name * t) list
  | TyFn of t list * t
  | TyOptional of t
  | TyVar of var ref
  | TyError

and var = Unbound of int | Link of t

let var_idx = ref 0

let fresh_var () =
  let id = !var_idx in
  incr var_idx;
  TyVar (ref (Unbound id))

let rec repr = function TyVar { contents = Link t } -> repr t | t -> t

let rec occurs_check id = function
  | TyVar { contents = Unbound id' } -> id = id'
  | TyVar { contents = Link t } -> occurs_check id t
  | TyTuple ts -> List.exists (occurs_check id) ts
  | TyArray t -> occurs_check id t
  | TyRecord fields -> List.exists (fun (_, t) -> occurs_check id t) fields
  | TyFn (params, ret) ->
    List.exists (occurs_check id) params || occurs_check id ret
  | TyOptional t -> occurs_check id t
  | TyNamed _ | TyUnit | TyError -> false

let rec show_with interner = function
  | TyUnit -> "Unit"
  | TyNamed name -> Interner.lookup interner name
  | TyTuple [] -> "()"
  | TyTuple ts ->
    "(" ^ String.concat ", " (List.map (show_with interner) ts) ^ ")"
  | TyArray t -> "[" ^ show_with interner t ^ "]"
  | TyRecord _ -> "{...}"
  | TyFn (params, ret) ->
    "("
    ^ String.concat ", " (List.map (show_with interner) params)
    ^ ") -> " ^ show_with interner ret
  | TyOptional t -> show_with interner t ^ "?"
  | TyVar { contents = Unbound id } -> "?" ^ string_of_int id
  | TyVar { contents = Link t } -> show_with interner t
  | TyError -> "<error>"

let show t = show_with (Interner.create ()) t

let rec unify_record fs1 fs2 =
  if List.length fs1 <> List.length fs2 then
    failwith
      (Printf.sprintf
         "mismatched record fields '%s' and '%s'"
         (show (TyRecord fs1))
         (show (TyRecord fs2)));
  List.iter2
    (fun (n1, t1) (n2, t2) ->
      if n1 <> n2 then
        failwith
          (Printf.sprintf
             "mismatched record fields '%s' and '%s'"
             (show (TyRecord fs1))
             (show (TyRecord fs2)));
      unify t1 t2)
    fs1
    fs2

and unify_fn args1 ret1 args2 ret2 =
  if List.length args1 <> List.length args2 then
    failwith
      (Printf.sprintf
         "expected %d argument(s), found %d"
         (List.length args1)
         (List.length args2));
  List.iter2 unify args1 args2;
  unify ret1 ret2

and unify t1 t2 =
  let t1 = repr t1 in
  let t2 = repr t2 in
  match (t1, t2) with
  | _ when t1 = t2 -> ()
  | TyVar ({ contents = Unbound id1 } as v1), TyVar { contents = Unbound id2 }
    ->
    if id1 <> id2 then v1 := Link t2
  | TyVar ({ contents = Unbound id } as v), t
  | t, TyVar ({ contents = Unbound id } as v) ->
    if occurs_check id t then
      failwith (Printf.sprintf "infinite type '%s'" (show t))
    else v := Link t
  | TyNamed n1, TyNamed n2 when n1 = n2 -> ()
  | TyTuple ts1, TyTuple ts2 when List.length ts1 = List.length ts2 ->
    List.iter2 unify ts1 ts2
  | TyArray t1, TyArray t2 -> unify t1 t2
  | TyRecord fs1, TyRecord fs2 -> unify_record fs1 fs2
  | TyFn (args1, ret1), TyFn (args2, ret2) -> unify_fn args1 ret1 args2 ret2
  | TyOptional t1, TyOptional t2 -> unify t1 t2
  | _ ->
    failwith
      (Printf.sprintf "expected type '%s', found type '%s'" (show t1) (show t2))

let rec from_node lookup (node : Node.ty) =
  match node.tkind with
  | Node.TyNamed name -> (
    match lookup name with Some ty -> ty | None -> TyError)
  | Node.TyTuple [] -> TyUnit
  | Node.TyTuple tys -> TyTuple (List.map (from_node lookup) tys)
  | Node.TyArray ty -> TyArray (from_node lookup ty)
  | Node.TyRecord fields ->
    TyRecord
      (List.map
         (fun (f : Node.field) -> (f.fname, from_node lookup f.fty))
         fields)
  | Node.TyFn (params, ret) ->
    let param_tys = List.map (from_node lookup) params in
    let ret_ty = Option.fold ~none:TyUnit ~some:(from_node lookup) ret in
    TyFn (param_tys, ret_ty)
  | Node.TyOptional ty -> TyOptional (from_node lookup ty)
  | Node.TyApp _ -> TyError (* TODO: generics *)
  | Node.TyError -> TyError

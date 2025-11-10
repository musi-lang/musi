open Musi_basic
open Musi_parse

type t =
  | TyUnit
  | TyBool
  | TyInt
  | TyNat
  | TyText
  | TyRune
  | TyTuple of t list
  | TyArray of t
  | TyRecord of (Interner.name * t) list
  | TyProc of t list * t
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
  | TyProc (params, ret) ->
    List.exists (occurs_check id) params || occurs_check id ret
  | TyOptional t -> occurs_check id t
  | _ -> false

let rec show = function
  | TyUnit -> "Unit"
  | TyBool -> "Bool"
  | TyInt -> "Int"
  | TyNat -> "Nat"
  | TyText -> "Text"
  | TyRune -> "Rune"
  | TyTuple [] -> "()"
  | TyTuple ts -> "(" ^ String.concat ", " (List.map show ts) ^ ")"
  | TyArray t -> "[" ^ show t ^ "]"
  | TyRecord _ -> "{...}"
  | TyProc (params, ret) ->
    "(" ^ String.concat ", " (List.map show params) ^ ") -> " ^ show ret
  | TyOptional t -> show t ^ "?"
  | TyVar { contents = Unbound id } -> "?" ^ string_of_int id
  | TyVar { contents = Link t } -> show t
  | TyError -> "<error>"

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

and unify_proc args1 ret1 args2 ret2 =
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
  | TyTuple ts1, TyTuple ts2 when List.length ts1 = List.length ts2 ->
    List.iter2 unify ts1 ts2
  | TyArray t1, TyArray t2 -> unify t1 t2
  | TyRecord fs1, TyRecord fs2 -> unify_record fs1 fs2
  | TyProc (args1, ret1), TyProc (args2, ret2) ->
    unify_proc args1 ret1 args2 ret2
  | TyOptional t1, TyOptional t2 -> unify t1 t2
  | _ ->
    failwith
      (Printf.sprintf "expected type '%s', found type '%s'" (show t1) (show t2))

let rec from_node (node : Node.ty) =
  match node.tkind with
  | Node.TyNamed _ -> TyError (* TODO: resolve named types *)
  | Node.TyTuple [] -> TyUnit
  | Node.TyTuple tys -> TyTuple (List.map from_node tys)
  | Node.TyArray ty -> TyArray (from_node ty)
  | Node.TyRecord fields ->
    TyRecord
      (List.map (fun (f : Node.field) -> (f.fname, from_node f.fty)) fields)
  | Node.TyProc (params, ret) ->
    let param_tys = List.map from_node params in
    let ret_ty = Option.fold ~none:TyUnit ~some:from_node ret in
    TyProc (param_tys, ret_ty)
  | Node.TyOptional ty -> TyOptional (from_node ty)
  | Node.TyApp _ -> TyError (* TODO: generics *)
  | Node.TyError -> TyError

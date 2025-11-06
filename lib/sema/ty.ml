(* ========================================
   TYPE REPRESENTATION
   ======================================== *)

type ty_var = Unbound of int * int | Link of ty
and ty = TyUnit | TyBool | TyInt | TyNat | TyVar of ty_var ref

(* ========================================
   TYPE HELPERS
   ======================================== *)

let rec repr = function TyVar { contents = Link ty } -> repr ty | ty -> ty

let rec occurs_check id = function
  | TyVar { contents = Unbound (id', _) } -> id = id'
  | TyVar { contents = Link ty } -> occurs_check id ty
  | TyUnit | TyBool | TyInt | TyNat -> false

(* ========================================
   TYPE CONVERSION
   ======================================== *)

let rec from_node_ty interner node_ty =
  match node_ty.Node.ty_kind with
  | Node.TyNamed name -> (
    let name_str = Interner.lookup interner name in
    match name_str with
    | "Unit" -> TyUnit
    | "Bool" -> TyBool
    | "Int" -> TyInt
    | "Nat" -> TyNat
    | _ -> TyUnit)
  | Node.TyTuple _ -> TyUnit
  | Node.TyArray _ -> TyUnit
  | Node.TySizedArray _ -> TyUnit
  | Node.TyProc _ -> TyUnit
  | Node.TyGeneric _ -> TyUnit
  | Node.TyOptional _ -> TyUnit
  | Node.TyFallible _ -> TyUnit

let rec show = function
  | TyUnit -> "Unit"
  | TyBool -> "Bool"
  | TyInt -> "Int"
  | TyNat -> "Nat"
  | TyVar { contents = Link ty } -> show ty
  | TyVar _ -> "<type>"

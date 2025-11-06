(* ========================================
   SYMBOL TYPES
   ======================================== *)

type symbol_kind =
  | SymVar of { mutable_ : bool }
  | SymProc of { param_count : int }

type symbol = {
    name : Interner.name
  ; kind : symbol_kind
  ; span : Span.t
  ; mutable ty : Ty.ty option
  ; mutable used : bool
}

type scope = (Interner.name, symbol) Hashtbl.t
type t = { scopes : scope Stack.t }

(* ========================================
   SCOPE MANAGEMENT
   ======================================== *)

let create () = { scopes = Stack.create () }
let push_scope t = Stack.push (Hashtbl.create 16) t.scopes
let pop_scope t = ignore (Stack.pop t.scopes)

(* ========================================
   SYMBOL OPERATIONS
   ======================================== *)

let add t sym = Hashtbl.add (Stack.top t.scopes) sym.name sym

let lookup t name =
  let rec search scopes =
    match Stack.pop scopes with
    | exception Stack.Empty -> None
    | scope -> (
      match Hashtbl.find_opt scope name with
      | Some sym ->
        sym.used <- true;
        Some sym
      | None -> search scopes)
  in
  let temp = Stack.copy t.scopes in
  search temp

let current_scope_symbols t =
  match Stack.top t.scopes with
  | exception Stack.Empty -> []
  | scope -> Hashtbl.fold (fun _ sym acc -> sym :: acc) scope []

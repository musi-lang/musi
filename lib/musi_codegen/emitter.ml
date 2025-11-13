open Musi_basic
open Musi_parse
open Musi_lex

type const_kind =
  | ConstInt of int64
  | ConstBin of float
  | ConstStr of Interner.name
  | ConstBool of bool
  | ConstUnit

type loop_ctx = { break_lbl : int; cont_lbl : int }
type instr_or_label = IInstr of Instr.t | ILabel of int

let instr_size interner = function
  | Instr.Call name | Instr.CallVirt name ->
    1 + String.length (Interner.lookup interner name) + 1
  | Instr.Br _ | Instr.BrTrue _ | Instr.BrFalse _ | Instr.LdCI4 _
  | Instr.LdCI8 _ | Instr.LdCN8 _ | Instr.LdCN16 _ | Instr.LdCN32 _
  | Instr.LdCN64 _ | Instr.LdCStr _ | Instr.LdLoc _ | Instr.StLoc _
  | Instr.LdArg _ | Instr.StArg _ | Instr.NewObj _ | Instr.LdFld _
  | Instr.StFld _ | Instr.IsInst _ | Instr.CastClass _ | Instr.Box _
  | Instr.UnboxAny _ ->
    5
  | Instr.LdCB32 _ | Instr.LdCB64 _ | Instr.LdCD32 _ | Instr.LdCD64 _ -> 5
  | _ -> 1

let resolve_labels interner code =
  let rec build_map pos acc = function
    | [] -> acc
    | ILabel lbl :: rest -> build_map pos ((lbl, pos) :: acc) rest
    | IInstr instr :: rest ->
      build_map (pos + instr_size interner instr) acc rest
  in
  let label_map = build_map 0 [] code in
  let resolve_offset lbl =
    match List.assoc_opt lbl label_map with Some offset -> offset | None -> 0
  in
  List.filter_map
    (function
      | ILabel _ -> None
      | IInstr (Instr.Br lbl) -> Some (Instr.Br (resolve_offset lbl))
      | IInstr (Instr.BrTrue lbl) -> Some (Instr.BrTrue (resolve_offset lbl))
      | IInstr (Instr.BrFalse lbl) -> Some (Instr.BrFalse (resolve_offset lbl))
      | IInstr instr -> Some instr)
    code

type proc_info = {
    proc_name : Interner.name
  ; param_count : int
  ; local_count : int
  ; code_offset : int
  ; is_extern : bool
  ; abi : Interner.name option
}

type t = {
    mutable const_pool : const_kind list
  ; mutable locals : (Interner.name * int) list
  ; mutable next_local : int
  ; mutable loop_stack : loop_ctx list
  ; mutable next_label : int
  ; mutable procs : proc_info list
  ; mutable next_proc_id : int
  ; mutable proc_map : (Interner.name * int) list
  ; mutable code : Instr.t list
  ; interner : Interner.t
}

let make () =
  let interner = Interner.create () in
  {
    const_pool = []
  ; locals = []
  ; next_local = 0
  ; loop_stack = []
  ; next_label = 0
  ; procs = []
  ; next_proc_id = 0
  ; proc_map = []
  ; code = []
  ; interner
  }

let fresh_label t =
  let lbl = t.next_label in
  t.next_label <- t.next_label + 1;
  lbl

let push_loop t break_lbl cont_lbl =
  t.loop_stack <- { break_lbl; cont_lbl } :: t.loop_stack

let pop_loop t =
  match t.loop_stack with _ :: rest -> t.loop_stack <- rest | [] -> ()

let curr_loop t = match t.loop_stack with ctx :: _ -> Some ctx | [] -> None
let find_local t name = List.assoc_opt name t.locals

let add_local t name =
  match find_local t name with
  | Some slot -> slot
  | None ->
    let slot = t.next_local in
    t.locals <- (name, slot) :: t.locals;
    t.next_local <- t.next_local + 1;
    slot

let add_const t c =
  match List.find_index (fun x -> x = c) t.const_pool with
  | Some idx -> idx
  | None ->
    let idx = List.length t.const_pool in
    t.const_pool <- t.const_pool @ [ c ];
    idx

let emit_expr_literal t = function
  | Node.LitInt s ->
    let n = Int64.of_string s in
    if n >= -32768L && n <= 32767L then
      [ Instr.LdCI4 (Int32.of_int (Int64.to_int n)) ]
    else
      let idx = add_const t (ConstInt n) in
      [ Instr.LdCN64 idx ]
  | Node.LitBin s ->
    let f = Float.of_string s in
    ignore (add_const t (ConstBin f));
    (* store in const pool for possible future use *)
    [ Instr.LdCB64 f ]
  | Node.LitStr name ->
    let idx = add_const t (ConstStr name) in
    [ Instr.LdCStr idx ]
  | Node.LitRune code ->
    let n = Int64.of_int code in
    if n >= 0L && n <= 255L then [ Instr.LdCI4 (Int32.of_int (Int64.to_int n)) ]
    else
      let idx = add_const t (ConstInt n) in
      [ Instr.LdCN64 idx ]
  | Node.LitBool b -> [ Instr.LdCI4 (if b then 1l else 0l) ]
  | Node.LitRecord _ -> []

let emit_expr_binary = function
  | Token.Plus -> [ Instr.Add ]
  | Token.Minus -> [ Instr.Sub ]
  | Token.Star -> [ Instr.Mul ]
  | Token.Slash -> [ Instr.Div ]
  | Token.KwMod -> [ Instr.RemUn ]
  | Token.KwAnd -> [ Instr.And ]
  | Token.KwOr -> [ Instr.Or ]
  | Token.KwXor -> [ Instr.Xor ]
  | Token.KwShl -> [ Instr.Shl ]
  | Token.KwShr -> [ Instr.Shr ]
  | Token.Eq -> [ Instr.Ceq ]
  | Token.EqSlashEq -> [ Instr.Ceq; Instr.Not ] (* not equal : equal + not *)
  | Token.Lt -> [ Instr.Clt ]
  | Token.Gt -> [ Instr.Cgt ]
  | Token.LtEq -> [ Instr.Cgt; Instr.Not ] (* <= : not + greater *)
  | Token.GtEq -> [ Instr.Clt; Instr.Not ] (* >= : not + less *)
  | _ -> []

let emit_expr_unary = function
  | Token.Minus -> [ Instr.Neg ]
  | Token.KwNot -> [ Instr.Not ]
  | _ -> []

let rec emit_expr t expr =
  match expr.Node.ekind with
  | Node.ExprLiteral lit -> emit_expr_literal t lit
  | Node.ExprBinary (op, left, right) ->
    emit_expr t left @ emit_expr t right @ emit_expr_binary op
  | Node.ExprUnary (op, operand) -> emit_expr t operand @ emit_expr_unary op
  | Node.ExprIdent name ->
    let slot = find_local t name |> Option.value ~default:0 in
    [ Instr.LdLoc slot ]
  | Node.ExprBlock exprs -> List.concat_map (emit_expr t) exprs
  | Node.ExprAssign (target, value) -> emit_expr_assign t target value
  | Node.ExprReturn None -> [ Instr.Ret ]
  | Node.ExprReturn (Some e) -> emit_expr t e @ [ Instr.Ret ]
  | Node.ExprBreak _ -> (
    match curr_loop t with Some ctx -> [ Instr.Br ctx.break_lbl ] | None -> [])
  | Node.ExprContinue -> (
    match curr_loop t with Some ctx -> [ Instr.Br ctx.cont_lbl ] | None -> [])
  | Node.ExprIf (cond, then_br, else_br) -> emit_expr_if t cond then_br else_br
  | Node.ExprWhile (cond, body) -> emit_expr_while t cond body
  | Node.ExprDo (body, cond_opt) -> emit_expr_do t body cond_opt
  | Node.ExprFor (pat, iter, body) -> emit_expr_for t pat iter body
  | Node.ExprCall (callee, args, _) -> emit_expr_call t callee args
  | Node.ExprBinding (_, _, pat, _, value, _) -> emit_expr_binding t pat value
  | Node.ExprTuple exprs | Node.ExprArray exprs ->
    List.concat_map (emit_expr t) exprs
  | Node.ExprRecord (_, fields, _) -> emit_expr_record t fields
  | Node.ExprField (obj, field, _) -> emit_expr_field t obj field
  | Node.ExprCast (e, _) | Node.ExprTest (e, _) -> emit_expr t e
  | Node.ExprUnwrap e
  | Node.ExprTry e
  | Node.ExprDefer e
  | Node.ExprAsync e
  | Node.ExprUnsafe e
  | Node.ExprAwait e ->
    emit_expr t e
  | Node.ExprYield (Some e) -> emit_expr t e
  | Node.ExprProc (_, _, params, _, body, mods) ->
    emit_expr_proc t params body mods
  | Node.ExprYield None | _ -> []

and emit_expr_assign t target value =
  let value_code = emit_expr t value in
  match target.Node.ekind with
  | Node.ExprIdent name ->
    let slot = add_local t name in
    value_code @ [ Instr.StLoc slot ]
  | _ -> value_code

and emit_expr_while t cond body =
  let loop_lbl = fresh_label t in
  let cond_lbl = fresh_label t in
  let end_lbl = fresh_label t in
  push_loop t end_lbl cond_lbl;
  let code =
    [ IInstr (Instr.Br cond_lbl) ]
    @ [ ILabel loop_lbl ]
    @ List.map (fun i -> IInstr i) (emit_expr t body)
    @ [ ILabel cond_lbl ]
    @ List.map (fun i -> IInstr i) (emit_expr t cond)
    @ [ IInstr (Instr.BrTrue loop_lbl) ]
    @ [ ILabel end_lbl ]
  in
  pop_loop t;
  resolve_labels (Interner.create ()) code

and emit_expr_do t body cond_opt =
  let span = body.Node.span in
  match cond_opt with
  | None ->
    (* `do { body }` -> `while true { body }` *)
    let true_expr = Node.make_expr_literal (Node.LitBool true) span in
    emit_expr_while t true_expr body
  | Some cond ->
    (* `do { body } while cond` -> `body; while cond { body }` *)
    emit_expr t body @ emit_expr_while t cond body

and emit_expr_if t cond then_br else_br =
  let else_lbl = fresh_label t in
  let end_lbl = fresh_label t in
  let cond_code = emit_expr t cond in
  let then_code = emit_expr t then_br in
  match else_br with
  | None ->
    let code =
      List.map (fun i -> IInstr i) cond_code
      @ [ IInstr (Instr.BrFalse else_lbl) ]
      @ List.map (fun i -> IInstr i) then_code
      @ [ ILabel else_lbl ]
    in
    resolve_labels (Interner.create ()) code
  | Some else_expr ->
    let else_code = emit_expr t else_expr in
    let code =
      List.map (fun i -> IInstr i) cond_code
      @ [ IInstr (Instr.BrFalse else_lbl) ]
      @ List.map (fun i -> IInstr i) then_code
      @ [ IInstr (Instr.Br end_lbl) ]
      @ [ ILabel else_lbl ]
      @ List.map (fun i -> IInstr i) else_code
      @ [ ILabel end_lbl ]
    in
    resolve_labels (Interner.create ()) code

and emit_expr_call t callee args =
  let args_code = List.concat_map (emit_expr t) args in
  match callee.Node.ekind with
  | Node.ExprIdent name -> args_code @ [ Instr.Call name ]
  | _ ->
    let callee_code = emit_expr t callee in
    let dummy_name = Interner.intern (Interner.create ()) "" in
    args_code @ callee_code @ [ Instr.Call dummy_name ]

and emit_expr_binding t pat value =
  match value.Node.ekind with
  | Node.ExprProc (_, _, params, _, body, mods) ->
    let proc_name =
      match pat.Node.pkind with
      | Node.PatIdent name -> name
      | _ -> Interner.intern (Interner.create ()) "<lambda>"
    in
    emit_expr_proc_named t proc_name params body mods
  | _ -> (
    let value_code = emit_expr t value in
    match pat.Node.pkind with
    | Node.PatIdent name ->
      let slot = add_local t name in
      value_code @ [ Instr.StLoc slot ]
    | _ -> value_code)

and emit_expr_for t pat iter body =
  let span = iter.Node.span in
  match iter.Node.ekind with
  | Node.ExprRange (start, end_, inclusive) ->
    (*  `for i in start..end do { body }` ->
        `i := start; while i < end do { body; i <- i + 1 }` *)
    let iter_name =
      match pat.Node.pkind with
      | Node.PatIdent name -> name
      | _ -> Interner.intern (Interner.create ()) "_i"
    in
    let iter_slot = add_local t iter_name in
    let cmp_op = if inclusive then Token.LtEq else Token.Lt in
    let init_code = emit_expr t start @ [ Instr.StLoc iter_slot ] in
    let cond_expr =
      Node.make_expr_binary
        cmp_op
        (Node.make_expr_ident iter_name span)
        end_
        span
    in
    let inc_expr =
      Node.make_expr
        (Node.ExprAssign
           ( Node.make_expr_ident iter_name span
           , Node.make_expr_binary
               Token.Plus
               (Node.make_expr_ident iter_name span)
               (Node.make_expr_literal (Node.LitInt "1") span)
               span ))
        span
    in
    let body_with_inc =
      Node.make_expr (Node.ExprBlock [ body; inc_expr ]) span
    in
    init_code @ emit_expr_while t cond_expr body_with_inc
  | _ -> []

and emit_expr_record _t fields =
  let field_count = List.length fields in
  [ Instr.NewObj field_count ]

and emit_expr_field t obj _field =
  let obj_code = emit_expr t obj in
  let field_idx = 0 in
  obj_code @ [ Instr.LdFld field_idx ]

and emit_expr_proc t params body mods =
  emit_expr_proc_named
    t
    (Interner.intern (Interner.create ()) "<lambda>")
    params
    body
    mods

and emit_expr_proc_named t proc_name params body mods =
  let proc_id = t.next_proc_id in
  t.next_proc_id <- t.next_proc_id + 1;
  let is_extern = Option.is_some mods.Node.abi in
  let saved_locals = t.locals in
  let saved_next_local = t.next_local in
  t.locals <- [];
  t.next_local <- 0;
  List.iter (fun p -> ignore (add_local t p.Node.pname)) params;
  let body_code =
    if is_extern then []
    else
      match body with
      | Some e -> emit_expr t e @ [ Instr.Ret ]
      | None -> [ Instr.Ret ]
  in
  let code_offset =
    List.fold_left (fun acc i -> acc + instr_size t.interner i) 0 t.code
  in
  let proc =
    {
      proc_name
    ; param_count = List.length params
    ; local_count = t.next_local
    ; code_offset
    ; is_extern
    ; abi = mods.Node.abi
    }
  in
  t.procs <- t.procs @ [ proc ];
  t.proc_map <- (proc_name, proc_id) :: t.proc_map;
  t.code <- t.code @ body_code;
  t.locals <- saved_locals;
  t.next_local <- saved_next_local;
  []

let emit_stmt t stmt =
  match stmt.Node.skind with
  | Node.StmtExpr (expr, _) -> emit_expr t expr
  | Node.StmtImport _ | Node.StmtExport _ -> []
  | _ -> []

let emit t stmts =
  let top_level = List.concat_map (emit_stmt t) stmts in
  t.code @ top_level

let const_pool t = t.const_pool
let procs t = t.procs

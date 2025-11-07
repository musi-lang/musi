(* ========================================
   TYPES
   ======================================== *)

type proc_info = { id : int; param_count : int; local_count : int }
type proc_table = (Interner.name, proc_info) Hashtbl.t
type label = int

type emitter_state = {
    interner : Interner.t
  ; proc_table : proc_table
  ; diags : Diagnostic.bag ref
  ; mutable const_pool : (string, int) Hashtbl.t
  ; mutable const_list : Metadata.constant list
  ; mutable locals : (Interner.name, int) Hashtbl.t
  ; mutable param_count : int
  ; mutable local_count : int
  ; mutable instrs : Instr.t list
  ; mutable max_stack : int
  ; mutable stack_depth : int
  ; mutable label_counter : int
}

(* ========================================
   STATE HELPERS
   ======================================== *)

let pat_name pat =
  match pat.Node.kind with
  | Node.PatBinding name | Node.ExprIdent name -> Some name
  | _ -> None

let make_state interner proc_table diags =
  {
    interner
  ; proc_table
  ; diags
  ; const_pool = Hashtbl.create 16
  ; const_list = []
  ; locals = Hashtbl.create 16
  ; param_count = 0
  ; local_count = 0
  ; instrs = []
  ; max_stack = 0
  ; stack_depth = 0
  ; label_counter = 0
  }

let error st msg span =
  st.diags := Diagnostic.add !(st.diags) (Diagnostic.error msg span)

let emit st instr =
  st.instrs <- instr :: st.instrs;
  match instr with
  | Instr.LdcI4 _ | Instr.LdcI4M1 | Instr.LdcUnit | Instr.LdcStr _
  | Instr.LdLoc _ | Instr.LdArg _ | Instr.Dup ->
    st.stack_depth <- st.stack_depth + 1;
    if st.stack_depth > st.max_stack then st.max_stack <- st.stack_depth
  | Instr.Pop | Instr.StLoc _ | Instr.Ret ->
    st.stack_depth <- st.stack_depth - 1
  | Instr.Add | Instr.Sub | Instr.Mul | Instr.Div | Instr.CmpEq | Instr.CmpNe
  | Instr.CmpLt | Instr.CmpGt | Instr.CmpLe | Instr.CmpGe ->
    st.stack_depth <- st.stack_depth - 1
  | Instr.BrFalse _ | Instr.BrTrue _ -> st.stack_depth <- st.stack_depth - 1
  | _ -> ()

let alloc_local st name =
  let idx = st.local_count in
  Hashtbl.add st.locals name idx;
  st.local_count <- st.local_count + 1;
  idx

let lookup_local st name = Hashtbl.find_opt st.locals name

let fresh_label st =
  let lbl = st.label_counter in
  st.label_counter <- st.label_counter + 1;
  lbl

let emits st instrs = List.iter (emit st) instrs

let add_const st str =
  match Hashtbl.find_opt st.const_pool str with
  | Some idx -> idx
  | None ->
    let idx = List.length st.const_list in
    Hashtbl.add st.const_pool str idx;
    st.const_list <- st.const_list @ [ Metadata.ConstText str ];
    idx

(* ========================================
   PASS 1: COLLECT PROCEDURES
   ======================================== *)

let get_link_key interner (modifiers : Node.modifiers) =
  let link_dec =
    List.find_opt
      (fun (dec : Node.decorator) -> Interner.lookup interner dec.name = "link")
      modifiers.decorators
  in
  match link_dec with
  | Some dec -> (
    match dec.args with
    | [ { Node.kind = Node.ExprLitText s; _ } ] ->
      Some (Interner.lookup interner s)
    | _ -> None)
  | None -> snd modifiers.is_extern

let collect_proc_node interner table next_id node =
  match node.Node.kind with
  | Node.StmtExpr e -> (
    match e.kind with
    | Node.ExprBinding { pat; init; _ } -> (
      match (pat_name pat, init.kind) with
      | Some name, Node.ExprProc { params; modifiers; _ } ->
        let param_count = List.length params in
        let info = { id = next_id; param_count; local_count = 0 } in
        Hashtbl.add table name info;
        let _ = get_link_key interner modifiers in
        (next_id + 1, true)
      | _ -> (next_id, false))
    | _ -> (next_id, false))
  | _ -> (next_id, false)

let collect_procs interner ast =
  let table = Hashtbl.create 16 in
  let main_info = { id = 0; param_count = 0; local_count = 0 } in
  let main_name = Interner.intern interner "<main>" in
  Hashtbl.add table main_name main_info;
  let rec walk next_id nodes =
    match nodes with
    | [] -> next_id
    | node :: rest ->
      let next_id', _ = collect_proc_node interner table next_id node in
      walk next_id' rest
  in
  let _ = walk 1 ast in
  table

(* ========================================
   PASS 2: EMIT BYTECODE
   ======================================== *)

let rec emit_expr st node =
  match node.Node.kind with
  | Node.ExprLitNumeric (s, _) -> emit_expr_lit_numeric st s
  | Node.ExprLitBool b -> emit st (Instr.LdcI4 (if b then 1l else 0l))
  | Node.ExprLitText name ->
    let str = Interner.lookup st.interner name in
    let idx = add_const st str in
    emit st (Instr.LdcStr idx)
  | Node.ExprIdent name -> emit_expr_ident st name node.span
  | Node.ExprBinary { op; left; right } -> emit_expr_binary st op left right
  | Node.ExprUnary { op; operand } -> emit_expr_unary st op operand
  | Node.ExprAssign { target; value } -> emit_expr_assign st target value
  | Node.ExprCall { callee; args } -> emit_expr_call st callee args
  | Node.ExprBlock { stmts; expr } -> emit_expr_block st stmts expr
  | Node.ExprIf { pat; then_branch; else_branch } ->
    emit_expr_if st pat then_branch else_branch
  | Node.ExprWhile { pat; body } -> emit_expr_while st pat body
  | Node.ExprReturn e -> emit_expr_return st e
  | _ -> emit st Instr.LdcUnit

and emit_expr_lit_numeric st s =
  let n = Int32.of_string s in
  if n = -1l then emit st Instr.LdcI4M1 else emit st (Instr.LdcI4 n)

and emit_expr_ident st name span =
  match lookup_local st name with
  | Some idx ->
    if idx < st.param_count then emit st (Instr.LdArg idx)
    else emit st (Instr.LdLoc (idx - st.param_count))
  | None ->
    let name_str = Interner.lookup st.interner name in
    error st (Printf.sprintf "undefined binding '%s'" name_str) span

and emit_expr_binary st op left right =
  emit_expr st left;
  emit_expr st right;
  match op with
  | Token.Plus -> emit st Instr.Add
  | Token.Minus -> emit st Instr.Sub
  | Token.Star -> emit st Instr.Mul
  | Token.Slash -> emit st Instr.Div
  | Token.Eq -> emit st Instr.CmpEq
  | Token.EqSlashEq -> emit st Instr.CmpNe
  | Token.Lt -> emit st Instr.CmpLt
  | Token.Gt -> emit st Instr.CmpGt
  | Token.LtEq -> emit st Instr.CmpLe
  | Token.GtEq -> emit st Instr.CmpGe
  | _ -> ()

and emit_expr_unary st op operand =
  emit_expr st operand;
  if op = Token.Minus then emit st Instr.Neg

and emit_expr_assign st target value =
  emit_expr st value;
  (match target.Node.kind with
  | Node.ExprIdent name -> (
    match lookup_local st name with
    | Some idx -> emit st (Instr.StLoc idx)
    | None ->
      let name_str = Interner.lookup st.interner name in
      error st (Printf.sprintf "undefined binding '%s'" name_str) target.span)
  | _ -> error st "invalid assignment target" target.span);
  emit st Instr.LdcUnit

and emit_expr_call st callee args =
  List.iter (emit_expr st) args.Node.items;
  match callee.Node.kind with
  | Node.ExprIdent name -> (
    match Hashtbl.find_opt st.proc_table name with
    | Some info -> emit st (Instr.Call info.id)
    | None ->
      let name_str = Interner.lookup st.interner name in
      error st (Printf.sprintf "undefined procedure '%s'" name_str) callee.span)
  | _ -> error st "invalid callee expression" callee.span

and emit_expr_block st stmts expr =
  List.iter (emit_stmt st) stmts;
  match expr with Some e -> emit_expr st e | None -> emit st Instr.LdcUnit

and emit_expr_return st e =
  (match e with
  | None -> emit st Instr.LdcUnit
  | Some expr -> emit_expr st expr);
  emit st Instr.Ret

and emit_expr_if st pat then_branch else_branch =
  emit_expr st pat;
  let else_lbl = fresh_label st in
  let end_lbl = fresh_label st in
  emit st (Instr.BrFalse else_lbl);
  emit_expr st then_branch;
  emits st [ Instr.Br end_lbl; Instr.Br else_lbl ];
  (match else_branch with
  | Some e -> emit_expr st e
  | None -> emit st Instr.LdcUnit);
  emit st (Instr.Br end_lbl)

and emit_expr_while st pat body =
  let start_lbl = fresh_label st in
  let end_lbl = fresh_label st in
  emit st (Instr.Br start_lbl);
  emit_expr st pat;
  emit st (Instr.BrFalse end_lbl);
  emit_expr st body;
  emits st [ Instr.Pop; Instr.Br start_lbl; Instr.Br end_lbl; Instr.LdcUnit ]

and emit_stmt st node =
  match node.Node.kind with
  | Node.StmtExpr e -> (
    match e.kind with
    | Node.ExprBinding { pat; init; _ } -> (
      match pat_name pat with
      | Some name -> (
        match init.kind with
        | Node.ExprProc _ -> ()
        | _ ->
          emit_expr st init;
          let idx = alloc_local st name in
          emit st (Instr.StLoc idx))
      | None -> ())
    | _ ->
      emit_expr st e;
      emit st Instr.Pop)
  | _ -> ()

let emit_expr_proc st params body =
  let param_count = List.length params in
  List.iteri
    (fun i (param : Node.param) -> Hashtbl.add st.locals param.name i)
    params;
  st.param_count <- param_count;
  st.local_count <- param_count;
  (match body with
  | Some b ->
    emit_expr st b;
    emit st Instr.Ret
  | None -> emits st [ Instr.LdcUnit; Instr.Ret ]);
  List.rev st.instrs

let emit_main_proc st ast =
  List.iter (emit_stmt st) ast;
  emits st [ Instr.LdcUnit; Instr.Ret ];
  List.rev st.instrs

let extract_export interner node =
  match node.Node.kind with
  | Node.StmtExpr
      { Node.kind = Node.ExprBinding { modifiers; pat; init; _ }; _ }
    when modifiers.Node.is_exported -> (
    match (pat_name pat, init.Node.kind) with
    | Some name, Node.ExprProc _ -> Some (Interner.lookup interner name)
    | _ -> None)
  | _ -> None

let extract_link_key interner proc_table node =
  match node.Node.kind with
  | Node.StmtExpr { Node.kind = Node.ExprBinding { pat; init; _ }; _ } -> (
    match (pat_name pat, init.Node.kind) with
    | Some name, Node.ExprProc { modifiers; _ } -> (
      match get_link_key interner modifiers with
      | Some link_key -> (
        match Hashtbl.find_opt proc_table name with
        | Some info -> Some (info.id, link_key)
        | None -> None)
      | None -> None)
    | _ -> None)
  | _ -> None

let collect_exports interner ast = List.filter_map (extract_export interner) ast

let collect_link_keys interner proc_table ast =
  List.filter_map (extract_link_key interner proc_table) ast

let emit_program interner proc_table diags ast =
  let procs = Array.make (Hashtbl.length proc_table) [] in
  let global_const_pool = Hashtbl.create 16 in
  let global_const_list = ref [] in
  let make_state_with_pool () =
    let st = make_state interner proc_table diags in
    st.const_pool <- global_const_pool;
    st.const_list <- !global_const_list;
    st
  in
  let main_st = make_state_with_pool () in
  procs.(0) <- emit_main_proc main_st ast;
  global_const_list := main_st.const_list;
  List.iter
    (fun node ->
      match node.Node.kind with
      | Node.StmtExpr e -> (
        match e.kind with
        | Node.ExprBinding { pat; init; _ } -> (
          match (pat_name pat, init.kind) with
          | Some name, Node.ExprProc { params; body; _ } -> (
            match Hashtbl.find_opt proc_table name with
            | Some info ->
              let proc_st = make_state_with_pool () in
              procs.(info.id) <- emit_expr_proc proc_st params body;
              global_const_list := proc_st.const_list
            | None -> ())
          | _ -> ())
        | _ -> ())
      | _ -> ())
    ast;
  let exports = collect_exports interner ast in
  let export_table =
    List.filter_map
      (fun name ->
        let name_interned = Interner.intern interner name in
        match Hashtbl.find_opt proc_table name_interned with
        | Some info -> Some (name, info.id)
        | None -> None)
      exports
  in
  let link_keys = collect_link_keys interner proc_table ast in
  let module_desc =
    {
      Metadata.module_name = None
    ; exports = export_table
    ; link_keys
    ; const_pool = !global_const_list
    }
  in
  (procs, module_desc)

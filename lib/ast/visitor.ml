open Basic
module Token = Lex.Token

type context = { mutable diags : Diagnostic.bag }

let mk_context () = { diags = Diagnostic.empty_bag }

let add_error ctx message span =
  ctx.diags <- Diagnostic.add ctx.diags (Diagnostic.error message span)

let add_warning ctx message span =
  ctx.diags <- Diagnostic.add ctx.diags (Diagnostic.warning message span)

type 'a result = ('a, Diagnostic.bag) Result.t

let ret_ok x = Ok x
let ret_err bag = Error bag
let bind_result = Result.bind

let map_over_list f list =
  List.fold_left
    (fun acc item -> bind_result acc (fun _ -> f item))
    (ret_ok ())
    list

let map_opt opt f =
  match opt with
  | Some x -> bind_result (ret_ok ()) (fun _ -> f x)
  | None -> ret_ok ()

let rec visit_expr ctx expr_node =
  let data = Node.Expr.data expr_node in
  match data with
  | Lit _ -> ret_ok ()
  | Ident _ -> ret_ok ()
  | Tmpl _ -> ret_ok ()
  | Tuple exprs -> map_over_list (visit_expr ctx) exprs
  | Block block -> map_over_list (visit_stmt ctx) block.stmts
  | Range { start; end_; _ } ->
    bind_result
      (map_opt start (visit_expr ctx))
      (fun _ -> map_opt end_ (visit_expr ctx))
  | If { conds; then_block; else_block } ->
    let acc =
      map_over_list
        (fun (pat, expr) ->
          bind_result (visit_expr ctx expr) (fun _ -> visit_pat ctx pat))
        conds
    in
    let acc' =
      bind_result acc (fun _ -> map_over_list (visit_stmt ctx) then_block.stmts)
    in
    bind_result acc' (fun _ ->
      map_opt else_block (fun block ->
        map_over_list (visit_stmt ctx) block.stmts))
  | Match { scrutinee; _ } -> visit_expr ctx scrutinee
  | For { binding; range; guard; body } ->
    let acc = visit_expr ctx range in
    let acc' =
      bind_result acc (fun _ ->
        match binding with
        | ForPat pat -> visit_pat ctx pat
        | ForIdent _ -> ret_ok ())
    in
    let acc'' = bind_result acc' (fun _ -> map_opt guard (visit_expr ctx)) in
    bind_result acc'' (fun _ -> map_over_list (visit_stmt ctx) body.stmts)
  | While { cond; guard; body } ->
    let acc =
      match cond with
      | Some (CaseBinding { pat; expr }) ->
        bind_result (visit_expr ctx expr) (fun _ -> visit_pat ctx pat)
      | Some (Expr expr) -> visit_expr ctx expr
      | None -> ret_ok ()
    in
    let acc' = bind_result acc (fun _ -> map_opt guard (visit_expr ctx)) in
    bind_result acc' (fun _ -> map_over_list (visit_stmt ctx) body.stmts)
  | Defer expr -> visit_expr ctx expr
  | Break mbe -> (
    match mbe with Some expr -> visit_expr ctx expr | None -> ret_ok ())
  | Cycle -> ret_ok ()
  | Unsafe block -> map_over_list (visit_stmt ctx) block.stmts
  | Assign { value; _ } -> visit_expr ctx value
  | Unary { arg; _ } -> visit_expr ctx arg
  | Call { callee; typ_args; args; _ } ->
    bind_result (visit_expr ctx callee) (fun _ ->
      bind_result
        (map_opt typ_args (map_over_list (visit_typ ctx)))
        (fun _ -> map_over_list (visit_expr ctx) args))
  | Member { obj; _ } -> visit_expr ctx obj
  | RecordLit _ -> ret_ok ()
  | Fn _ -> ret_ok ()
  | Record _ -> ret_ok ()
  | Choice _ -> ret_ok ()

and visit_stmt ctx stmt_node =
  let data = Node.Stmt.data stmt_node in
  match data with
  | Import _ -> ret_ok ()
  | Export _ -> ret_ok ()
  | Bind { binding; value; _ } ->
    bind_result
      (map_opt binding.typ_annot (visit_typ ctx))
      (fun _ -> visit_expr ctx value)
  | Extern _ -> ret_ok ()
  | Expr expr -> visit_expr ctx expr

and visit_pat ctx pat_node =
  let data = Node.Pat.data pat_node in
  match data with
  | Bind binding -> map_opt binding.typ_annot (visit_typ ctx)
  | Lit _ -> ret_ok ()
  | Wild -> ret_ok ()
  | Ident _ -> ret_ok ()
  | Record _ -> ret_ok ()
  | Ctor { args; _ } -> map_over_list (visit_pat ctx) args
  | Tuple pats -> map_over_list (visit_pat ctx) pats

and visit_typ ctx typ_node =
  let data = Node.Typ.data typ_node in
  match data with
  | Ptr typ -> bind_result (ret_ok ()) (fun _ -> visit_typ ctx typ)
  | Array { size; elem } ->
    bind_result (map_opt size (visit_expr ctx)) (fun _ -> visit_typ ctx elem)
  | Ident _ -> ret_ok ()
  | App { args; _ } -> map_over_list (visit_typ ctx) args
  | Tuple typs -> map_over_list (visit_typ ctx) typs
  | Fn { params; ret } ->
    bind_result
      (map_over_list (visit_typ ctx) params)
      (fun _ -> map_opt ret (visit_typ ctx))
  | Record _ -> ret_ok ()
  | Optional typ -> visit_typ ctx typ

let visit_prog ctx prog = map_over_list (visit_stmt ctx) prog

let count_nodes prog =
  let ctx = mk_context () in
  let counter = ref 0 in
  let count_expr expr_node =
    incr counter;
    match visit_expr ctx expr_node with Ok () -> () | Error _ -> ()
  in
  let count_stmt stmt_node =
    let data = Node.Stmt.data stmt_node in
    match data with Expr expr -> count_expr expr | _ -> ()
  in
  List.iter count_stmt prog;
  !counter

let find_idents prog =
  let idents = ref [] in
  let collect_expr expr_node =
    let data = Node.Expr.data expr_node in
    match data with Ident id -> idents := id :: !idents | _ -> ()
  in
  let collect_stmt stmt_node =
    let data = Node.Stmt.data stmt_node in
    match data with Expr expr -> collect_expr expr | _ -> ()
  in
  List.iter collect_stmt prog;
  !idents

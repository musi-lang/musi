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

let rec visit_expr ctx expr_node =
  let data = Node.Expr.data expr_node in
  match data with
  | Lit _ -> ret_ok ()
  | Ident _ -> ret_ok ()
  | Tmpl _ -> ret_ok ()
  | Tuple exprs ->
    List.fold_left
      (fun acc expr -> bind_result acc (fun _ -> visit_expr ctx expr))
      (ret_ok ())
      exprs
  | Block block ->
    List.fold_left
      (fun acc stmt -> bind_result acc (fun _ -> visit_stmt ctx stmt))
      (ret_ok ())
      block.stmts
  | Range { start; end_; _ } -> (
    let acc =
      match start with
      | Some expr -> bind_result (ret_ok ()) (fun _ -> visit_expr ctx expr)
      | None -> ret_ok ()
    in
    match end_ with
    | Some expr -> bind_result acc (fun _ -> visit_expr ctx expr)
    | None -> acc)
  | If { conds; then_block; else_block } -> (
    let acc =
      List.fold_left
        (fun acc (pat, expr) ->
          bind_result acc (fun _ ->
            bind_result (visit_expr ctx expr) (fun _ -> visit_pat ctx pat)))
        (ret_ok ())
        conds
    in
    let acc =
      List.fold_left
        (fun acc stmt -> bind_result acc (fun _ -> visit_stmt ctx stmt))
        acc
        then_block.stmts
    in
    match else_block with
    | Some block ->
      List.fold_left
        (fun acc stmt -> bind_result acc (fun _ -> visit_stmt ctx stmt))
        acc
        block.stmts
    | None -> acc)
  | Match { scrutinee; _ } -> visit_expr ctx scrutinee
  | For { binding; range; guard; body } ->
    let acc = visit_expr ctx range in
    let acc =
      bind_result acc (fun _ ->
        match binding with
        | ForPat pat -> visit_pat ctx pat
        | ForIdent _ -> ret_ok ())
    in
    let acc =
      bind_result acc (fun _ ->
        match guard with
        | Some guard_expr -> visit_expr ctx guard_expr
        | None -> ret_ok ())
    in
    bind_result acc (fun _ ->
      List.fold_left
        (fun acc stmt -> bind_result acc (fun _ -> visit_stmt ctx stmt))
        (ret_ok ())
        body.stmts)
  | While { cond; guard; body } ->
    let acc =
      match cond with
      | Some (CaseBinding { pat; expr }) ->
        bind_result (visit_expr ctx expr) (fun _ -> visit_pat ctx pat)
      | Some (Expr expr) -> visit_expr ctx expr
      | None -> ret_ok ()
    in
    let acc =
      bind_result acc (fun _ ->
        match guard with
        | Some guard_expr -> visit_expr ctx guard_expr
        | None -> ret_ok ())
    in
    bind_result acc (fun _ ->
      List.fold_left
        (fun acc stmt -> bind_result acc (fun _ -> visit_stmt ctx stmt))
        (ret_ok ())
        body.stmts)
  | Defer expr -> visit_expr ctx expr
  | Break mbe -> (
    match mbe with Some expr -> visit_expr ctx expr | None -> ret_ok ())
  | Cycle -> ret_ok ()
  | Unsafe block ->
    List.fold_left
      (fun acc stmt -> bind_result acc (fun _ -> visit_stmt ctx stmt))
      (ret_ok ())
      block.stmts
  | Assign { value; _ } -> visit_expr ctx value
  | Unary { arg; _ } -> visit_expr ctx arg
  | Call { callee; typ_args; args; _ } ->
    let acc = visit_expr ctx callee in
    let acc =
      match typ_args with
      | Some typs ->
        List.fold_left
          (fun acc typ -> bind_result acc (fun _ -> visit_typ ctx typ))
          acc
          typs
      | None -> acc
    in
    List.fold_left
      (fun acc expr -> bind_result acc (fun _ -> visit_expr ctx expr))
      acc
      args
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
    let acc =
      match binding.typ_annot with
      | Some typ -> bind_result (ret_ok ()) (fun _ -> visit_typ ctx typ)
      | None -> ret_ok ()
    in
    bind_result acc (fun _ -> visit_expr ctx value)
  | Extern _ -> ret_ok ()
  | Expr expr -> visit_expr ctx expr

and visit_pat ctx pat_node =
  let data = Node.Pat.data pat_node in
  match data with
  | Bind binding -> (
    match binding.typ_annot with
    | Some typ -> bind_result (ret_ok ()) (fun _ -> visit_typ ctx typ)
    | None -> ret_ok ())
  | Lit _ -> ret_ok ()
  | Wild -> ret_ok ()
  | Ident _ -> ret_ok ()
  | Record _ -> ret_ok ()
  | Ctor { args; _ } ->
    List.fold_left
      (fun acc pat -> bind_result acc (fun _ -> visit_pat ctx pat))
      (ret_ok ())
      args
  | Tuple pats ->
    List.fold_left
      (fun acc pat -> bind_result acc (fun _ -> visit_pat ctx pat))
      (ret_ok ())
      pats

and visit_typ ctx typ_node =
  let data = Node.Typ.data typ_node in
  match data with
  | Ptr typ -> bind_result (ret_ok ()) (fun _ -> visit_typ ctx typ)
  | Array { size; elem } ->
    let acc =
      match size with
      | Some expr -> bind_result (ret_ok ()) (fun _ -> visit_expr ctx expr)
      | None -> ret_ok ()
    in
    bind_result acc (fun _ -> visit_typ ctx elem)
  | Ident _ -> ret_ok ()
  | App { args; _ } ->
    List.fold_left
      (fun acc typ -> bind_result acc (fun _ -> visit_typ ctx typ))
      (ret_ok ())
      args
  | Tuple typs ->
    List.fold_left
      (fun acc typ -> bind_result acc (fun _ -> visit_typ ctx typ))
      (ret_ok ())
      typs
  | Fn { params; ret } -> (
    let acc =
      List.fold_left
        (fun acc typ -> bind_result acc (fun _ -> visit_typ ctx typ))
        (ret_ok ())
        params
    in
    match ret with
    | Some typ -> bind_result acc (fun _ -> visit_typ ctx typ)
    | None -> acc)
  | Record _ -> ret_ok ()
  | Optional typ -> bind_result (ret_ok ()) (fun _ -> visit_typ ctx typ)

let visit_prog ctx prog =
  List.fold_left
    (fun acc stmt -> bind_result acc (fun _ -> visit_stmt ctx stmt))
    (ret_ok ())
    prog

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

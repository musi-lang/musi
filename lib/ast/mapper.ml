open Basic
open Node

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

type 'a t = {
    expr : expr -> 'a result
  ; stmt : stmt -> 'a result
  ; pat : pat -> 'a result
  ; typ : typ -> 'a result
}

let map_over_list f list =
  List.fold_left
    (fun acc item -> bind_result acc (fun _ -> f item))
    (Ok ())
    list

let map_opt opt f =
  match opt with Some x -> bind_result (Ok ()) (fun _ -> f x) | None -> Ok ()

let rec map_expr mapper ctx expr_node =
  let data = expr_node.data in
  let _ = mapper.expr expr_node in
  match data with
  | ExprLit _ -> Ok ()
  | ExprIdent _ -> Ok ()
  | ExprTmpl _ -> Ok ()
  | ExprTuple exprs -> map_over_list (map_expr mapper ctx) exprs
  | ExprBlock block ->
    let acc = map_over_list (map_stmt mapper ctx) block.stmts in
    bind_result acc (fun _ -> map_opt block.ret (map_expr mapper ctx))
  | ExprRange { start; end_; _ } ->
    let acc = map_opt start (map_expr mapper ctx) in
    bind_result acc (fun _ -> map_opt end_ (map_expr mapper ctx))
  | ExprIf { conds; then_block; else_block } ->
    let acc =
      map_over_list
        (fun (pat, expr) ->
          bind_result (map_expr mapper ctx expr) (fun _ ->
            map_pat mapper ctx pat))
        conds
    in
    let acc' =
      bind_result acc (fun _ ->
        map_over_list (map_stmt mapper ctx) then_block.stmts)
    in
    bind_result acc' (fun _ ->
      map_opt else_block (fun block ->
        map_over_list (map_stmt mapper ctx) block.stmts))
  | ExprMatch { scrutinee; _ } -> map_expr mapper ctx scrutinee
  | ExprFor { binding; range; guard; body } ->
    let acc = map_expr mapper ctx range in
    let acc' =
      bind_result acc (fun _ ->
        match binding with
        | ForPat pat -> map_pat mapper ctx pat
        | ForIdent _ -> Ok ())
    in
    let acc'' =
      bind_result acc' (fun _ -> map_opt guard (map_expr mapper ctx))
    in
    bind_result acc'' (fun _ -> map_over_list (map_stmt mapper ctx) body.stmts)
  | ExprWhile { cond; guard; body } ->
    let acc =
      match cond with
      | Some (CondCaseBinding { pat; expr }) ->
        bind_result (map_expr mapper ctx expr) (fun _ -> map_pat mapper ctx pat)
      | Some (CondExpr expr) -> map_expr mapper ctx expr
      | None -> Ok ()
    in
    let acc' = bind_result acc (fun _ -> map_opt guard (map_expr mapper ctx)) in
    bind_result acc' (fun _ -> map_over_list (map_stmt mapper ctx) body.stmts)
  | ExprDefer expr -> map_expr mapper ctx expr
  | ExprBreak mbe -> map_opt mbe (map_expr mapper ctx)
  | ExprCycle -> Ok ()
  | ExprReturn value -> map_opt value (map_expr mapper ctx)
  | ExprUnsafe block -> map_over_list (map_stmt mapper ctx) block.stmts
  | ExprAssign { value; _ } -> map_expr mapper ctx value
  | ExprUnary { arg; _ } -> map_expr mapper ctx arg
  | ExprCall { callee; typ_args; args; _ } ->
    let acc = map_expr mapper ctx callee in
    let acc' =
      bind_result acc (fun _ ->
        map_opt typ_args (map_over_list (map_typ mapper ctx)))
    in
    bind_result acc' (fun _ -> map_over_list (map_expr mapper ctx) args)
  | ExprMember { obj; _ } -> map_expr mapper ctx obj
  | ExprRecordLit _ -> Ok ()
  | ExprFn _ -> Ok ()
  | ExprRecord _ -> Ok ()
  | ExprChoice _ -> Ok ()

and map_stmt mapper ctx stmt_node =
  let data = stmt_node.data in
  let _ = mapper.stmt stmt_node in
  match data with
  | StmtImport _ -> Ok ()
  | StmtExport _ -> Ok ()
  | StmtBind { binding; value; _ } ->
    let acc = map_opt binding.typ_annot (map_typ mapper ctx) in
    bind_result acc (fun _ -> map_expr mapper ctx value)
  | StmtExtern _ -> Ok ()
  | StmtExpr expr -> map_expr mapper ctx expr

and map_pat mapper ctx pat_node =
  let data = pat_node.data in
  let _ = mapper.pat pat_node in
  match data with
  | PatBind binding -> map_opt binding.typ_annot (map_typ mapper ctx)
  | PatLit _ -> Ok ()
  | PatWild -> Ok ()
  | PatIdent _ -> Ok ()
  | PatRecord _ -> Ok ()
  | PatCtor { args; _ } -> map_over_list (map_pat mapper ctx) args
  | PatTuple pats -> map_over_list (map_pat mapper ctx) pats

and map_typ mapper ctx typ_node =
  let data = typ_node.data in
  let _ = mapper.typ typ_node in
  match data with
  | TypPtr typ -> bind_result (Ok ()) (fun _ -> map_typ mapper ctx typ)
  | TypArray { size; elem } ->
    let acc = map_opt size (map_expr mapper ctx) in
    bind_result acc (fun _ -> map_typ mapper ctx elem)
  | TypIdent _ -> Ok ()
  | TypApp { args; _ } -> map_over_list (map_typ mapper ctx) args
  | TypTuple typs -> map_over_list (map_typ mapper ctx) typs
  | TypFn { params; ret } ->
    let acc = map_over_list (map_typ mapper ctx) params in
    bind_result acc (fun _ -> map_opt ret (map_typ mapper ctx))
  | TypRecord _ -> Ok ()
  | TypOptional typ -> map_typ mapper ctx typ

let map mapper ctx prog = map_over_list (map_stmt mapper ctx) prog

let iter mapper ctx prog =
  let _ = map mapper ctx prog in
  ()

let fold f acc prog =
  List.fold_left
    (fun acc stmt ->
      match acc with Error e -> Error e | Ok current_acc -> f current_acc stmt)
    (Ok acc)
    prog

let count_nodes prog =
  let ctx = mk_context () in
  let counter = ref 0 in
  let counter_mapper =
    {
      expr =
        (fun _ ->
          incr counter;
          Ok ())
    ; stmt = (fun _ -> Ok ())
    ; pat = (fun _ -> Ok ())
    ; typ = (fun _ -> Ok ())
    }
  in
  let _ = map counter_mapper ctx prog in
  !counter

let find_idents prog =
  let ctx = mk_context () in
  let idents = ref [] in
  let collect_expr expr_node =
    let data = expr_node.data in
    match data with
    | ExprIdent id ->
      idents := id :: !idents;
      Ok ()
    | _ -> Ok ()
  in
  let collect_stmt stmt_node =
    let data = stmt_node.data in
    match data with StmtExpr expr -> collect_expr expr | _ -> Ok ()
  in
  let collect_mapper =
    {
      expr = collect_expr
    ; stmt = collect_stmt
    ; pat = (fun _ -> Ok ())
    ; typ = (fun _ -> Ok ())
    }
  in
  let _ = map collect_mapper ctx prog in
  !idents

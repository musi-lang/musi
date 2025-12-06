module Mapper = struct
  open Basic
  module Token = Lex.Token
  module Node = Node

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
      expr : Node.Expr.t -> 'a result
    ; stmt : Node.Stmt.t -> 'a result
    ; pat : Node.Pat.t -> 'a result
    ; typ : Node.Typ.t -> 'a result
  }

  let map_over_list f list =
    List.fold_left
      (fun acc item -> bind_result acc (fun _ -> f item))
      (Ok ())
      list

  let map_opt opt f =
    match opt with
    | Some x -> bind_result (Ok ()) (fun _ -> f x)
    | None -> Ok ()

  let rec map_expr mapper ctx expr_node =
    let data = Node.Expr.data expr_node in
    let _ = mapper.expr expr_node in
    match data with
    | Lit _ -> Ok ()
    | Ident _ -> Ok ()
    | Tmpl _ -> Ok ()
    | Tuple exprs -> map_over_list (map_expr mapper ctx) exprs
    | Block block ->
      let acc = map_over_list (map_stmt mapper ctx) block.stmts in
      bind_result acc (fun _ -> map_opt block.ret (map_expr mapper ctx))
    | Range { start; end_; _ } ->
      let acc = map_opt start (map_expr mapper ctx) in
      bind_result acc (fun _ -> map_opt end_ (map_expr mapper ctx))
    | If { conds; then_block; else_block } ->
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
    | Match { scrutinee; _ } -> map_expr mapper ctx scrutinee
    | For { binding; range; guard; body } ->
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
      bind_result acc'' (fun _ ->
        map_over_list (map_stmt mapper ctx) body.stmts)
    | While { cond; guard; body } ->
      let acc =
        match cond with
        | Some (CaseBinding { pat; expr }) ->
          bind_result (map_expr mapper ctx expr) (fun _ ->
            map_pat mapper ctx pat)
        | Some (Expr expr) -> map_expr mapper ctx expr
        | None -> Ok ()
      in
      let acc' =
        bind_result acc (fun _ -> map_opt guard (map_expr mapper ctx))
      in
      bind_result acc' (fun _ -> map_over_list (map_stmt mapper ctx) body.stmts)
    | Defer expr -> map_expr mapper ctx expr
    | Break mbe -> map_opt mbe (map_expr mapper ctx)
    | Cycle -> Ok ()
    | Return value -> map_opt value (map_expr mapper ctx)
    | Unsafe block -> map_over_list (map_stmt mapper ctx) block.stmts
    | Assign { value; _ } -> map_expr mapper ctx value
    | Unary { arg; _ } -> map_expr mapper ctx arg
    | Call { callee; typ_args; args; _ } ->
      let acc = map_expr mapper ctx callee in
      let acc' =
        bind_result acc (fun _ ->
          map_opt typ_args (map_over_list (map_typ mapper ctx)))
      in
      bind_result acc' (fun _ -> map_over_list (map_expr mapper ctx) args)
    | Member { obj; _ } -> map_expr mapper ctx obj
    | RecordLit _ -> Ok ()
    | Fn _ -> Ok ()
    | Record _ -> Ok ()
    | Choice _ -> Ok ()

  and map_stmt mapper ctx stmt_node =
    let data = Node.Stmt.data stmt_node in
    let _ = mapper.stmt stmt_node in
    match data with
    | Import _ -> Ok ()
    | Export _ -> Ok ()
    | Bind { binding; value; _ } ->
      let acc = map_opt binding.typ_annot (map_typ mapper ctx) in
      bind_result acc (fun _ -> map_expr mapper ctx value)
    | Extern _ -> Ok ()
    | Expr expr -> map_expr mapper ctx expr

  and map_pat mapper ctx pat_node =
    let data = Node.Pat.data pat_node in
    let _ = mapper.pat pat_node in
    match data with
    | Bind binding -> map_opt binding.typ_annot (map_typ mapper ctx)
    | Lit _ -> Ok ()
    | Wild -> Ok ()
    | Ident _ -> Ok ()
    | Record _ -> Ok ()
    | Ctor { args; _ } -> map_over_list (map_pat mapper ctx) args
    | Tuple pats -> map_over_list (map_pat mapper ctx) pats

  and map_typ mapper ctx typ_node =
    let data = Node.Typ.data typ_node in
    let _ = mapper.typ typ_node in
    match data with
    | Ptr typ -> bind_result (Ok ()) (fun _ -> map_typ mapper ctx typ)
    | Array { size; elem } ->
      let acc = map_opt size (map_expr mapper ctx) in
      bind_result acc (fun _ -> map_typ mapper ctx elem)
    | Ident _ -> Ok ()
    | App { args; _ } -> map_over_list (map_typ mapper ctx) args
    | Tuple typs -> map_over_list (map_typ mapper ctx) typs
    | Fn { params; ret } ->
      let acc = map_over_list (map_typ mapper ctx) params in
      bind_result acc (fun _ -> map_opt ret (map_typ mapper ctx))
    | Record _ -> Ok ()
    | Optional typ -> map_typ mapper ctx typ

  let map mapper ctx prog = map_over_list (map_stmt mapper ctx) prog

  let iter mapper ctx prog =
    let _ = map mapper ctx prog in
    ()

  let fold mapper acc ctx prog =
    List.fold_left
      (fun _ stmt -> bind_result (Ok ()) (fun _ -> map_stmt mapper ctx stmt))
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
      let data = Node.Expr.data expr_node in
      match data with
      | Ident id ->
        idents := id :: !idents;
        Ok ()
      | _ -> Ok ()
    in
    let collect_stmt stmt_node =
      let data = Node.Stmt.data stmt_node in
      match data with Expr expr -> collect_expr expr | _ -> Ok ()
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
end

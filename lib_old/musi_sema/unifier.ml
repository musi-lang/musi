let rec unify_record fs1 fs2 =
  if List.length fs1 <> List.length fs2 then
    failwith
      (Printf.sprintf
         "mismatched record fields '%s' and '%s'"
         (Types.show (Types.TyRecord fs1))
         (Types.show (Types.TyRecord fs2)));
  List.iter2
    (fun (n1, t1) (n2, t2) ->
      if n1 <> n2 then
        failwith
          (Printf.sprintf
             "mismatched record fields '%s' and '%s'"
             (Types.show (Types.TyRecord fs1))
             (Types.show (Types.TyRecord fs2)));
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
  let t1 = Types.repr t1 in
  let t2 = Types.repr t2 in
  match (t1, t2) with
  | _ when t1 = t2 -> ()
  | ( Types.TyVar ({ contents = Types.Unbound id1 } as v1)
    , Types.TyVar { contents = Types.Unbound id2 } ) ->
    if id1 <> id2 then v1 := Types.Link t2
  | Types.TyVar ({ contents = Types.Unbound id } as v), t
  | t, Types.TyVar ({ contents = Types.Unbound id } as v) ->
    if Types.occurs_check id t then
      failwith (Printf.sprintf "infinite type '%s'" (Types.show t))
    else v := Types.Link t
  | Types.TyNamed n1, Types.TyNamed n2 when n1 = n2 -> ()
  | Types.TyTuple ts1, Types.TyTuple ts2 when List.length ts1 = List.length ts2
    ->
    List.iter2 unify ts1 ts2
  | Types.TyArray t1, Types.TyArray t2 -> unify t1 t2
  | Types.TyRecord fs1, Types.TyRecord fs2 -> unify_record fs1 fs2
  | Types.TyFn (args1, ret1), Types.TyFn (args2, ret2) ->
    unify_fn args1 ret1 args2 ret2
  | Types.TyOptional t1, Types.TyOptional t2 -> unify t1 t2
  | Types.TyScheme (_, t1), t2 -> unify t1 t2
  | t1, Types.TyScheme (_, t2) -> unify t1 t2
  | _ ->
    failwith
      (Printf.sprintf
         "expected type '%s', found type '%s'"
         (Types.show t1)
         (Types.show t2))

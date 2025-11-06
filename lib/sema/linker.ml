(* ========================================
   TYPES
   ======================================== *)

type module_id = string

type module_info = {
    id : module_id
  ; path : string
  ; ast : Node.t list
  ; imports : module_id list
  ; exports : Interner.name list
}

type t = { interner : Interner.t; diags : Diagnostic.bag ref }

let create interner = { interner; diags = ref Diagnostic.empty_bag }

let error t msg span =
  t.diags := Diagnostic.add !(t.diags) (Diagnostic.error msg span)

(* ========================================
   MODULE PATH RESOLUTION
   ======================================== *)

let resolve_module_path current_file import_path =
  if String.starts_with ~prefix:"stdlib/" import_path then
    let subpath = String.sub import_path 7 (String.length import_path - 7) in
    Filename.concat "stdlib" (subpath ^ ".ms")
  else if
    String.starts_with ~prefix:"./" import_path
    || String.starts_with ~prefix:"../" import_path
  then
    let dir = Filename.dirname current_file in
    Filename.concat dir (import_path ^ ".ms")
  else import_path ^ ".ms"

(* ========================================
   IMPORT EXTRACTION
   ======================================== *)

let extract_imports ast =
  List.filter_map
    (fun node ->
      match node.Node.kind with
      | Node.StmtImport { path; _ } -> Some path
      | _ -> None)
    ast

let extract_exports ast =
  List.concat_map
    (fun node ->
      match node.Node.kind with
      | Node.StmtExport { items } -> items.Node.items
      | Node.StmtExpr expr -> (
        match expr.Node.kind with
        | Node.ExprBinding { modifiers; pat; _ } ->
          if modifiers.Node.is_exported then
            match pat.Node.kind with
            | Node.ExprIdent name | Node.PatBinding name -> [ name ]
            | _ -> []
          else []
        | Node.ExprProc { modifiers; _ } ->
          if modifiers.Node.is_exported then [] else []
        | _ -> [])
      | _ -> [])
    ast

(* ========================================
   IMPORT GRAPH BUILDING
   ======================================== *)

let build_import_graph t entry_file =
  let visited = Hashtbl.create 16 in
  let modules = ref [] in
  let rec visit current_file =
    if Hashtbl.mem visited current_file then ()
    else if not (Sys.file_exists current_file) then
      error
        t
        (Printf.sprintf "file '%s' not found" current_file)
        (Span.make 0 0 0)
    else (
      Hashtbl.add visited current_file ();
      let ic = open_in current_file in
      let source = really_input_string ic (in_channel_length ic) in
      close_in ic;
      let lexer = Lexer.make 0 source t.interner in
      let tokens, lex_diags = Lexer.lex lexer in
      t.diags := Diagnostic.merge [ !(t.diags); lex_diags ];
      let parser = Parser.make tokens t.interner in
      let ast, parse_diags = Parser.parse parser in
      t.diags := Diagnostic.merge [ !(t.diags); parse_diags ];
      let import_names = extract_imports ast in
      let exports = extract_exports ast in
      let import_paths =
        List.map
          (fun name ->
            let import_str = Interner.lookup t.interner name in
            resolve_module_path current_file import_str)
          import_names
      in
      let info =
        {
          id = current_file
        ; path = current_file
        ; ast
        ; imports = import_paths
        ; exports
        }
      in
      modules := info :: !modules;
      List.iter visit import_paths)
  in
  visit entry_file;
  (!modules, !(t.diags))

(* ========================================
   TOPOLOGICAL SORT
   ======================================== *)

let topological_sort t modules =
  let graph = Hashtbl.create (List.length modules) in
  let in_degree = Hashtbl.create (List.length modules) in
  List.iter
    (fun m ->
      Hashtbl.add graph m.id m;
      Hashtbl.add in_degree m.id 0)
    modules;
  List.iter
    (fun m ->
      List.iter
        (fun dep ->
          match Hashtbl.find_opt in_degree dep with
          | Some count -> Hashtbl.replace in_degree dep (count + 1)
          | None -> ())
        m.imports)
    modules;
  let queue = Queue.create () in
  Hashtbl.iter (fun id count -> if count = 0 then Queue.add id queue) in_degree;
  let rec process acc =
    if Queue.is_empty queue then (
      if List.length acc = List.length modules then List.rev acc
      else
        let cycle = List.find (fun m -> not (List.mem m acc)) modules in
        error
          t
          (Printf.sprintf "circular dependency detected involving %s" cycle.id)
          (Span.make 0 0 0);
        List.rev acc)
    else
      let id = Queue.take queue in
      let m = Hashtbl.find graph id in
      List.iter
        (fun dep ->
          match Hashtbl.find_opt in_degree dep with
          | Some count ->
            let new_count = count - 1 in
            Hashtbl.replace in_degree dep new_count;
            if new_count = 0 then Queue.add dep queue
          | None -> ())
        m.imports;
      process (m :: acc)
  in
  (process [], !(t.diags))

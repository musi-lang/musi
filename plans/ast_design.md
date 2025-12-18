# Musi Language AST Design Specification

## Overview

This document specifies the Abstract Syntax Tree (AST) structure for the Musi programming language, following ESTree-style conventions with OCaml-specific adaptations.

## Core Design Principles

1. **Record-based nodes**: All AST nodes are records with `kind` and `span` fields
2. **Grammar prefix matching**: Node kind names match grammar rule prefixes exactly (`expr_ident` → `ExprIdent`)
3. **Parser concerns separated**: Precedence levels are handled by parser, not reflected in AST structure
4. **Boolean flags**: Optional behaviors represented as boolean flags rather than separate node types

## Core Node Types

### Expression Nodes

```ocaml
type expr = {
  kind: expr_kind;
  span: span;
}

and expr_kind =
  (* Literals and identifiers *)
  | ExprLit of lit_kind
  | ExprIdent of ident

  (* Collection literals *)
  | ExprLitTuple of expr list
  | ExprLitArray of expr list
  | ExprLitRecord of ident option * record_content

  (* Control flow *)
  | ExprBlock of stmt list * expr option
  | ExprIf of expr * expr * expr
  | ExprWhile of expr * expr
  | ExprFor of ident * expr * expr
  | ExprMatch of expr * match_case list
  | ExprTry of expr * (ident option * expr) option
  | ExprReturn of expr option
  | ExprDefer of expr
  | ExprBreak of expr option
  | ExprCycle
  | ExprUnsafe of expr

  (* Declarations *)
  | ExprImport of string
  | ExprExtern of string option * bool * fn_sig list
  | ExprRecord of attr list * modifier * ident option * ty_params * record_field list
  | ExprSum of attr list * modifier * ident option * ty_params * sum_case list
  | ExprFn of attr list * modifier * fn_sig * expr
  | ExprBind of modifier * bool * ident * ty option * expr * expr

  (* Operations *)
  | ExprCall of expr * expr list
  | ExprIndex of expr * expr * bool    (* bool: true for optional chain *)
  | ExprMember of expr * ident * bool  (* bool: true for optional chain *)
  | ExprDeref of expr
  | ExprUnary of Token.t * expr
  | ExprBinary of expr * Token.t * expr
```

### Pattern Nodes

```ocaml
type pat = {
  kind: pat_kind;
  span: span;
}

and pat_kind =
  | PatIdent of ident
  | PatLit of literal
  | PatWild
  | PatLitTuple of pat list
  | PatLitArray of pat list
  | PatLitRecord of ident * pat_field list
  | PatVariant of ident * ty list * pat option
  | PatCons of pat * pat
```

### Type Nodes

```ocaml
type ty = {
  kind: ty_kind;
  span: span;
}

and ty_kind =
  | TyIdent of ident
  | TyApp of ident * ty list
  | TyOptional of ty
  | TyArray of int option * ty
  | TyPtr of ty
  | TyFn of ty * ty
  | TyTuple of ty list
```

### Statement Nodes

```ocaml
type stmt = {
  kind: stmt_kind;
  span: span;
}

and stmt_kind =
  | StmtExpr of expr
```

## Supporting Types

### Literals

```ocaml
type literal =
  | LitInt of string
  | LitFloat of string
  | LitString of string
  | LitTemplate of template_part list
  | LitRune of char
  | LitBool of bool

and template_part =
  | TemplateText of string
  | TemplateExpr of expr
```

### Attributes

```ocaml
type attr = {
  attr_name: ident;
  attr_args: attr_arg list;
}

and attr_arg =
  | AttrArgPos of ident * literal
  | AttrArgNamed of ident * ident * literal
```

### Modifiers

```ocaml
type modifier = {
  is_export: bool;
  is_extern: (string option * bool) option;  (* Some(path, unsafe) or None *)
  is_unsafe: bool;
}
```

### Function Signatures

```ocaml
type fn_sig = {
  fn_name: ident option;
  fn_ty_params: ty_params;
  fn_params: param list;
  fn_ret_ty: ty option;
}

and param = {
  param_var: bool;
  param_name: ident;
  param_ty: ty option;
  param_default: expr option;
}
```

### Record Types

```ocaml
type record_content =
  | RecordFields of record_field list
  | RecordWith of expr * record_field list

and record_field = {
  field_var: bool;
  field_name: ident;
  field_ty: ty option;
  field_default: expr option;
}
```

### Sum Type Cases

```ocaml
type sum_case = {
  case_name: ident;
  case_types: ty list;
  case_params: ty list;
}
```

### Pattern Fields

```ocaml
type pat_field = {
  field_name: ident;
}
```

### Match Cases

```ocaml
type match_case = {
  case_pattern: pat;
  case_expr: expr;
}
```

## Grammar Mappings

### Expression Grammar to AST

```md
| Grammar Rule | AST Node Kind | Fields |
|--------------|---------------|--------|
| `expr_lit` | `ExprLit` | `literal` |
| `expr_ident` | `ExprIdent` | `ident` |
| `expr_lit_tuple` | `ExprLitTuple` | `expr list` |
| `expr_lit_array` | `ExprLitArray` | `expr list` |
| `expr_lit_record` | `ExprLitRecord` | `ident option * record_content` |
| `expr_block` | `ExprBlock` | `stmt list * expr option` |
| `expr_if` | `ExprIf` | `expr * expr * expr` |
| `expr_while` | `ExprWhile` | `expr * expr` |
| `expr_for` | `ExprFor` | `ident * expr * expr` |
| `expr_match` | `ExprMatch` | `expr * match_case list` |
| `expr_try` | `ExprTry` | `expr * (ident option * expr) option` |
| `expr_return` | `ExprReturn` | `expr option` |
| `expr_defer` | `ExprDefer` | `expr` |
| `expr_break` | `ExprBreak` | `expr option` |
| `expr_cycle` | `ExprCycle` | unit |
| `expr_unsafe` | `ExprUnsafe` | `expr` |
| `expr_import` | `ExprImport` | `string` |
| `expr_extern` | `ExprExtern` | `string option * bool * fn_sig list` |
| `expr_record` | `ExprRecord` | `attr list * modifier * ident * ty_params * record_field list` |
| `expr_sum` | `ExprSum` | `attr list * modifier * ident * ty_params * sum_case list` |
| `expr_fn` | `ExprFn` | `attr list * modifier * fn_sig * expr` |
| `expr_bind` | `ExprBind` | `modifier * bool * ident * ty option * expr * expr` |
```

### Pattern Grammar to AST

```md
| Grammar Rule | AST Node Kind | Fields |
|--------------|---------------|--------|
| `pat_ident` | `PatIdent` | `ident` |
| `pat_lit` | `PatLit` | `literal` |
| `pat_wild` | `PatWild` | unit |
| `pat_lit_tuple` | `PatLitTuple` | `pat list` |
| `pat_lit_array` | `PatLitArray` | `pat list` |
| `pat_lit_record` | `PatLitRecord` | `ident option * pat_field list` |
| `pat_variant` | `PatVariant` | `ident * ty list * pat option` |
| `pat_cons` | `PatCons` | `pat * pat` |
```

### Type Grammar to AST

```md
| Grammar Rule | AST Node Kind | Fields |
|--------------|---------------|--------|
| `ty_ident` | `TyIdent` | `ident` |
| `ty_app` | `TyApp` | `ident * ty list` |
| `ty_optional` | `TyOptional` | `ty` |
| `ty_array` | `TyArray` | `int option * ty` |
| `ty_ptr` | `TyPtr` | `ty` |
| `ty_fn` | `TyFn` | `ty * ty` |
| `ty_tuple` | `TyTuple` | `ty list` |
```

## Examples

### Expression Example

```musi
val x := 10 + 20;
```

```ocaml
{
  kind = ExprBind(
    {is_export = false; is_extern = None; is_unsafe = false},
    false,
    "x",
    None,
    {
      kind = ExprBinary(
        {kind = ExprLit(LitInt "10"); span = span1},
        Token.Plus,
        {kind = ExprLit(LitInt "20"); span = span2}
      );
      span = span3
    },
    {kind = ExprLit(LitInt "0"); span = span4}  (* dummy for continuation *)
  );
  span = span5
}
```

### Pattern Example

```musi
match result {
case Succ(value) => value,
case Fail(_) => 0
};
```

```ocaml
{
  kind = ExprMatch(
    {kind = ExprIdent "result"; span = span1},
    [
      {
        case_pat = {
          kind = PatVariant("Succ", [], Some {kind = PatIdent "value"; span = span2});
          span = span3
        };
        case_expr = {kind = ExprIdent "value"; span = span4}
      };
      {
        case_pat = {
          kind = PatVariant("Fail", [], Some {kind = PatWild; span = span5});
          span = span6
        };
        case_expr = {kind = ExprLit(LitInt "0"); span = span7}
      }
    ]
  );
  span = span8
}
```

## AST Node Relationships

```mermaid
graph TD
    expr[expr: {kind, span}]
    pat[pat: {kind, span}]
    ty[ty: {kind, span}]
    stmt[stmt: {kind, span}]

    expr --> expr_kind
    pat --> pat_kind
    ty --> ty_kind
    stmt --> stmt_kind

    expr_kind --> ExprLit[lit_kind]
    expr_kind --> ExprIdent[ident]
    expr_kind --> ExprLitTuple[expr list]
    expr_kind --> ExprLitArray[expr list]
    expr_kind --> ExprLitRecord[ident option * record_content]
    expr_kind --> ExprBlock[stmt list * expr option]
    expr_kind --> ExprIf[expr * expr * expr]
    expr_kind --> ExprWhile[expr * expr]
    expr_kind --> ExprFor[ident * expr * expr]
    expr_kind --> ExprMatch[expr * match_case list]
    expr_kind --> ExprTry[expr * (ident option * expr) option]
    expr_kind --> ExprReturn[expr option]
    expr_kind --> ExprDefer[expr]
    expr_kind --> ExprBreak[expr option]
    expr_kind --> ExprCycle[unit]
    expr_kind --> ExprUnsafe[expr]
    expr_kind --> ExprImport[string]
    expr_kind --> ExprExtern[string option * bool * fn_sig list]
    expr_kind --> ExprRecord[attr list * modifier * ident option * ty_params * record_field list]
    expr_kind --> ExprSum[attr list * modifier * ident option * ty_params * sum_case list]
    expr_kind --> ExprFn[attr list * modifier * fn_sig * expr]
    expr_kind --> ExprBind[modifier * bool * ident * ty option * expr * expr]
    expr_kind --> ExprCall[expr * expr list]
    expr_kind --> ExprIndex[expr * expr * bool]
    expr_kind --> ExprMember[expr * ident * bool]
    expr_kind --> ExprDeref[expr]
    expr_kind --> ExprUnary[Token.t * expr]
    expr_kind --> ExprBinary[expr * Token.t * expr]

    pat_kind --> PatIdent[ident]
    pat_kind --> PatLit[lit_kind]
    pat_kind --> PatWild[unit]
    pat_kind --> PatLitTuple[pat list]
    pat_kind --> PatLitArray[pat list]
    pat_kind --> PatLitRecord[ident option * pat_field list]
    pat_kind --> PatVariant[ident * ty list * pat option]
    pat_kind --> PatCons[pat * pat]

    ty_kind --> TyIdent[ident]
    ty_kind --> TyApp[ident * ty list]
    ty_kind --> TyOptional[ty]
    ty_kind --> TyArray[int option * ty]
    ty_kind --> TyPtr[ty]
    ty_kind --> TyFn[ty * ty]
    ty_kind --> TyTuple[ty list]

    stmt_kind --> StmtExpr[expr]
```

## Implementation Notes

- All nodes contain source location information via the `span` field
- The parser handles operator precedence; AST uses uniform `ExprBinary` and `ExprUnary` nodes
- Optional chaining represented as boolean flag on `ExprMember`
- Modifiers use record with boolean flags rather than sum types
- Grammar auxiliary elements (`aux_`) that are purely syntactic are not represented in AST

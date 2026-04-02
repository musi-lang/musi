# Grammar (EBNF + Precedence)

`grammar/Musi.g4` is the canonical, tool-supported grammar.

This document is the compact, implementer-facing formalization intended to be easy to implement in “primitive simple” languages (C/Go) with:

- a hand-written lexer
- recursive descent for statements/constructs
- Pratt parsing for expressions

It also defines CST shaping rules (“no unnecessary nodes”).

## Tokens (summary)

Keywords:

`and as case class data effect export foreign handle if import in instance law let mut rec not of opaque or perform quote resume shl shr where with xor`

Fixed punctuation / operators (maximal munch):

`:= <- -> ~> => /= <= >= <: ?. !. ... .{ .[ :? :?> |>`

Singles:

`@ # ( ) [ ] { } , ; . : | + - * / % = < > _`

Literals:

- `INT_LIT`, `FLOAT_LIT`
- `STRING_LIT`, `FSTRING_LIT`, `RUNE_LIT`
- `IDENT`, `ESCAPED_IDENT`

## Expression precedence (term expressions)

Higher number = tighter binding.

| BP | Operators / forms                     | Assoc |
| -- | -------------------------------------- | ----- |
| 26 | calls `()`, bracket apply `[]`, postfix field/index/update | left  |
| 24 | prefix `- not mut`                     | right |
| 22 | user `SYMBOLIC_OP`                     | left  |
| 20 | `* / %`                                | left  |
| 18 | `+ -`                                  | left  |
| 16 | `shl shr`                              | left  |
| 14 | `= /= < > <= >=`                       | none  |
| 12 | `and`                                  | left  |
| 10 | `xor`                                  | left  |
| 8  | `or`                                   | left  |
| 7  | `-> ~>`                                | right |
| 6  | `|>`                                   | left  |
| 2  | `<-`                                   | right |

## EBNF

Notation:

- `X?` optional, `X*` repetition, `X+` one-or-more
- terminals are quoted (`"let"`) or shown as token names (`IDENT`)

### Root

```
root      = stmt* EOF ;
stmt      = expr ";" ;
```

### Expressions (Pratt)

This is the *contract* for a Pratt parser; the exact EBNF expansion can be derived from the table above.

Atoms (unique FIRST tokens):

```
atom =
    literal
  | splice
  | ident
  | op_ident
  | pi_expr
  | paren_form
  | array_type_expr
  | array_lit
  | record_lit
  | dot_prefix
  | case_expr
  | let_expr
  | resume_expr
  | import_expr
  | data_expr
  | effect_expr
  | class_expr
  | instance_expr
  | perform_expr
  | handle_expr
  | foreign_expr
  | quote_expr
  | with_attrs_expr
  ;
```

Postfix ops:

```
postfix =
    "(" arg_list? ")"
  | "[" expr_list? "]"
  | ".[" expr_list? "]"
  | ".{" record_fields? "}"
  | "." field_target
  | "?." field_target
  | "!." field_target
  | ":?" expr ("as" ident)?
  | ":?>" expr
  ;
```

### Bindings

Single modifier slot: exactly one of `mut` or `rec`.

```
let_modifier = "mut" | "rec" ;

let_expr =
  "let" let_modifier? pattern
    bracket_params?
    params?
    where_clause?
    with_clause?
    type_annot?
  ":=" expr ;
```

### Typed Positions

There is no separate `type_expr` grammar. Any typed position reuses `expr`, and semantic phases decide which expressions are valid there.

That includes:

- annotations `":" expr`
- variant payloads and record field declarations
- effect items and `where` constraints
- built-in universe names like `Type` and `Type0`, which are ordinary identifiers in the lexer/parser

### Patterns

`as` direction is `pat as name`.

```
pattern        = pattern_as ("or" pattern_as)* ;
pattern_as     = pattern_primary ("as" ident)? ;
pattern_primary =
    "_" | literal | ident
  | "." ident ("(" pat_list? ")")?
  | "{" rec_pat_fields? "}"
  | "(" pat_list? ")"
  | "[" pat_list? "]"
  ;
```

### Preserved Type-Valued Surface Forms

The first-class cleanup preserves the existing spellings as ordinary expressions:

```
pi_expr         = "(" ident ":" expr ")" ("->" | "~>") expr ;
array_type_expr = "[" dim_list? "]" prefix_expr ;
```

Bracket application remains general:

```
apply_expr = expr "[" expr_list? "]" ;
```

Indexing remains distinct:

```
index_expr = expr ".[" expr_list? "]" ;
```

## CST shaping rules (“no unnecessary nodes”)

The syntax layer produces a full-fidelity CST, but it must avoid “padding nodes”.

Rules:

- Do **not** create nodes for separator padding (leading/trailing commas/pipes/semicolons).
- Prefer representing parentheses by **rewrapping** an existing node to include surrounding tokens, rather than introducing a `GroupExpr` wrapper node.
- Create list nodes only when they carry meaning (e.g., `ParamList`, `ArgList`), not for generic `ExprList` unless it improves tooling.
- Every construct corresponds to exactly one node kind (no “ExprLevel17” nodes mirroring precedence tiers).

Minimal node inventory (term layer):

- `Root`, `Stmt`
- `Literal`, `Name`, `Binary`, `Prefix`
- `Call`, `Apply`, `Index`, `RecordUpdate`, `FieldAccess`, `OptionalChain`, `ForceAccess`, `TypeTest`, `TypeCast`
- `Lambda`, `Pi`, `Tuple`, `Sequence`, `Array`, `Record`, `Variant`
- `Case` (+ `CaseArm`), `Handle` (+ `HandleClause`)
- `Let`, `Import`, `Resume`, `Perform`, `Instance`, `Quote`, `Splice`
- `Attr` (+ `AttrArg`, `AttrValue`)

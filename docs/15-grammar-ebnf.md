# Grammar (EBNF + Precedence)

`grammar/Musi.g4` is the canonical, tool-supported grammar.

This document is the compact, implementer-facing formalization intended to be easy to implement in “primitive simple” languages (C/Go) with:

- a hand-written lexer
- recursive descent for statements/constructs
- a flat infix-chain parse for expressions, with precedence/associativity resolved semantically via fixity declarations

It also defines CST shaping rules (“no unnecessary nodes”).

## Tokens (summary)

Keywords:

`and as case class data effect export foreign forall handle if import in infix infixl infixr instance law let mut rec not of opaque or perform quote resume shl shr where with xor`

Fixed punctuation / operators (maximal munch):

`:= -> ~> => /= <= >= <: ... .{ .[ :? :?> |>`

Singles:

`@ # \ ( ) [ ] { } , ; . : | + - * / % = < > _`

Literals:

- `INT_LIT`, `FLOAT_LIT`
- `STRING_LIT`, `RUNE_LIT`
- template literals (backticks) tokenized as `TEMPLATE_NO_SUBST` or `TEMPLATE_HEAD`/`TEMPLATE_MIDDLE`/`TEMPLATE_TAIL`
- `IDENT`

## Operator precedence (semantic)

Higher number = tighter binding.

This is **not** a parser contract anymore. Parsing produces a flat infix chain (see EBNF below), and a later phase folds it into a tree using:

- built-in fixities for built-in operators/keywords
- explicit fixity declarations (`infixl`/`infixr`/`infix`) for user-defined symbolic operators

| BP  | Operators / forms                                          | Assoc |
| --- | ---------------------------------------------------------- | ----- |
| 26  | calls `()`, bracket apply `[]`, postfix field/index/update | left  |
| 24  | prefix `- not mut`                                         | right |
| 20  | `* / %`                                                    | left  |
| 18  | `+ -`                                                      | left  |
| 16  | `shl shr`                                                  | left  |
| 14  | `= /= < > <= >=`                                           | none  |
| 12  | `and`                                                      | left  |
| 10  | `xor`                                                      | left  |
| 8   | `or`                                                       | left  |
| 7   | `-> ~>`                                                    | right |
| 6   | `                                                          | >`    | left |
| 2   | `:=`                                                       | right |

### Fixity declarations

User-defined symbolic operators (and optionally operator spellings like `(+)`) may have their precedence/associativity declared at the root level:

```musi
infixl 6 (++);
infixr 5 (**);
infix 4 (<=>);
```

- `infixl n op` left-associative
- `infixr n op` right-associative
- `infix n op` non-associative

## EBNF

Notation:

- `X?` optional, `X*` repetition, `X+` one-or-more
- terminals are quoted (`"let"`) or shown as token names (`IDENT`)

### Root

```
root      = root_stmt* EOF ;
root_stmt = fixity_decl | stmt ;
stmt      = expr ";" ;

fixity_decl = ("infixl" | "infixr" | "infix") INT_LIT op_ident ";" ;
```

### Expressions (flat infix chain)

This is the parser contract:

- Parse `expr` as a flat chain of `prefix_expr` separated by `infix_op`.
- Then fold the chain into an AST using the fixity environment.

Atoms (unique FIRST tokens):

```
atom =
    literal
  | template_lit
  | splice
  | ident
  | op_ident
  | pi_expr
  | lambda_expr
  | paren_form
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

Template literals (lexer-driven tokens):

```
template_lit =
    TEMPLATE_NO_SUBST
  | TEMPLATE_HEAD expr (TEMPLATE_MIDDLE expr)* TEMPLATE_TAIL
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
  | ":?" expr ("as" ident)?
  | ":?>" expr
  ;
```

Infix chains:

```
expr       = infix_expr ;
infix_expr = prefix_expr (infix_op prefix_expr)* ;
```

`infix_op` includes built-in operators/keywords plus user-defined `SYMBOLIC_OP`.

### Bindings

Single modifier slot: `rec`.

```
let_modifier = "rec" ;

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
pi_expr = "forall" "(" ident ":" expr ")" ("->" | "~>") expr ;
lambda_expr = "\\" params (":" expr)? "=>" expr ;
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

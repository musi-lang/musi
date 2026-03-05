# Musi — notes for AI assistants

## Language design rationale

Musi has two non-negotiable design constraints. Every proposed grammar change must satisfy both.

### 1. Strict LL(1)

The grammar is strictly LL(1): every parse decision requires **exactly one token of lookahead**, no exceptions. This means:

- No left recursion (expressions use Pratt / iterated `{...}` instead)
- All alternatives in every rule have disjoint FIRST sets
- When a nullable alternative exists, FIRST and FOLLOW sets must also be disjoint
- LL(2) is not acceptable, even "just this once"

When proposing new syntax, compute FIRST sets explicitly and verify disjointness before suggesting.

### 2. Mathematical purity

Syntax must reflect type-theoretic meaning directly:

| Concept | Theory | Separator | Delimiter |
|---|---|---|---|
| Record | Product `A × B` | `,` (conjunction) | `{}` |
| Choice | Sum `A + B` | `\|` (disjunction) | `{}` |

Consequences:
- `choice` uses `|` between variants, `{}` for the body — not `()`, not `case` prefix
- `record` uses `,` between fields, `{}` for the body
- Grouping in types uses `()` only where product-type semantics apply — no separate `ast_ty_group`
- Spread uses `<..` (a compound token; `<` never precedes `..` in valid expressions since `..` is strictly infix)

## Key grammar decisions

- `ast_ty_named = lex_id, ["[", [ty_list], "]"]` — left-factored; covers both bare type names and type applications
- `ast_pat_ident = lex_id, [suffix]` — left-factored; covers variable binding, positional sum patterns, and named-field patterns
- `ast_expr_paren` — left-factored `(` form covering unit `()`, single expression `(e)`, tuple `(e, ...)`, and block `(e; ...)`
- `ast_expr_with_prefix = [ast_attrs], ast_expr_after_attrs` — all constructs with optional attribute prefix go through this single rule; dispatches on keyword
- `ast_fn_kind` — unified rule for `fn`: `lex_id ...` = named definition, `[ty_params] params ... =>` = lambda; LL(1) because a name (letter) and `(` are disjoint first tokens
- `ast_expr_label = "label", lex_id, ast_block` — labeled blocks replace the old `name: while` prefix syntax; `label` is a keyword so FIRST is unambiguous
- `ast_rec_lit_field = (field_base, bind_init) | (spread_op, ast_expr)` — `<..` spread is LL(1) since it never starts `field_base`
- `ast_rec_update` does not exist as a separate rule — functional record update is `.{ <..base, field := val }` using spread

## Key files

- `grammar.ebnf` — canonical language grammar; the source of truth
- `crates/` — Rust implementation
- `tools/vscode/syntaxes/musi.tmLanguage.json` — VS Code TextMate highlighting
- `tools/vscode/snippets/musi_snippets.json` — VS Code snippets

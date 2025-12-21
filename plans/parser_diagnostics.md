# Parser Diagnostics Implementation Plan

## Objective

Refactor `Parser.ml` to use the centralized `Errors.ml` system, replacing ad-hoc string error messages with typed, structured diagnostics. This ensures consistency with the Lexer and adheres to the new "Error Archetypes" defined in `docs/DIAGNOSTICS.md`.

## 1. Schema Extensions (`Errors.ml`)

We will introduce a `syntactic_error` variant to matching the `lexical_error` structure.

```ocaml
type syntactic_error =
  | UnexpectedToken of { expected: string option; got: Token.t }
  | MissingToken of { expected: Token.t }
  | ExpectedIdent of { got: Token.t }
  | ExpectedExpr of { got: Token.t }
  | ExpectedType of { got: Token.t }
  | InvalidModifier of { modifier: string; valid_ctx: string }
  | UnclosedBlock of { open_tok: Token.t }
  | MalformedExpr of string (* e.g. "missing '{' after record expression" *)
```

## 2. Taxonomy Alignment

All messages must follow the `docs/DIAGNOSTICS.md` archetypes:

- **Unexpected**: `unexpected %0`
  - *Msg*: "unexpected token 'val'"
  - *Hint*: "expected expression" or "expected ';'"
- **Missing**: `missing %0`
  - *Msg*: "missing ';'"
  - *Hint*: "add ';' to end statement"
- **Unclosed**: `unclosed %0`
  - *Msg*: "unclosed record literal"
  - *Hint*: "expected '}'"

## 3. Specific Parser Changes

### Variable Declarations

- **Old**: `error_at "expected binding keyword"`
- **New**: `report (Parse (UnexpectedToken { expected = Some "val/var"; got = t }))`

### Generic Instantiation

*With the new `[...]` syntax:*

- **Old**: Checks for `<`
- **New**: Must enforce matching `[` and `]`.
- **Error**: `MissingToken { expected = Token.RBrack }` if `]` is missing after type arguments.

### Statements

- **Semicolons**: Mandatory check.
- **Error**: `MissingToken { expected = Token.Semicolon }`
- **Hint**: "statements must be terminated with ';'"

## 4. Recovery Strategy

The Parser handles errors by synchronizing. This logic remains but will now emit typed errors before skipping.

1. **Emit**: `Reporter.report (Errors.Parse err) span`
2. **Sync**: Skip tokens until `is_sync_char` (`;`, `}`, etc).

## 5. Generic Syntax Migration

The parser implementation must be updated to support the Logic Change from `<T>` to `[T]`:

1. `parse_ty_app`: Expect `[` instead of `<`.
2. `parse_fn_sig`: Expect `[` for type params.
3. `parse_expr_binary`: `<` is now strictly a comparison operator. Remove any "is this a generic?" lookahead logic.

## 6. Verification Steps

1. **Update `Errors.ml`**: Add `parse_error` type.
2. **Refactor `Parser.ml`**: Replace all `error_at` strings with constructors.
3. **Fix Generics Parsing**: Implement `[` / `]` logic.
4. **Update Tests**: Rewrite `parse_test.ml` to expect new error messages and new syntax (`List[T]`).

# §2 — Lexical

## 2.1 Character Classes

```ebnf
letter  = A–Z | a–z
digit   = 0–9
xdigit  = digit | A–F | a–f
```

## 2.2 Identifiers

```ebnf
ident         = plain_ident | escaped_ident ;
plain_ident   = letter , { letter | digit | "_" } ;
escaped_ident = "`" , { any - "`" } , "`" ;
ty_ident      = "'" , letter , { letter | digit | "_" } ;
op_ident      = "(" , op_chars , ")" ;
```

- Type variables prefix with `'` — visually distinct from value identifiers.
- Escaped identifiers allow any characters between backticks (interop with reserved words).
- `op_ident` wraps overloadable operator symbols in `()` for first-class use.

## 2.3 Literals

```ebnf
lit_int     = dec_int | hex_int | oct_int | bin_int ;
dec_int     = digit , { digit | "_" } ;
hex_int     = "0x" , xdigit , { xdigit | "_" } ;
oct_int     = "0o" , ("0".."7") , { ("0".."7") | "_" } ;
bin_int     = "0b" , ("0"|"1") , { ("0"|"1") | "_" } ;
lit_float   = digit , { digit } , "." , digit , { digit } ,
              [ ("e"|"E") , ["+"|"-"] , digit , { digit } ] ;
lit_string  = '"' , { str_char } , '"' ;
lit_fstring = 'f"' , { fstr_part } , '"' ;
lit_rune    = "'" , rune_char , "'" ;
fstr_part   = "{" , ast_expr , "}" | str_char ;
str_char    = "\\" , any | any - '"' ;
rune_char   = "\\" , any | any - "'" ;
```

Numeric separator: `1_000_000`. Scientific: `1.5e10`.

## 2.4 Compound Tokens (maximal munch)

| Token | Meaning |
|-------|---------|
| `:=`  | binding / initialisation |
| `<-`  | mutation — `var` bindings only |
| `->`  | pure function arrow |
| `~>`  | effectful function arrow |
| `<:`  | subtype constraint — type position only |
| `:>`  | supertype constraint — type position only |
| `?.`  | optional chain |
| `??`  | nil coalesce |
| `..`  | inclusive range |
| `..<` | exclusive range |
| `...` | spread / splat |
| `<<`  | left shift |
| `>>`  | right shift |
| `/=`  | inequality (`!=` does not exist) |
| `<=`  | less-or-equal |
| `>=`  | greater-or-equal |
| `\|>` | pipe |
| `::`  | cons |

## 2.5 Keywords

`and` `await` `class` `defer` `exists` `export` `forall` `given` `if` `import` `in` `inout` `law` `let` `match` `not` `of` `or` `over` `ref` `return` `spawn` `try` `under` `var` `where` `xor`

> `Int` `String` `Bool` etc. are stdlib identifiers, not keywords. They can be shadowed.

## 2.6 Operator Classes

**Overloadable** (via typeclass): `+ - * / % = /= < > <= >= << >> :: |>`

**Not overloadable**: `:= <- -> ~> . ?. .. ..< ... ?? : <: :> and or not xor`

`and`/`or`/`not`/`xor` are **type-directed**: logical on `Bool`, bitwise on integers. No separate `&&`/`||`/`!`/`&`/`|`/`^`/`~` operators exist.

## 2.7 Semicolons

Mandatory statement terminators. No automatic semicolon insertion. A missing `;` is a syntax error. This makes the grammar fully context-free — the parser never inspects newlines.

## 2.8 Comments

```ebnf
line_comment = "//" , { any - "\n" } ;
doc_comment  = "///" , { any - "\n" } ;
block_comment = "/*" , { any } , "*/" ;   // does not nest
```

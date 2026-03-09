# §8 — Formal Grammar

ISO/IEC 14977 EBNF. Maximal munch throughout.

## 8.1 Critical Rules

- Semicolons are **mandatory** after every `ast_stmt`. No ASI.
- `var` appears in `ast_var` and `ast_pat` only — never inside `ast_ty`.
- `inout` appears in `param` mode position only — precedes the name, not the type.
- `mut` is **not a keyword**. It does not exist.
- `/=` is inequality. `!=` does not exist.
- `and`/`or`/`not`/`xor` are keywords — no `&&`/`||`/`!`/`&`/`|`/`^`/`~`.
- Record type fields: **semicolons**. Record literal fields: **commas**.
- Record literals use leading dot: `.{ ... }` or `Name.{ ... }`.
- `(,)` = empty tuple. `(;)` = empty sequence. `()` = unit.

## 8.2 Lexical Grammar

```ebnf
letter    = "A".."Z" | "a".."z" ;
digit     = "0".."9" ;
xdigit    = digit | "A".."F" | "a".."f" ;

ident         = plain_ident | escaped_ident ;
plain_ident   = letter , { letter | digit | "_" } ;
escaped_ident = "`" , { any - "`" } , "`" ;
ty_ident      = "'" , letter , { letter | digit | "_" } ;
op_ident      = "(" , op_chars , ")" ;
op_chars      = "+" | "-" | "*" | "/" | "%" | "=" | "/="
              | "<" | ">" | "<=" | ">=" | "<<" | ">>"
              | "::" | "|>" ;

lit        = lit_int | lit_float | lit_string | lit_fstring | lit_rune ;
lit_int    = dec_int | hex_int | oct_int | bin_int ;
dec_int    = digit , { digit | "_" } ;
hex_int    = "0x" , xdigit , { xdigit | "_" } ;
oct_int    = "0o" , ("0".."7") , { ("0".."7") | "_" } ;
bin_int    = "0b" , ("0"|"1") , { ("0"|"1") | "_" } ;
lit_float  = digit , { digit } , "." , digit , { digit } ,
             [ ("e"|"E") , ["+"|"-"] , digit , { digit } ] ;
lit_string  = '"' , { str_char } , '"' ;
lit_fstring = 'f"' , { fstr_part } , '"' ;
lit_rune    = "'" , rune_char , "'" ;
fstr_part  = "{" , ast_expr , "}" | str_char ;
str_char   = "\\" , any | any - '"' ;
rune_char  = "\\" , any | any - "'" ;

comment = "//" , { any - newline }
        | "///" , { any - newline }
        | "/*" , { any } , "*/" ;   (* block comments do not nest *)
```

## 8.3 Parser Grammar

```ebnf
ast_root  = { ast_stmt } ;
ast_stmt  = ast_expr , ";" ;

(* Precedence: lower rule = tighter binding.
   Boolean-algebra model: not > and > xor > or > comparisons.
   This differs from C — do not apply C precedence. *)

ast_expr          = ast_expr_nil_coal , [ "<-" , ast_expr ] ;
ast_expr_nil_coal = ast_expr_pipe , [ "??" , ast_expr_nil_coal ] ;
ast_expr_pipe     = ast_expr_cmp , { "|>" , ast_expr_cmp } ;
ast_expr_cmp      = ast_expr_disj ,
                    [ ( "=" | "/=" | "<" | ">" | "<=" | ">=" | "in" ) ,
                      ast_expr_disj ] ;
ast_expr_disj     = ast_expr_xor , { "or" , ast_expr_xor } ;
ast_expr_xor      = ast_expr_conj , { "xor" , ast_expr_conj } ;
ast_expr_conj     = ast_expr_range , { "and" , ast_expr_range } ;
ast_expr_range    = ast_expr_cons , [ ( ".." | "..<" ) , ast_expr_cons ] ;
ast_expr_cons     = ast_expr_sft , [ "::" , ast_expr_cons ] ;
ast_expr_sft      = ast_expr_add , { ( "<<" | ">>" ) , ast_expr_add } ;
ast_expr_add      = ast_expr_mul , { ( "+" | "-" ) , ast_expr_mul } ;
ast_expr_mul      = ast_expr_pre , { ( "*" | "/" | "%" ) , ast_expr_pre } ;
ast_expr_pre      = ( "-" | "not" ) , ast_expr_pre | ast_expr_post ;
ast_expr_post     = ast_expr_atom , { postfix_op } ;

(* No separate bitwise layers — and/or/xor/not are type-directed.
   No & | ^ ~ operators exist. *)

postfix_op = "(" , [ arg_list ] , ")"
           | ".[" , [ expr_list ] , "]"
           | ".{" , rec_lit_fields , "}"
           | "." , ( ident | lit_int )
           | "?." , ( ident | lit_int ) ;

ast_expr_atom =
    lit | ident
  | ast_paren | ast_array | ast_rec_lit
  | ast_dot_pfx | ast_piecewise | ast_match
  | ast_let | ast_var | ast_return | ast_defer
  | ast_spawn | ast_await | ast_try | ast_import
  | ast_forall | ast_exists
  | ast_with_attrs ;

(* Parenthesised forms — delimiter determines kind.
   ()   = unit
   (,)  = empty tuple
   (;)  = empty sequence, evaluates to ()
   (a)  = parenthesised expr
   (a,) = single-element tuple
   (a, b) = tuple
   (a; b) = sequence, returns b
   (a; b;) = sequence, trailing ; discards, returns () *)
ast_paren    = "(" , paren_body , ")" , [ fn_tail ] ;
fn_tail      = [ ":" , ast_ty ] , ( "->" | "~>" ) , ast_expr ;

(* When fn_tail is present, paren contents are reinterpreted as parameters.
   ":" before the arrow is the return type annotation.
   "->" = pure function, "~>" = effectful function.
   The parser validates parameter syntax after disambiguation. *)
paren_body   =
    (* empty *)                                       (* unit: () *)
  | ","                                               (* empty tuple: (,) *)
  | ";"                                               (* empty sequence: (;) *)
  | ast_expr , paren_cont ;
paren_cont   =
    (* empty *)                                       (* parenthesised expr: (a) *)
  | "," , [ expr_list ] , [ "," ]                    (* tuple *)
  | ";" , { ast_stmt } , ast_expr                    (* sequence, returns last expr *)
  | ";" , { ast_stmt } , ";" ;                       (* sequence, trailing ;, returns () *)

ast_array    = "[" , [ array_items ] , "]" ;
array_items  = array_item , { "," , array_item } ;
array_item   = "..." , ast_expr | ast_expr ;

(* Record literals: leading dot, comma-separated, := initialisers.
   Named: Point.{ x := 1, y := 2 }   Anonymous: .{ x := 1, y := 2 } *)
ast_rec_lit      = [ ident ] , ".{" , [ rec_lit_fields ] , "}" ;
rec_lit_fields   = rec_lit_field , { "," , rec_lit_field } ;
rec_lit_field    = ident , [ ":=" , ast_expr ] | "..." , ast_expr ;

(* Piecewise — sole conditional form. No if/else. No ternary. *)
ast_piecewise = "(" , pw_arm , { "|" , pw_arm } , ")" ;
pw_arm        = ast_expr , "if" , pw_guard ;
pw_guard      = "_" | ast_expr ;

ast_match    = "match" , ast_expr ,
               "(" , match_arm , { "|" , match_arm } , ")" ;
match_arm    = [ attrs ] , ast_pat , [ "if" , ast_expr ] , "=>" , ast_expr ;

(* Immutable binding. ref = heap allocation. in = ML-style scope. *)
ast_let      = "let" , [ "ref" ] , ast_pat ,
               [ ty_annot ] , ":=" , ast_expr ,
               [ "in" , "(" , ast_expr , ")" ] ;

(* Mutable binding. var appears here and only here — never in ast_ty. *)
ast_var      = "var" , [ "ref" ] , ast_pat ,
               [ ty_annot ] , ":=" , ast_expr ;

ast_dot_pfx  = "." , ident , [ "(" , [ expr_list ] , ")" ] ;
ast_return   = "return" , [ ast_expr ] ;
ast_defer    = "defer" , ast_expr ;   (* ast_paren handles block form *)
ast_spawn    = "spawn" , ast_expr ;
ast_await    = "await" , ast_expr ;
ast_try      = "try" , ast_expr ;
ast_import   = "import" , lit_string ;
ast_forall   = "forall" , ty_param_list , [ where_clause ] , "->" , ast_expr ;
ast_exists   = "exists" , ty_param_list , [ where_clause ] , "->" , ast_expr ;

ast_with_attrs = attrs , ast_decl ;
ast_decl =
    [ "export" ] , ( ast_let | ast_var )
  | "export" , "{" , export_list , "}" , [ ":=" , ast_import ]
  | ast_class_decl
  | ast_given_decl
  | ast_effect_decl ;

ast_class_decl  = "class" , ident , [ "over" , ty_param_list ] ,
                  [ where_clause ] ,
                  "{" , { class_member } , "}" ;
ast_given_decl  = "given" , ast_ty_named , [ "over" , ty_param_list ] ,
                  [ where_clause ] ,
                  "{" , { class_member } , "}" ;
ast_effect_decl = "effect" , ident , [ "of" , ty_param_list ] ,
                  "{" , { effect_op , ";" } , "}" ;
effect_op       = ident , ":" , ast_ty ;
class_member   = ( fn_decl | law_decl ) , ";" ;
fn_decl        = "let" , op_or_ident , params , [ ty_annot ] ,
                 [ ":=" , ast_expr ] ;
law_decl       = "law" , ident , ":=" , ast_expr ;
op_or_ident    = ident | op_ident ;
params         = "(" , [ param_list ] , ")" ;
param_list     = param , { "," , param } ;

(* Parameter modes — inout precedes name, not type.
   No modifier  = immutable copy
   var          = mutable local copy
   inout        = copy-in copy-out, Ada-style, clean call site *)
param          = [ "inout" | "var" ] , ident , [ ty_annot ] , [ ":=" , ast_expr ] ;

export_list    = export_item , { "," , export_item } ;
export_item    = ident , [ "as" , ident ] ;

(* Types. var never appears here. inout never appears here.
   Record type fields: semicolons.
   ref T = heap-allocated reference type.  *)
ast_ty       = ast_ty_arrow ;
ast_ty_arrow = ast_ty_eff , { ( "->" | "~>" ) , ast_ty_eff } ;
ast_ty_eff   = ast_ty_sum , [ "under" , effect_set ] ;
ast_ty_sum   = ast_ty_prod , { "+" , ast_ty_prod } ;
ast_ty_prod  = ast_ty_base , { "*" , ast_ty_base } ;
ast_ty_base  =
    ty_ident
  | "?" , ast_ty_base
  | "ref" , ast_ty_base
  | ident , [ "of" , ty_arg_list ]
  | "(" , [ ty_list ] , ")"
  | "[" , [ lit_int ] , "]" , ast_ty
  | "{" , rec_ty_fields , [ ";" , "..." ] , "}"
  | "{" , ast_ty , "|" , ast_expr , "}"
  | "forall" , ty_param_list , [ where_clause ] , "->" , ast_ty
  | "exists" , ty_param_list , [ where_clause ] , "->" , ast_ty ;

(* Record type fields: semicolons as separators.
   Fields may have default values. *)
rec_ty_fields  = rec_ty_field , { ";" , rec_ty_field } ;
rec_ty_field   = ident , ":" , ast_ty , [ ":=" , ast_expr ] ;

ty_list        = ast_ty , { "," , ast_ty } ;
ty_arg_list    = ast_ty , { "," , ast_ty } ;
ty_param_list  = ty_ident , { "," , ty_ident } ;
ty_annot       = ":" , ast_ty ;
ty_named_list  = ast_ty_named , { "," , ast_ty_named } ;
ast_ty_named   = ident , [ "of" , ty_arg_list ] ;
effect_set     = "{" , effect_list , "}" ;
effect_list    = effect_item , { "," , effect_item } ;
effect_item    = ident , [ "of" , ast_ty ] | ty_ident ;
where_clause   = "where" , constraint , { "," , constraint } ;
constraint     = ty_ident , ( "<:" | ":>" ) , ast_ty_named ;

(* Patterns. var in pattern = mutable bind. . prefix = UDN destructor. *)
ast_pat         = ast_pat_primary ,
                  [ "or" , ast_pat_primary , { "or" , ast_pat_primary } ] ;
ast_pat_primary =
    "_" | lit
  | [ "var" ] , ident , [ pat_suffix ]
  | "." , ident , [ "(" , [ pat_list ] , ")" ]
  | "{" , [ rec_pat_fields ] , "}"
  | "(" , [ pat_list ] , ")"
  | "[" , [ pat_list ] , "]" ;
pat_suffix      = "(" , [ pat_list ] , ")" | "{" , [ rec_pat_fields ] , "}" ;
rec_pat_fields  = rec_pat_field , { "," , rec_pat_field } ;
rec_pat_field   = [ "var" ] , ident , [ ":" , ast_pat ] ;
pat_list        = ast_pat , { "," , ast_pat } ;

attrs      = attr , { attr } ;
attr       = "#[" , attr_body , "]" ;
attr_body  = ident | ident , ":=" , attr_value ;
attr_value = lit | "(" , lit , { "," , lit } , ")" ;

expr_list  = ast_expr , { "," , ast_expr } ;
arg_list   = arg , { "," , arg } ;
arg        = "..." | ast_expr ;
```

## 8.4 FIRST & FOLLOW (selected)

| Non-terminal | FIRST |
|---|---|
| `ast_expr` | `lit ident ( [ .{ . match let var forall exists return defer spawn await try import export class given effect #[` |
| `ast_ty` | `ident ' ? ( [ { ref forall exists` |
| `ast_pat` | `_ lit ident var . { ( [` |
| `ast_piecewise` | `(` |
| `postfix_op` | `( .[ .{ . ?.` |

| Non-terminal | FOLLOW |
|---|---|
| `ast_expr` | `; ) ] , => if \| in` |
| `ast_ty` | `, ) ] } => ; \| of over under where` |
| `ast_pat` | `=> if , ) ]` |

## 8.5 LL(1) Disambiguation

| Decision point | Resolution |
|---|---|
| `let...in` vs `let` stmt | After `let pat := expr`: `in` → scoped; anything else → stmt |
| `let` vs `var` | Distinct keywords, distinct productions |
| `*` in type vs expr | Context always known from enclosing production |
| Piecewise `_` vs expr | `_` checked first in `pw_guard` |
| `(` paren vs piecewise | Piecewise is `( expr if guard \| ... )` — `if` after first expr disambiguates |
| `.{` record literal vs `.` field | `.{` is atom-level `ast_rec_lit`; `.` postfix is field access |
| `{` record type vs refinement | Record: fields followed by `;`. Refinement: type followed by `\|` |
| `inout`/`var` in param vs type | `inout`/`var` appear only in `param` production — not reachable from `ast_ty` |
| `and`/`or`/`xor` in expr vs pattern | `or` in pattern position = or-pattern; context always known |
| `()` vs `(,)` vs `(;)` | Empty parens: next token `)` = unit, `,` = empty tuple, `;` = empty sequence |

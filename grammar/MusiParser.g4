// Musi Language - ANTLR4 Parser Grammar
// 
// Canonical parser half of the tool-supported Musi grammar. Token definitions and template
// interpolation modes live in `MusiLexer.g4`.

parser grammar MusiParser;

options {
	tokenVocab = MusiLexer;
}

// ----------------------------------------------------------------------------- Parser
// 
// Surface policy: - `let` names values. Form keywords (`data`, `shape`, `import`, `if`, `match`,
// `defer`, `yield`, `unsafe`, `pin`) build expressions; they do not name values. - Keywords never
// become callees. `import "a"` and tuple imports like `import ("a", "b")` remain keyword syntax,
// not calls on an identifier named `import`. - Structural blocks use `{ ... }`; imperative/sequence
// blocks use `( ... )`. - Lambdas must start with `\`, so `=>` can remain unambiguous branch-arm
// syntax too.

root: root_stmt* EOF;

root_stmt: declaration SEMICOLON | stmt;

// Bodyless `let` declarations require semantic permission such as `@axiom`.
declaration: fn_decl;

stmt: expr SEMICOLON;

// --- Expressions (semantic precedence; parse as flat infix chain) ---

expr: infix_expr;

infix_expr: prefix_expr (infix_op prefix_expr)*;

infix_op:
	COLON_EQ
	| COLON_QUESTION_GT
	| COLON_GT
	| PIPE_GT
	| MINUS_GT
	| QUESTION_QUESTION
	| KW_OR
	| KW_XOR
	| KW_AND
	| EQ
	| SLASH_EQ
	| LT
	| GT
	| LT_EQ
	| GT_EQ
	| TILDE_EQ
	| DOT_DOT
	| DOT_DOT_LT
	| KW_IN
	| PLUS
	| MINUS
	| STAR
	| SLASH
	| PERCENT;

prefix_expr: (MINUS | KW_KNOWN | KW_NOT | KW_MUT) prefix_expr
	| postfix_expr;

postfix_expr: atom postfix_op*;

postfix_op: call_op | bracket_apply_op | access_op;

call_op: LPAREN arg_list? RPAREN;

bracket_apply_op: LBRACKET expr_list? RBRACKET;

access_op:
	DOT field_target
	| DOT_LBRACKET expr_list? RBRACKET
	| DOT_LPAREN op_name RPAREN;

field_target: ident | INT_LIT;

// --- Atoms (unique FIRST tokens) ---

atom:
	literal
	| template_expr
	| ident
	| op_ident
	| lambda_expr
	| paren_expr
	| array_lit_expr
	| record_literal_expr
	| dot_prefix_expr
	| if_expr
	| match_expr
	| let_expr
	| defer_expr
	| yield_expr
	| import_expr
	| data_expr
	| shape_expr
	| unsafe_expr
	| pin_expr
	| with_mods_expr;

literal: INT_LIT | FLOAT_LIT | STRING_LIT | RUNE_LIT;

template_expr:
	TEMPLATE_NO_SUBST
	| TEMPLATE_HEAD expr (TEMPLATE_MIDDLE expr)* TEMPLATE_TAIL;

ident: IDENT;

op_ident: LPAREN op_name RPAREN;

op_name: op_single | word_op;

op_single:
	PLUS
	| MINUS
	| STAR
	| SLASH
	| PERCENT
	| EQ
	| SLASH_EQ
	| LT
	| LT_EQ
	| GT
	| GT_EQ;

word_op: KW_AND | KW_IN | KW_NOT | KW_OR | KW_XOR;

lambda_expr: BACKSLASH params (COLON expr)? EQ_GT expr;

paren_expr:
	LPAREN RPAREN
	| LPAREN grouped_or_tuple_body RPAREN
	| LPAREN sequence_body RPAREN;

grouped_or_tuple_body: expr (COMMA expr_list? COMMA?)?;

sequence_body: expr (SEMICOLON expr)* SEMICOLON?;

match_expr:
	KW_MATCH expr LPAREN PIPE? match_arm (PIPE match_arm)* PIPE? RPAREN;

match_arm: attrs? pattern EQ_GT expr;

array_lit_expr:
	LBRACKET comma_pad array_items? comma_pad RBRACKET;

array_items: array_item (COMMA array_item)*;

array_item: spread | expr;

record_literal_expr: LBRACE record_fields RBRACE;

record_fields:
	comma_pad (record_field (COMMA record_field)*)? comma_pad;

record_field: ident (COLON_EQ expr)? | spread;

spread: DOT_DOT_DOT expr;

dot_prefix_expr: DOT ident (LPAREN variant_arg_list? RPAREN)?;

variant_arg_list: variant_arg (COMMA variant_arg)* COMMA?;

variant_arg: ident COLON_EQ expr | expr;

if_expr: KW_IF expr KW_THEN expr KW_ELSE expr;

defer_expr: KW_DEFER expr (KW_WHERE expr)?;

yield_expr: KW_YIELD expr;

import_expr: KW_IMPORT expr;

let_expr:
	KW_LET KW_RECUR? let_head bracket_params? params? type_annot? where_clause? COLON_EQ expr (
		KW_ELSE expr
	)?;

let_head: receiver_method_head | pattern;

receiver_method_head: LPAREN ident type_annot RPAREN DOT ident;

bracket_params:
	LBRACKET bracket_param (COMMA bracket_param)* COMMA? RBRACKET;

bracket_param: ident type_annot?;

data_expr: KW_DATA LBRACE data_body RBRACE;

data_body: variant_list | rec_def_fields | PIPE | SEMICOLON;

variant_list: PIPE? variant (PIPE variant)* PIPE?;

variant:
	attrs? ident variant_payload_defs? (MINUS_GT expr)? (
		COLON_EQ expr
	)?;

variant_payload_defs:
	LPAREN variant_payload_def (COMMA variant_payload_def)* COMMA? RPAREN;

variant_payload_def: ident COLON expr | expr;

rec_def_fields:
	SEMICOLON? rec_def_field (SEMICOLON rec_def_field)* SEMICOLON?;

rec_def_field: KW_LET ident COLON expr (COLON_EQ expr)?;

shape_expr:
	KW_SHAPE (KW_WHERE constraint (COMMA constraint)* COMMA?)? LBRACE structural_members RBRACE;

unsafe_expr: KW_UNSAFE paren_expr;

pin_expr: KW_PIN expr KW_AS IDENT KW_IN expr;

with_mods_expr:
	attrs modifier* (expr | let_expr)
	| modifier+ (expr | let_expr);

modifier: attr | export_mod | hidden_mod;

hidden_mod: KW_HIDDEN;

export_mod: KW_EXPORT;

let_rest:
	pattern bracket_params? params? type_annot? where_clause? (
		COLON_EQ expr
	)?;

fn_decl:
	attrs? KW_LET op_or_ident bracket_params? params? type_annot? (
		COLON_EQ expr
	)?;

op_or_ident: ident | op_ident;

// Structural member lists accept leading and trailing separators, matching `{ ; x; }`.
structural_members:
	SEMICOLON* (
		structural_member (SEMICOLON+ structural_member)* SEMICOLON*
	)?;

structural_member: fn_decl;

// --- Unified annotation / constraint helpers ---

type_annot: COLON type_expr;

type_expr: QUESTION type_expr | type_expr BANG type_expr | expr;

where_clause: KW_WHERE constraint (COMMA constraint)* COMMA?;

constraint: ident (PIPE_EQ | TILDE_EQ) expr;

// --- Patterns ---

pattern: pattern_or;

pattern_or: pattern_as (KW_OR pattern_as)*;

pattern_as: pattern_primary (KW_AS ident)?;

pattern_primary:
	UNDERSCORE
	| literal
	| ident
	| DOT ident (LPAREN variant_pat_arg_list? RPAREN)?
	| LBRACE rec_pat_fields? RBRACE
	| LPAREN pat_list? RPAREN
	| LBRACKET pat_list? RBRACKET;

variant_pat_arg_list:
	variant_pat_arg (COMMA variant_pat_arg)* COMMA?;

variant_pat_arg: ident COLON_EQ pattern | pattern;

rec_pat_fields: rec_pat_field (COMMA rec_pat_field)* COMMA?;

rec_pat_field: ident (COLON pattern)?;

pat_list: pattern (COMMA pattern)* COMMA?;

// --- Attributes ---

attrs: attr+;

attr: AT attr_path (LPAREN attr_args? RPAREN)?;

attr_path: ident (DOT ident)*;

attr_args: attr_arg (COMMA attr_arg)*;

attr_arg: ident COLON_EQ attr_value | attr_value;

attr_value:
	STRING_LIT
	| INT_LIT
	| RUNE_LIT
	| attr_variant
	| stack_effect
	| attr_array
	| attr_record;

stack_effect: LBRACKET expr_list? SEMICOLON expr_list? RBRACKET;

attr_variant: DOT ident (LPAREN attr_value_list? RPAREN)?;

attr_array: LBRACKET attr_value_list? RBRACKET;

attr_record: LBRACE attr_record_fields? RBRACE;

attr_record_fields:
	attr_record_field (COMMA attr_record_field)* comma_pad;

attr_record_field: ident COLON_EQ attr_value;

attr_value_list: attr_value (COMMA attr_value)* comma_pad;

// --- Shared helpers ---

expr_list: expr (COMMA expr)*;

arg_list: arg (COMMA arg)*;

arg: spread | expr;

params: LPAREN param_list? RPAREN;

param_list: param (COMMA param)*;

param: KW_KNOWN? ident type_annot? (COLON_EQ expr)?;

ident_list: comma_pad ident (COMMA ident)* comma_pad;

comma_pad: COMMA*;

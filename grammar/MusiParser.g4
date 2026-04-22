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
// Surface policy: - `let` names values. `law` names obligations. Trust roots are `let` bindings
// with metadata such as `@axiom`, not extra declaration keywords. Other form keywords (`data`,
// `effect`, `shape`, `given`, `answer`, `ask`, `handle`, `import`, ...) build expressions; they do
// not name values. - Keywords never become callees. `import("a")` remains keyword syntax (or an
// error), not a function call on an identifier named `import`. - Structural blocks use `{ ... }`;
// imperative/sequence blocks use `( ... )`. - Lambdas must start with `\`, so `=>` can remain
// unambiguous branch-arm syntax too.

root: root_stmt* EOF;

root_stmt: declaration SEMICOLON | stmt;

// Bodyless `let` declarations require semantic permission such as `@axiom`.
declaration: fn_decl | law_decl;

stmt: expr SEMICOLON;

// --- Expressions (semantic precedence; parse as flat infix chain) ---

expr: infix_expr;

infix_expr: prefix_expr (infix_op prefix_expr)*;

infix_op:
	COLON_EQ
	| PIPE_GT
	| MINUS_GT
	| TILDE_GT
	| TILDE_EQ
	| QUESTION_QUESTION
	| KW_OR
	| KW_CATCH
	| KW_XOR
	| KW_AND
	| EQ
	| SLASH_EQ
	| LT
	| GT
	| LT_EQ
	| GT_EQ
	| LT_DOT_DOT
	| LT_DOT_DOT_LT
	| DOT_DOT
	| DOT_DOT_LT
	| KW_IN
	| KW_SHL
	| KW_SHR
	| PLUS
	| MINUS
	| STAR
	| SLASH
	| PERCENT;

prefix_expr: (
		MINUS
		| KW_ANY
		| KW_COMPTIME
		| KW_KNOWN
		| KW_NOT
		| KW_MUT
		| KW_SOME
		| DOT_DOT
		| DOT_DOT_LT
	) prefix_expr
	| postfix_expr;

postfix_expr: atom postfix_op*;

postfix_op:
	call_op
	| bracket_apply_op
	| access_op
	| type_test_op
	| type_cast_op;

call_op: LPAREN arg_list? RPAREN;

bracket_apply_op: LBRACKET expr_list? RBRACKET;

// Access-edge tokens are compound where the edge changes behavior (`?.`, `!.`) or where maximal
// munch avoids ambiguity (`.[`, `.` followed by operator selection). `?.[` and `?.(` are not
// separate lexer tokens: the parser reads `?.` plus a selector.
access_op:
	DOT field_target
	| DOT_LBRACKET expr_list? RBRACKET
	| DOT_LPAREN op_name RPAREN
	| QUESTION_DOT access_selector
	| BANG_DOT access_selector;

access_selector:
	field_target
	| LBRACKET expr_list? RBRACKET
	| LPAREN op_name RPAREN;

// `as` aliases an already matched/refined value. It is not import/type/module alias syntax.
type_test_op: COLON_QUESTION expr (KW_AS ident)?;

type_cast_op: COLON_QUESTION_GT expr;

field_target: ident | INT_LIT;

// --- Atoms (unique FIRST tokens) ---

atom:
	literal
	| template_expr
	| splice
	| ident
	| op_ident
	| lambda_expr
	| paren_expr
	| array_lit_expr
	| record_literal_expr
	| dot_prefix_expr
	| match_expr
	| let_expr
	| resume_expr
	| import_expr
	| native_expr
	| data_expr
	| effect_expr
	| shape_expr
	| given_expr
	| ask_expr
	| answer_lit_expr
	| unsafe_expr
	| handle_expr
	| quote_expr
	| with_mods_expr;

literal: INT_LIT | FLOAT_LIT | STRING_LIT | RUNE_LIT;

template_expr:
	TEMPLATE_NO_SUBST
	| TEMPLATE_HEAD expr (TEMPLATE_MIDDLE expr)* TEMPLATE_TAIL;

ident: IDENT;

op_ident: LPAREN op_name RPAREN;

op_name: op_single | SYMBOLIC_OP | word_op;

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

word_op:
	KW_AND
	| KW_CATCH
	| KW_IN
	| KW_NOT
	| KW_OR
	| KW_SHL
	| KW_SHR
	| KW_XOR;

lambda_expr: BACKSLASH params (COLON expr)? EQ_GT expr;

paren_expr:
	LPAREN RPAREN
	| LPAREN COMMA RPAREN
	| LPAREN SEMICOLON RPAREN
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

resume_expr: KW_RESUME expr?;

import_expr:
	KW_IMPORT (LPAREN import_block_items? RPAREN | expr);

import_block_items: expr (SEMICOLON expr)* SEMICOLON?;

native_expr:
	KW_NATIVE STRING_LIT? (
		KW_LET let_rest
		| LPAREN (KW_LET let_rest SEMICOLON)* RPAREN
	);

let_modifier: KW_REC;

let_expr:
	KW_LET let_modifier? let_head bracket_params? params? type_annot? where_clause? require_clause?
		COLON_EQ expr;

let_head: receiver_method_head | pattern;

receiver_method_head: LPAREN ident type_annot RPAREN DOT ident;

bracket_params:
	LBRACKET bracket_param (COMMA bracket_param)* COMMA? RBRACKET;

bracket_param: ident type_annot?;

require_clause: KW_REQUIRE effect_set;

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

rec_def_field: ident COLON expr (COLON_EQ expr)?;

effect_expr: KW_EFFECT LBRACE structural_members RBRACE;

shape_expr:
	KW_SHAPE (KW_WHERE constraint (COMMA constraint)* COMMA?)? LBRACE structural_members RBRACE;

given_expr:
	KW_GIVEN bracket_params? expr where_clause? LBRACE structural_members RBRACE;

ask_expr: KW_ASK expr;

unsafe_expr:
	KW_UNSAFE LBRACE stmt* RBRACE
	| KW_UNSAFE KW_PIN expr KW_AS IDENT KW_IN expr;

answer_lit_expr:
	KW_ANSWER prefix_expr LBRACE structural_fn_members RBRACE;

handle_expr: KW_HANDLE expr KW_ANSWER prefix_expr;

// `quote` creates hygienic syntax objects. Splices use `#` and are valid only under quote.
quote_expr: KW_QUOTE (LPAREN expr RPAREN | LBRACE stmt* RBRACE);

splice:
	HASH ident
	| HASH LPAREN expr RPAREN
	| HASH LBRACKET expr_list? RBRACKET;

with_mods_expr:
	attrs modifier* (expr | let_expr)
	| modifier+ (expr | let_expr);

modifier: attr | export_mod | partial_mod;

partial_mod: KW_PARTIAL;

export_mod: KW_EXPORT (KW_NATIVE STRING_LIT?)?;

let_rest:
	let_modifier? pattern bracket_params? params? type_annot? where_clause? require_clause? (
		COLON_EQ expr
	)?;

fn_decl:
	attrs? KW_LET op_or_ident bracket_params? params? type_annot? (
		COLON_EQ expr
	)?;

op_or_ident: ident | op_ident;

law_decl:
	attrs? KW_LAW op_or_ident bracket_params? params COLON_EQ expr;

// Structural member lists accept leading and trailing separators, matching `{ ; x; }`.
structural_members:
	SEMICOLON* (
		structural_member (SEMICOLON+ structural_member)* SEMICOLON*
	)?;

structural_member: fn_decl | law_decl;

structural_fn_members:
	SEMICOLON* (fn_decl (SEMICOLON+ fn_decl)* SEMICOLON*)?;

// --- Unified annotation / constraint helpers ---

type_annot: COLON type_expr;

type_expr: expr;

effect_set: LBRACE comma_pad effect_entries? comma_pad RBRACE;

effect_entries: effect_list (COMMA effect_rest)? | effect_rest;

effect_rest: DOT_DOT_DOT ident;

effect_list: effect_item (COMMA effect_item)*;

effect_item: ident (LBRACKET expr RBRACKET)?;

where_clause: KW_WHERE constraint (COMMA constraint)* COMMA?;

constraint:
	ident LT_COLON expr
	| ident COLON expr
	| ident TILDE_EQ expr;

// --- Patterns ---

pattern: pattern_primary;

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

rec_pat_fields: rec_pat_field (COMMA rec_pat_field)*;

rec_pat_field: ident (COLON pattern)?;

pat_list: pattern (COMMA pattern)*;

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
	| attr_array
	| attr_record;

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

param: KW_COMPTIME? ident type_annot? (COLON_EQ expr)?;

ident_list: comma_pad ident (COMMA ident)* comma_pad;

comma_pad: COMMA*;

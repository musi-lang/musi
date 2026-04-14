// Musi Language - ANTLR4 Parser Grammar
// 
// Canonical parser half of the tool-supported Musi grammar. Token definitions and template
// interpolation modes live in `MusiLexer.g4`.

parser grammar MusiParser;

options {
	tokenVocab = MusiLexer;
}

// ----------------------------------------------------------------------------- Parser

root: root_stmt* EOF;

root_stmt: fixity_decl | stmt;

stmt: expr SEMICOLON;

fixity_decl:
	(KW_INFIXL | KW_INFIXR | KW_INFIX) INT_LIT op_ident SEMICOLON;

// --- Expressions (semantic precedence; parse as flat infix chain) ---

expr: infix_expr;

infix_expr: prefix_expr (infix_op prefix_expr)*;

infix_op:
	COLON_EQ
	| PIPE_GT
	| MINUS_GT
	| TILDE_GT
	| KW_OR
	| KW_XOR
	| KW_AND
	| EQ
	| SLASH_EQ
	| LT
	| GT
	| LT_EQ
	| GT_EQ
	| KW_IN
	| KW_SHL
	| KW_SHR
	| PLUS
	| MINUS
	| STAR
	| SLASH
	| PERCENT
	| SYMBOLIC_OP;

prefix_expr: (MINUS | KW_NOT | KW_MUT) prefix_expr
	| postfix_expr;

postfix_expr: atom postfix_op*;

postfix_op:
	call_op
	| bracket_apply_op
	| index_op
	| field_access_op
	| type_test_op
	| type_cast_op;

call_op: LPAREN arg_list? RPAREN;

bracket_apply_op: LBRACKET expr_list? RBRACKET;

index_op: DOT_LBRACKET expr_list? RBRACKET;

field_access_op: DOT field_target;

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
	| pi_expr
	| lambda_expr
	| paren_expr
	| array_lit_expr
	| record_literal_expr
	| dot_prefix_expr
	| match_expr
	| let_expr
	| resume_expr
	| import_expr
	| data_expr
	| effect_expr
	| class_expr
	| instance_expr
	| request_expr
	| handle_expr
	| quote_expr
	| with_mods_expr;

literal: INT_LIT | FLOAT_LIT | STRING_LIT | RUNE_LIT;

template_expr:
	TEMPLATE_NO_SUBST
	| TEMPLATE_HEAD expr (TEMPLATE_MIDDLE expr)* TEMPLATE_TAIL;

ident: IDENT;

op_ident: LPAREN (SYMBOLIC_OP | op_single) RPAREN;

op_single: PLUS | MINUS | STAR | SLASH | PERCENT | EQ | LT | GT;

pi_expr:
	KW_FORALL LPAREN ident COLON expr RPAREN (
		MINUS_GT
		| TILDE_GT
	) expr;

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

match_arm: attrs? pattern (KW_IF expr)? EQ_GT expr;

array_lit_expr:
	LBRACKET comma_pad array_items? comma_pad RBRACKET;

array_items: array_item (COMMA array_item)*;

array_item: spread | expr;

record_literal_expr: LBRACE record_fields RBRACE;

record_fields:
	comma_pad (record_field (COMMA record_field)*)? comma_pad;

record_field: ident (COLON_EQ expr)? | spread;

spread: DOT_DOT_DOT expr;

dot_prefix_expr: DOT ident (LPAREN expr_list? RPAREN)?;

resume_expr: KW_RESUME expr?;

import_expr: KW_IMPORT expr;

let_modifier: KW_REC;

let_expr:
	KW_LET let_modifier? let_head bracket_params? params? type_annot? where_clause? using_clause?
		COLON_EQ expr;

let_head: receiver_let_head | pattern;

receiver_let_head:
	LPAREN KW_MUT? ident COLON expr RPAREN DOT ident;

bracket_params: LBRACKET ident (COMMA ident)* COMMA? RBRACKET;

using_clause: KW_USING effect_set;

data_expr: KW_DATA LBRACE data_body RBRACE;

data_body: variant_list | rec_def_fields | PIPE | SEMICOLON;

variant_list: PIPE? variant (PIPE variant)* PIPE?;

variant: attrs? ident (COLON expr)? (COLON_EQ expr)?;

rec_def_fields:
	SEMICOLON? rec_def_field (SEMICOLON rec_def_field)* SEMICOLON?;

rec_def_field: ident COLON expr (COLON_EQ expr)?;

effect_expr:
	KW_EFFECT LBRACE (effect_member (SEMICOLON effect_member)*)? SEMICOLON? RBRACE;

effect_member: fn_decl | law_decl;

class_expr: KW_CLASS where_clause? LBRACE class_member* RBRACE;

class_member: (fn_decl | law_decl) SEMICOLON?;

instance_expr:
	KW_INSTANCE bracket_params? expr where_clause? instance_body;

request_expr: KW_REQUEST expr;

handle_expr:
	KW_HANDLE expr (handler_expr | KW_USING prefix_expr);

handler_expr:
	KW_USING ident LBRACE (handle_clause SEMICOLON?)* RBRACE;

handle_clause:
	ident EQ_GT expr
	| ident LPAREN ident_list? RPAREN EQ_GT expr;

foreign_let_group:
	LPAREN (KW_LET foreign_binding SEMICOLON)+ RPAREN;

quote_expr: KW_QUOTE (LPAREN expr RPAREN | LBRACE stmt* RBRACE);

splice:
	HASH ident
	| HASH LPAREN expr RPAREN
	| HASH LBRACKET expr_list? RBRACKET;

with_mods_expr:
	attrs export_mod? foreign_mod? (
		expr
		| foreign_let_group
		| let_expr
	)
	| export_mod foreign_mod? (
		expr
		| foreign_let_group
		| let_expr
	)
	| foreign_mod (foreign_let_group | let_expr);

modifier: attr | export_mod | foreign_mod;

export_mod: KW_EXPORT KW_OPAQUE?;

foreign_mod: KW_FOREIGN STRING_LIT?;

let_rest:
	let_modifier? pattern bracket_params? params? type_annot? where_clause? using_clause? (
		COLON_EQ expr
	)?;

instance_body: LBRACE class_member* RBRACE;

foreign_binding:
	attrs? ident bracket_params? params? where_clause? (
		COLON expr
	)?;

fn_decl:
	KW_LET op_or_ident params? type_annot? (COLON_EQ expr)?;

law_decl: KW_LAW ident params? COLON_EQ expr;

op_or_ident: ident | op_ident;

// --- Unified annotation / constraint helpers ---

type_annot: COLON type_expr;

type_expr: expr;

effect_set: LBRACE comma_pad effect_entries? comma_pad RBRACE;

effect_entries: effect_list (COMMA effect_rest)? | effect_rest;

effect_rest: DOT_DOT_DOT ident;

effect_list: effect_item (COMMA effect_item)*;

effect_item: ident (LBRACKET expr RBRACKET)?;

where_clause: KW_WHERE constraint (COMMA constraint)* COMMA?;

constraint: ident LT_COLON expr | ident COLON expr;

// --- Patterns ---

pattern: pattern_as (KW_OR pattern_as)*;

pattern_as: pattern_primary (KW_AS ident)?;

pattern_primary:
	UNDERSCORE
	| literal
	| ident
	| DOT ident (LPAREN pat_list? RPAREN)?
	| LBRACE rec_pat_fields? RBRACE
	| LPAREN pat_list? RPAREN
	| LBRACKET pat_list? RBRACKET;

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

param: ident type_annot? (COLON_EQ expr)?;

ident_list: comma_pad ident (COMMA ident)* comma_pad;

comma_pad: COMMA*;

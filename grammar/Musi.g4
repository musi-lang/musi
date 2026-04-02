// Musi Language - ANTLR4 Grammar (v2)
// 
// Canonical, tool-supported grammar for the Musi surface syntax.
// 
// Design goals: - expression-first, Pratt-friendly precedence structure - maximal munch
// tokenization (see fixed tokens) - compact and mechanically checkable
// 
// Known gaps (tracked in `docs/14-antlr-grammar-tracker.md`):
// - symbolic operator “precedence by family” (currently one tier)
// - template literal interpolation (lexer+parser in crates_new support it; this grammar is behind)

grammar Musi;

// ----------------------------------------------------------------------------- Parser
// -----------------------------------------------------------------------------

root: stmt* EOF;

stmt: expr SEMICOLON;

// --- Expressions (Pratt precedence as rule ladder) ---

expr: assign_expr;

assign_expr: pipe_expr (LT_MINUS assign_expr)?;

pipe_expr: arrow_expr (PIPE_GT arrow_expr)*;

arrow_expr: or_expr ((MINUS_GT | TILDE_GT) arrow_expr)?;

or_expr: xor_expr (KW_OR xor_expr)*;

xor_expr: and_expr (KW_XOR and_expr)*;

and_expr: compare_expr (KW_AND compare_expr)*;

compare_expr:
		shift_expr (
			(EQ | SLASH_EQ | LT | GT | LT_EQ | GT_EQ | KW_IN) shift_expr
		)?;

shift_expr: add_expr ((KW_SHL | KW_SHR) add_expr)*;

add_expr: mul_expr ((PLUS | MINUS) mul_expr)*;

mul_expr: symop_expr ((STAR | SLASH | PERCENT) symop_expr)*;

symop_expr: prefix_expr (SYMBOLIC_OP prefix_expr)*;

prefix_expr: (MINUS | KW_NOT | KW_MUT) prefix_expr
	| postfix_expr;

postfix_expr: atom postfix_op*;

postfix_op:
	call_op
	| bracket_apply_op
	| index_op
	| record_update_op
	| field_access_op
	| optional_chain_op
	| force_access_op
	| type_test_op
	| type_cast_op;

call_op: LPAREN arg_list? RPAREN;

bracket_apply_op: LBRACKET expr_list? RBRACKET;

index_op: DOT_LBRACKET expr_list? RBRACKET;

record_update_op: DOT_LBRACE record_fields? RBRACE;

field_access_op: DOT field_target;

optional_chain_op: Q_DOT field_target;

force_access_op: BANG_DOT field_target;

type_test_op: COLON_QUESTION arrow_expr (KW_AS ident)?;

type_cast_op: COLON_QUESTION_GT arrow_expr;

field_target: ident | INT_LIT;

// --- Atoms (unique FIRST tokens) ---

atom:
	literal
	| splice
	| ident
	| op_ident
	| pi_expr
	| lambda_expr
	| paren_expr
	| array_type_expr
	| array_expr
	| record_literal_expr
	| dot_prefix_expr
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
	| with_attrs_expr;

literal:
	INT_LIT
	| FLOAT_LIT
	| STRING_LIT
	| TEMPLATE_LIT
	| RUNE_LIT;

ident: IDENT;

op_ident: LPAREN (SYMBOLIC_OP | op_single) RPAREN;

op_single: PLUS | MINUS | STAR | SLASH | PERCENT | EQ | LT | GT;

pi_expr:
	LPAREN ident COLON expr RPAREN (MINUS_GT | TILDE_GT) expr;

lambda_expr: LPAREN param_list? RPAREN (COLON expr)? EQ_GT expr;

paren_expr:
	LPAREN RPAREN
	| LPAREN COMMA RPAREN
	| LPAREN SEMICOLON RPAREN
	| LPAREN grouped_or_tuple_body RPAREN
	| LPAREN sequence_body RPAREN;

grouped_or_tuple_body: expr (COMMA expr_list? COMMA?)?;

sequence_body: expr (SEMICOLON expr)* SEMICOLON?;

case_expr:
	KW_CASE expr KW_OF LPAREN PIPE? case_arm (PIPE case_arm)* PIPE? RPAREN;

case_arm: attrs? pattern (KW_IF expr)? EQ_GT expr;

array_type_expr: LBRACKET dim_list? RBRACKET prefix_expr;

array_expr: LBRACKET comma_pad array_items? comma_pad RBRACKET;

array_items: array_item (COMMA array_item)*;

array_item: spread | expr;

record_literal_expr: LBRACE record_fields? RBRACE;

record_fields:
	comma_pad (record_field (COMMA record_field)*)? comma_pad;

record_field: ident (COLON_EQ expr)? | spread;

spread: DOT_DOT_DOT expr;

dot_prefix_expr: DOT ident (LPAREN expr_list? RPAREN)?;

resume_expr: KW_RESUME expr?;

import_expr: KW_IMPORT expr;

let_modifier: KW_MUT | KW_REC;

let_expr:
	KW_LET let_modifier? pattern bracket_params? params? where_clause? with_clause? type_annot?
		COLON_EQ expr;

bracket_params: LBRACKET ident (COMMA ident)* COMMA? RBRACKET;

with_clause: KW_WITH effect_set;

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
	KW_INSTANCE bracket_params? where_clause? expr instance_body;

perform_expr: KW_PERFORM expr;

handle_expr:
	KW_HANDLE expr KW_WITH ident KW_OF LPAREN PIPE? handle_clause (
		PIPE handle_clause
	)* PIPE? RPAREN;

handle_clause:
	ident EQ_GT expr
	| ident LPAREN ident_list? RPAREN EQ_GT expr;

foreign_expr:
	KW_FOREIGN STRING_LIT? (KW_LET let_rest | foreign_let_group);

foreign_let_group:
	LPAREN (KW_LET foreign_binding SEMICOLON)* RPAREN;

quote_expr: KW_QUOTE (LPAREN expr RPAREN | LBRACE stmt* RBRACE);

splice:
	HASH ident
	| HASH LPAREN expr RPAREN
	| HASH LBRACKET expr_list? RBRACKET;

with_attrs_expr: attrs expr;

decl: export_decl | let_expr | foreign_expr | instance_expr;

export_decl: KW_EXPORT export_tail;

export_tail: (KW_OPAQUE)? (KW_FOREIGN STRING_LIT?)? (
		KW_LET let_rest
		| foreign_let_group
	)
	| instance_expr;

let_rest:
	let_modifier? pattern bracket_params? params? where_clause? with_clause? type_annot? (
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

type_annot: COLON expr;

dim_list: dim (COMMA dim)*;

dim: INT_LIT | ident | UNDERSCORE;

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

rec_pat_field: KW_MUT? ident (COLON pattern)?;

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

param: KW_MUT? ident type_annot? (COLON_EQ expr)?;

ident_list: comma_pad ident (COMMA ident)* comma_pad;

comma_pad: COMMA*;

// ----------------------------------------------------------------------------- Lexer
// -----------------------------------------------------------------------------

// Keywords.
KW_AND: 'and';
KW_AS: 'as';
KW_CASE: 'case';
KW_CLASS: 'class';
KW_DATA: 'data';
KW_EFFECT: 'effect';
KW_EXPORT: 'export';
KW_FOREIGN: 'foreign';
KW_HANDLE: 'handle';
KW_IF: 'if';
KW_IMPORT: 'import';
KW_IN: 'in';
KW_INSTANCE: 'instance';
KW_LAW: 'law';
KW_LET: 'let';
KW_MUT: 'mut';
KW_REC: 'rec';
KW_NOT: 'not';
KW_OF: 'of';
KW_OPAQUE: 'opaque';
KW_OR: 'or';
KW_PERFORM: 'perform';
KW_QUOTE: 'quote';
KW_RESUME: 'resume';
KW_SHL: 'shl';
KW_SHR: 'shr';
KW_WHERE: 'where';
KW_WITH: 'with';
KW_XOR: 'xor';

// Fixed tokens (maximal munch).
COLON_QUESTION_GT: ':?>';
DOT_DOT_DOT: '...';
DOT_LBRACE: '.{';
DOT_LBRACKET: '.[';
EQ_GT: '=>';
BANG_DOT: '!.';
MINUS_GT: '->';
SLASH_EQ: '/=';
LT_EQ: '<=';
LT_COLON: '<:';
LT_MINUS: '<-';
GT_EQ: '>=';
Q_DOT: '?.';
PIPE_GT: '|>';
TILDE_GT: '~>';
COLON_EQ: ':=';
COLON_QUESTION: ':?';

// Prefixes.
AT: '@';
HASH: '#';

// Delimiters / separators.
LBRACE: '{';
RBRACE: '}';
LBRACKET: '[';
RBRACKET: ']';
LPAREN: '(';
RPAREN: ')';
COMMA: ',';
SEMICOLON: ';';

// Single-char operators / punctuation.
DOT: '.';
COLON: ':';
PIPE: '|';
PLUS: '+';
MINUS: '-';
STAR: '*';
SLASH: '/';
PERCENT: '%';
EQ: '=';
LT: '<';
GT: '>';
UNDERSCORE: '_';

// Literals.
FLOAT_LIT:
	DEC_DIGITS '.' DEC_DIGITS EXP_PART?
	| '.' DEC_DIGITS EXP_PART?
	| DEC_DIGITS EXP_PART;

INT_LIT:
	'0x' HEX_DIGITS
	| '0o' OCT_DIGITS
	| '0b' BIN_DIGITS
	| DEC_DIGITS;

STRING_LIT: '"' (ESC_SEQ | ~["\\])* '"';

TEMPLATE_LIT: '`' (ESC_SEQ | ~[`\\])* '`';

RUNE_LIT: '\'' (ESC_SEQ | ~['\\])* '\'';

// Idents.
IDENT: LETTER (LETTER | DIGIT | UNDERSCORE)*;

// User-defined symbolic operators.
SYMBOLIC_OP: SYM_CHAR SYM_CHAR+;

// Trivia (hidden channel).
LINE_DOC_COMMENT: '///' ~[\n]* -> channel(HIDDEN);

LINE_COMMENT: '//' ~[\n]* -> channel(HIDDEN);

BLOCK_DOC_COMMENT: '/**' .*? '*/' -> channel(HIDDEN);

BLOCK_COMMENT: '/*' .*? '*/' -> channel(HIDDEN);

NEWLINE: '\n' -> channel(HIDDEN);

WS: [ \t\r]+ -> channel(HIDDEN);

// Fragments.
fragment LETTER: [A-Za-z];
fragment DIGIT: [0-9];
fragment HEXDIGIT: [0-9a-fA-F];
fragment SYM_CHAR: [*+\\-/%=<>];

fragment DEC_DIGITS: DIGIT (DIGIT | '_' DIGIT)*;
fragment HEX_DIGITS: HEXDIGIT (HEXDIGIT | '_' HEXDIGIT)*;
fragment OCT_DIGITS: [0-7] ([0-7] | '_' [0-7])*;
fragment BIN_DIGITS: [01] ([01] | '_' [01])*;

fragment EXP_PART: [eE] [+-]? DEC_DIGITS;

fragment ESC_SEQ:
	'\\\\' (
		['"`$\\\\nrt0]
		| 'x' HEXDIGIT HEXDIGIT
		| 'u' HEXDIGIT HEXDIGIT HEXDIGIT HEXDIGIT (
			HEXDIGIT HEXDIGIT
		)?
	);

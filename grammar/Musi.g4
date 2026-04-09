// Musi Language - ANTLR4 Grammar (v2)
// 
// Canonical, tool-supported grammar for the Musi surface syntax.
// 
// Design goals: - expression-first - maximal munch tokenization (see fixed tokens) - compact and
// mechanically checkable. Infix operator precedence/associativity is resolved semantically via
// explicit fixity declarations.

grammar Musi;

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
	LT_MINUS
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

literal: INT_LIT | FLOAT_LIT | STRING_LIT | RUNE_LIT;

template_expr:
	TEMPLATE_BEGIN (
		TEMPLATE_TEXT
		| TEMPLATE_INTERP_BEGIN expr RBRACE
	)* TEMPLATE_END;

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

case_expr:
	KW_CASE expr KW_OF LPAREN PIPE? case_arm (PIPE case_arm)* PIPE? RPAREN;

case_arm: attrs? pattern (KW_IF expr)? EQ_GT expr;

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

let_modifier: KW_REC;

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
KW_FORALL: 'forall';
KW_HANDLE: 'handle';
KW_IF: 'if';
KW_IMPORT: 'import';
KW_IN: 'in';
KW_INFIX: 'infix';
KW_INFIXL: 'infixl';
KW_INFIXR: 'infixr';
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
BACKSLASH: '\\';

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

STRING_LIT: '"' (ESC_SEQ | ~["\\\r\n])* '"';

TEMPLATE_BEGIN: '`' -> pushMode(TEMPLATE);

RUNE_LIT: '\'' (ESC_SEQ | ~['\\\r\n])* '\'';

// Idents.
IDENT: LETTER (LETTER | DIGIT | UNDERSCORE)*;

// User-defined symbolic operators.
SYMBOLIC_OP: SYM_CHAR SYM_CHAR+;

// Trivia (hidden channel).
LINE_DOC_COMMENT: '///' ~[\r\n]* -> channel(HIDDEN);

LINE_COMMENT: '//' ~[\r\n]* -> channel(HIDDEN);

BLOCK_DOC_COMMENT: '/**' .*? '*/' -> channel(HIDDEN);

BLOCK_COMMENT: '/*' .*? '*/' -> channel(HIDDEN);

NEWLINE: '\n' -> channel(HIDDEN);

WS: [ \t\r]+ -> channel(HIDDEN);

// ----------------------------------------------------------------------------- Template literal
// modes (portable; no target-language code)
// -----------------------------------------------------------------------------

mode TEMPLATE;

TEMPLATE_END: '`' -> popMode;

TEMPLATE_INTERP_BEGIN: '${' -> pushMode(INTERP_TOP);

TEMPLATE_TEXT: TEMPLATE_CHUNK_CHAR+;

mode INTERP_TOP;

INTERP_TEMPLATE_BEGIN:
	'`' -> type(TEMPLATE_BEGIN), pushMode(TEMPLATE);

INTERP_DOT_LBRACE:
	'.{' -> type(DOT_LBRACE), pushMode(INTERP_NESTED);

INTERP_LBRACE: '{' -> type(LBRACE), pushMode(INTERP_NESTED);

TEMPLATE_INTERP_END: '}' -> type(RBRACE), popMode;

// Keywords.
I_KW_AND: 'and' -> type(KW_AND);
I_KW_AS: 'as' -> type(KW_AS);
I_KW_CASE: 'case' -> type(KW_CASE);
I_KW_CLASS: 'class' -> type(KW_CLASS);
I_KW_DATA: 'data' -> type(KW_DATA);
I_KW_EFFECT: 'effect' -> type(KW_EFFECT);
I_KW_EXPORT: 'export' -> type(KW_EXPORT);
I_KW_FOREIGN: 'foreign' -> type(KW_FOREIGN);
I_KW_FORALL: 'forall' -> type(KW_FORALL);
I_KW_HANDLE: 'handle' -> type(KW_HANDLE);
I_KW_IF: 'if' -> type(KW_IF);
I_KW_IMPORT: 'import' -> type(KW_IMPORT);
I_KW_IN: 'in' -> type(KW_IN);
I_KW_INFIX: 'infix' -> type(KW_INFIX);
I_KW_INFIXL: 'infixl' -> type(KW_INFIXL);
I_KW_INFIXR: 'infixr' -> type(KW_INFIXR);
I_KW_INSTANCE: 'instance' -> type(KW_INSTANCE);
I_KW_LAW: 'law' -> type(KW_LAW);
I_KW_LET: 'let' -> type(KW_LET);
I_KW_MUT: 'mut' -> type(KW_MUT);
I_KW_REC: 'rec' -> type(KW_REC);
I_KW_NOT: 'not' -> type(KW_NOT);
I_KW_OF: 'of' -> type(KW_OF);
I_KW_OPAQUE: 'opaque' -> type(KW_OPAQUE);
I_KW_OR: 'or' -> type(KW_OR);
I_KW_PERFORM: 'perform' -> type(KW_PERFORM);
I_KW_QUOTE: 'quote' -> type(KW_QUOTE);
I_KW_RESUME: 'resume' -> type(KW_RESUME);
I_KW_SHL: 'shl' -> type(KW_SHL);
I_KW_SHR: 'shr' -> type(KW_SHR);
I_KW_WHERE: 'where' -> type(KW_WHERE);
I_KW_WITH: 'with' -> type(KW_WITH);
I_KW_XOR: 'xor' -> type(KW_XOR);

// Fixed tokens (maximal munch).
I_COLON_QUESTION_GT: ':?>' -> type(COLON_QUESTION_GT);
I_DOT_DOT_DOT: '...' -> type(DOT_DOT_DOT);
I_DOT_LBRACKET: '.[' -> type(DOT_LBRACKET);
I_EQ_GT: '=>' -> type(EQ_GT);
I_BANG_DOT: '!.' -> type(BANG_DOT);
I_MINUS_GT: '->' -> type(MINUS_GT);
I_SLASH_EQ: '/=' -> type(SLASH_EQ);
I_LT_EQ: '<=' -> type(LT_EQ);
I_LT_COLON: '<:' -> type(LT_COLON);
I_LT_MINUS: '<-' -> type(LT_MINUS);
I_GT_EQ: '>=' -> type(GT_EQ);
I_Q_DOT: '?.' -> type(Q_DOT);
I_PIPE_GT: '|>' -> type(PIPE_GT);
I_TILDE_GT: '~>' -> type(TILDE_GT);
I_COLON_EQ: ':=' -> type(COLON_EQ);
I_COLON_QUESTION: ':?' -> type(COLON_QUESTION);

// Prefixes.
I_AT: '@' -> type(AT);
I_HASH: '#' -> type(HASH);
I_BACKSLASH: '\\' -> type(BACKSLASH);

// Delimiters / separators.
I_LBRACKET: '[' -> type(LBRACKET);
I_RBRACKET: ']' -> type(RBRACKET);
I_LPAREN: '(' -> type(LPAREN);
I_RPAREN: ')' -> type(RPAREN);
I_COMMA: ',' -> type(COMMA);
I_SEMICOLON: ';' -> type(SEMICOLON);

// Single-char operators / punctuation.
I_DOT: '.' -> type(DOT);
I_COLON: ':' -> type(COLON);
I_PIPE: '|' -> type(PIPE);
I_PLUS: '+' -> type(PLUS);
I_MINUS: '-' -> type(MINUS);
I_STAR: '*' -> type(STAR);
I_SLASH: '/' -> type(SLASH);
I_PERCENT: '%' -> type(PERCENT);
I_EQ: '=' -> type(EQ);
I_LT: '<' -> type(LT);
I_GT: '>' -> type(GT);
I_UNDERSCORE: '_' -> type(UNDERSCORE);

// Literals.
I_FLOAT_LIT:
	(
		DEC_DIGITS '.' DEC_DIGITS EXP_PART?
		| '.' DEC_DIGITS EXP_PART?
		| DEC_DIGITS EXP_PART
	) -> type(FLOAT_LIT);

I_INT_LIT:
	(
		'0x' HEX_DIGITS
		| '0o' OCT_DIGITS
		| '0b' BIN_DIGITS
		| DEC_DIGITS
	) -> type(INT_LIT);

I_STRING_LIT:
	'"' (ESC_SEQ | ~["\\\r\n])* '"' -> type(STRING_LIT);

I_RUNE_LIT:
	'\'' (ESC_SEQ | ~['\\\r\n])* '\'' -> type(RUNE_LIT);

// Idents.
I_IDENT: LETTER (LETTER | DIGIT | UNDERSCORE)* -> type(IDENT);

// User-defined symbolic operators.
I_SYMBOLIC_OP: SYM_CHAR SYM_CHAR+ -> type(SYMBOLIC_OP);

// Trivia (hidden channel).
I_LINE_DOC_COMMENT:
	'///' ~[\r\n]* -> type(LINE_DOC_COMMENT), channel(HIDDEN);

I_LINE_COMMENT:
	'//' ~[\r\n]* -> type(LINE_COMMENT), channel(HIDDEN);

I_BLOCK_DOC_COMMENT:
	'/**' .*? '*/' -> type(BLOCK_DOC_COMMENT), channel(HIDDEN);

I_BLOCK_COMMENT:
	'/*' .*? '*/' -> type(BLOCK_COMMENT), channel(HIDDEN);

I_NEWLINE: '\n' -> type(NEWLINE), channel(HIDDEN);

I_WS: [ \t\r]+ -> type(WS), channel(HIDDEN);

mode INTERP_NESTED;

N_INTERP_TEMPLATE_BEGIN:
	'`' -> type(TEMPLATE_BEGIN), pushMode(TEMPLATE);

N_INTERP_DOT_LBRACE:
	'.{' -> type(DOT_LBRACE), pushMode(INTERP_NESTED);

N_INTERP_LBRACE:
	'{' -> type(LBRACE), pushMode(INTERP_NESTED);

N_INTERP_RBRACE: '}' -> type(RBRACE), popMode;

// Keywords.
N_KW_AND: 'and' -> type(KW_AND);
N_KW_AS: 'as' -> type(KW_AS);
N_KW_CASE: 'case' -> type(KW_CASE);
N_KW_CLASS: 'class' -> type(KW_CLASS);
N_KW_DATA: 'data' -> type(KW_DATA);
N_KW_EFFECT: 'effect' -> type(KW_EFFECT);
N_KW_EXPORT: 'export' -> type(KW_EXPORT);
N_KW_FOREIGN: 'foreign' -> type(KW_FOREIGN);
N_KW_FORALL: 'forall' -> type(KW_FORALL);
N_KW_HANDLE: 'handle' -> type(KW_HANDLE);
N_KW_IF: 'if' -> type(KW_IF);
N_KW_IMPORT: 'import' -> type(KW_IMPORT);
N_KW_IN: 'in' -> type(KW_IN);
N_KW_INFIX: 'infix' -> type(KW_INFIX);
N_KW_INFIXL: 'infixl' -> type(KW_INFIXL);
N_KW_INFIXR: 'infixr' -> type(KW_INFIXR);
N_KW_INSTANCE: 'instance' -> type(KW_INSTANCE);
N_KW_LAW: 'law' -> type(KW_LAW);
N_KW_LET: 'let' -> type(KW_LET);
N_KW_MUT: 'mut' -> type(KW_MUT);
N_KW_REC: 'rec' -> type(KW_REC);
N_KW_NOT: 'not' -> type(KW_NOT);
N_KW_OF: 'of' -> type(KW_OF);
N_KW_OPAQUE: 'opaque' -> type(KW_OPAQUE);
N_KW_OR: 'or' -> type(KW_OR);
N_KW_PERFORM: 'perform' -> type(KW_PERFORM);
N_KW_QUOTE: 'quote' -> type(KW_QUOTE);
N_KW_RESUME: 'resume' -> type(KW_RESUME);
N_KW_SHL: 'shl' -> type(KW_SHL);
N_KW_SHR: 'shr' -> type(KW_SHR);
N_KW_WHERE: 'where' -> type(KW_WHERE);
N_KW_WITH: 'with' -> type(KW_WITH);
N_KW_XOR: 'xor' -> type(KW_XOR);

// Fixed tokens (maximal munch).
N_COLON_QUESTION_GT: ':?>' -> type(COLON_QUESTION_GT);
N_DOT_DOT_DOT: '...' -> type(DOT_DOT_DOT);
N_DOT_LBRACKET: '.[' -> type(DOT_LBRACKET);
N_EQ_GT: '=>' -> type(EQ_GT);
N_BANG_DOT: '!.' -> type(BANG_DOT);
N_MINUS_GT: '->' -> type(MINUS_GT);
N_SLASH_EQ: '/=' -> type(SLASH_EQ);
N_LT_EQ: '<=' -> type(LT_EQ);
N_LT_COLON: '<:' -> type(LT_COLON);
N_LT_MINUS: '<-' -> type(LT_MINUS);
N_GT_EQ: '>=' -> type(GT_EQ);
N_Q_DOT: '?.' -> type(Q_DOT);
N_PIPE_GT: '|>' -> type(PIPE_GT);
N_TILDE_GT: '~>' -> type(TILDE_GT);
N_COLON_EQ: ':=' -> type(COLON_EQ);
N_COLON_QUESTION: ':?' -> type(COLON_QUESTION);

// Prefixes.
N_AT: '@' -> type(AT);
N_HASH: '#' -> type(HASH);
N_BACKSLASH: '\\' -> type(BACKSLASH);

// Delimiters / separators.
N_LBRACKET: '[' -> type(LBRACKET);
N_RBRACKET: ']' -> type(RBRACKET);
N_LPAREN: '(' -> type(LPAREN);
N_RPAREN: ')' -> type(RPAREN);
N_COMMA: ',' -> type(COMMA);
N_SEMICOLON: ';' -> type(SEMICOLON);

// Single-char operators / punctuation.
N_DOT: '.' -> type(DOT);
N_COLON: ':' -> type(COLON);
N_PIPE: '|' -> type(PIPE);
N_PLUS: '+' -> type(PLUS);
N_MINUS: '-' -> type(MINUS);
N_STAR: '*' -> type(STAR);
N_SLASH: '/' -> type(SLASH);
N_PERCENT: '%' -> type(PERCENT);
N_EQ: '=' -> type(EQ);
N_LT: '<' -> type(LT);
N_GT: '>' -> type(GT);
N_UNDERSCORE: '_' -> type(UNDERSCORE);

// Literals.
N_FLOAT_LIT:
	(
		DEC_DIGITS '.' DEC_DIGITS EXP_PART?
		| '.' DEC_DIGITS EXP_PART?
		| DEC_DIGITS EXP_PART
	) -> type(FLOAT_LIT);

N_INT_LIT:
	(
		'0x' HEX_DIGITS
		| '0o' OCT_DIGITS
		| '0b' BIN_DIGITS
		| DEC_DIGITS
	) -> type(INT_LIT);

N_STRING_LIT:
	'"' (ESC_SEQ | ~["\\\r\n])* '"' -> type(STRING_LIT);

N_RUNE_LIT:
	'\'' (ESC_SEQ | ~['\\\r\n])* '\'' -> type(RUNE_LIT);

// Idents.
N_IDENT: LETTER (LETTER | DIGIT | UNDERSCORE)* -> type(IDENT);

// User-defined symbolic operators.
N_SYMBOLIC_OP: SYM_CHAR SYM_CHAR+ -> type(SYMBOLIC_OP);

// Trivia (hidden channel).
N_LINE_DOC_COMMENT:
	'///' ~[\r\n]* -> type(LINE_DOC_COMMENT), channel(HIDDEN);

N_LINE_COMMENT:
	'//' ~[\r\n]* -> type(LINE_COMMENT), channel(HIDDEN);

N_BLOCK_DOC_COMMENT:
	'/**' .*? '*/' -> type(BLOCK_DOC_COMMENT), channel(HIDDEN);

N_BLOCK_COMMENT:
	'/*' .*? '*/' -> type(BLOCK_COMMENT), channel(HIDDEN);

N_NEWLINE: '\n' -> type(NEWLINE), channel(HIDDEN);

N_WS: [ \t\r]+ -> type(WS), channel(HIDDEN);

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

fragment TEMPLATE_CHUNK_CHAR: ESC_SEQ | '$' ~'{' | ~[`\\$];

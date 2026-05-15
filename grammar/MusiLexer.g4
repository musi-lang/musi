// Musi Language - ANTLR4 Lexer Grammar
// 
// Canonical lexer half of the tool-supported Musi grammar. Template interpolation modes live here
// because ANTLR only allows modes in lexer grammars.

lexer grammar MusiLexer;

// ----------------------------------------------------------------------------- Lexer

// Keywords. Form keywords construct syntax forms; they are never callable identifiers. For example,
// `import "a"` and `import ("a", "b")` are import forms, not calls to a value named `import`.
KW_AND: 'and';
KW_AS: 'as';
KW_MATCH: 'match';
KW_DATA: 'data';
KW_DEFER: 'defer';
KW_ELSE: 'else';
KW_ERASED: 'erased';
KW_EXPORT: 'export';
KW_HIDDEN: 'hidden';
KW_IF: 'if';
KW_IMPORT: 'import';
KW_IN: 'in';
KW_KNOWN: 'known';
KW_LET: 'let';
KW_MUT: 'mut';
KW_NOT: 'not';
KW_OR: 'or';
KW_SHAPE: 'shape';
KW_PIN: 'pin';
KW_RECUR: 'recur';
KW_THEN: 'then';
KW_UNSAFE: 'unsafe';
KW_WHERE: 'where';
KW_XOR: 'xor';
KW_YIELD: 'yield';

// Fixed tokens (maximal munch).
COLON_EQ: ':=';
DOT_DOT_DOT: '...';
DOT_DOT_LT: '..<';
DOT_DOT: '..';
DOT_LBRACKET: '.[';
// `.(` selects first-class operator members such as `Eq.(=)`.
DOT_LPAREN: '.(';
QUESTION_QUESTION: '??';
COLON_QUESTION_GT: ':?>';
COLON_GT: ':>';
EQ_GT: '=>';
MINUS_GT: '->';
TILDE_EQ: '~=';
PIPE_EQ: '|=';
SLASH_EQ: '/=';
LT_EQ: '<=';
GT_EQ: '>=';
PIPE_GT: '|>';

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
QUESTION: '?';
BANG: '!';
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
	DEC_DIGITS '.' DEC_DIGITS EXP_PART? FLOAT_SUFFIX?
	| '.' DEC_DIGITS EXP_PART? FLOAT_SUFFIX?
	| DEC_DIGITS EXP_PART FLOAT_SUFFIX?;

INT_LIT:
	(
		'0x' HEX_DIGITS
		| '0o' OCT_DIGITS
		| '0b' BIN_DIGITS
		| DEC_DIGITS
	) INT_SUFFIX?;

STRING_LIT: '"' (ESC_SEQ | ~["\\\r\n])* '"';

// Template literal tokens. These are chunk tokens that include their boundary markers in the token
// text (matching the Rust frontend).
TEMPLATE_NO_SUBST: '`' (TEMPLATE_CHUNK_CHAR | '$' ~'{')* '`';

TEMPLATE_HEAD:
	'`' (TEMPLATE_CHUNK_CHAR | '$' ~'{')* '${' -> pushMode(INTERP_TOP);

RUNE_LIT: '\'' (ESC_SEQ | ~['\\\r\n])* '\'';

// Idents.
IDENT: LETTER (LETTER | DIGIT | UNDERSCORE)*;

// User-defined symbolic operators.
SYMBOLIC_OP: SYM_CHAR SYM_CHAR+;

// Trivia (hidden channel).
LINE_MODULE_DOC_COMMENT: '--!' ~[\r\n]* -> channel(HIDDEN);

LINE_DOC_COMMENT: '---' ~[\r\n]* -> channel(HIDDEN);

LINE_COMMENT: '--' ~[\r\n]* -> channel(HIDDEN);

BLOCK_MODULE_DOC_COMMENT: '/-!' .*? '-/' -> channel(HIDDEN);

BLOCK_DOC_COMMENT: '/--' .*? '-/' -> channel(HIDDEN);

BLOCK_COMMENT: '/-' .*? '-/' -> channel(HIDDEN);

NEWLINE: '\n' -> channel(HIDDEN);

WS: [ \t\r]+ -> channel(HIDDEN);

// ----------------------------------------------------------------------------- Template literal
// modes (portable; no target-language code)
// -----------------------------------------------------------------------------

mode INTERP_TOP;

TEMPLATE_TAIL:
	'}' (TEMPLATE_CHUNK_CHAR | '$' ~'{')* '`' -> popMode;

TEMPLATE_MIDDLE: '}' (TEMPLATE_CHUNK_CHAR | '$' ~'{')* '${';

INTERP_TEMPLATE_NO_SUBST:
	'`' (TEMPLATE_CHUNK_CHAR | '$' ~'{')* '`' -> type(TEMPLATE_NO_SUBST);

INTERP_TEMPLATE_HEAD:
	'`' (TEMPLATE_CHUNK_CHAR | '$' ~'{')* '${' -> type(TEMPLATE_HEAD), pushMode(INTERP_TOP);

INTERP_LBRACE: '{' -> type(LBRACE), pushMode(INTERP_NESTED);

// Keywords.
I_KW_AND: 'and' -> type(KW_AND);
I_KW_AS: 'as' -> type(KW_AS);
I_KW_MATCH: 'match' -> type(KW_MATCH);
I_KW_DATA: 'data' -> type(KW_DATA);
I_KW_DEFER: 'defer' -> type(KW_DEFER);
I_KW_ELSE: 'else' -> type(KW_ELSE);
I_KW_ERASED: 'erased' -> type(KW_ERASED);
I_KW_EXPORT: 'export' -> type(KW_EXPORT);
I_KW_HIDDEN: 'hidden' -> type(KW_HIDDEN);
I_KW_IF: 'if' -> type(KW_IF);
I_KW_IMPORT: 'import' -> type(KW_IMPORT);
I_KW_IN: 'in' -> type(KW_IN);
I_KW_KNOWN: 'known' -> type(KW_KNOWN);
I_KW_LET: 'let' -> type(KW_LET);
I_KW_MUT: 'mut' -> type(KW_MUT);
I_KW_NOT: 'not' -> type(KW_NOT);
I_KW_OR: 'or' -> type(KW_OR);
I_KW_SHAPE: 'shape' -> type(KW_SHAPE);
I_KW_PIN: 'pin' -> type(KW_PIN);
I_KW_RECUR: 'recur' -> type(KW_RECUR);
I_KW_THEN: 'then' -> type(KW_THEN);
I_KW_UNSAFE: 'unsafe' -> type(KW_UNSAFE);
I_KW_WHERE: 'where' -> type(KW_WHERE);
I_KW_XOR: 'xor' -> type(KW_XOR);
I_KW_YIELD: 'yield' -> type(KW_YIELD);

// Fixed tokens (maximal munch).
I_COLON_QUESTION_GT: ':?>' -> type(COLON_QUESTION_GT);
I_COLON_GT: ':>' -> type(COLON_GT);
I_COLON_EQ: ':=' -> type(COLON_EQ);
I_DOT_DOT_DOT: '...' -> type(DOT_DOT_DOT);
I_DOT_DOT_LT: '..<' -> type(DOT_DOT_LT);
I_DOT_DOT: '..' -> type(DOT_DOT);
I_DOT_LBRACKET: '.[' -> type(DOT_LBRACKET);
I_DOT_LPAREN: '.(' -> type(DOT_LPAREN);
I_QUESTION_QUESTION: '??' -> type(QUESTION_QUESTION);
I_EQ_GT: '=>' -> type(EQ_GT);
I_MINUS_GT: '->' -> type(MINUS_GT);
I_TILDE_EQ: '~=' -> type(TILDE_EQ);
I_SLASH_EQ: '/=' -> type(SLASH_EQ);
I_LT_EQ: '<=' -> type(LT_EQ);
I_GT_EQ: '>=' -> type(GT_EQ);
I_PIPE_EQ: '|=' -> type(PIPE_EQ);
I_PIPE_GT: '|>' -> type(PIPE_GT);

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
I_QUESTION: '?' -> type(QUESTION);
I_BANG: '!' -> type(BANG);
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
		DEC_DIGITS '.' DEC_DIGITS EXP_PART? FLOAT_SUFFIX?
		| '.' DEC_DIGITS EXP_PART? FLOAT_SUFFIX?
		| DEC_DIGITS EXP_PART FLOAT_SUFFIX?
	) -> type(FLOAT_LIT);

I_INT_LIT:
	(
		'0x' HEX_DIGITS
		| '0o' OCT_DIGITS
		| '0b' BIN_DIGITS
		| DEC_DIGITS
	) INT_SUFFIX? -> type(INT_LIT);

I_STRING_LIT:
	'"' (ESC_SEQ | ~["\\\r\n])* '"' -> type(STRING_LIT);

I_RUNE_LIT:
	'\'' (ESC_SEQ | ~['\\\r\n])* '\'' -> type(RUNE_LIT);

// Idents.
I_IDENT: LETTER (LETTER | DIGIT | UNDERSCORE)* -> type(IDENT);

// User-defined symbolic operators.
I_SYMBOLIC_OP: SYM_CHAR SYM_CHAR+ -> type(SYMBOLIC_OP);

// Trivia (hidden channel).
I_LINE_MODULE_DOC_COMMENT:
	'--!' ~[\r\n]* -> type(LINE_MODULE_DOC_COMMENT), channel(HIDDEN);

I_LINE_DOC_COMMENT:
	'---' ~[\r\n]* -> type(LINE_DOC_COMMENT), channel(HIDDEN);

I_LINE_COMMENT:
	'--' ~[\r\n]* -> type(LINE_COMMENT), channel(HIDDEN);

I_BLOCK_MODULE_DOC_COMMENT:
	'/-!' .*? '-/' -> type(BLOCK_MODULE_DOC_COMMENT), channel(HIDDEN);

I_BLOCK_DOC_COMMENT:
	'/--' .*? '-/' -> type(BLOCK_DOC_COMMENT), channel(HIDDEN);

I_BLOCK_COMMENT:
	'/-' .*? '-/' -> type(BLOCK_COMMENT), channel(HIDDEN);

I_NEWLINE: '\n' -> type(NEWLINE), channel(HIDDEN);

I_WS: [ \t\r]+ -> type(WS), channel(HIDDEN);

mode INTERP_NESTED;

N_INTERP_TEMPLATE_NO_SUBST:
	'`' (TEMPLATE_CHUNK_CHAR | '$' ~'{')* '`' -> type(TEMPLATE_NO_SUBST);

N_INTERP_TEMPLATE_HEAD:
	'`' (TEMPLATE_CHUNK_CHAR | '$' ~'{')* '${' -> type(TEMPLATE_HEAD), pushMode(INTERP_TOP);

N_INTERP_LBRACE:
	'{' -> type(LBRACE), pushMode(INTERP_NESTED);

N_INTERP_RBRACE: '}' -> type(RBRACE), popMode;

// Keywords.
N_KW_AND: 'and' -> type(KW_AND);
N_KW_AS: 'as' -> type(KW_AS);
N_KW_MATCH: 'match' -> type(KW_MATCH);
N_KW_DATA: 'data' -> type(KW_DATA);
N_KW_DEFER: 'defer' -> type(KW_DEFER);
N_KW_ELSE: 'else' -> type(KW_ELSE);
N_KW_ERASED: 'erased' -> type(KW_ERASED);
N_KW_EXPORT: 'export' -> type(KW_EXPORT);
N_KW_HIDDEN: 'hidden' -> type(KW_HIDDEN);
N_KW_IF: 'if' -> type(KW_IF);
N_KW_IMPORT: 'import' -> type(KW_IMPORT);
N_KW_IN: 'in' -> type(KW_IN);
N_KW_KNOWN: 'known' -> type(KW_KNOWN);
N_KW_LET: 'let' -> type(KW_LET);
N_KW_MUT: 'mut' -> type(KW_MUT);
N_KW_NOT: 'not' -> type(KW_NOT);
N_KW_OR: 'or' -> type(KW_OR);
N_KW_SHAPE: 'shape' -> type(KW_SHAPE);
N_KW_PIN: 'pin' -> type(KW_PIN);
N_KW_RECUR: 'recur' -> type(KW_RECUR);
N_KW_THEN: 'then' -> type(KW_THEN);
N_KW_UNSAFE: 'unsafe' -> type(KW_UNSAFE);
N_KW_WHERE: 'where' -> type(KW_WHERE);
N_KW_XOR: 'xor' -> type(KW_XOR);
N_KW_YIELD: 'yield' -> type(KW_YIELD);

// Fixed tokens (maximal munch).
N_COLON_QUESTION_GT: ':?>' -> type(COLON_QUESTION_GT);
N_COLON_GT: ':>' -> type(COLON_GT);
N_COLON_EQ: ':=' -> type(COLON_EQ);
N_DOT_DOT_DOT: '...' -> type(DOT_DOT_DOT);
N_DOT_DOT_LT: '..<' -> type(DOT_DOT_LT);
N_DOT_DOT: '..' -> type(DOT_DOT);
N_DOT_LBRACKET: '.[' -> type(DOT_LBRACKET);
N_DOT_LPAREN: '.(' -> type(DOT_LPAREN);
N_QUESTION_QUESTION: '??' -> type(QUESTION_QUESTION);
N_EQ_GT: '=>' -> type(EQ_GT);
N_MINUS_GT: '->' -> type(MINUS_GT);
N_TILDE_EQ: '~=' -> type(TILDE_EQ);
N_SLASH_EQ: '/=' -> type(SLASH_EQ);
N_LT_EQ: '<=' -> type(LT_EQ);
N_GT_EQ: '>=' -> type(GT_EQ);
N_PIPE_EQ: '|=' -> type(PIPE_EQ);
N_PIPE_GT: '|>' -> type(PIPE_GT);

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
N_QUESTION: '?' -> type(QUESTION);
N_BANG: '!' -> type(BANG);
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
		DEC_DIGITS '.' DEC_DIGITS EXP_PART? FLOAT_SUFFIX?
		| '.' DEC_DIGITS EXP_PART? FLOAT_SUFFIX?
		| DEC_DIGITS EXP_PART FLOAT_SUFFIX?
	) -> type(FLOAT_LIT);

N_INT_LIT:
	(
		'0x' HEX_DIGITS
		| '0o' OCT_DIGITS
		| '0b' BIN_DIGITS
		| DEC_DIGITS
	) INT_SUFFIX? -> type(INT_LIT);

N_STRING_LIT:
	'"' (ESC_SEQ | ~["\\\r\n])* '"' -> type(STRING_LIT);

N_RUNE_LIT:
	'\'' (ESC_SEQ | ~['\\\r\n])* '\'' -> type(RUNE_LIT);

// Idents.
N_IDENT: LETTER (LETTER | DIGIT | UNDERSCORE)* -> type(IDENT);

// User-defined symbolic operators.
N_SYMBOLIC_OP: SYM_CHAR SYM_CHAR+ -> type(SYMBOLIC_OP);

// Trivia (hidden channel).
N_LINE_MODULE_DOC_COMMENT:
	'--!' ~[\r\n]* -> type(LINE_MODULE_DOC_COMMENT), channel(HIDDEN);

N_LINE_DOC_COMMENT:
	'---' ~[\r\n]* -> type(LINE_DOC_COMMENT), channel(HIDDEN);

N_LINE_COMMENT:
	'--' ~[\r\n]* -> type(LINE_COMMENT), channel(HIDDEN);

N_BLOCK_MODULE_DOC_COMMENT:
	'/-!' .*? '-/' -> type(BLOCK_MODULE_DOC_COMMENT), channel(HIDDEN);

N_BLOCK_DOC_COMMENT:
	'/--' .*? '-/' -> type(BLOCK_DOC_COMMENT), channel(HIDDEN);

N_BLOCK_COMMENT:
	'/-' .*? '-/' -> type(BLOCK_COMMENT), channel(HIDDEN);

N_NEWLINE: '\n' -> type(NEWLINE), channel(HIDDEN);

N_WS: [ \t\r]+ -> type(WS), channel(HIDDEN);

// Fragments.
fragment LETTER: [A-Za-z];
fragment DIGIT: [0-9];
fragment HEXDIGIT: [0-9a-fA-F];
fragment SYM_CHAR: [*+\-/%=<>\\];

fragment DEC_DIGITS: DIGIT (DIGIT | '_' DIGIT)*;
fragment HEX_DIGITS: HEXDIGIT (HEXDIGIT | '_' HEXDIGIT)*;
fragment OCT_DIGITS: [0-7] ([0-7] | '_' [0-7])*;
fragment BIN_DIGITS: [01] ([01] | '_' [01])*;

fragment EXP_PART: [eE] [+-]? DEC_DIGITS;

fragment INT_SUFFIX:
	'i8'
	| 'i16'
	| 'i32'
	| 'i64'
	| 'n8'
	| 'n16'
	| 'n32'
	| 'n64'
	| 'i'
	| 'n';

fragment FLOAT_SUFFIX: 'f32' | 'f64' | 'f';

fragment ESC_SEQ:
	'\\\\' (
		['"`$\\nrt0]
		| 'x' HEXDIGIT HEXDIGIT
		| 'u' HEXDIGIT HEXDIGIT HEXDIGIT HEXDIGIT (
			HEXDIGIT HEXDIGIT
		)?
	);

fragment TEMPLATE_CHUNK_CHAR: ESC_SEQ | '$' ~'{' | ~[`\\$];

// Musi Language - ANTLR4 Lexer Grammar
// 
// Canonical lexer half of the tool-supported Musi grammar. Template interpolation modes live here
// because ANTLR only allows modes in lexer grammars.

lexer grammar MusiLexer;

// ----------------------------------------------------------------------------- Lexer

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
KW_USING: 'using';
KW_WHERE: 'where';
KW_XOR: 'xor';

// Fixed tokens (maximal munch).
COLON_EQ: ':=';
COLON_QUESTION_GT: ':?>';
DOT_DOT_DOT: '...';
DOT_LBRACE: '.{';
DOT_LBRACKET: '.[';
EQ_GT: '=>';
MINUS_GT: '->';
SLASH_EQ: '/=';
LT_EQ: '<=';
LT_COLON: '<:';
GT_EQ: '>=';
PIPE_GT: '|>';
TILDE_GT: '~>';
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
LINE_DOC_COMMENT: '///' ~[\r\n]* -> channel(HIDDEN);

LINE_COMMENT: '//' ~[\r\n]* -> channel(HIDDEN);

BLOCK_DOC_COMMENT: '/**' .*? '*/' -> channel(HIDDEN);

BLOCK_COMMENT: '/*' .*? '*/' -> channel(HIDDEN);

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

INTERP_DOT_LBRACE:
	'.{' -> type(DOT_LBRACE), pushMode(INTERP_NESTED);

INTERP_LBRACE: '{' -> type(LBRACE), pushMode(INTERP_NESTED);

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
I_KW_USING: 'using' -> type(KW_USING);
I_KW_WHERE: 'where' -> type(KW_WHERE);
I_KW_XOR: 'xor' -> type(KW_XOR);

// Fixed tokens (maximal munch).
I_COLON_EQ: ':=' -> type(COLON_EQ);
I_COLON_QUESTION_GT: ':?>' -> type(COLON_QUESTION_GT);
I_DOT_DOT_DOT: '...' -> type(DOT_DOT_DOT);
I_DOT_LBRACKET: '.[' -> type(DOT_LBRACKET);
I_EQ_GT: '=>' -> type(EQ_GT);
I_MINUS_GT: '->' -> type(MINUS_GT);
I_SLASH_EQ: '/=' -> type(SLASH_EQ);
I_LT_EQ: '<=' -> type(LT_EQ);
I_LT_COLON: '<:' -> type(LT_COLON);
I_GT_EQ: '>=' -> type(GT_EQ);
I_PIPE_GT: '|>' -> type(PIPE_GT);
I_TILDE_GT: '~>' -> type(TILDE_GT);
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

N_INTERP_TEMPLATE_NO_SUBST:
	'`' (TEMPLATE_CHUNK_CHAR | '$' ~'{')* '`' -> type(TEMPLATE_NO_SUBST);

N_INTERP_TEMPLATE_HEAD:
	'`' (TEMPLATE_CHUNK_CHAR | '$' ~'{')* '${' -> type(TEMPLATE_HEAD), pushMode(INTERP_TOP);

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
N_KW_USING: 'using' -> type(KW_USING);
N_KW_WHERE: 'where' -> type(KW_WHERE);
N_KW_XOR: 'xor' -> type(KW_XOR);

// Fixed tokens (maximal munch).
N_COLON_EQ: ':=' -> type(COLON_EQ);
N_COLON_QUESTION_GT: ':?>' -> type(COLON_QUESTION_GT);
N_DOT_DOT_DOT: '...' -> type(DOT_DOT_DOT);
N_DOT_LBRACKET: '.[' -> type(DOT_LBRACKET);
N_EQ_GT: '=>' -> type(EQ_GT);
N_MINUS_GT: '->' -> type(MINUS_GT);
N_SLASH_EQ: '/=' -> type(SLASH_EQ);
N_LT_EQ: '<=' -> type(LT_EQ);
N_LT_COLON: '<:' -> type(LT_COLON);
N_GT_EQ: '>=' -> type(GT_EQ);
N_PIPE_GT: '|>' -> type(PIPE_GT);
N_TILDE_GT: '~>' -> type(TILDE_GT);
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
fragment SYM_CHAR: [*+\-/%=<>\\];

fragment DEC_DIGITS: DIGIT (DIGIT | '_' DIGIT)*;
fragment HEX_DIGITS: HEXDIGIT (HEXDIGIT | '_' HEXDIGIT)*;
fragment OCT_DIGITS: [0-7] ([0-7] | '_' [0-7])*;
fragment BIN_DIGITS: [01] ([01] | '_' [01])*;

fragment EXP_PART: [eE] [+-]? DEC_DIGITS;

fragment ESC_SEQ:
	'\\\\' (
		['"`$\\nrt0]
		| 'x' HEXDIGIT HEXDIGIT
		| 'u' HEXDIGIT HEXDIGIT HEXDIGIT HEXDIGIT (
			HEXDIGIT HEXDIGIT
		)?
	);

fragment TEMPLATE_CHUNK_CHAR: ESC_SEQ | '$' ~'{' | ~[`\\$];

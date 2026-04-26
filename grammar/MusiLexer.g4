// Musi Language - ANTLR4 Lexer Grammar
// 
// Canonical lexer half of the tool-supported Musi grammar. Template interpolation modes live here
// because ANTLR only allows modes in lexer grammars.

lexer grammar MusiLexer;

// ----------------------------------------------------------------------------- Lexer

// Keywords. Form keywords construct syntax forms; they are never callable identifiers. For example,
// `import("a")` is an import form, not a call to a value named `import`.
KW_AND: 'and';
KW_AS: 'as';
KW_ASK: 'ask';
KW_ANY: 'any';
KW_ANSWER: 'answer';
KW_CATCH: 'catch';
KW_MATCH: 'match';
KW_NATIVE: 'native';
KW_OPAQUE: 'opaque';
KW_DATA: 'data';
KW_EFFECT: 'effect';
KW_EXPORT: 'export';
KW_GIVEN: 'given';
KW_LAW: 'law';
KW_PARTIAL: 'partial';
KW_HANDLE: 'handle';
KW_IF: 'if';
KW_IMPORT: 'import';
KW_IN: 'in';
KW_KNOWN: 'known';
KW_LET: 'let';
KW_MUT: 'mut';
KW_REC: 'rec';
KW_NOT: 'not';
KW_OR: 'or';
KW_REQUIRE: 'require';
KW_QUOTE: 'quote';
KW_RESUME: 'resume';
KW_SHAPE: 'shape';
KW_PIN: 'pin';
KW_UNSAFE: 'unsafe';
KW_SHL: 'shl';
KW_SHR: 'shr';
KW_SOME: 'some';
KW_WHERE: 'where';
KW_XOR: 'xor';

// Fixed tokens (maximal munch).
COLON_EQ: ':=';
COLON_QUESTION_GT: ':?>';
DOT_DOT_DOT: '...';
DOT_DOT_LT: '..<';
DOT_DOT: '..';
DOT_LBRACKET: '.[';
// `.(` selects first-class operator members such as `Eq.(=)` or `x?.(+)`.
DOT_LPAREN: '.(';
QUESTION_DOT: '?.';
BANG_DOT: '!.';
QUESTION_QUESTION: '??';
EQ_GT: '=>';
MINUS_GT: '->';
SLASH_EQ: '/=';
LT_EQ: '<=';
LT_COLON: '<:';
LT_DOT_DOT_LT: '<..<';
LT_DOT_DOT: '<..';
GT_EQ: '>=';
PIPE_GT: '|>';
TILDE_GT: '~>';
TILDE_EQ: '~=';
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
I_KW_ASK: 'ask' -> type(KW_ASK);
I_KW_ANY: 'any' -> type(KW_ANY);
I_KW_ANSWER: 'answer' -> type(KW_ANSWER);
I_KW_CATCH: 'catch' -> type(KW_CATCH);
I_KW_MATCH: 'match' -> type(KW_MATCH);
I_KW_NATIVE: 'native' -> type(KW_NATIVE);
I_KW_OPAQUE: 'opaque' -> type(KW_OPAQUE);
I_KW_DATA: 'data' -> type(KW_DATA);
I_KW_EFFECT: 'effect' -> type(KW_EFFECT);
I_KW_EXPORT: 'export' -> type(KW_EXPORT);
I_KW_GIVEN: 'given' -> type(KW_GIVEN);
I_KW_LAW: 'law' -> type(KW_LAW);
I_KW_PARTIAL: 'partial' -> type(KW_PARTIAL);
I_KW_HANDLE: 'handle' -> type(KW_HANDLE);
I_KW_IF: 'if' -> type(KW_IF);
I_KW_IMPORT: 'import' -> type(KW_IMPORT);
I_KW_IN: 'in' -> type(KW_IN);
I_KW_KNOWN: 'known' -> type(KW_KNOWN);
I_KW_LET: 'let' -> type(KW_LET);
I_KW_MUT: 'mut' -> type(KW_MUT);
I_KW_REC: 'rec' -> type(KW_REC);
I_KW_NOT: 'not' -> type(KW_NOT);
I_KW_OR: 'or' -> type(KW_OR);
I_KW_REQUIRE: 'require' -> type(KW_REQUIRE);
I_KW_QUOTE: 'quote' -> type(KW_QUOTE);
I_KW_RESUME: 'resume' -> type(KW_RESUME);
I_KW_SHAPE: 'shape' -> type(KW_SHAPE);
I_KW_PIN: 'pin' -> type(KW_PIN);
I_KW_UNSAFE: 'unsafe' -> type(KW_UNSAFE);
I_KW_SHL: 'shl' -> type(KW_SHL);
I_KW_SHR: 'shr' -> type(KW_SHR);
I_KW_SOME: 'some' -> type(KW_SOME);
I_KW_WHERE: 'where' -> type(KW_WHERE);
I_KW_XOR: 'xor' -> type(KW_XOR);

// Fixed tokens (maximal munch).
I_COLON_EQ: ':=' -> type(COLON_EQ);
I_COLON_QUESTION_GT: ':?>' -> type(COLON_QUESTION_GT);
I_DOT_DOT_DOT: '...' -> type(DOT_DOT_DOT);
I_DOT_LBRACKET: '.[' -> type(DOT_LBRACKET);
I_DOT_LPAREN: '.(' -> type(DOT_LPAREN);
I_QUESTION_DOT: '?.' -> type(QUESTION_DOT);
I_BANG_DOT: '!.' -> type(BANG_DOT);
I_QUESTION_QUESTION: '??' -> type(QUESTION_QUESTION);
I_EQ_GT: '=>' -> type(EQ_GT);
I_MINUS_GT: '->' -> type(MINUS_GT);
I_SLASH_EQ: '/=' -> type(SLASH_EQ);
I_LT_EQ: '<=' -> type(LT_EQ);
I_LT_COLON: '<:' -> type(LT_COLON);
I_LT_DOT_DOT_LT: '<..<' -> type(LT_DOT_DOT_LT);
I_LT_DOT_DOT: '<..' -> type(LT_DOT_DOT);
I_GT_EQ: '>=' -> type(GT_EQ);
I_PIPE_GT: '|>' -> type(PIPE_GT);
I_TILDE_GT: '~>' -> type(TILDE_GT);
I_TILDE_EQ: '~=' -> type(TILDE_EQ);
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
N_KW_ASK: 'ask' -> type(KW_ASK);
N_KW_ANY: 'any' -> type(KW_ANY);
N_KW_ANSWER: 'answer' -> type(KW_ANSWER);
N_KW_CATCH: 'catch' -> type(KW_CATCH);
N_KW_MATCH: 'match' -> type(KW_MATCH);
N_KW_NATIVE: 'native' -> type(KW_NATIVE);
N_KW_OPAQUE: 'opaque' -> type(KW_OPAQUE);
N_KW_DATA: 'data' -> type(KW_DATA);
N_KW_EFFECT: 'effect' -> type(KW_EFFECT);
N_KW_EXPORT: 'export' -> type(KW_EXPORT);
N_KW_GIVEN: 'given' -> type(KW_GIVEN);
N_KW_LAW: 'law' -> type(KW_LAW);
N_KW_PARTIAL: 'partial' -> type(KW_PARTIAL);
N_KW_HANDLE: 'handle' -> type(KW_HANDLE);
N_KW_IF: 'if' -> type(KW_IF);
N_KW_IMPORT: 'import' -> type(KW_IMPORT);
N_KW_IN: 'in' -> type(KW_IN);
N_KW_KNOWN: 'known' -> type(KW_KNOWN);
N_KW_LET: 'let' -> type(KW_LET);
N_KW_MUT: 'mut' -> type(KW_MUT);
N_KW_REC: 'rec' -> type(KW_REC);
N_KW_NOT: 'not' -> type(KW_NOT);
N_KW_OR: 'or' -> type(KW_OR);
N_KW_REQUIRE: 'require' -> type(KW_REQUIRE);
N_KW_QUOTE: 'quote' -> type(KW_QUOTE);
N_KW_RESUME: 'resume' -> type(KW_RESUME);
N_KW_SHAPE: 'shape' -> type(KW_SHAPE);
N_KW_PIN: 'pin' -> type(KW_PIN);
N_KW_UNSAFE: 'unsafe' -> type(KW_UNSAFE);
N_KW_SHL: 'shl' -> type(KW_SHL);
N_KW_SHR: 'shr' -> type(KW_SHR);
N_KW_SOME: 'some' -> type(KW_SOME);
N_KW_WHERE: 'where' -> type(KW_WHERE);
N_KW_XOR: 'xor' -> type(KW_XOR);

// Fixed tokens (maximal munch).
N_COLON_EQ: ':=' -> type(COLON_EQ);
N_COLON_QUESTION_GT: ':?>' -> type(COLON_QUESTION_GT);
N_DOT_DOT_DOT: '...' -> type(DOT_DOT_DOT);
N_DOT_LBRACKET: '.[' -> type(DOT_LBRACKET);
N_DOT_LPAREN: '.(' -> type(DOT_LPAREN);
N_QUESTION_DOT: '?.' -> type(QUESTION_DOT);
N_BANG_DOT: '!.' -> type(BANG_DOT);
N_QUESTION_QUESTION: '??' -> type(QUESTION_QUESTION);
N_EQ_GT: '=>' -> type(EQ_GT);
N_MINUS_GT: '->' -> type(MINUS_GT);
N_SLASH_EQ: '/=' -> type(SLASH_EQ);
N_LT_EQ: '<=' -> type(LT_EQ);
N_LT_COLON: '<:' -> type(LT_COLON);
N_GT_EQ: '>=' -> type(GT_EQ);
N_PIPE_GT: '|>' -> type(PIPE_GT);
N_TILDE_GT: '~>' -> type(TILDE_GT);
N_TILDE_EQ: '~=' -> type(TILDE_EQ);
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

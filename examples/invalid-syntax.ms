// ============================================================================
// INVALID SYNTAX EXAMPLES
// ============================================================================
// yhis file contains examples of syntax that should produce parse errors.
// each example is followed by comment indicating what error should occur.
//
// NOTE: currently many of these parse without errors due to lenient parser.
// these should be fixed to provide better error detection.

// ============================================================================
// MATCH EXPRESSION ERRORS
// ============================================================================

// ERROR: missing comma between match cases (currently allowed but shouldn't be)
fn area(s: Shape): Float32 {
  match s {
  case Circle(r) => 3.14159 * r ** 2        // should require comma here
  case Rect(w, h) => w * h                  // this line should error
  case Triangle(a, b, c) => {               // this line should error
    val s := (a + b + c) / 2.0;
    return sqrt(s * (s - a) * (s - b) * (s - c));
  }
  }
}

// ERROR: missing comma in match case with multiple patterns
match value {
case A, B => "A or B"    // missing comma
case C => "C"
}

// ERROR: missing comma in record pattern
match obj {
case Point.{ x := 1      // missing comma
           y := 2 } => x + y
}

// ============================================================================
// FUNCTION AND CALL ERRORS
// ============================================================================

// ERROR: function call without parentheses (should be required)
fn test() {
  val result := writeln "hello world";  // should require writeln("hello world")
}

// ERROR: missing parentheses in function definition
fn add a: Int32 b: Int32: Int32 {  // should be fn add(a: Int32, b: Int32): Int32
  return a + b;
}

// ERROR: missing comma in function parameters
fn test(a: Int32 b: Int32) {  // should require comma
  return a + b;
}

// ERROR: missing comma in function arguments
val result := add(1 2);  // should require comma

// ============================================================================
// ARRAY AND TUPLE ERRORS
// ============================================================================

// ERROR: array indexing without brackets
val x := arr[0;   // should require ]

// ERROR: array literal without commas
val arr := [1 2 3];  // should require commas

// ERROR: tuple without commas
val tuple := (1 2 3);  // should require commas

// ERROR: array type without closing bracket
val arr: Int32[ = [];  // should require ]

// ============================================================================
// RECORD AND TYPE ERRORS
// ============================================================================

// ERROR: record fields without commas
record Point {
  x: Int32    // should require comma
  y: Int32
}

// ERROR: record literal without commas
val p := Point.{ x := 1 y := 2 };  // should require comma

// ERROR: Type parameters without commas
alias MyType<T U> = (T, U);  // should require comma

// ERROR: missing angle bracket closure
val x: List<Int = [];  // should require >

// ============================================================================
// STATEMENT ERRORS
// ============================================================================

// ERROR: missing semicolon after statement
fn bad() {
  val x := 1
  val y := 2      // should require semicolon after previous line
}

// ERROR: missing semicolon in block
{
  val a := 1
  val b := 2      // should require semicolon
}

// ============================================================================
// PATTERN MATCHING ERRORS
// ============================================================================

// ERROR: pattern without comma
match value {
case A, B => "multiple"    // should require comma
}

// ERROR: tuple pattern without commas
match tuple {
case (a b) => a + b         // should require commas
}

// ERROR: array pattern without commas
match arr {
case [a b c] => a + b + c   // should require commas
}

// ============================================================================
// BRACKET AND BRACE ERRORS
// ============================================================================

// ERROR: unmatched parentheses
val x := (1 + 2;  // should require )

// ERROR: unmatched braces
fn broken() {
  return 1;     // should require }

// ERROR: unmatched brackets
val arr := [1, 2, 3;  // should require ]

// ============================================================================
// KEYWORD AND IDENTIFIER ERRORS
// ============================================================================

// ERROR: invalid identifier
val invalid-name := 1;  // should error (hyphens not allowed)

// ERROR: missing keyword
test() {
  return 1;  // ,issing 'fn' before test
}

// ERROR: invalid keyword usage
val := 1;  // should be 'val x := 1'

// ============================================================================
// TYPE ANNOTATION ERRORS
// ============================================================================

// ERROR: missing colon in type annotation
val x Int32 := 1;  // should be 'val x: Int32 := 1'

// ERROR: missing colon in record field
record Bad {
  x Int32        // should be 'x: Int32'
}

// ERROR: missing colon in function parameter
fn bad(param Int32) {  // should be 'param: Int32'
  return param;
}

// ============================================================================
// OPERATOR ERRORS
// ============================================================================

// ERROR: missing operator
val x := 1 +;  // should require right operand

// ERROR: invalid operator placement
val x := + 1;  // should require left operand or different syntax

// ERROR: chain without separators
val x := 1+2+ 3;  // missing operand after last +

// ============================================================================
// IMPORT AND EXTERN ERRORS
// ============================================================================

// ERROR: import without string
import;  // should be 'import "module"'

// ERROR: extern without braces
extern "C";  // should be extern "C" { ... }

// ERROR: extern function without semicolon
extern "C" {
  fn malloc(size: Nat64): ^Any  // should require ;
}

// ============================================================================
// LITERAL ERRORS
// ============================================================================

// ERROR: unterminated string
val s := "unterminated;  // should require closing "

// ERROR: invalid escape sequence
val s := "\x";  // should require valid hex digits

// ERROR: invalid number format
val x := 0xG;  // should require valid hex digits

// ERROR: rune without closing quote
val c := 'x;  // should require closing '

// ============================================================================
// ADVANCED SYNTAX ERRORS
// ============================================================================

// ERROR: template string without closing
val s := $"Hello {name;  // should require }

// ERROR: attribute without closing
[<export;  // should require >]

// ERROR: type annotation in wrong place
fn test(): Int32 (x: Int32) {  // parens should come before colon

// ERROR: nested record without proper syntax
val r := Point.{ x := 1 { y := 2 } };  // should be proper nested syntax

// ============================================================================
// RECOVERY AND ERROR CASCADING
// ============================================================================

// these should show how parser recovers from errors and prevents cascading

// multiple errors on same line
val x: Int32 := "string"; y: String := 123;

// errors that should not cascade
fn broken_function(a: Int32, b: String {
  return a + b;  // missing closing paren in parameter list
}

// ============================================================================
// EDGE CASES
// ============================================================================

// empty constructs that should error
val empty_tuple := ();  // this should be valid actually
val empty_array := [];  // this should be valid
match x {}  // empty match should error

// single element constructs
val single := (1,);  // should this be valid tuple or error?
val array := [1,];   // should this be valid array or error?

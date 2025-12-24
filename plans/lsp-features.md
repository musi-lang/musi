# LSP Feature Implementation Plan - Musi Language

## Current State Assessment

### ✅ Fully Implemented Features

- **TextMate Highlighting**: Syntax highlighting via VSCode extensions
- **Document Synchronization**: Full text document sync with open/change events
- **Syntax Diagnostics**: Parse errors with squiggles in editor and error reporting
- **Document Symbols**: Comprehensive symbol extraction including:
  - Functions with parameter symbols and signatures
  - Records with field symbols
  - Sum types with case symbols
  - Variables (val/var) and constants
  - Type aliases and imports
- **Semantic Tokens**: Full implementation with token classification (functions, variables, enum members)
- **LSP Server**: Complete protocol implementation with proper error handling
- **Symbol Hierarchy**: Detailed symbol kinds (Function, Struct, Enum, Variable, Constant, Field, EnumMember, Parameter, Module)

### 🟡 Partially Implemented

- **Semantic Tokens**: Code complete but disabled in server capabilities (commented out)

### ❌ Missing Core Features

The missing features that would achieve 100% completion:

## Musi Language Overview

**Key Syntax Features** (from actual grammar):

- **Bindings**: `val x := 42;` (immutable), `var y := 0;` (mutable)
- **Functions**: `fn add(x: Int32, y: Int32): Int32 { x + y };`
- **Records**: `record Point { x: Bin32; y: Bin32 };`
- **Sums**: `sum Option { case Some(value), case None };`
- **Pattern Matching**: `match opt { case Some(v) => use(v), case None => {} };`
- **Control Flow**: `if`, `while`, `for`, `match`, `try`
- **Template Strings**: `$"Hello {name}"`
- **Import**: `import "@std/io";`
- **Extern**: `extern "C" unsafe { fn malloc(size: Nat64): ^Any; };`

## Phase 1: Essential Missing Features (Immediate Implementation)

### 1. Enable Semantic Tokens

**Priority**: High
**Complexity**: Low
**Dependencies**: Already implemented, just needs activation

**Implementation Strategy:**

- Uncomment semantic tokens in server capabilities
- Test semantic token functionality in VSCode
- Verify token classification accuracy

**Files to Modify:**

- `tools/lsp/lib/lsp_server.ml`: Uncomment semantic tokens capability

**Expected Features:**

```musi
// Semantic highlighting for different token types
val PI := 3.14159;         // -> Constant token highlighting
fn add(a: Int32, b: Int32) // -> Function + parameter highlighting
Point.x                    // -> Variable + field highlighting
```

### 2. Code Completion

**Priority**: High
**Complexity**: Medium
**Dependencies**: `lib/lex`, `lib/ast`, `lib/parse`

**Implementation Strategy:**

- Keyword completion: `val`, `var`, `fn`, `record`, `sum`, `if`, `while`, `match`, etc.
- Identifier completion from current document
- Type name completion from standard library
- Import path completion

**Files to Modify:**

- `tools/lsp/lib/lsp_server.ml`: Add completion handler
- `tools/lsp/lib/lsp_completion.ml`: New file for completion logic
- `lib/lex/token.ml`: Extract keyword list for completion

**Expected Features:**

```musi
// Keywords: val, var, fn, record, sum, if, while, match, etc.
v<completion shows: val, var>

// Function completion
fn <completion shows function signatures from current scope>

// Record field completion
Point.{ x := 1.0, <completion shows: y := >

// Import completion
import "@std/<completion shows: io, math, etc>"
```

### 3. Code Formatting

**Priority**: High
**Complexity**: Medium
**Dependencies**: `lib/ast`, `lib/parse`

**Implementation Strategy:**

- AST-based formatting for consistent indentation
- Expression and statement formatting
- Function parameter alignment
- Record/sum type formatting

**Files to Modify:**

- `tools/lsp/lib/lsp_server.ml`: Add formatting handler
- `tools/lsp/lib/lsp_format.ml`: New file for formatting logic

**Expected Features:**

```musi
// Before formatting:
val x:=1+2;fn foo(a:Int32,b:String): Int32{return a;};

// After formatting:
val x := 1 + 2;

fn foo(a: Int32, b: String): Int32 {
  return a;
};

// Records formatting:
record Point{x:Bin32;y:Bin32};

// Becomes:
record Point {
  x: Bin32;
  y: Bin32
};
```

## Phase 2: Advanced Features (Require lib/sema)

### 1. Type-Aware Completion

**Priority**: High
**Complexity**: High
**Dependencies**: `lib/sema` (symbol resolution)

**Features:**

- Type-based completion suggestions
- Method/field completion on records
- Import completion with module information

### 2. Go to Definition

**Priority**: High
**Complexity**: High
**Dependencies**: `lib/sema`

**Features:**

- Jump to function/variable definitions
- Cross-file navigation
- Parameter and field definitions

### 3. Hover Information

**Priority**: Medium
**Complexity**: Medium
**Dependencies**: `lib/sema`

**Features:**

- Type information on hover
- Function signatures
- Variable declarations

### 4. Find References

**Priority**: Medium
**Complexity**: High
**Dependencies**: `lib/sema`

**Features:**

- Find all references to symbols
- Rename symbol support
- Cross-file reference counting

### 5. Advanced Code Actions

**Priority**: Low
**Complexity**: High
**Dependencies**: `lib/sema`

**Features:**

- Auto-import missing modules
- Type-based refactorings
- Generate function stubs

## Implementation Priority Order

### 1. Enable Semantic Tokens (Day 1)

**Focus**: Immediate productivity boost with syntax highlighting

**Implementation Steps:**

1. Uncomment semantic tokens in server capabilities
2. Test in VSCode to ensure proper highlighting
3. Verify token classification accuracy
4. Performance testing

**Success Criteria:**

- Semantic highlighting works in VSCode
- Correct token classification (functions, variables, etc.)
- No performance degradation
- <10ms response time

### 2. Code Completion (Week 1)

**Focus**: Most valuable for developer productivity

**Implementation Steps:**

1. Extract Musi keywords from lexer/token definitions
2. Implement basic identifier completion from current document
3. Add type name completion (Int32, String, Bool, etc.)
4. Test completion accuracy and performance

**Success Criteria:**

- 90%+ accuracy for Musi keywords
- Fast completion (<50ms response)
- Works well with large files

### 3. Code Formatting (Week 2)

**Focus**: Improves code consistency and readability

**Implementation Steps:**

1. Design formatting rules for Musi syntax
2. Implement AST-based formatter
3. Add formatting LSP handler
4. Test formatting consistency

**Success Criteria:**

- Consistent output across different input styles
- Handles complex nested expressions
- Preserves user preferences where possible

## Technical Implementation Details

### Current Implementation Architecture

```ocaml
(* tools/lsp/lib/lsp_symbols.ml - Already Implemented *)
type implemented_symbol =
  | Function of string * fn_sig * span
  | Record of string * record_field list * span
  | Sum of string * sum_case list * span
  | Variable of string * ty option * span
  | Constant of string * lit * span
  | Parameter of string * ty option * span
  | Import of string * span

(* Complete symbol collection with hierarchy and details *)
val collect_symbols : source:string -> ast:stmt list -> Types.DocumentSymbol.t list
```

### Semantic Tokens Implementation (Ready to Enable)

```ocaml
(* tools/lsp/lib/lsp_tokens.ml - Already Implemented *)
val collect_tokens : source:string -> ast:stmt list -> int list

(* Token classification for:
   - Functions (callable entities)
   - Variables (val/var bindings)
   - EnumMembers (capitalized identifiers)
   - Parameters (function arguments)
*)
```

### Completion Architecture (To Implement)

```ocaml
(* tools/lsp/lib/lsp_completion.ml - To Implement *)
type completion_item = {
    label: string;
    kind: CompletionItemKind.t;
    detail: string option;
    documentation: string option
}

val handle_completion :
  uri:string ->
  position:Lsp.Types.Position.t ->
  completion_item list

(* Musi-specific completion functions *)
val complete_musi_keywords : unit -> string list
val complete_identifiers : source:string -> position:int -> string list
val complete_type_names : unit -> string list
val complete_imports : base_path:string -> string list
```

### Formatting Architecture (To Implement)

```ocaml
(* tools/lsp/lib/lsp_format.ml - To Implement *)
type format_options = {
    indent_size: int;
    max_line_length: int;
    align_params: bool
}

val format_document :
  source:string ->
  ast:stmt list ->
  format_options ->
  formatted_source:string

(* Musi-specific formatting *)
val format_binding : expr -> string
val format_function : expr -> string
val format_record : expr -> string
val format_sum : expr -> string
```

## Testing Strategy

### Unit Tests

- Semantic token accuracy tests (functions vs variables vs enum members)
- Symbol extraction tests for all implemented language constructs
- Completion accuracy tests with real Musi code
- Formatting consistency tests across syntax variations

### Integration Tests

- Full LSP workflow tests with semantic tokens enabled
- VSCode extension integration
- Error handling and recovery tests
- Performance testing with large files

### Performance Tests

- Semantic token response time (<10ms target)
- Large file handling (10k+ lines)
- Completion response time (<50ms target)
- Memory usage optimization

## Success Metrics

1. **Semantic Tokens**:
   - Accurate token classification for all Musi constructs
   - <10ms response time
   - Good integration with VSCode semantic highlighting

2. **Completion**:
   - 90%+ accuracy for Musi keywords and identifiers
   - <50ms response time
   - Works with files up to 10k lines

3. **Formatting**:
   - Consistent output for all Musi syntax
   - Handles complex nested expressions
   - Preserves semantic meaning

4. **Symbols** (Already Implemented):
   - Complete coverage of all Musi constructs
   - <100ms resolution time
   - Good integration with VSCode features

5. **Reliability**:
   - <1% crash rate during normal usage
   - Graceful handling of parse errors
   - Clear error messages for users

## Implementation Notes

### Current Implementation Strengths

1. **Robust Symbol Collection**: Already handles all major Musi constructs with proper hierarchy and detailed information
2. **Semantic Token Framework**: Complete implementation ready for activation
3. **Error Handling**: Graceful degradation when parsing fails
4. **Performance**: Efficient AST traversal and symbol extraction

### Musi Language Specific Considerations

1. **Keyword Completion**: Focus on Musi-specific keywords like `val`, `var`, `fn`, `record`, `sum`, `match`, `case`, etc.

2. **Type System**: Support for gradual typing with type inference means completion should handle both annotated and unannotated code.

3. **Pattern Matching**: Special consideration for `case` patterns in completion.

4. **Template Strings**: `$"..."` syntax requires special handling.

5. **Import System**: Support for both `std/` and relative imports.

### Error Handling

- All features should degrade gracefully if AST parsing fails
- Parse errors should not crash the LSP server
- Error messages should be actionable for users
- Consider incremental features that work even with partial parsing

## Next Steps

1. **Enable semantic tokens** (immediate 5-minute task)
2. **Test semantic highlighting** in VSCode
3. **Implement code completion** (highest value missing feature)
4. **Add code formatting** for consistency
5. **Plan advanced features** requiring lib/sema

## References

- Language Specification: `docs/LANGUAGE.md`
- Grammar: `grammar.ebnf`
- Example Syntax: `tools/vscode/syntaxes/syntax.ms`
- Contributing Guidelines: `CONTRIBUTING.md`
- VSCode Extension: `tools/vscode/`

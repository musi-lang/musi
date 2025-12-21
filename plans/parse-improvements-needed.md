# Parser Improvements Needed - Specific Issues

## 1. Match Expression Comma Handling (CRITICAL)

**Current Issue**: Line 590 in `lib/parse/parser.ml`

```ocaml
ignore (match_token p [ Token.Comma; Token.Semicolon ]);
```

**Problem**: This ignores whether commas exist between match cases, allowing invalid syntax like:

```musi
match value {
case A => "A"
case B => "B"    // Missing comma - should error
}
```

**Fix Needed**: Replace with proper comma checking that errors on missing separators.

## 2. Generic List Parsing Issues

**Current Issue**: `parse_list` function (line 79-84) uses `match_token` which silently ignores missing separators.

**Problem**: This affects:

- Function parameters: `fn test(a: Int32 b: Int32)` (missing comma)
- Function arguments: `add(1 2)` (missing comma)
- Array elements: `[1 2 3]` (missing commas)
- Type parameters: `List<Int String>` (missing comma)
- Record fields: `{ x: Int32 y: Int32 }` (missing semicolon (trailing last allowed))

**Fix Needed**: Make separator handling strict for contexts that require them.

## 3. Statement Semicolon Handling

**Current Issue**: `parse_expr_block` (line 547) uses lenient semicolon checking.

**Problem**: Allows missing semicolons:

```musi
{
  val x := 1    // Missing semicolon - should error
  val y := 2
}
```

**Fix Needed**: Require semicolons between statements in blocks.

## 4. Function Signature Parsing

**Current Issue**: Function parameter parsing may be too lenient.

**Problem**: Need to verify that function signatures properly validate:

- Parameter separator commas
- Type annotation colons
- Parameter name requirements

## 5. Record and Type Definition Parsing

**Current Issue**: Record fields and sum cases may not strictly require separators.

**Problem**: Need to ensure:

- Record fields require commas or semicolons
- Sum case parameters require commas
- Type parameter lists require commas

## Implementation Strategy

### Phase 1: Critical Match Expression Fix

- Replace `ignore (match_token ...)` with explicit comma checking
- Add error messages for missing separators
- Test with various match patterns

### Phase 2: List Separator Strictness

- Create strict vs lenient list parsing functions
- Apply strict parsing to: function params, args, array lits, type params
- Keep lenient parsing for contexts where it's appropriate (like record field defaults)

### Phase 3: Statement Semicolon Enforcement

- Make semicolon requirements stricter in blocks
- Ensure proper error recovery and reporting

### Phase 4: Testing and Validation

- Create comprehensive test cases for each improvement
- Verify error messages are clear and actionable
- Test error recovery doesn't break valid syntax

## Error Message Examples

**Match cases missing comma**:

```text
error: expected ',' after 'match' case
  case A => "A"
             ^
```

**Function parameters missing comma**:

```text
error: expected ',' between parameter list
fn test(a: Int32 b: Int32)
                ^
```

**Array elements missing comma**:
t

```text
error: expected ',' between element list
[1 2 3]
  ^
```

## Testing Strategy

1. **Unit Tests**: Test each parsing function with invalid syntax
2. **Integration Tests**: Test error recovery with complex invalid code
3. **Error Message Tests**: Verify error locations and messages are accurate
4. **Regression Tests**: Ensure valid syntax still parses correctly

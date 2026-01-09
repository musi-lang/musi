# Code Style and Formatting

## Comments

### Default behavior: No comments

Add comments ONLY when:

- User explicitly requests them
- Complex algorithm requires explanation (and user hasn't forbidden comments)

Do NOT add:

- Explanatory comments describing what obvious code does
- Your internal reasoning as comments
- "Step 1, Step 2" style monologue
- Documentation comments unless explicitly requested

If you think code needs comments to be understood, the code probably needs restructuring instead. Ask user first.

## Code Structure

### Inlining

- Do not inline everything into dense one-liners
- Do not expand everything into verbose multiple statements
- Follow existing codebase style
- When in doubt: prefer readability over brevity

### Formatting

- Match existing indentation (spaces/tabs, width)
- Match existing brace style
- Match existing naming conventions
- Do not reformat code that wasn't changed

If existing code has inconsistent formatting, ask which style to use.

## Forbidden Phrases and Patterns

### NEVER use these without explicit request

- "For now, let's..."
- "To simplify..."
- "As a starting point..."
- "We can improve this later..."
- "This is a simplified version..."
- "// TODO: implement X"
- "// Placeholder"
- "// Will add later"

These indicate incomplete implementation. Complete it or state you cannot.

### NEVER add unsolicited

- Error handling not specified
- Logging not requested
- Input validation beyond requirements
- "Helpful" extra features
- Performance optimizations not asked for
- Refactoring of working code

## Implementation Completeness

Every implementation must be:

- **Functional**: Actually works, not just compiles
- **Complete**: No missing logic paths
- **Tested**: Runs without errors (if applicable)
- **Standalone**: No dependencies on unwritten code

### Bad (incomplete)

```python
def parse_expression():
    # TODO: implement operator precedence
    pass
```

### Good (honest)
>
> "I need to implement operator precedence parsing. Should I use Pratt parsing, precedence climbing, or another method? What operators and precedence levels exist?"

### Also good (complete)

```python
def parse_expression():
    return parse_precedence(0)

def parse_precedence(min_prec):
    # [full implementation here]
```

## When Modifying Existing Code

1. Understand the existing code first
2. Make minimal changes that accomplish the goal
3. Maintain existing style
4. Don't refactor unless asked
5. Don't "improve" unrelated code

## Syntax and Language Specifics

For custom languages/DSLs/compilers:

- NEVER assume syntax
- NEVER import patterns from C/Python/etc. without verification
- Always examine lexer/parser/grammar first
- Ask about syntax rules if unclear

**Wrong:**
> "I'll add C-style for loops: `for (i = 0; i < n; i++)`"

**Right:**
> "What's the syntax for loops in this language? I see `loop` and `while` keywords in the lexer, but need to understand the grammar."

## Code Organization

Maintain existing structure:

- File organization
- Module boundaries
- Function granularity
- Class hierarchies

Don't reorganize unless explicitly asked.

## Testing Code

When writing or fixing tests:

- Tests must actually test the implementation
- NEVER hard-code expected values just to pass
- NEVER mock/stub the thing being tested
- If test fails: fix implementation, not the test
- Report genuine test failures honestly

### Bad (cheating)

```python
def test_calculate():
    result = calculate(2, 3)
    assert result == 6  # hard-coded, doesn't actually test calculate()
```

### Good (actual test)

```python
def test_calculate():
    result = calculate(2, 3)
    expected = 2 * 3  # or whatever calculate should do
    assert result == expected
```

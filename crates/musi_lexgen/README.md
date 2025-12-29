# musi_lexgen

Tool for building lexers using Rust enums.

## What it does

Lexer turns text into tokens (small parts like keywords or symbols).
This tool builds lexer automatically from enum definition.

## Features

- **Longest Match**: Finds longest symbols first (matches `..<` before `..`).
- **Keyword Lookup**: Finds words like `fn` or `val`.
- **Space Skipping**: Ignores spaces or new lines automatically.
- **Easy API**: Returns `Option(Token)`. Returns `None` for unknown text.
- **Position Tracking**: Gives start and end position for every part found.

## How to use

Define enum and add `Lexer` derive:

```rust
use musi_lexgen::Lexer;

#[derive(Lexer, Debug, PartialEq)]
#[lexer(skip = r"[ \t\n\r]+")] // ignore spaces
pub enum Token {
    #[token("fn")]     KwFn,    // matches 'fn'
    #[token("val")]    KwVal,   // matches 'val'
    #[token("..<")]    DotDotLt, // matches '..<' before '..'
    #[token("..")]     DotDot,
    #[token(".")]      Dot,
}

fn main() {
    let input = "fn val ..<";
    let mut lexer = Token::lexer(input);

    // get parts one by one
    while let Some((token, start, end)) = lexer.next() {
        println!("{token:?} found at {start}..{end}");
    }
}
```

## How it works

Tool builds fast matching logic.
Words starting with letters or underscores match keywords.
Other symbols match using longest pattern first.
Unknown text returns `None` with span found.

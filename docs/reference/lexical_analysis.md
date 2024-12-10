# Lexical Analysis

When you write Musi code, the compiler first breaks it down into small pieces
called tokens. This process is called lexical analysis. Think of it like
breaking a sentence into words - before we can understand the meaning, we need
to know where each word starts and ends.

For example:

```musi
let x := 5 + 3
```

...is broken down into the following tokens:

- `Let` let
- `Identifier` x
- `ColonEquals` :=
- `Literal(Number)` 5
- `Plus` +
- `Literal(Number)` 3
- `Eof`

You might wonder, _"Why is there `Eof` at the end?"_. The `Eof` (End of File)
token is like the last page of a storybook - it tells us we've reached the end.
This helps the computer know when to stop reading, just like how you know a
story is finished when you see "The End".

### What is Cursor?

The Cursor is like your finger following words as you read a book. It helps us
keep track of exactly where we are in the code.

### How does the Cursor work?

The Cursor starts at the beginning and does three main jobs, just like following
a treasure map:

1. Keeps track of position:
   - Points to the current letter, like your finger under a word in a book
   - Remembers which line it's on, like knowing which page you're reading
   - Can peek ahead without moving, like glancing at the next word in a sentence

2. Moves through the code:
   - Goes forward letter by letter, like walking step by step
   - Can jump ahead when needed, like taking a shortcut
   - Stops at the end, like reaching the X on a treasure map

3. Helps find patterns:
   - Spots special symbols like `:=` or `->` or `=>`, like finding signposts on
     a path
   - Groups letters into words, like sorting building blocks by colour
   - Collects text between positions, like picking apples between two trees

For example, when reading:

```musi
let hello := "world"
```

The Cursor moves through each character, like following stepping stones across a
river:

1. Starts at `l` in `let`
2. Steps through each letter one at a time
3. Groups `let` as one word, like putting puzzle pieces together
4. Spots the `:=` pattern, like finding a special mark on the map
5. Finds `"world"` between quotes, like finding a message in a bottle

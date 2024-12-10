## Introduction

This document describes the Musi programming language.

I have chosen to use simple UK English when writing this documentation. I prefer
UK English spelling, and as someone once said, "An idiot admires complexity, a
genius admires simplicity." I aim to keep things simple because I often find
documents like the Rust Book difficult to understand, as they use complex words
that go beyond my B2-level English. Many people reading this may speak English
as their second or third language.

It would not be helpful to include too many implementation details in this
reference document - these may change, and different implementations may work in
different ways. At present, musi-rs is the main version of the language that
will later be rewritten in itself. For now, all new features will be added to
the Rust version (which works as a testing ground) until we create a mature
self-contained compiler, which will then replace the Rust version.

## Implementations

As mentioned earlier, there is a main version of Musi written in Rust.

Other versions may be added to this table below:

| Name    | Description                                                                                    | Website                                             |
| ------- | ---------------------------------------------------------------------------------------------- | --------------------------------------------------- |
| musi-rs | The original version of Musi, written in Rust. New language features will usually appear here. | [musi-lang/musi](https://github.com/musi-lang/musi) |

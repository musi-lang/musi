---
title: "Results, I/O, and Effects"
description: "Translate Rust `Result` and I/O boundary habits into Musi stdlib results and I/O."
group: "Musi for Developers"
section: "Rust Developers"
order: 8
slug: "results-effects"
summary: "Rust `Result` maps to `@std/result`; standard input starts with `@std/io`."
---

Rust often uses `Result` for recoverable failure:

```rust
fn read_port(text: &str) -> Result<u16, String> {
    text.parse::<u16>().map_err(|_| String::from("bad port"))
}

let parsed = Ok(8080);
parsed
```

Musi uses `@std/result` for the same recoverable shape.

{{snippet:rust-result-data}}

That keeps recoverable failure in the value, with helpers for fallback or mapping.

Rust also uses `Result` for outside work such as reading from standard input:

```rust
fn read_line() -> std::result::Result<String, std::io::Error> {
    let mut line = String::new();
    std::io::stdin().read_line(&mut line)?;
    Ok(line)
}
```

Musi user code reaches for `@std/io` first. The runtime request stays under that wrapper.

{{snippet:rust-effect-request}}

Use stdlib results when the value itself can be one of several outcomes. Use stdlib I/O for ordinary terminal boundaries. Define custom effects when the program needs a capability that is not already in the standard library.

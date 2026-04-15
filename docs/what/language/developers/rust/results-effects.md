---
title: "Results, Requests, and Effects"
description: "Translate Rust `Result` and I/O boundary habits into Musi data and effects."
group: "Musi for Developers"
section: "Rust Developers"
order: 8
slug: "results-effects"
summary: "Rust `Result` maps to data for recoverable failure; outside work maps to effect requests."
---

Rust often uses `Result` for recoverable failure:

```rust
fn read_port(text: &str) -> Result<u16, String> {
    text.parse::<u16>().map_err(|_| String::from("bad port"))
}

let parsed = Ok(8080);
parsed
```

Musi can model that same recoverable shape as data.

{{snippet:rust-result-data}}

That is ordinary data: a value is either `Ok` or `Error`.

Rust also uses `Result` for outside work such as reading from standard input:

```rust
fn read_line() -> std::result::Result<String, std::io::Error> {
    let mut line = String::new();
    std::io::stdin().read_line(&mut line)?;
    Ok(line)
}
```

Musi writes outside work as a request to an effect.

{{snippet:rust-effect-request}}

Use data results when the value itself can be one of several outcomes. Use effects when the computation asks an outside interpreter to do work.

---
title: "Runtime"
description: "Learn what runtime-backed imports are for and why they are separate from stdlib helpers."
group: "Effects and Runtime"
section: "Effects and Runtime"
order: 29
slug: "runtime"
summary: "Use musi:runtime for runtime-backed capabilities and host services."
---
Runtime operations are the built-in doors to the host system. They cover things such as process data, file access, logging, time, randomness, text helpers, and path handling.

{{snippet:chapter-runtime}}

Treat runtime calls as boundary work. The result may depend on the machine, the current process, a file on disk, or the moment the code runs.

## Boundary code stays narrow

Read from the runtime at the edge of the program, then move back to ordinary values. A backup tool may read file text through the runtime, but parsing the backup manifest should be a plain function that accepts text.

## Naming the outside world

Runtime names should make the outside dependency obvious. `fsReadText`, `timeMonotonicMs`, and `envGet` tell different stories. That clarity helps formatting, highlighting, diagnostics, and human review all agree on what the code is doing.

Effect chapters draw a line between local calculation and outside answers. Adding prices is local. Reading time, asking a process, or writing a log needs an answer from outside the expression.

A useful test is to ask whether the result could be known from the input alone. If yes, keep the function plain. If no, name the request and make the effect boundary visible.

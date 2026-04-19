---
title: "Imports and Exports"
description: "Use imports and exports after package shape is clear."
group: "Code Organization"
section: "Code Organization"
order: 17
slug: "imports-and-exports"
summary: "Bring code in explicitly and expose only what other files need."
---
Imports say what this file depends on. Exports say what this file offers to others. Together they form the public hallway between rooms in the package.

{{snippet:chapter-imports-and-exports}}

A clean import list helps readers see the file's outside needs before reading the body. A clean export list keeps accidental helpers from becoming promises.

## Imports should feel boring

If an import surprises you, the file may be doing work from the wrong layer. A receipt formatter importing low-level runtime operations is a smell unless formatting truly owns that boundary.

## Exports create commitments

Once another file uses an exported name, changing it becomes a package-wide decision. Export the stable idea, not every helper that made the idea possible.

Imports and exports are also the first map a new reader gets. A file that imports payment, clock, and logging code announces that it crosses a boundary. A file that exports only `formatReceipt` announces that the rest is private craft.

Organization chapters are about where code lives. A package is like a small shop, files are shelves, imports are items brought in from another shelf, and exports are items placed at the counter for other code to use.

Messy organization usually starts when every file reaches for every name. Keep imports narrow, export only the names another file needs, and let file names explain the job before a reader opens them.

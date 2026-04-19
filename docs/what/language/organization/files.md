---
title: "Files"
description: "Separate file reading from package structure to reduce beginner overload."
group: "Code Organization"
section: "Code Organization"
order: 15
slug: "files"
summary: "Know what a single file means before building a package."
---
Files are the first boundary a reader sees. A good file has one main reason to exist: payment totals, animal records, calendar rules, or command-line entry points.

{{snippet:chapter-files}}

Keep related declarations together and move unrelated ones apart. A file that contains car maintenance rules and user-login code will confuse both readers and tooling.

## File names as signs

Name files like signs in a building. A person should know which door to open before reading every declaration inside. `invoice.ms`, `address.ms`, and `runtime.ms` each promise a different topic.

## Private helpers stay nearby

If a helper only serves one exported declaration, keep it close. If several files need it, move it behind a small exported API instead of copying the helper around the package.

Organization chapters are about where code lives. A package is like a small shop, files are shelves, imports are items brought in from another shelf, and exports are items placed at the counter for other code to use.

Messy organization usually starts when every file reaches for every name. Keep imports narrow, export only the names another file needs, and let file names explain the job before a reader opens them.

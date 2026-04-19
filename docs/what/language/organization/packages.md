---
title: "Packages"
description: "Learn package roots and entry files after single-file work makes sense."
group: "Code Organization"
section: "Code Organization"
order: 16
slug: "packages"
summary: "Move from one file to package-managed code without changing mental models."
---
A package is where files, imports, tests, and configuration agree on one project. It is the difference between a single recipe card and a small cookbook with chapters.

{{snippet:chapter-packages}}

Use packages when code has a name, a set of source files, and repeatable commands. The manifest keeps that information in one place so editors, formatters, tests, and command-line tools do not guess differently.

## What belongs together

Put files in the same package when they ship and change together. A restaurant app might keep menu, order, and receipt code together. A reusable date library should not live inside that app if other projects will use it.

## Configuration is part of the contract

Formatter width, source roots, and package metadata affect how the project is read. Keeping those values in the package makes behavior predictable outside one developer's editor.

Organization chapters are about where code lives. A package is like a small shop, files are shelves, imports are items brought in from another shelf, and exports are items placed at the counter for other code to use.

Messy organization usually starts when every file reaches for every name. Keep imports narrow, export only the names another file needs, and let file names explain the job before a reader opens them.

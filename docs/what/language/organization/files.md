---
title: "Files"
description: "Separate file reading from package structure to reduce beginner overload."
group: "Code organization"
section: "Code organization"
order: 15
slug: "files"
summary: "Know what a single file means before building a package."
---

{{snippet:first-file}}

## What

A Musi file is a direct unit of source.

## Why

Readers usually understand one file before they understand package manifests.

## How

- Start with one `index.ms`.
- Keep related code close while learning.
- Move to package structure only when it helps.

## Try it

- Create a scratch file.
- Rename it once.
- Run `music check` on it.

## Common mistake

Do not jump into manifest details before one-file flow feels ordinary.

## Next

Continue to [Packages](/docs/language/organization/packages).

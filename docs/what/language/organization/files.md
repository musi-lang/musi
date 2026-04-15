---
title: "Files"
description: "Separate file reading from package structure to reduce beginner overload."
group: "Code organization"
section: "Code organization"
order: 15
slug: "files"
summary: "Know what a single file means before building a package."
---

{{snippet:chapter-files}}

## In this chapter

A Musi file is direct unit of source code.
The smallest useful file can hold bindings and a final expression, and that same top-to-bottom reading model continues even after programs get larger.
This page exists so package structure never hides the simpler truth that code still starts in files.

## Why it matters

People often jump into package layout too early and lose track of where language code actually lives.
If docs only explain package commands, beginners still ask what one file means, what belongs in it, and how much ceremony it needs.
Anchoring on the file keeps later organization topics grounded.

## Walk through it

Read the example as one self-contained source file.
Everything important is visible in one place: one binding, one final result, one clear evaluation path.
When learning, create scratch files freely, keep related code close together, and only reach for multi-file organization when the single-file story starts feeling cramped.

## Try it next

- Create one scratch Musi file.
- Put one binding and one final expression in it.
- Check it directly with `music check`.

## Common mistake

Do not assume package structure replaces the need to understand one-file evaluation flow.

## Next

Continue to [Packages](/docs/language/organization/packages) to see when a single file stops being enough.

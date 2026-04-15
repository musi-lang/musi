---
title: "Files"
description: "Separate file reading from package structure to reduce beginner overload."
group: "Code Organization"
section: "Code Organization"
order: 15
slug: "files"
summary: "Know what a single file means before building a package."
---

A file is the first module boundary most readers see. It gives names a place to live and gives imports something concrete to point at.

{{snippet:chapter-files}}

## Reading Model

Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.

## Practical Rule

Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.

Continue to [Packages](/learn/book/organization/modules/packages).

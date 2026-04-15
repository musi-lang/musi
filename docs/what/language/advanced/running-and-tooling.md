---
title: "Running and Tooling"
description: "Bring the learning path back to commands and workflow once language basics are in place."
group: "Advanced and Tooling"
section: "Advanced and Tooling"
order: 34
slug: "running-and-tooling"
summary: "Finish with the everyday command flow for checking, running, and building code."
---

{{snippet:chapter-running-and-tooling}}

## Boundary Tool

Musi has everyday commands for package workflow and direct file workflow.
This page brings them together at end of language path so readers can connect all earlier examples to routine habits: check code, run packages, build outputs, and run tests.
The command surface is small enough to learn, but distinct enough to deserve a final summary.

## When to Reach for It

After learning syntax, users still need operational confidence.
They want to know which command to run when checking a file, when to use package commands, and how testing fits into normal iteration.
A workflow chapter turns scattered command knowledge into repeatable practice.

## Read the Boundary

Read `musi run`, `musi check`, `musi build`, and `musi test` as package-root commands for project lifecycle.
Read `music check index.ms`, `music build index.ms`, and `music run index.seam` as direct lane for single-file or lower-level work.
When in doubt, ask first whether you are inside a package or handling one file directly; that decision usually picks the right command family immediately.

## Small Exercise

- Run one direct `music check` on a scratch file.
- Run one package command inside generated project.
- Use `musi test` after adding one tiny test.

## Mistake to Avoid

Do not memorize commands as flat list; group them by direct-file lane versus package lane.

## Next Page

Continue back through any chapter you need, now that you have both language model and workflow model tied together.

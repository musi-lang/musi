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

Read `musi run`, `musi check`, `musi build`, `musi fmt`, and `musi test` as package-root commands for project lifecycle.
Read `music check index.ms`, `music build index.ms`, and `music run index.seam` as direct lane for single-file or lower-level work.
Read `music disasm index.ms` as the SEAM HIL view and `music disasm --level seam index.ms` as the lowered `.seam` IL view.
Read `musi fmt` as the Deno-style formatter for `.ms` files and Musi code fences in Markdown.
Configure it through `musi.json` under `fmt`.
The formatter starts from `profile` (`standard`, `compact`, or `expanded`), then applies explicit overrides such as `lineWidth`, `indentWidth`, `useTabs`, `trailingCommas`, `bracePosition`, `matchArmIndent`, `matchArmArrowAlignment`, `callArgumentLayout`, `declarationParameterLayout`, `recordFieldLayout`, `effectMemberParameterLayout`, and `operatorBreak`.
Use `matchArmIndent` to choose pipe-aligned or block-indented match arms, and `matchArmArrowAlignment` to choose whether `=>` stays compact or aligns within match-arm runs.
Use the `*Layout` fields to keep fitting groups automatic or force block layout.
The CLI can override common style choices with `musi fmt --profile`, `--match-arm-indent`, `--match-arm-arrow-alignment`, and `--operator-break`.
Read hover and semantic highlighting in editors as sema-backed: record members stay properties, dot-callable UDNS heads (`value.name(...)`) stay dot-callables.
When in doubt, ask first whether you are inside a package or handling one file directly; that decision usually picks the right command family immediately.

## Small Exercise

- Run one direct `music check` on a scratch file.
- Run one package command inside generated project.
- Use `musi test` after adding one tiny test.

## Mistake to Avoid

Do not memorize commands as flat list; group them by direct-file lane versus package lane.

## Next Page

Continue back through any chapter you need, now that you have both language model and workflow model tied together.

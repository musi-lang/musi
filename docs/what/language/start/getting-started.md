---
title: "Getting Started"
description: "Install prerequisites, install Musi with script or Cargo, and learn when to use music versus musi."
group: "Start"
section: "Start"
order: 1
slug: "getting-started"
summary: "Install tools, install Musi, and learn the two command lanes."
---

Getting started in Musi has two tracks: direct file work and package work. The commands are separate because they answer different questions.

{{snippet:chapter-getting-started}}

`music` is the direct-file tool. Use it when one entry file is enough. It is the fast lane for learning syntax and testing a small idea.

`musi` is the package tool. Use it when a directory has `musi.json`, package imports, standard-library helpers, tests, targets, and project-level configuration.

Think of it like a notebook and a workshop. `music check index.ms` is the notebook: one page, one idea, fast feedback. `musi check` and `musi test` are the workshop: project layout, tests, and repeatable commands.

## First File

Create `index.ms` with a few bindings and a final expression. That final expression is the value the file leaves behind; there is no required `main` function.

## Package Start

Use `musi init hello` when the code needs a manifest. From that point, prefer `musi check` and `musi test` inside the package.

## Mistake to Avoid

Do not treat `music` and `musi` as duplicate spellings. Pick by scope: one file uses `music`; one package uses `musi`.

Continue to [First Program](/learn/book/start/foundations/first-program).
